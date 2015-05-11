;; The MIT License (MIT)
;;
;; Copyright (c) 2015 Richard Hull
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(ns wam.l0.compiler
  (:require
    [clojure.set :refer [union]]
    [wam.l0.instruction-set :refer :all]
    [wam.l0.parser :refer [parse-all]]
    [wam.l0.grammar :as g]
    [wam.l0.graph-search :refer :all]))

(defn registers
  "Generates an infinite incrementing sequence of register symbols,
   e.g. X1, X2, X3, X4 ..."
  [prefix]
  (->>
    (iterate inc 1)
    (map #(symbol (str prefix %)))))

(defn register-allocation
  "Variable registers are allocated to a term on a least available index basis
   such that (1) register X1 is always allocated to the otutermost term, and
   (2) the same register is allocated to all occurrences of a given variable.
   For example, registers are allocated to the variables of the term
   p(Z, h(Z,W), f(W)) as follows:

     X1 = p(X2, X3, X4)
     X2 = Z
     X3 = h(X2, X5)
     X4 = f(X5)
     X5 = W

   This amounts to saying that a term is seen as a flattened conjunctive set
   of equations of the form Xi = X or Xi = f(Xi1, ..., Xin), n>=0 where
   the Xi's are all distinct new variable names.

   A hashmap is returned with key as the term, and the value as allocated
   register,"
  [term]
  (zipmap
    (bfs term)
    (registers 'X)))

(defn functor
  "Extracts the functor from a structure, and creates as a
   symbol suitable for consumption within the instruction set."
  [structure]
  (-> structure :functor pr-str symbol))

(def query-builder
  {:structure-walker
   dfs-post-order

   :instruction-builder
   (fn [term register seen? arg?]
     (if (seen? term)
       (list set-value register)
       (cond
         (instance? wam.l0.grammar.Structure term)
         (list put-structure (functor term) register)

         (instance? wam.l0.grammar.Variable term)
         (list set-variable register)

         :else nil)))})


(def program-builder
  {:structure-walker
    dfs-pre-order

   :instruction-builder
   (fn [term register seen? arg?]
       (cond
         (instance? wam.l0.grammar.Structure term)
         (if arg?
           (list unify-variable register)
           (list get-structure (functor term) register))

         (instance? wam.l0.grammar.Variable term)
         (if (seen? term)
           (list unify-value register)
           (list unify-variable register))

         :else nil))})

(defn compile-structure
  [instruction-builder structure register-allocation seen?]
  (loop [args (:args structure)
         seen? seen?
         result [(instruction-builder
                      structure
                      (register-allocation structure)
                      seen?
                      false)]]
    (if (empty? args)
      result
      (recur
        (rest args)
        (conj seen? (first args))
        (conj
          result
          (instruction-builder
            (first args)
            (register-allocation (first args))
            seen?
            true))))))

(defn emit-instructions
  "Constructs a sequence of instructions (missing the context argument)
   suitable for threading with a context. The builder determines how
   the structures in the term are walked (generally pre-order for
   programs, and post-order for queries), and emits the most
   appropriate instructions for each structure, which is reliant on
   which arguments have been previously processed."
  [builder term]
  (let [structure-walker (:structure-walker builder)
        instruction-builder (:instruction-builder builder)
        register-allocation (register-allocation term)]
    (loop [structures (structure-walker term)
           seen? #{}
           result []]
      (if (empty? structures)
        result
        (let [structure (first structures)]
          (recur
            (rest structures)
            (conj (union seen? (set (:args structure))) structure)
            (concat
              result
              (compile-structure
                instruction-builder
                structure
                register-allocation
                seen?))))))))

(defn exec
  "Execute an instruction with respect to the supplied context, if
   the fail flag has not been set. If the context has failed, then
   just return the context unchanged (i.e. don't execute the instruction).
   This causes the remaining instructions to also fall through."
  [ctx [instr & args]]
  (if-not (:fail ctx)
    (apply instr ctx args)
    ctx))

(defn compile-term
  "Emits a sequence of instructions that equates to provided term according
   to the rules of the builder. Returns a function which is capable of
   executing the instructions given a context."
  [builder term]
  (let [instrs (emit-instructions builder term)]
    (fn [ctx]
      (reduce exec ctx instrs))))

(defn query [ctx expression]
  (let [executor (->>
                   expression
                   (parse-all g/structure)
                   (compile-term query-builder))]
    (executor ctx)))

(defn program [ctx expression]
  (let [executor (->>
                   expression
                   (parse-all g/structure)
                   (compile-term program))]
    (executor ctx)))

(comment

  (use 'table.core)
  (use 'wam.l0.store)

  ; Some helper functions to get round limitations in table
  (defn inflate [table]
    (let [max-cols (reduce max 0 (map count table))]
      (map #(take max-cols (lazy-cat % (repeat nil))) table)))

  (defn headers [& headers]
    (fn [table] (cons headers table)))

  (def table' (comp table inflate (headers "instr" "arg1" "arg2")))

  (def x  (parse-all g/structure "p(Z, h(Z, W), f(W))") )
  (def y  (parse-all g/structure "p(f(X), h(Y, f(a)), Y)") )
  (def z  (parse-all g/structure "f(X, g(X,a))") )

  (table (register-allocation y))
  (table (register-allocation x))

;  +-----------------------------------+-------+
;  | key                               | value |
;  +-----------------------------------+-------+
;  | wam.l0.grammar.Structure@d2562d9f | X1    |
;  | wam.l0.grammar.Variable@d77f6b5c  | X2    |
;  | wam.l0.grammar.Structure@c8c464ec | X3    |
;  | wam.l0.grammar.Structure@b1308ecc | X4    |
;  | wam.l0.grammar.Variable@d77f7490  | X5    |
;  +-----------------------------------+-------+

  (table' (emit-instructions query-builder x))

;  +----------------------------------------------+------+------+
;  | instr                                        | arg1 | arg2 |
;  +----------------------------------------------+------+------+
;  | wam.l0.instruction_set$put_structure@14f613e | h|2  | X3   |
;  | wam.l0.instruction_set$set_variable@94c1c0   | X2   |      |
;  | wam.l0.instruction_set$set_variable@94c1c0   | X5   |      |
;  | wam.l0.instruction_set$put_structure@14f613e | f|1  | X4   |
;  | wam.l0.instruction_set$set_value@45176e      | X5   |      |
;  | wam.l0.instruction_set$put_structure@14f613e | p|3  | X1   |
;  | wam.l0.instruction_set$set_value@45176e      | X2   |      |
;  | wam.l0.instruction_set$set_value@45176e      | X3   |      |
;  | wam.l0.instruction_set$set_value@45176e      | X4   |      |
;  +----------------------------------------------+------+------+

  (table' (emit-instructions program-builder y))

;  +-----------------------------------------------+------+------+
;  | instr                                         | arg1 | arg2 |
;  +-----------------------------------------------+------+------+
;  | wam.l0.instruction_set$get_structure@1458d55  | p|3  | X1   |
;  | wam.l0.instruction_set$unify_variable@1c40c01 | X2   |      |
;  | wam.l0.instruction_set$unify_variable@1c40c01 | X3   |      |
;  | wam.l0.instruction_set$unify_variable@1c40c01 | X4   |      |
;  | wam.l0.instruction_set$get_structure@1458d55  | f|1  | X2   |
;  | wam.l0.instruction_set$unify_variable@1c40c01 | X5   |      |
;  | wam.l0.instruction_set$get_structure@1458d55  | h|2  | X3   |
;  | wam.l0.instruction_set$unify_value@f92e0d     | X4   |      |
;  | wam.l0.instruction_set$unify_variable@1c40c01 | X6   |      |
;  | wam.l0.instruction_set$get_structure@1458d55  | f|1  | X6   |
;  | wam.l0.instruction_set$unify_variable@1c40c01 | X7   |      |
;  | wam.l0.instruction_set$get_structure@1458d55  | a|0  | X7   |
;  +-----------------------------------------------+------+------+

(def ctx (make-context))

(def query0
  (->>
    "p(Z, h(Z, W), f(W))"
    (parse-all g/structure)
    (compile-term query-builder)))

(-> ctx query0 heap table)

;  +-----+---------+
;  | key | value   |
;  +-----+---------+
;  | 0   | [STR 1] |
;  | 1   | h|2     |
;  | 2   | [REF 2] |
;  | 3   | [REF 3] |
;  | 4   | [STR 5] |
;  | 5   | f|1     |
;  | 6   | [REF 3] |
;  | 7   | [STR 8] |
;  | 8   | p|3     |
;  | 9   | [REF 2] |
;  | 10  | [STR 1] |
;  | 11  | [STR 5] |
;  +-----+---------+

(def ctx1
(->
  ctx
  (query "f(X, g(X, a))")
  (query "f(b, Y)")))

 (->
  ctx1
  heap
  table
  )


;  +-----+----------+
;  | key | value    |
;  +-----+----------+
;  | 0   | [STR 1]  |
;  | 1   | a|0      |
;  | 2   | [STR 3]  |
;  | 3   | g|2      |
;  | 4   | [REF 4]  |
;  | 5   | [STR 1]  |
;  | 6   | [STR 7]  |   <-- a1
;  | 7   | f|2      |
;  | 8   | [REF 4]  |   <-- aX
;  | 9   | [STR 3]  |
;  | 10  | [STR 11] |
;  | 11  | b|0      |
;  | 12  | [STR 13] |   <-- a2
;  | 13  | f|2      |
;  | 14  | [STR 11] |
;  | 15  | [REF 15] |   <-- aY
;  +-----+----------+

  (->
    ctx1
    (unify 6 12)
    heap
    table)

;  +-----+----------+
;  | key | value    |
;  +-----+----------+
;  | 0   | [STR 1]  |
;  | 1   | a|0      |
;  | 2   | [STR 3]  |
;  | 3   | g|2      |
;  | 4   | [REF 4]  |
;  | 5   | [STR 1]  |
;  | 6   | [STR 7]  |   <-- a1
;  | 7   | f|2      |
;  | 8   | [REF 14] |   <-- aX
;  | 9   | [STR 3]  |
;  | 10  | [STR 11] |
;  | 11  | b|0      |
;  | 12  | [STR 13] |   <-- a2
;  | 13  | f|2      |
;  | 14  | [STR 11] |
;  | 15  | [REF 9]  |   <-- aY
;  +-----+----------+

(defn tee [v func]
  (func v)
  v)


(->
  ctx
  ; p(Z, h(Z, W), f(W))
  (put-structure 'h|2, 'X3)
  (set-variable 'X2)
  (set-variable 'X5)
  (put-structure 'f|1, 'X4)
  (set-value 'X5)
  (put-structure 'p|3, 'X1)
  (set-value 'X2)
  (set-value 'X3)
  (set-value 'X4)

  (tee (comp table heap))
  (tee (comp table registers))

  ; p(f(X), h(Y, f(a)), Y)
  (get-structure 'p|3, 'X1)
  (unify-variable 'X2)
  (unify-variable 'X3)
  (unify-variable 'X4)
  (get-structure 'f|1, 'X2)
  (unify-variable 'X5)
  (get-structure 'h|2, 'X3)
  (unify-value 'X4)
  (unify-variable 'X6)
  (get-structure 'f|1, 'X6)
  (unify-variable 'X7)
  (get-structure 'a|0, 'X7)

  (tee (comp table heap))
  (tee (comp table registers))
  )


)


