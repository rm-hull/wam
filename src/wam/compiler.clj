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

(ns wam.compiler
  (:require
    [clojure.string :as s]
    [clojure.set :refer [union]]
    [wam.instruction-set :refer :all]
    [wam.store :refer [friendly]]
    [wam.parser :refer [parse-all]]
    [wam.grammar :as g]
    [wam.graph-search :refer :all]))

(defn register-names
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
    (register-names 'X)))

(def query-builder
  {:structure-walker
   dfs-post-order

   :instruction-builder
   (fn [term register seen? arg?]
     (if (seen? term)
       (list set-value register)
       (cond
         (instance? wam.grammar.Structure term)
         (list put-structure (:functor term) register)

         (instance? wam.grammar.Variable term)
         (list set-variable register)

         :else nil)))})


(def program-builder
  {:structure-walker
    dfs-pre-order

   :instruction-builder
   (fn [term register seen? arg?]
       (cond
         (instance? wam.grammar.Structure term)
         (if arg?
           (list unify-variable register)
           (list get-structure (:functor term) register))

         (instance? wam.grammar.Variable term)
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
  [builder term register-allocation]
  (let [structure-walker (:structure-walker builder)
        instruction-builder (:instruction-builder builder)]
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

(defn assoc-variables [ctx register-allocation]
  (->>
    register-allocation
    (filter #(instance? wam.grammar.Variable (first %)))
    (update ctx :variables concat)))

(defn single-step
  "Execute an instruction with respect to the supplied context, if
   the fail flag has not been set. If the context has failed, then
   just return the context unchanged (i.e. don't execute the instruction).
   This causes the remaining instructions to also fall through."
  [ctx [instr & args]]
  (if-not (:fail ctx)
    (do
      (when (:trace ctx)
        (println (friendly (cons instr args))))
      (apply instr ctx args))
    ctx))

(defn compile-term
  "Emits a sequence of instructions that equates to provided term according
   to the rules of the builder. Returns a function which is capable of
   executing the instructions given a context."
  [builder term]
  (let [register-allocation (register-allocation term)
        instrs (emit-instructions builder term register-allocation)]
    (fn [ctx]
      (reduce single-step (assoc-variables ctx register-allocation) instrs))))

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
                   (compile-term program-builder))]
    (executor ctx)))
