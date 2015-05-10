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


;; ℳ₀ machine instructions
(ns wam.l0.instruction-set
  (:require [clojure.string :refer [split]]))

(defn put-structure
  "This instruction marks the beginning of a structure (without
   embedded substructures) occurring as a goal argument. The
   instruction pushes the functor f|N for the structure onto the
   heap, and puts a corresponding structure pointer into register
   Xi. Execution then proceeds in \"write\" mode."
  [ctx f|N Xi]
  (let [h (get-in ctx [:pointer :h])
        v ['STR (inc h)]]
    (->
      ctx
      (assoc-in [:store h] v)
      (assoc-in [:store (inc h)] f|N)
      (assoc-in [:registers Xi] v)
      (update-in [:pointer :h] (partial + 2)))))

; Slight discrepancy between what David Warren refers to as 'put_variable'
; and Aït-Kaci as 'set_variable'
(defn set-variable
  "This instruction represents an argument of the final goal that is an
   unbound variable. THe instruction creates an unbound variable on the
   heap, and puts a reference to it in the Xi register."
  [ctx Xi]
  (let [h (get-in ctx [:pointer :h])
        v ['REF h]]
    (->
      ctx
      (assoc-in [:store h] v)
      (assoc-in [:registers Xi] v)
      (update-in [:pointer :h] inc))))

(defn set-value [ctx Xi]
  (let [h (get-in ctx [:pointer :h])
        v (get-in ctx [:registers Xi])]
    (->
      ctx
      (assoc-in [:store h] v)
      (update-in [:pointer :h] inc))))


(comment
  ; Exercise 2.1 (pg. 9)
  ; Compiled code for L0 query ?-p(Z,h(Z,W),f(W)).
  (use 'table.core)

  (def context {
    :fail false
    :mode :read
    :pointer {:h 0 :s 0}
    :store (sorted-map)
    :registers (sorted-map)})

  (def context
    (->
      context
      (put-structure 'h|2, 'X3)
      (set-variable 'X2)
      (set-variable 'X5)
      (put-structure 'f|1, 'X4)
      (set-value 'X5)
      (put-structure 'p|3, 'X1)
      (set-value 'X2)
      (set-value 'X3)
      (set-value 'X4)))

  (table (context :store))

  ; +-----+---------+
  ; | key | value   |
  ; +-----+---------+
  ; | 0   | [STR 1] |
  ; | 1   | h|2     |
  ; | 2   | [REF 2] |
  ; | 3   | [REF 3] |
  ; | 4   | [STR 5] |
  ; | 5   | f|1     |
  ; | 6   | [REF 3] |
  ; | 7   | [STR 8] |
  ; | 8   | p|3     |
  ; | 9   | [REF 2] |
  ; | 10  | [STR 1] |
  ; | 11  | [STR 5] |
  ; +-----+---------+
)

(defn deref
  "Follows a possible reference chain until it reaches either an unbound REF
   cell or a non-REF cell, the address of which it returns. The effect of
   dereferencing is none other than composing variable substitutions."
  [ctx addr]
  (let [cell (get-in ctx [:store addr])]
    (cond
      (not (seq? cell))
      addr

      (and (= (first cell) 'REF) (not= (second cell) addr))
      (recur ctx (second cell))

      :else addr)))

(def ^:private push conj)

(defn ^:private push-args
  "Push the folllwing n args onto the stack, counting up from v1,
   interleaving with v2, incrementing at each reduction."
  [stack n v1 v2]
  (reduce
    (fn [stack i] ( ->
                    stack
                    (push (+ v1 i))
                    (push (+ v2 i))))
    stack
    (range 1 (inc n))))

(defn ^:private arity
  "Determine the arity given a functor symbol representation"
  [functor]
  (-> functor name (split #"\|") second (Integer/parseInt)))

(def ^:private cell-value
  "Convenience wrapper to obtain the cell value"
  second)

(defn ^:private cell-type?
  "Function maker to determine if a cell is of a given type,
   presently the known types are REF (reference) or STR (structure)."
  [type]
  (fn [tag]
    (= (first tag) type)))

(def ^:private ref?
  "Convenience wrapper for REF cell types"
  (cell-type? 'REF))

(def ^:private str?
  "Convenience wrapper for STR cell types"
  (cell-type? 'STR))

(defn bind
  "Effectuate the binding of the heap cell to the address"
  [ctx addr ref]
  (let [cell (get-in ctx [:store addr])]
    (if (str? cell) ; dont overwrite STR, flip the ref / addr and try again
      (bind ctx ref addr)
      (assoc-in ctx [:store addr] ['REF ref]))))

(defn unify
  "Unification algorithm based on the UNION/FIND method [AHU74], where
   variable substitutions are built, applied, and composed through
   dereference pointers. The unification operation is performed on a
   pair of store addresses, and applied for all functors and their
   arguments, repeated iteratively until the stack is exhausted."
  [ctx a1 a2]

  (loop [ctx ctx
         fail false
         stack (-> [] (push a1) (push a2))]

    (if (or fail (empty? stack))
      (assoc ctx :fail fail)
      (let [d1 (deref ctx (peek stack))
            d2 (deref ctx (peek (pop stack)))
            stack (pop (pop stack))]
        (if (= d1 d2)
          (recur ctx fail stack)
          (let [cell1 (get-in ctx [:store d1])
                cell2 (get-in ctx [:store d2])]
            (if (or (ref? cell1) (ref? cell2))
              (recur (bind ctx d1 d2) fail stack)
              (let [v1 (cell-value cell1)
                    v2 (cell-value cell2)
                    f|N1 (get-in ctx [:store v1])
                    f|N2 (get-in ctx [:store v2])]
                (if (= f|N1 f|N2)
                  (recur ctx fail (push-args stack (arity f|N1) v1 v2))
                  (recur ctx true stack))))))))))

(defn get-structure [ctx f|N, Xi]
  (let [addr (deref ctx Xi)
        cell (get-in ctx [:store addr])]
    (cond
      (not (seq? cell))
      (assoc-in ctx [:fail] true)

      (= (first cell) 'REF)
      (let [h (get-in ctx [:pointer :h])
            v ['STR (inc h)]]
        (->
          ctx
          (assoc-in [:store h] v)
          (assoc-in [:store (inc h)] f|N)
          (bind addr h)
          (update-in [:pointer :h] (partial + 2))
          (assoc-in [:mode] :write)))

      (= (first cell) 'STR)
      (let [a (second cell)]
        (cond->
          ctx

          (= (get-in [:store a]) f|N)
          (->
            (assoc-in [:pointer :s] (inc a))
            (assoc-in [:mode] :read))

          :else
          (assoc-in ctx [:fail] true))))))


(defn unify-variable [ctx Xi]
  (condp = (:mode ctx)

    :read
    (let [s (get-in ctx [:pointer :s])]
      (->
        ctx
        (assoc-in [:registers Xi] (get-in ctx [:store s]))
        (update-in [:pointer :s] inc)))

    :write
    (let [h (get-in ctx [:pointer :h])
          v ['REF h]]
      (->
        ctx
        (assoc-in [:store h] v)
        (assoc-in [:registers Xi] v)
        (update-in [:pointer :h] inc)
        (update-in [:pointer :s] inc)))))


(defn unify-value [ctx Xi]
  (condp = (:mode ctx)

    :read
    (let [s (get-in ctx [:pointer :s])]
      (->
        ctx
        (unify Xi s)
        (update-in [:pointer :s] inc)))

    :write
    (let [h (get-in ctx [:pointer :h])]
      (->
        ctx
        (assoc-in [:store h] Xi)
        (update-in [:pointer :h] inc)
        (update-in [:pointer :s] inc)))))
