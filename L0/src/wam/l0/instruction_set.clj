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
  (:refer-clojure :exclude [deref])
  (:require
    [clojure.string :refer [split]]
    [wam.l0.store :as s]))

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
      (s/set-heap h v)
      (s/set-heap (inc h) f|N)
      (s/set-register Xi v)
      (s/increment :h)
      (s/increment :h))))

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
      (s/set-heap h v)
      (s/set-register Xi v)
      (s/increment :h))))

(defn set-value [ctx Xi]
  (let [h (get-in ctx [:pointer :h])
        v (s/get-register ctx Xi)]
    (->
      ctx
      (s/set-heap h v)
      (s/increment :h))))


(comment
  ; Exercise 2.1 (pg. 9)
  ; Compiled code for L0 query ?-p(Z,h(Z,W),f(W)).
  (use 'table.core)

  (def ctx (s/make-context))

  (->
    ctx
    (put-structure 'h|2, 'X3)
    (set-variable 'X2)
    (set-variable 'X5)
    (put-structure 'f|1, 'X4)
    (set-value 'X5)
    (put-structure 'p|3, 'X1)
    (set-value 'X2)
    (set-value 'X3)
    (set-value 'X4)
    s/heap
    table)

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

(defn ^:private deref
  "Follows a possible reference chain until it reaches either an unbound REF
   cell or a non-REF cell, the address of which it returns. The effect of
   dereferencing is none other than composing variable substitutions."
  [ctx addr]
  (if (symbol? addr)
    (deref ctx (s/register-address ctx addr))
    (let [cell (get-in ctx [:store addr])]
      (cond
        (not (seq? cell))
        addr

        (and (ref? cell) (not= (cell-value cell) addr))
        (recur ctx (cell-value cell))

        :else addr))))

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

(defn bind
  "Effectuate the binding of the heap cell to the address"
  [ctx addr ref]
  (let [cell (get-in ctx [:store addr])]
    (if (str? cell) ; dont overwrite STR, flip the ref / addr and try again
      (bind ctx ref addr)
      (s/set-heap ctx addr ['REF ref]))))

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

(defn get-structure [ctx f|N Xi]
  (let [addr (deref ctx Xi)
        cell (get-in ctx [:store addr])]
    (cond
      (not (coll? cell))
      (s/fail ctx)

      (ref? cell)
      (let [h (get-in ctx [:pointer :h])
            v ['STR (inc h)]]
        (->
          ctx
          (s/set-heap h v)
          (s/set-heap (inc h) f|N)
          (bind addr h)
          (s/increment :h)
          (s/increment :h)
          (s/mode :write)))

      (str? cell)
      (let [a (cell-value cell)]
        (if (= (get-in ctx [:store a]) f|N)
          (->
            ctx
            (assoc-in [:pointer :s] (inc a))
            (s/mode :read))
          (s/fail ctx))))))


(defn unify-variable [ctx Xi]
  (condp = (:mode ctx)

    :read
    (let [s (get-in ctx [:pointer :s])]
      (->
        ctx
        (s/set-register Xi (get-in ctx [:store s]))
        (s/increment :s)))

    :write
    (let [h (get-in ctx [:pointer :h])
          v ['REF h]]
      (->
        ctx
        (s/set-heap h v)
        (s/set-register Xi v)
        (s/increment :h)
        (s/increment :s)))))


(defn unify-value [ctx Xi]
  (condp = (:mode ctx)

    :read
    (let [s (get-in ctx [:pointer :s])]
      (->
        ctx
        (unify Xi s)
        (s/increment :s)))

    :write
    (let [h (get-in ctx [:pointer :h])]
      (->
        ctx
        (s/set-heap h (s/get-register ctx Xi))
        (s/increment :h)
        (s/increment :s)))))


