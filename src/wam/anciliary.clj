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


(ns wam.anciliary
  (:refer-clojure :exclude [deref])
  (:require
    [clojure.string :refer [split join]]
    [wam.store :as s]))

(def arity
  "Determine the arity given a functor (as either a symbol or a
   wam.grammar.Functor) representation"
  (memoize
    (fn [functor]
      (if (symbol? functor)
        (-> functor name (split #"\|") second (Integer/parseInt))
        (:arg-count functor)))))

(def cell-type
  "Convenience wrapper to obtain the cell type"
  first)

(def cell-value
  "Convenience wrapper to obtain the cell value"
  second)

(defn ^:private cell-type?
  "Function maker to determine if a cell is of a given type,
   presently the known types are REF (reference) or STR (structure)."
  [allowed-types]
  (fn [cell]
    (and
      (coll? cell)
      (= (count cell) 2)
      (contains? allowed-types (cell-type cell)))))

(def ref?
  "Convenience wrapper for REF cell types"
  (cell-type? #{'REF}))

(def str?
  "Convenience wrapper for STR cell types"
  (cell-type? #{'STR}))

(def cell?
  "Convenience wrapper for any cell types"
  (cell-type? #{'REF 'STR}))

(defn deref
  "Follows a possible reference chain until it reaches either an unbound REF
   cell or a non-REF cell, the address of which it returns. The effect of
   dereferencing is none other than composing variable substitutions."
  [ctx addr]
  (if (symbol? addr)
    (deref ctx (s/register-address ctx addr))
    (let [cell (s/get-store ctx addr)]
      (cond
        (not (cell? cell))
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
  [ctx a1 a2]
  (let [cell1 (s/get-store ctx a1)
        cell2 (s/get-store ctx a2)]
    (if (and (ref? cell1) (or (not (ref? cell2)) (< a2 a1)))
      (s/set-store ctx a1 cell2)
      (s/set-store ctx a2 cell1))))

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
      (s/fail ctx fail)
      (let [d1 (deref ctx (peek stack))
            d2 (deref ctx (peek (pop stack)))
            stack (pop (pop stack))]
        (if (= d1 d2)
          (recur ctx fail stack)
          (let [cell1 (s/get-store ctx d1)
                cell2 (s/get-store ctx d2)]
            (if (or (ref? cell1) (ref? cell2))
              (recur (bind ctx d1 d2) fail stack)
              (let [v1 (cell-value cell1)
                    v2 (cell-value cell2)
                    f|N1 (s/get-store ctx v1)
                    f|N2 (s/get-store ctx v2)]
                (if (= f|N1 f|N2)
                  (recur ctx fail (push-args stack (arity f|N1) v1 v2))
                  (recur ctx true stack))))))))))



(declare resolve-struct)

(defn- resolve-functor [ctx addr]
  (let [functor (s/get-store ctx addr)
        process-args (fn []
                       (for [i (range (arity functor))]
                         (resolve-struct ctx (+ addr i 1))))
        decorate (fn [coll] (if (seq coll) (str "(" (join ", " coll) ")")))]
    (str (:name functor) (decorate (process-args)))))


(defn resolve-struct [ctx addr]
  (let [v (s/get-store ctx (deref ctx addr))]
    (if (str? v)
      (resolve-functor ctx (cell-value v))
      (throw (IllegalArgumentException. "No structure at address" addr)))))

