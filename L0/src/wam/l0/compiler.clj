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
    [wam.l0.parser :refer [parse-all]]
    [wam.l0.grammar :refer :all]
    [wam.l0.graph-search :refer :all]))

(defn registers [prefix]
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


(comment
  (use 'table.core)
  (use 'clojure.pprint)
  (parse-all structure "h(Z,f(Y,3),X,X1)")
  (pprint  (parse-all structure "p(Z, h(Z, W), f(W))") )
  (parse-all structure "p(5)")

  (def x  (parse-all structure "p(Z, h(Z, W), f(W))") )
  (def y  (parse-all structure "p(f(X), h(Y, f(a)), Y)") )
  (def z  (parse-all structure "f(X, g(X,a))") )
  (dfs-pre-order x)
  (dfs-post-order x)
  (pprint (bfs x))

  (register-allocation x)
  (register-allocation y)
  (register-allocation z)

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

  (dfs-pre-order y)
  (dfs-post-order z)
)
