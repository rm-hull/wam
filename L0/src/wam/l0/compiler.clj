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

(defn substitutions [s]
  (zipmap
    (bfs s)
    (registers 'X)))


(comment
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

  (substitutions x)
  (substitutions y)
  (substitutions z)
  (dfs-pre-order y)
  (dfs-post-order z)
)
