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

(ns wam.l0.graph-search
  (:require [wam.l0.grammar :refer :all]))

(defn traverse-pre [term]
  (cond
    (seq? term)
    (map traverse-pre term)

    (instance? wam.l0.grammar.Structure term)
    (cons term (traverse-pre (:args term)))

    :else nil))

(defn traverse-post [term]
  (cond
    (seq? term)
    (map traverse-post term)

    (instance? wam.l0.grammar.Structure term)
    (concat (traverse-post (:args term)) [term])

    :else nil))

(defn dfs-pre-order [term]
  (remove nil? (flatten (traverse-pre term))))

(defn dfs-post-order [term]
  (remove nil? (flatten (traverse-post term))))

(defn append [queue coll]
  (if (empty? coll)
    queue
    (recur
      (conj queue (first coll))
      (rest coll))))

(defn queue
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn bfs [term]
  (loop [queue (queue [term])
         seen #{}
         result []]
    (if (empty? queue)
      result
      (let [v (peek queue)
            queue (pop queue)]

        (cond
          (seen v)
          (recur queue seen result)

          (instance? wam.l0.grammar.Structure v)
          (recur (append queue (:args v)) (conj seen v) (conj result v))

          :else
          (recur queue (conj seen v) (conj result v)))))))
