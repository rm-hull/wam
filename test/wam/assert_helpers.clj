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


(ns wam.assert-helpers
  (:require
   [clojure.string :as str]
   [table.core :as t]
   [wam.store :as s]))

(defn heap [offset]
  (+ s/heap-start offset))

(defn register [offset]
  (+ s/register-start offset))

; Some helper functions to get round limitations in table
(defn- inflate [table]
  (let [max-cols (reduce max 0 (map count table))]
    (map #(take max-cols (lazy-cat % (repeat nil))) table)))

(defn- headers [& headers]
  (fn [table] (cons headers table)))

(def instr
  (comp
   t/table
   inflate
   (headers "instr" "arg1" "arg2")
   (partial map (fn [[instr & args]] (cons (s/func-name instr) args)))))

(defn- line-trim [s]
  (->>
   s
   (str/split-lines)
   (map str/trim)
   (remove empty?)
   (str/join "\n")))

(defn tbl= [actual expected]
  (=
   (line-trim (with-out-str (t/table actual)))
   (line-trim expected)))

(defn instr= [actual expected]
  (=
   (line-trim (with-out-str (instr actual)))
   (line-trim expected)))

