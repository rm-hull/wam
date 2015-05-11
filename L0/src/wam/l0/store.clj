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


(ns wam.l0.store
  (:require [clojure.string :refer [split]]))

(def ^:private register-number
  (memoize
    (fn [Xi]
      (->> Xi str (re-find #"\d+") Integer/parseInt))))

(defn pointer [ctx ptr]
  (get-in ctx [:pointer ptr]))

(defn increment [ctx ptr]
  (update-in ctx [:pointer ptr] inc))

(defn register-address [ctx Xi]
  (+ (register-number Xi) (pointer ctx :x)))

(defn get-store [ctx addr]
  (get-in ctx [:store addr]))

(defn get-register [ctx Xi]
  (let [addr (register-address ctx Xi)]
    (get-store ctx addr)))

(defn set-store [ctx addr v]
  (assoc-in ctx [:store addr] v))

(defn set-register [ctx Xi v]
  (let [addr (register-address ctx Xi)]
    (set-store ctx addr v)))

(def heap-start 0)
(def register-start 1000)

(defn make-context []
  { :fail false
    :mode :read
    :pointer {:h heap-start :s heap-start :x register-start}
    :store (sorted-map) })

(defn heap [ctx]
  (->>
    ctx
    :store
    (filter (fn [[k v]] (and (>= k heap-start) (< k register-start))))
    (into (sorted-map))))

(defn registers [ctx]
  (->>
    ctx
    :store
;    (filter (fn [[k v]] (and (>= k heap-start) (< k register-start))))
    (filter (fn [[k v]] (>= k register-start)))
    (map (fn [[k v]] [(symbol (str "X" (- k register-start))) v]))
    (into {})))

(defn fail [ctx]
  (assoc ctx :fail true))

(defn mode [ctx new-mode]
  (assoc ctx :mode new-mode))

