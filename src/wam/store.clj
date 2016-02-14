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


(ns wam.store
  (:require
    [table.core :refer [table table-str]]
    [clojure.string :refer [split-lines]]))

(def ^:private supported-modes #{:read :write})
(def ^:private supported-pointers #{:h :s :x})
(def ^:private supports-incrementing #{:h :s})
(def ^:private heap-start 0)
(def ^:private register-start 1000)

(def ^:private register-number
  (memoize
    (fn [Xi]
      (->> Xi str (re-find #"\d+") Integer/parseInt))))

(defn pointer [ctx ptr]
  (if (supported-pointers ptr)
    (get-in ctx [:pointer ptr])
    (throw (IllegalArgumentException. (str "Unsuported pointer " ptr)))))

(defn increment [ctx ptr]
  (if (supports-incrementing ptr)
    (update-in ctx [:pointer ptr] inc)
    (throw (IllegalArgumentException. (str "Unsuported pointer " ptr)))))

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

(defn make-context []
  { :fail false
    :mode :read
    :pointer {
      :h heap-start
      :s heap-start      ; FIXME: is this offset correct?
      :x register-start}
    :store {} })

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
    (filter (fn [[k v]] (>= k register-start)))
    (map (fn [[k v]] [(symbol (str "X" (- k register-start))) v]))
    (into (sorted-map))))

(defn variables [ctx]
  (->>
    ctx
    :variables
    (into (sorted-map))))

(defn fail
  ([ctx] (fail ctx true))
  ([ctx status] (assoc ctx :fail status)))

(defn mode [ctx new-mode]
  (if (supported-modes new-mode)
    (assoc ctx :mode new-mode)
    (throw (IllegalArgumentException. (str "Unsupported mode " new-mode)))))

(defn diag [ctx]
  (let [inflate (fn [data] (lazy-cat data (repeat nil)))
        heap (split-lines (table-str (heap ctx) :style :unicode ))
        regs (inflate (split-lines (table-str (registers ctx) :style :unicode)))
        vars (inflate (split-lines (table-str (variables ctx) :style :unicode)))
        data (map list heap regs vars)]
    (table (cons ["Heap" "Registers" "Variables"] data) :style :borderless))
  ctx)

