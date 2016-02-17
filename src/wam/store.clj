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
    [clojure.string :refer [join split-lines]]))

(def ^:private supported-modes #{:read :write})
(def ^:private supported-pointers #{:p :np :h :s})
(def program-pointer-start 0)
(def heap-start 1000)
(def heap-size 1000)
(def heap-end (+ heap-start heap-size))
(def register-start (inc heap-end))
(def register-size 30)
(def register-end (+ register-start register-size))

(def register-address
  (memoize
    (fn [Xi]
      (let [offset (->> Xi str (re-find #"\d+") Integer/parseInt)]
      (if (> offset register-size)
        (throw (IllegalArgumentException.))
        (+ register-start offset))))))

(defn pointer [ctx ptr]
  (if (supported-pointers ptr)
    (get-in ctx [:pointer ptr])
    (throw (IllegalArgumentException. (str "Unsuported pointer " ptr)))))

(defn increment [ctx ptr]
  (if (supported-pointers ptr)
    (update-in ctx [:pointer ptr] inc)
    (throw (IllegalArgumentException. (str "Unsuported pointer " ptr)))))

(defn program-address [ctx p|N]
  (:start-addr (get-in ctx [:program-offsets p|N])))

(defn get-store [ctx addr]
  (get-in ctx [:store addr]))

(defn get-register [ctx Xi]
  (let [addr (register-address Xi)]
    (get-store ctx addr)))

(defn set-store [ctx addr v]
  (assoc-in ctx [:store addr] v))

(defn set-register [ctx Xi v]
  (let [addr (register-address Xi)]
    (set-store ctx addr v)))

(defn make-context []
  { :fail false
    :mode :read
    :pointer {
      :p program-pointer-start  ;; Program pointer
      :np program-pointer-start ;; next instr pointer
      :h heap-start             ;; Top of heap
      :s heap-start             ;; Structure pointer
      }
    :store {}
    :program-offsets {} })


(defn load [ctx p|N instrs]
  (let [len (count instrs)
        np (pointer ctx :np)
        ctx (->
              ctx
              (assoc-in [:program-offsets p|N] {:start-addr np :size len})
              (update-in [:pointer :np] (partial + len)))]
    (loop [ctx ctx
           i np
           [instr & more] instrs]
      (if (nil? instr)
        ctx
        (recur
          (set-store ctx i instr)
          (inc i)
          more)))))

(defn fail
  ([ctx] (fail ctx true))
  ([ctx status] (assoc ctx :fail status)))

(defn mode [ctx new-mode]
  (if (supported-modes new-mode)
    (assoc ctx :mode new-mode)
    (throw (IllegalArgumentException. (str "Unsupported mode " new-mode)))))

;; == Diagnostic tools ==
;; move out into a separate namespace

(defn ^:private extract-from-store
  ([ctx start end]
    (extract-from-store ctx start end identity))

  ([ctx start end row-mapper]
    (->>
      ctx
      :store
      (filter (fn [[k v]] (<= start k end)))
      (map row-mapper)
      (into (sorted-map)))))

(defn heap [ctx]
  (extract-from-store ctx heap-start heap-end))

(defn registers [ctx]
  (extract-from-store ctx register-start register-end
    (fn [[k v]] [(symbol (str "X" (- k register-start))) v])))

(defn variables [ctx]
  (->>
    ctx
    :variables
    (into (sorted-map))))

(defn func-name [func]
  (second (re-find #"\$(.*)@" (str func))))

(defn friendly [[instr & args]]
  (str (func-name instr) " " (join ", " args)))

(defn program [ctx p|N]
  (if-let [prog (get-in ctx [:program-offsets p|N])]
    (extract-from-store
      ctx
      (:start-addr prog)
      (+ (:start-addr prog) (:size prog))
      (fn [[k v]] [k (friendly v)]))))

(defn diag [ctx]
  (let [inflate (fn [data] (lazy-cat data (repeat nil)))
        heap (split-lines (table-str (heap ctx) :style :unicode ))
        regs (inflate (split-lines (table-str (registers ctx) :style :unicode)))
        vars (inflate (split-lines (table-str (variables ctx) :style :unicode)))
        data (map list heap regs vars)]

    (when (:fail ctx)
      (println "FAILED"))

    (table (cons ["Heap" "Registers" "Variables"] data) :style :borderless))
  ctx)

