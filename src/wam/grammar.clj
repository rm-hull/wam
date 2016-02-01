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

(ns wam.grammar
  (:refer-clojure :exclude [list])
  (:require [wam.parser :refer :all]))

(defrecord Constant [value])
(defrecord Variable [name])
(defrecord Structure [functor args])
(defrecord Functor [name arg-count])

(defmethod print-method Structure [x ^java.io.Writer writer]
  (print-method (-> x :functor :name) writer)
  (when-not (empty? (:args x))
    (print-method (:args x) writer)))

(defmethod print-method Variable [x ^java.io.Writer writer]
  (print-method (:name x) writer))

(defmethod print-method Constant [x ^java.io.Writer writer]
  (print-method (:value x) writer))

(defmethod print-method Functor [x ^java.io.Writer writer]
  (print-method (:name x) writer)
  (.write writer "|")
  (print-method (:arg-count x) writer))


(def digit (from-re #"[0-9]"))

(def number
  (do*
    (v <- (plus digit))
    (return (Integer/parseInt (apply str v)))))

(def lower-alpha (from-re #"[a-z]"))

(def upper-alpha (from-re #"[A-Z]"))

(def alpha-num (any-of lower-alpha upper-alpha digit))

(def predicate
  (do*
    (a <- lower-alpha)
    (as <- (many alpha-num))
    (return (apply str (cons a as)))))

(def constant
  (do*
    ; (c <- (any-of predicate number))
    (n <- number)
    (return (Constant. n))))

(def variable
  (or-else
    (do*
      (a <- upper-alpha)
      (as <- (many alpha-num))
      (return (Variable. (symbol (apply str (cons a as))))))
    (do*
      (match "_")
      (return (Variable. '_)))))

(declare list)

(def structure
  (or-else
    (do*
      (p <- predicate)
      (return (Structure.
                (Functor. (symbol  p) 0)
                nil)))
    (do*
      (p <- predicate)
      (match "(")
      (l <- list)
      (match ")")
      (return (Structure.
                (Functor. (symbol p) (count l))
                l)))))

(def element
  (any-of variable constant structure))

(def list
  (do*
    (fst <- element)
    (rst <- (many (do* spaces (match ",") spaces element)))
    (return (cons fst rst))))
