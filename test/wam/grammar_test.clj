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


(ns wam.grammar-test
  (:import
   [java.text ParseException]
   [wam.grammar Constant Variable Structure Functor])
  (:require
   [clojure.test :refer :all]
   [jasentaa.parser :refer [parse-all]]
   [wam.grammar :refer [structure predicate constant variable]]))

(deftest check-variable
  (testing "Variables"
    (is (= (parse-all variable "W") (Variable. 'W)))
    (is (= (parse-all variable "TEMP") (Variable. 'TEMP)))
    (is (= (parse-all variable "Temp") (Variable. 'Temp)))
    (is (= (parse-all variable "_") (Variable. '_)))
    (is (thrown-with-msg?
         ParseException #"Unable to parse text"
         (parse-all variable "w")))
    (is (not= (parse-all variable "W") (Variable. 'X)))))

(deftest check-constant
  (testing "Constants"
    (is (= (parse-all constant "3") (Constant. 3)))
    (is (thrown-with-msg?
         ParseException #"Unable to parse text"
         (parse-all constant "W")))))

(deftest check-predicate
  (testing "Predicates"
    (is (= (parse-all predicate "fred") "fred"))
    (is (= (parse-all predicate "b4rn3y") "b4rn3y"))
    (is (thrown-with-msg?
         ParseException #"Unable to parse text"
         (parse-all predicate "Wilma!")))))

(deftest check-structure
  (testing "Structures"
    (is (= (parse-all structure "p") (Structure. (Functor. 'p 0) nil)))
    (is (= (parse-all structure "f(W)") (Structure. (Functor. 'f 1) (list (Variable. 'W)))))
    (is (= (parse-all structure "h(Z, W)") (Structure. (Functor. 'h 2) (list (Variable. 'Z) (Variable. 'W)))))
    (is (= (parse-all structure "h(Z,W)") (Structure. (Functor. 'h 2) (list (Variable. 'Z) (Variable. 'W)))))
    (is (= (parse-all structure "p(Z, h(Z, W), f(W))")
           (Structure.
            (Functor. 'p 3)
            (list
             (Variable. 'Z)
             (Structure.
              (Functor. 'h 2)
              (list
               (Variable. 'Z)
               (Variable. 'W)))
             (Structure.
              (Functor. 'f 1)
              (list
               (Variable. 'W)))))))))

(deftest writer-output
  (testing "Output rendering"
    (is (= (with-out-str (print (parse-all structure "p(Z,h(Z,W),f(W))")))
           "p(Z h(Z W) f(W))"))))
