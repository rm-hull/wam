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


(ns wam.functor-test
  (:refer-clojure :exclude [name])
  (:require
    [clojure.test :refer :all]
    [wam.functor :refer :all]))

(deftest check-arity
  (testing "arity"
    (testing "symbol"
      (is (= (arity 'f|0) 0))
      (is (= (arity 'f|1) 1))
      (is (= (arity 'f|39) 39)))
    (testing "record"
      (is (= (arity (wam.grammar.Functor. "f" 0)) 0))
      (is (= (arity (wam.grammar.Functor. "f" 1)) 1))
      (is (= (arity (wam.grammar.Functor. "f" 39)) 39)))
    (testing "map"
      (is (= (arity {:name "f" :arg-count 0}) 0))
      (is (= (arity {:name "f" :arg-count 1}) 1))
      (is (= (arity {:name "f" :arg-count 39}) 39)))))


(deftest check-name
  (testing "name"
    (testing "symbol"
      (is (= (name 'f|0) "f"))
      (is (= (name 'g|1) "g"))
      (is (= (name 'father|39) "father")))
    (testing "record"
      (is (= (name (wam.grammar.Functor. "f" 0)) "f"))
      (is (= (name (wam.grammar.Functor. "g" 1)) "g"))
      (is (= (name (wam.grammar.Functor. "father" 39)) "father")))
    (testing "map"
      (is (= (name {:name "f" :arg-count 0}) "f"))
      (is (= (name {:name "g" :arg-count 1}) "g"))
      (is (= (name {:name "father" :arg-count 39}) "father")))))

