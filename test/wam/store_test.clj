;; The MIT License (MIT)
;;
;; Copyright (c) 2016 Richard Hull
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


(ns wam.store-test
  (:require
    [clojure.test :refer :all]
    [wam.assert-helpers :refer :all]
    [wam.store :as s]))

(deftest check-make-context
  (testing "Initial context creation"
    (let [ctx (s/make-context)]
      (is (= (ctx :fail) false))
      (is (= (ctx :mode) :read))
      (is (= (pointer ctx :h) 0))
      (is (= (pointer ctx :s) 0))
      (is (= (pointer ctx :x) 1000))
      (is (empty? (ctx :store))))))

(deftest check-fail
  (testing "Fail instruction"
    (let [ctx (s/make-context)]
      (is (false? (ctx :fail)))
      (is (true? ((s/fail ctx) :fail)))
      (is (true? ((s/fail ctx true) :fail)))
      (is (false? ((s/fail ctx false) :fail))))))

(deftest check-pointer-access
  (testing "Pointer access"
      (is (= (pointer ctx :h) 0))
      (is (= (pointer ctx :s) 0))
      (is (= (pointer ctx :x) 1000))
    (let [ctx (s/make-context)]
      (is (thrown? IllegalArgumentException (s/pointer ctx nil)))
      (is (thrown? IllegalArgumentException (s/pointer ctx :banana))))))

(deftest check-pointer-increment
  (testing "Pointer incrementing"
      (is (= (-> ctx (increment :h) (pointer :h)) 1))
      (is (= (-> ctx (increment :s) (pointer :s)) 1))
      (is (thrown? IllegalArgumentException (increment ctx :x)))
    (let [ctx (s/make-context)]
      (is (thrown? IllegalArgumentException (s/increment ctx nil)))
      (is (thrown? IllegalArgumentException (s/increment ctx :banana))))))

(deftest check-register-address
  (testing "Register addressing"
    (let [ctx (s/make-context)]
      (is (= (s/register-address 'X1) (+ s/register-start 1)))
      (is (= (s/register-address 'X14) (+ s/register-start 14)))
      (is (= (s/register-address 'A3) (+ s/register-start 3)))
      (is (thrown? IllegalArgumentException (s/register-address 'X55))))))

(deftest check-mode
  (testing "Set mode"
    (let [ctx (s/make-context)]
      (is (= (ctx :mode) :read))
      (is (= ((s/mode ctx :write) :mode) :write))
      (is (thrown? IllegalArgumentException (s/mode ctx nil)))
      (is (thrown? IllegalArgumentException (s/mode ctx :banana))))))

