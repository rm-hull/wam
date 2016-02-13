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
    [wam.store :refer :all]))

(deftest check-make-context
  (testing "Initial context creation"
    (let [ctx (make-context)]
      (is (= (ctx :fail) false))
      (is (= (ctx :mode) :read))
      (is (= (pointer ctx :h) 0))
      (is (= (pointer ctx :s) 0))
      (is (= (pointer ctx :x) 1000))
      (is (empty? (ctx :store))))))

(deftest check-fail
  (testing "Fail instruction"
    (let [ctx (make-context)]
      (is (= (ctx :fail) false))
      (is (= ((fail ctx) :fail) true)))))

(deftest check-pointer-access
  (testing "Pointer access"
    (let [ctx (make-context)]
      (is (= (pointer ctx :h) 0))
      (is (thrown? IllegalArgumentException (pointer ctx nil)))
      (is (thrown? IllegalArgumentException (pointer ctx :banana))))))

(deftest check-pointer-increment
  (testing "Pointer incrementing"
    (let [ctx (make-context)]
      (is (= (-> ctx (increment :h) (pointer :h)) 1))
      (is (= (-> ctx (increment :s) (pointer :s)) 1))
      (is (thrown? IllegalArgumentException (increment ctx :x)))
      (is (thrown? IllegalArgumentException (increment ctx nil)))
      (is (thrown? IllegalArgumentException (increment ctx :banana))))))

(deftest check-register-address
  (testing "Register addressing"
    (let [ctx (make-context)]
      (is (= (register-address ctx 'X1) 1001))
      (is (= (register-address ctx 'X55) 1055))
      (is (= (register-address ctx 'A3) 1003)))))

(deftest check-mode
  (testing "Set mode"
    (let [ctx (make-context)]
      (is (= (ctx :mode) :read))
      (is (= ((mode ctx :write) :mode) :write))
      (is (thrown? IllegalArgumentException (mode ctx nil)))
      (is (thrown? IllegalArgumentException (mode ctx :banana))))))
