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


(ns wam.anciliary-test
  (:refer-clojure :exclude [deref])
  (:require
    [clojure.test :refer :all]
    [wam.store :as s]
    [wam.anciliary :refer :all]))

(deftest check-ref?
  (testing "ref?"
    (is (false? (ref? [])))
    (is (false? (ref? [1])))
    (is (false? (ref? [1 2])))
    (is (false? (ref? [1 2 3])))
    (is (false? (ref? nil)))
    (is (false? (ref? ['STR 3])))
    (is (false? (ref? ['REF 3 3])))
    (is (true? (ref? ['REF 3])))))

(deftest check-str?
  (testing "str?"
    (is (false? (str? [])))
    (is (false? (str? [1])))
    (is (false? (str? [1 2])))
    (is (false? (str? [1 2 3])))
    (is (false? (str? nil)))
    (is (true? (str? ['STR 3])))
    (is (false? (str? ['STR 3 5])))
    (is (false? (str? ['REF 3])))))

(deftest check-cell?
  (testing "cell?"
    (is (false? (cell? [])))
    (is (false? (cell? [1])))
    (is (false? (cell? [1 2])))
    (is (false? (cell? [1 2 3])))
    (is (false? (cell? nil)))
    (is (false? (cell? ['STR 3 6 6])))
    (is (false? (cell? ['REF 3 5])))
    (is (true? (cell? ['STR 3])))
    (is (true? (cell? ['REF 3])))))

(deftest check-deref
  (testing "deref follow refs"
    (let [ctx (->
                (s/make-context)
                (s/set-store 0 ['REF 2])
                (s/set-store 1 ['REF 3])
                (s/set-store 2 ['REF 1])
                (s/set-store 3 ['REF 3])
                (s/set-store 4 ['STR 5])
                (s/set-store 5 'f|2)
                (s/set-store 6 ['REF 3])
                (s/set-register 'X3 ['REF 4]))]
      (is (= (deref ctx 0) 3))
      (is (= (deref ctx 1) 3))
      (is (= (deref ctx 2) 3))
      (is (= (deref ctx 3) 3))
      (is (= (deref ctx 4) 4))
      (is (= (deref ctx 5) 5))
      (is (= (deref ctx 6) 3))
      (is (= (deref ctx 'X3) 4)))))


