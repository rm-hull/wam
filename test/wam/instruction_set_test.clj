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


;; ℳ₀ machine instructions
(ns wam.instruction-set-test
  (:require
    [clojure.test :refer :all]
    [wam.assert-helpers :refer :all]
    [wam.instruction-set :refer :all]
    [wam.store :as s]))

(def ctx (s/make-context))

(deftest check-set-value
  (testing "set-value"
    (let [new-ctx (->
                    ctx
                    (s/set-register 'X3 27)
                    (set-value 'X3))]
      (is (= (s/pointer new-ctx :h) 1))
      (is (= (s/get-store new-ctx 0) 27)))))

(deftest check-set-variable
  (testing "set-variable"
    (let [new-ctx (->
                    ctx
                    (s/set-register 'X4 55)
                    (set-variable 'X4))]
      (is (= (s/pointer new-ctx :h) 1))
      (is (= (s/get-store new-ctx 0) ['REF 0]))
      (is (= (s/get-register new-ctx 'X4) ['REF 0])))))

(deftest check-put-structure
  (testing "put-structure"
    (let [new-ctx (->
                    ctx
                    (put-structure 'f|n 'X3))]
      (is (= (s/pointer new-ctx :h) 2))
      (is (= (s/get-store new-ctx 0) ['STR 1]))
      (is (= (s/get-store new-ctx 1) 'f|n))
      (is (= (s/get-register new-ctx 'X3) ['STR 1])))))

(deftest check-get-structure
  (testing "get-structure")
    (let [ctx (->
                ctx
                (s/set-store 0 ['REF 3])
                (s/set-store 1 ['STR 2])
                (s/set-store 2 'f|0)
                (s/set-store 3 ['REF 3])
                (assoc-in [:pointer :h] 4)
                (s/set-register 'X1 ['REF 0])
                (s/set-register 'X2 ['REF 1])
                (s/set-register 'X3 ['REF 2]))]

      (testing "REF"
        (let [new-ctx (get-structure ctx 'g|2 'X1)]
          (is (= (s/pointer new-ctx :h) 6))
          (is (= (:mode new-ctx) :write))
          (is (= (s/get-store new-ctx 3) ['STR 5]))
          (is (= (s/get-store new-ctx 4) ['STR 5]))
          (is (= (s/get-store new-ctx 5) 'g|2))))

      (testing "STR (fail)"
        (let [new-ctx (get-structure ctx 'g|2 'X2)]
          (is (true? (:fail new-ctx)))))

      (testing "STR (match)"
        (let [new-ctx (get-structure ctx 'f|0 'X2)]
          (is (= (get-in new-ctx [:pointer :s]) 3))
          (is (= (:mode new-ctx) :read))
          (is (false? (:fail new-ctx)))))

      (testing "no match"
        (let [new-ctx (get-structure ctx 'g|2 'X3)]
          (is (true? (:fail new-ctx)))))))

(deftest check-unify-variable
  (testing "unify-variable"
    (testing "read-mode"
      (let [new-ctx (->
                      ctx
                      (s/mode :read)
                      (s/set-store 0 ['REF 3])
                      (unify-variable 'X1))]
        (is (= (s/get-register new-ctx 'X1) ['REF 3]))
        (is (= (s/pointer new-ctx :s) 1))))

    (testing "write-mode"
      (let [new-ctx (->
                      ctx
                      (s/mode :write)
                      (assoc-in [:pointer :h] 2)
                      (assoc-in [:pointer :s] 5)
                      (unify-variable 'X1))]
        (is (= (s/get-store new-ctx 2) ['REF 2]))
        (is (= (s/get-register new-ctx 'X1) ['REF 2]))
        (is (= (s/pointer new-ctx :h) 3))
        (is (= (s/pointer new-ctx :s) 6))))

    (testing "unknown mode"
      (is (thrown? IllegalArgumentException
                   (->
                     ctx
                     (assoc :mode :banana)
                     (unify-variable 'X5)))))))

(deftest check-unify-value
  (testing "unify-value"
    (testing "read-mode"
      ;; TODO
      )

    (testing "write-mode"
      (let [new-ctx (->
                      ctx
                      (s/mode :write)
                      (assoc-in [:pointer :h] 9)
                      (assoc-in [:pointer :s] 3)
                      (s/set-register 'X2 ['STR 1])
                      (unify-value 'X2))]
        (is (= (s/get-store new-ctx 9) ['STR 1]))
        (is (= (s/pointer new-ctx :h) 10))
        (is (= (s/pointer new-ctx :s) 4))))

    (testing "unknown mode"
      (is (thrown? IllegalArgumentException
                   (->
                     ctx
                     (assoc :mode :banana)
                     (unify-value 'X5)))))))

(deftest ex2.1
  ; Compiled code for L0 query ?-p(Z,h(Z,W),f(W)).

  (is (tbl=
    (->
      ctx
      (put-structure 'h|2, 'X3)
      (set-variable 'X2)
      (set-variable 'X5)
      (put-structure 'f|1, 'X4)
      (set-value 'X5)
      (put-structure 'p|3, 'X1)
      (set-value 'X2)
      (set-value 'X3)
      (set-value 'X4)
      s/heap)

      "+-----+---------+
       | key | value   |
       +-----+---------+
       | 0   | [STR 1] |
       | 1   | h|2     |
       | 2   | [REF 2] |
       | 3   | [REF 3] |
       | 4   | [STR 5] |
       | 5   | f|1     |
       | 6   | [REF 3] |
       | 7   | [STR 8] |
       | 8   | p|3     |
       | 9   | [REF 2] |
       | 10  | [STR 1] |
       | 11  | [STR 5] |
       +-----+---------+")))

(deftest check-put-variable
  (testing "put-variable"
    (let [new-ctx (-> ctx (put-variable 'X4 'A1))]
      (is (= (s/pointer new-ctx :h) 1))
      (is (= (s/get-store new-ctx 0) ['REF 0]))
      (is (= (s/get-register new-ctx 'X4) ['REF 0]))
      (is (= (s/get-register new-ctx 'A1) ['REF 0])))))


(deftest check-put-value
  (testing "put-value"
    (let [new-ctx (->
                    ctx
                    (s/set-register 'X4 32)
                    (put-value 'X4 'A1))]
      (is (= (s/get-register new-ctx 'A1) 32)))))


(deftest check-get-variable
  (testing "get-variable"
    (let [new-ctx (->
                    ctx
                    (s/set-register 'A1 99)
                    (get-variable 'X4 'A1))]
      (is (= (s/get-register new-ctx 'X4) 99)))))

(deftest check-get-value
  (testing "get-value"
    (let [new-ctx (->
                    ctx
                    (s/set-register 'A1 99)
                    (get-value 'X1 'A1))]
      (is (false? (:fail new-ctx 'X4))))))
