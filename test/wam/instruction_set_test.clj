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
    [wam.anciliary :refer [resolve-struct]]
    [wam.instruction-set :refer :all]
    [wam.store :as s]))

(def ctx (s/make-context))

(deftest check-set-value
  (testing "set-value"
    (let [new-ctx (->
                    ctx
                    (s/set-register 'X3 27)
                    (set-value 'X3))]
      (is (= (s/pointer new-ctx :h) (heap 1)))
      (is (= (s/get-store new-ctx (heap 0)) 27)))))

(deftest check-set-variable
  (testing "set-variable"
    (let [new-ctx (->
                    ctx
                    (s/set-register 'X4 55)
                    (set-variable 'X4))]
      (is (= (s/pointer new-ctx :h) (heap 1)))
      (is (= (s/get-store new-ctx (heap 0)) ['REF (heap 0)]))
      (is (= (s/get-register new-ctx 'X4) ['REF (heap 0)])))))

(deftest check-put-structure
  (testing "put-structure"
    (let [new-ctx (->
                    ctx
                    (put-structure 'f|n 'X3))]
      (is (= (s/pointer new-ctx :h) (heap 2)))
      (is (= (s/get-store new-ctx (heap 0)) ['STR (heap 1)]))
      (is (= (s/get-store new-ctx (heap 1)) 'f|n))
      (is (= (s/get-register new-ctx 'X3) ['STR (heap 1)])))))

(deftest check-get-structure
  (testing "get-structure")
    (let [ctx (->
                ctx
                (s/set-store (heap 0) ['REF (heap 3)])
                (s/set-store (heap 1) ['STR (heap 2)])
                (s/set-store (heap 2) 'f|0)
                (s/set-store (heap 3) ['REF (heap 3)])
                (assoc-in [:pointer :h] (heap 4))
                (s/set-register 'X1 ['REF (heap 0)])
                (s/set-register 'X2 ['REF (heap 1)])
                (s/set-register 'X3 ['REF (heap 2)]))]

      (testing "REF"
        (let [new-ctx (get-structure ctx 'g|2 'X1)]
          (is (= (s/pointer new-ctx :h) (heap 6)))
          (is (= (:mode new-ctx) :write))
          (is (false? (:fail new-ctx)))
          (is (= (s/get-store new-ctx (heap 3)) ['STR (heap 5)]))
          (is (= (s/get-store new-ctx (heap 4)) ['STR (heap 5)]))
          (is (= (s/get-store new-ctx (heap 5)) 'g|2))))

      (testing "STR (fail)"
        (let [new-ctx (get-structure ctx 'g|2 'X2)]
          (is (true? (:fail new-ctx)))))

      (testing "STR (match)"
        (let [new-ctx (get-structure ctx 'f|0 'X2)]
          (is (= (s/pointer new-ctx :s) (heap 3)))
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
                      (s/set-store (heap 0) ['REF 3])
                      (unify-variable 'X1))]
        (is (= (s/get-register new-ctx 'X1) ['REF 3]))
        (is (= (s/pointer new-ctx :s) (heap 1)))))

    (testing "write-mode"
      (let [new-ctx (->
                      ctx
                      (s/mode :write)
                      (assoc-in [:pointer :h] (heap 2))
                      (assoc-in [:pointer :s] (heap 5))
                      (unify-variable 'X1))]
        (is (= (s/get-store new-ctx (heap 2)) ['REF (heap 2)]))
        (is (= (s/get-register new-ctx 'X1) ['REF (heap 2)]))
        (is (= (s/pointer new-ctx :h) (heap 3)))
        (is (= (s/pointer new-ctx :s) (heap 6)))))

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
      "+------+------------+
       | key  | value      |
       +------+------------+
       | 1000 | [STR 1001] |
       | 1001 | h|2        |
       | 1002 | [REF 1002] |
       | 1003 | [REF 1003] |
       | 1004 | [STR 1005] |
       | 1005 | f|1        |
       | 1006 | [REF 1003] |
       | 1007 | [STR 1008] |
       | 1008 | p|3        |
       | 1009 | [REF 1002] |
       | 1010 | [STR 1001] |
       | 1011 | [STR 1005] |
       +------+------------+")))

(deftest check-put-variable
  (testing "put-variable"
    (let [new-ctx (-> ctx (put-variable 'X4 'A1))]
      (is (= (s/pointer new-ctx :h) (heap 1)))
      (is (= (s/get-store new-ctx (heap 0)) ['REF (heap 0)]))
      (is (= (s/get-register new-ctx 'X4) ['REF (heap 0)]))
      (is (= (s/get-register new-ctx 'A1) ['REF (heap 0)])))))


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


(deftest check-call
  (testing "call"
    (testing "non-existent program"
      (let [ctx (->
                  (s/make-context)
                  (call 'p|5))]
        (is (true? (:fail ctx)))
        (is (= (s/pointer ctx :p) 0))))

    (testing "simple proceed"
      (let [ctx (->
                  (s/make-context)
                  (s/load 'h|2 [[proceed]])
                  (call 'h|2))]

        (is (false? (:fail ctx)))
        (is (= (s/pointer ctx :p) 1))))))

(deftest ex2.6
  (is (tbl=
        (->
          (s/make-context)
          (put-variable 'X4, 'A1)
          (put-structure 'h|2, 'A2)
          (set-value 'X4)
          (set-variable 'X5)
          (put-structure 'f|1, 'A3)
          (set-value 'X5)
          s/heap)
         "+------+------------+
          | key  | value      |
          +------+------------+
          | 1000 | [REF 1000] |
          | 1001 | [STR 1002] |
          | 1002 | h|2        |
          | 1003 | [REF 1000] |
          | 1004 | [REF 1004] |
          | 1005 | [STR 1006] |
          | 1006 | f|1        |
          | 1007 | [REF 1004] |
          +------+------------+")))

(deftest ex2.7
  (let [p|3 (list
              [get-structure 'f|1, 'A1]
              [unify-variable 'X4]
              [get-structure 'h|2, 'A2]
              [unify-variable 'X5]
              [unify-variable 'X6]
              [get-value 'X5, 'A3]
              [get-structure 'f|1, 'X6]
              [unify-variable 'X7]
              [get-structure 'a|0, 'X7]
              [proceed])
        ctx (->
              (s/make-context)
              (put-variable 'X4, 'A1)
              (put-structure 'h|2, 'A2)
              (set-value 'X4)
              (set-variable 'X5)
              (put-structure 'f|1, 'A3)
              (set-value 'X5)
              (s/load 'p|3 p|3)
              (call 'p|3))

        W (resolve-struct ctx (s/register-address 'X4))
        X (resolve-struct ctx (s/register-address 'X4))
        Y (resolve-struct ctx (s/register-address 'A3))
        Z (resolve-struct ctx (s/register-address 'A1))]

    (s/diag ctx)

    (println "W =" W)
    (println "X =" X)
    (println "Y =" Y)
    (println "Z =" Z)
    ))

; Heap                Registers           Variables
; -------------------------------------------------
; ┌─────┬──────────┐  ┌─────┬──────────┐  ┌───────┐
; │ key │ value    │  │ key │ value    │  │ value │
; ├─────┼──────────┤  ├─────┼──────────┤  ├───────┤
; │ 0   ╎ [STR 9]  │  │ X1  ╎ [REF 0]  │  └───────┘
; │ 1   ╎ [STR 2]  │  │ X2  ╎ [STR 2]  │
; │ 2   ╎ h|2      │  │ X3  ╎ [STR 6]  │
; │ 3   ╎ [REF 0]  │  │ X4  ╎ [REF 10] │
; │ 4   ╎ [STR 12] │  │ X5  ╎ [REF 0]  │
; │ 5   ╎ [STR 6]  │  │ X6  ╎ [REF 4]  │
; │ 6   ╎ f|1      │  │ X7  ╎ [REF 13] │
; │ 7   ╎ [REF 4]  │  └─────┴──────────┘
; │ 8   ╎ [STR 9]  │
; │ 9   ╎ f|1      │
; │ 10  ╎ [REF 4]  │
; │ 11  ╎ [STR 12] │
; │ 12  ╎ f|1      │
; │ 13  ╎ [STR 15] │
; │ 14  ╎ [STR 15] │
; │ 15  ╎ a|0      │
; └─────┴──────────┘

