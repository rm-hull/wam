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


(ns wam.compiler-test
  (:require
    [clojure.test :refer :all]
    [jasentaa.parser :refer [parse-all]]
    [wam.assert-helpers :refer :all]
    [wam.anciliary :refer [unify resolve-struct]]
    [wam.compiler :refer :all]
    [wam.instruction-set :refer :all]
    [wam.grammar :refer [structure]]
    [wam.store :as s]))

(deftest check-register-allocation
  (testing "Register allocation"
    (is (tbl= (register-allocation (parse-all structure "p(Z, h(Z, W), f(W))"))
      "+------------------+-------+
       | key              | value |
       +------------------+-------+
       | p(Z h(Z W) f(W)) | X1    |
       | Z                | X2    |
       | h(Z W)           | X3    |
       | f(W)             | X4    |
       | W                | X5    |
       +------------------+-------+"))

    (is (tbl= (register-allocation (parse-all structure "p(f(X), h(Y, f(a)), Y)"))
      "+---------------------+-------+
       | key                 | value |
       +---------------------+-------+
       | p(f(X) h(Y f(a)) Y) | X1    |
       | f(X)                | X2    |
       | h(Y f(a))           | X3    |
       | Y                   | X4    |
       | X                   | X5    |
       | f(a)                | X6    |
       | a                   | X7    |
       +---------------------+-------+"))

    (is (tbl= (register-allocation (parse-all structure "f(X, g(X,a))"))
      "+-------------+-------+
       | key         | value |
       +-------------+-------+
       | f(X g(X a)) | X1    |
       | X           | X2    |
       | g(X a)      | X3    |
       | a           | X4    |
       +-------------+-------+"))))

(deftest check-query-builder
  (testing "Query builder"
    (let [term (parse-all structure "f(X, g(X,a))")
          register-allocation (register-allocation term)]
      (is (instr= (emit-instructions query-builder term register-allocation)
        "+---------------+------+------+
         | instr         | arg1 | arg2 |
         +---------------+------+------+
         | put_structure | a|0  | X4   |
         | put_structure | g|2  | X3   |
         | set_variable  | X2   |      |
         | set_value     | X4   |      |
         | put_structure | f|2  | X1   |
         | set_value     | X2   |      |
         | set_value     | X3   |      |
         +---------------+------+------+")))

    (let [term (parse-all structure "p(Z, h(Z, W), f(W))")
          register-allocation (register-allocation term)]
      (is (instr= (emit-instructions query-builder term register-allocation)
        "+---------------+------+------+
         | instr         | arg1 | arg2 |
         +---------------+------+------+
         | put_structure | h|2  | X3   |
         | set_variable  | X2   |      |
         | set_variable  | X5   |      |
         | put_structure | f|1  | X4   |
         | set_value     | X5   |      |
         | put_structure | p|3  | X1   |
         | set_value     | X2   |      |
         | set_value     | X3   |      |
         | set_value     | X4   |      |
         +---------------+------+------+")))))

(deftest check-program-builder
  (testing "Program builder"
    (let [term (parse-all structure "p(f(X), h(Y, f(a)), Y)")
          register-allocation (register-allocation term)]
      (is (instr= (emit-instructions program-builder term register-allocation)
        "+----------------+------+------+
         | instr          | arg1 | arg2 |
         +----------------+------+------+
         | get_structure  | p|3  | X1   |
         | unify_variable | X2   |      |
         | unify_variable | X3   |      |
         | unify_variable | X4   |      |
         | get_structure  | f|1  | X2   |
         | unify_variable | X5   |      |
         | get_structure  | h|2  | X3   |
         | unify_value    | X4   |      |
         | unify_variable | X6   |      |
         | get_structure  | f|1  | X6   |
         | unify_variable | X7   |      |
         | get_structure  | a|0  | X7   |
         +----------------+------+------+")))))

(deftest check-compile
  (testing "Query compilation"
    (let [q (->>
              "p(Z, h(Z, W), f(W))"
              (parse-all structure)
              (compile-term query-builder))]
      (is (tbl= (-> (s/make-context) q s/heap)
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
         +------+------------+"))))

  (testing "Sequential queries"
    (is (tbl=
          (->
            (s/make-context)
            (query "f(X, g(X, a))")
            (query "f(b, Y)")
            s/heap)
          "+------+------------+
           | key  | value      |
           +------+------------+
           | 1000 | [STR 1001] |
           | 1001 | a|0        |
           | 1002 | [STR 1003] |
           | 1003 | g|2        |
           | 1004 | [REF 1004] |
           | 1005 | [STR 1001] |
           | 1006 | [STR 1007] |
           | 1007 | f|2        |
           | 1008 | [REF 1004] |
           | 1009 | [STR 1003] |
           | 1010 | [STR 1011] |
           | 1011 | b|0        |
           | 1012 | [STR 1013] |
           | 1013 | f|2        |
           | 1014 | [STR 1011] |
           | 1015 | [REF 1015] |
           +------+------------+")))

  (testing "Unification"
    (is (tbl=
          (->
            (s/make-context)
            (query "f(X, g(X, a))")
            (query "f(b, Y)")
            (unify (heap 6) (heap 12))
            s/heap )
          "+------+------------+
           | key  | value      |
           +------+------------+
           | 1000 | [STR 1001] |
           | 1001 | a|0        |
           | 1002 | [STR 1003] |
           | 1003 | g|2        |
           | 1004 | [STR 1011] |
           | 1005 | [STR 1001] |
           | 1006 | [STR 1007] |
           | 1007 | f|2        |
           | 1008 | [REF 1004] |
           | 1009 | [STR 1003] |
           | 1010 | [STR 1011] |
           | 1011 | b|0        |
           | 1012 | [STR 1013] |
           | 1013 | f|2        |
           | 1014 | [STR 1011] |
           | 1015 | [STR 1003] |
           +------+------------+"))))

(deftest ex2.2
  (let [ctx (->
              (s/make-context)
              (query "f(X, g(X, a))")
              (query "f(b, Y)")
              (unify (heap 12) (heap 6)))]
    (is (= (resolve-struct ctx (s/register-address 'X2)) "b"))
    (is (= (resolve-struct ctx (s/register-address 'X3)) "g(b, a)"))))

(deftest ex2.3
  (let [ctx (->
              (s/make-context)

              ; fig 2.3: compiled code for ℒ₀ query ?- p(Z, h(Z, W), f(W)).
              (put-structure 'h|2, 'X3)
              (set-variable 'X2)
              (set-variable 'X5)
              (put-structure 'f|1, 'X4)
              (set-value 'X5)
              (put-structure 'p|3, 'X1)
              (set-value 'X2)
              (set-value 'X3)
              (set-value 'X4)

              ; fig 2.4: compiled code for ℒ₀ query ?- p(f(X), h(Y, f(a)), Y).
              (get-structure 'p|3, 'X1)
              (unify-variable 'X2)
              (unify-variable 'X3)
              (unify-variable 'X4)
              (get-structure 'f|1, 'X2)
              (unify-variable 'X5)
              (get-structure 'h|2, 'X3)
              (unify-value 'X4)
              (unify-variable 'X6)
              (get-structure 'f|1, 'X6)
              (unify-variable 'X7)
              (get-structure 'a|0, 'X7))

        W (resolve-struct ctx (s/register-address 'X5))
        X (resolve-struct ctx (s/register-address 'X5))
        Y (resolve-struct ctx (s/register-address 'X4))
        Z (resolve-struct ctx (s/register-address 'X2))]
    (is (= W "f(a)"))
    (is (= X "f(a)"))
    (is (= Y "f(f(a))"))
    (is (= Z "f(f(a))"))))

(deftest ex2.5
  (let [ctx (->
              (s/make-context)
              (query "p(Z, h(Z, W), f(W))")
              (program "p(f(X), h(Y, f(a)), Y)"))

        W (resolve-struct ctx (s/register-address 'X5))
        X (resolve-struct ctx (s/register-address 'X5))
        Y (resolve-struct ctx (s/register-address 'X4))
        Z (resolve-struct ctx (s/register-address 'X2))]
    (is (= W "f(a)"))
    (is (= X "f(a)"))
    (is (= Y "f(f(a))"))
    (is (= Z "f(f(a))"))))


(defn tee [v func]
  (func v)
  v)

(->
  (s/make-context)
  (assoc :trace true)
  (query "father(R, henry)")
  (program "father(richard, henry)")
  s/diag
  (tee #(println "R" (resolve-struct % 1002))))

; put_structure henry|0, X3
; put_structure father|2, X1
; set_variable X2
; set_value X3
; get_structure father|2, X1
; unify_variable X2
; unify_variable X3
; get_structure richard|0, X2
; get_structure henry|0, X3
;
; Heap                 Registers          Variables
; -------------------------------------------------------
; ┌─────┬───────────┐  ┌─────┬─────────┐  ┌─────┬───────┐
; │ key │ value     │  │ key │ value   │  │ key │ value │
; ├─────┼───────────┤  ├─────┼─────────┤  ├─────┼───────┤
; │ 0   ╎ [STR 1]   │  │ X1  ╎ [STR 3] │  │ R   ╎ X2    │
; │ 1   ╎ henry|0   │  │ X2  ╎ [REF 4] │  └─────┴───────┘
; │ 2   ╎ [STR 3]   │  │ X3  ╎ [STR 1] │
; │ 3   ╎ father|2  │  └─────┴─────────┘
; │ 4   ╎ [STR 7]   │
; │ 5   ╎ [STR 1]   │
; │ 6   ╎ [STR 7]   │
; │ 7   ╎ richard|0 │
; └─────┴───────────┘
;
; R richard
; {:fail false, :mode :read, :pointer {:h 8, :s 2, :x 1000}, :store {0 [STR 1], 7 richard|0, 1001 [STR 3], 1 henry|0, 4 [STR 7], 1002 [REF 4], 1003 [STR 1], 6 [STR 7], 3 father|2, 2 [STR 3], 5 [STR 1]}, :trace true, :variables ([R X2])}

(->
  (s/make-context)
  (assoc :trace true)
  (query "father(R, henry)")
  (program "father(henry, richard)")
  s/diag
  (tee #(println "R" (resolve-struct % 1002))))

; put_structure henry|0, X3
; put_structure father|2, X1
; set_variable X2
; set_value X3
; get_structure father|2, X1
; unify_variable X2
; unify_variable X3
; get_structure henry|0, X2
; get_structure richard|0, X3
;
; Heap                Registers          Variables
; ------------------------------------------------------
; ┌─────┬──────────┐  ┌─────┬─────────┐  ┌─────┬───────┐
; │ key │ value    │  │ key │ value   │  │ key │ value │
; ├─────┼──────────┤  ├─────┼─────────┤  ├─────┼───────┤
; │ 0   ╎ [STR 1]  │  │ X1  ╎ [STR 3] │  │ R   ╎ X2    │
; │ 1   ╎ henry|0  │  │ X2  ╎ [REF 4] │  └─────┴───────┘
; │ 2   ╎ [STR 3]  │  │ X3  ╎ [STR 1] │
; │ 3   ╎ father|2 │  └─────┴─────────┘
; │ 4   ╎ [STR 7]  │
; │ 5   ╎ [STR 1]  │
; │ 6   ╎ [STR 7]  │
; │ 7   ╎ henry|0  │
; └─────┴──────────┘
;
; R henry
; {:fail true, :mode :write, :pointer {:h 8, :s 6, :x 1000}, :store {0 [STR 1], 7 henry|0, 1001 [STR 3], 1 henry|0, 4 [STR 7], 1002 [REF 4], 1003 [STR 1], 6 [STR 7], 3 father|2, 2 [STR 3], 5 [STR 1]}, :trace true, :variables ([R X2])}

(->
  (s/make-context)
  (assoc :trace true)
  (query "father(richard, J)")
  (program "father(W, K)")
  s/diag
  ;(tee #(println "J" (resolve-struct % 1003)))
  ;(tee #(println "K" (resolve-struct % 1003)))
  ;(tee #(println "W" (resolve-struct % 1002)))

  )

