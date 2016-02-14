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
    [clojure.string :as s]
    [clojure.test :refer :all]
    [wam.assert-helpers :refer :all]
    [wam.anciliary :refer [unify resolve-struct]]
    [wam.compiler :refer :all]
    [wam.instruction-set :refer :all]
    [wam.parser :refer [parse-all]]
    [wam.grammar :refer [structure]]
    [wam.store :refer [heap registers variables make-context diag]]))

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
      (is (tbl= (-> (make-context) q heap)
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
         +-----+---------+"))))

  (testing "Sequential queries"
    (is (tbl=
          (->
            (make-context)
            (query "f(X, g(X, a))")
            (query "f(b, Y)")
            heap)
          "+-----+----------+
           | key | value    |
           +-----+----------+
           | 0   | [STR 1]  |
           | 1   | a|0      |
           | 2   | [STR 3]  |
           | 3   | g|2      |
           | 4   | [REF 4]  |
           | 5   | [STR 1]  |
           | 6   | [STR 7]  |
           | 7   | f|2      |
           | 8   | [REF 4]  |
           | 9   | [STR 3]  |
           | 10  | [STR 11] |
           | 11  | b|0      |
           | 12  | [STR 13] |
           | 13  | f|2      |
           | 14  | [STR 11] |
           | 15  | [REF 15] |
           +-----+----------+")))

  (testing "Unification"
    (is (tbl=
          (->
            (make-context)
            (query "f(X, g(X, a))")
            (query "f(b, Y)")
            (unify 6 12)
            heap)
          "+-----+----------+
           | key | value    |
           +-----+----------+
           | 0   | [STR 1]  |
           | 1   | a|0      |
           | 2   | [STR 3]  |
           | 3   | g|2      |
           | 4   | [STR 11] |
           | 5   | [STR 1]  |
           | 6   | [STR 7]  |
           | 7   | f|2      |
           | 8   | [REF 4]  |
           | 9   | [STR 3]  |
           | 10  | [STR 11] |
           | 11  | b|0      |
           | 12  | [STR 13] |
           | 13  | f|2      |
           | 14  | [STR 11] |
           | 15  | [STR 3]  |
           +-----+----------+"))))

(defn tee [v func]
  (func v)
  v)


(->
  (make-context)
  (query "f(X, g(X, a))")
  (query "f(b, Y)")
  (unify 12 6)
  diag
  (tee #(println ">" (resolve-struct % 6)))
  (tee #(println "<" (resolve-struct % 12)))
  (tee #(println "X" (resolve-struct % 1002)))
  (tee #(println "Y" (resolve-struct % 1003))))

; Heap                Registers           Variables
; -------------------------------------------------------
; ┌─────┬──────────┐  ┌─────┬──────────┐  ┌─────┬───────┐
; │ key │ value    │  │ key │ value    │  │ key │ value │
; ├─────┼──────────┤  ├─────┼──────────┤  ├─────┼───────┤
; │ 0   ╎ [STR 1]  │  │ X1  ╎ [STR 13] │  │ X   ╎ X2    │
; │ 1   ╎ a|0      │  │ X2  ╎ [STR 11] │  │ Y   ╎ X3    │
; │ 2   ╎ [STR 3]  │  │ X3  ╎ [REF 15] │  └─────┴───────┘
; │ 3   ╎ g|2      │  │ X4  ╎ [STR 1]  │
; │ 4   ╎ [STR 11] │  └─────┴──────────┘
; │ 5   ╎ [STR 1]  │
; │ 6   ╎ [STR 7]  │
; │ 7   ╎ f|2      │
; │ 8   ╎ [REF 4]  │
; │ 9   ╎ [STR 3]  │
; │ 10  ╎ [STR 11] │
; │ 11  ╎ b|0      │
; │ 12  ╎ [STR 13] │
; │ 13  ╎ f|2      │
; │ 14  ╎ [STR 11] │
; │ 15  ╎ [STR 3]  │
; └─────┴──────────┘
;
; > f(b, g(b, a))
; < f(b, g(b, a))
; X b
; Y g(b, a)

(->
  (make-context)
  ; p(Z, h(Z, W), f(W))
  (put-structure 'h|2, 'X3)  ;X3 = h
  (set-variable 'X2)         ; Z
  (set-variable 'X5)         ; W
  (put-structure 'f|1, 'X4)  ;X4 = f
  (set-value 'X5)            ; W
  (put-structure 'p|3, 'X1)  ;X1 = p
  (set-value 'X2)            ; Z
  (set-value 'X3)            ; X3
  (set-value 'X4)            ; X4

  ; p(f(X), h(Y, f(a)), Y)
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
  (get-structure 'a|0, 'X7)
  diag)

; Heap                Registers           Variables
; -------------------------------------------------
; ┌─────┬──────────┐  ┌─────┬──────────┐  ┌───────┐
; │ key │ value    │  │ key │ value    │  │ value │
; ├─────┼──────────┤  ├─────┼──────────┤  ├───────┤
; │ 0   ╎ [STR 1]  │  │ X1  ╎ [STR 8]  │  └───────┘
; │ 1   ╎ h|2      │  │ X2  ╎ [REF 2]  │
; │ 2   ╎ [STR 13] │  │ X3  ╎ [STR 1]  │
; │ 3   ╎ [STR 16] │  │ X4  ╎ [STR 5]  │
; │ 4   ╎ [STR 5]  │  │ X5  ╎ [REF 14] │
; │ 5   ╎ f|1      │  │ X6  ╎ [REF 3]  │
; │ 6   ╎ [REF 3]  │  │ X7  ╎ [REF 17] │
; │ 7   ╎ [STR 8]  │  └─────┴──────────┘
; │ 8   ╎ p|3      │
; │ 9   ╎ [REF 2]  │
; │ 10  ╎ [STR 1]  │
; │ 11  ╎ [STR 5]  │
; │ 12  ╎ [STR 13] │
; │ 13  ╎ f|1      │
; │ 14  ╎ [REF 3]  │
; │ 15  ╎ [STR 16] │
; │ 16  ╎ f|1      │
; │ 17  ╎ [STR 19] │
; │ 18  ╎ [STR 19] │
; │ 19  ╎ a|0      │
; └─────┴──────────┘





(->
  (make-context)
  (assoc :trace true)
  (query "p(Z, h(Z, W), f(W))")
  (program "p(f(X), h(Y, f(a)), Y)")
  diag
  (tee #(println "p" (resolve-struct % 7)))
  (tee #(println "W" (resolve-struct % 1005)))
  (tee #(println "X" (resolve-struct % 1005)))
  (tee #(println "Y" (resolve-struct % 1004)))
  (tee #(println "Z" (resolve-struct % 1002))))

; put_structure h|2, X3
; set_variable X2
; set_variable X5
; put_structure f|1, X4
; set_value X5
; put_structure p|3, X1
; set_value X2
; set_value X3
; set_value X4
; get_structure p|3, X1
; unify_variable X2
; unify_variable X3
; unify_variable X4
; get_structure f|1, X2
; unify_variable X5
; get_structure h|2, X3
; unify_value X4
; unify_variable X6
; get_structure f|1, X6
; unify_variable X7
; get_structure a|0, X7
;
; Heap                Registers           Variables
; -------------------------------------------------------
; ┌─────┬──────────┐  ┌─────┬──────────┐  ┌─────┬───────┐
; │ key │ value    │  │ key │ value    │  │ key │ value │
; ├─────┼──────────┤  ├─────┼──────────┤  ├─────┼───────┤
; │ 0   ╎ [STR 1]  │  │ X1  ╎ [STR 8]  │  │ W   ╎ X5    │
; │ 1   ╎ h|2      │  │ X2  ╎ [REF 2]  │  │ X   ╎ X5    │
; │ 2   ╎ [STR 13] │  │ X3  ╎ [STR 1]  │  │ Y   ╎ X4    │
; │ 3   ╎ [STR 16] │  │ X4  ╎ [STR 5]  │  │ Z   ╎ X2    │
; │ 4   ╎ [STR 5]  │  │ X5  ╎ [REF 14] │  └─────┴───────┘
; │ 5   ╎ f|1      │  │ X6  ╎ [REF 3]  │
; │ 6   ╎ [REF 3]  │  │ X7  ╎ [REF 17] │
; │ 7   ╎ [STR 8]  │  └─────┴──────────┘
; │ 8   ╎ p|3      │
; │ 9   ╎ [REF 2]  │
; │ 10  ╎ [STR 1]  │
; │ 11  ╎ [STR 5]  │
; │ 12  ╎ [STR 13] │
; │ 13  ╎ f|1      │
; │ 14  ╎ [REF 3]  │
; │ 15  ╎ [STR 16] │
; │ 16  ╎ f|1      │
; │ 17  ╎ [STR 19] │
; │ 18  ╎ [STR 19] │
; │ 19  ╎ a|0      │
; └─────┴──────────┘




(->
  (make-context)
  (assoc :trace true)
  (query "p(f(X), h(Y, f(a)), Y)")
  (program "p(Z, h(Z, W), f(W))")
  diag
  (tee #(println "p" (resolve-struct % 12)))
  (tee #(println "W" (resolve-struct % 1005)))
  (tee #(println "X" (resolve-struct % 1005)))
  (tee #(println "Y" (resolve-struct % 1004)))
  (tee #(println "Z" (resolve-struct % 1002))))

; put_structure f|1, X2
; set_variable X5
; put_structure a|0, X7
; put_structure f|1, X6
; set_value X7
; put_structure h|2, X3
; set_variable X4
; set_value X6
; put_structure p|3, X1
; set_value X2
; set_value X3
; set_value X4
; get_structure p|3, X1
; unify_variable X2
; unify_variable X3
; unify_variable X4
; get_structure h|2, X3
; unify_value X2
; unify_variable X5
; get_structure f|1, X4
; unify_value X5
;
; Heap                Registers           Variables
; -------------------------------------------------------
; ┌─────┬──────────┐  ┌─────┬──────────┐  ┌─────┬───────┐
; │ key │ value    │  │ key │ value    │  │ key │ value │
; ├─────┼──────────┤  ├─────┼──────────┤  ├─────┼───────┤
; │ 0   ╎ [STR 1]  │  │ X1  ╎ [STR 13] │  │ W   ╎ X5    │
; │ 1   ╎ f|1      │  │ X2  ╎ [STR 1]  │  │ X   ╎ X5    │
; │ 2   ╎ [STR 6]  │  │ X3  ╎ [STR 9]  │  │ Y   ╎ X4    │
; │ 3   ╎ [STR 4]  │  │ X4  ╎ [REF 10] │  │ Z   ╎ X2    │
; │ 4   ╎ a|0      │  │ X5  ╎ [STR 6]  │  └─────┴───────┘
; │ 5   ╎ [STR 6]  │  │ X6  ╎ [STR 6]  │
; │ 6   ╎ f|1      │  │ X7  ╎ [STR 4]  │
; │ 7   ╎ [STR 4]  │  └─────┴──────────┘
; │ 8   ╎ [STR 9]  │
; │ 9   ╎ h|2      │
; │ 10  ╎ [STR 1]  │
; │ 11  ╎ [STR 6]  │
; │ 12  ╎ [STR 13] │
; │ 13  ╎ p|3      │
; │ 14  ╎ [STR 1]  │
; │ 15  ╎ [STR 9]  │
; │ 16  ╎ [REF 10] │
; └─────┴──────────┘

(->
  (make-context)
  (assoc :trace true)
  (query "father(R, henry)")
  (program "father(richard, henry)")
  diag
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
  (make-context)
  (assoc :trace true)
  (query "father(R, henry)")
  (program "father(henry, richard)")
  diag
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
  (make-context)
  (assoc :trace true)
  (query "father(richard, J)")
  (program "father(W, K)")
  diag
  ;(tee #(println "J" (resolve-struct % 1003)))
  ;(tee #(println "K" (resolve-struct % 1003)))
  ;(tee #(println "W" (resolve-struct % 1002)))

  )

