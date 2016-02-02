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
    [wam.assert-helpers :refer :all]
    [wam.anciliary :refer [unify]]
    [wam.compiler :refer :all]
    [wam.parser :refer [parse-all]]
    [wam.grammar :refer [structure]]
    [wam.store :refer [heap registers make-context]]
    [table.core :refer :all]))

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
    (is (instr= (emit-instructions query-builder (parse-all structure "f(X, g(X,a))"))
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
       +---------------+------+------+"))

    (is (instr= (emit-instructions query-builder (parse-all structure "p(Z, h(Z, W), f(W))"))
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
       +---------------+------+------+"))))

(deftest check-program-builder
  (testing "Program builder"
    (is (instr= (emit-instructions program-builder (parse-all structure "p(f(X), h(Y, f(a)), Y)"))
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
       +----------------+------+------+"))))

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
           | 4   | [REF 4]  |
           | 5   | [STR 1]  |
           | 6   | [STR 7]  |
           | 7   | f|2      |
           | 8   | [STR 11] |
           | 9   | [STR 3]  |
           | 10  | [STR 11] |
           | 11  | b|0      |
           | 12  | [STR 13] |
           | 13  | f|2      |
           | 14  | [STR 11] |
           | 15  | [STR 3]  |
           +-----+----------+"))))

