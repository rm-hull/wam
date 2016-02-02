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
    [wam.compiler :refer :all]
    [wam.parser :refer [parse-all]]
    [wam.grammar :refer [structure]]
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
