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
(ns wam.l0.instruction-set)

(defn put-structure
  "This instruction marks the beginning of a structure (without
   embedded substructures) occurring as a goal argument. The
   instruction pushes the functor f|N for the structure onto the
   heap, and puts a corresponding structure pointer into register
   Xi. Execution then proceeds in \"write\" mode."
  [ctx f|N Xi]
  (let [h (get-in ctx [:pointer :h])
        v ['STR (inc h)]]
    (->
      ctx
      (assoc-in [:heap h] v)
      (assoc-in [:heap (inc h)] f|N)
      (assoc-in [:registers Xi] v)
      (update-in [:pointer :h] (partial + 2)))))

; Slight discrepancy between what David Warren refers to as 'put_variable'
; and Aït-Kaci as 'set_variable'
(defn set-variable
  "This instruction represents an argument of the final goal that is an
   unbound variable. THe instruction creates an unbound variable on the
   heap, and puts a reference to it in the Xi register."
  [ctx Xi]
  (let [h (get-in ctx [:pointer :h])
        v ['REF h]]
    (->
      ctx
      (assoc-in [:heap h] v)
      (assoc-in [:registers Xi] v)
      (update-in [:pointer :h] inc))))

(defn set-value [ctx Xi]
  (let [h (get-in ctx [:pointer :h])
        v (get-in ctx [:registers Xi])]
    (->
      ctx
      (assoc-in [:heap h] v)
      (update-in [:pointer :h] inc))))


(comment
  ; Exercise 2.1 (pg. 9)
  ; Compiled code for L0 query ?-p(Z,h(Z,W),f(W)).
  (use '[table.core :only [table]])

  (def context {
    :pointer {:h 0}
    :heap (sorted-map)
    :registers (sorted-map)})

  (->
    context
    (put-structure 'h|2, 'X3)
    (set-variable 'X2)
    (set-variable 'X5)
    (put-structure 'f|1, 'X4)
    (set-value 'X5)
    (put-structure 'p|3, 'X1)
    (set-value 'X2)
    (set-value 'X3)
    (set-value 'X4)
    :heap
    table)
  ; +-----+---------+
  ; | key | value   |
  ; +-----+---------+
  ; | 0   | [STR 1] |
  ; | 1   | h|2     |
  ; | 2   | [REF 2] |
  ; | 3   | [REF 3] |
  ; | 4   | [STR 5] |
  ; | 5   | f|1     |
  ; | 6   | [REF 3] |
  ; | 7   | [STR 8] |
  ; | 8   | p|3     |
  ; | 9   | [REF 2] |
  ; | 10  | [STR 1] |
  ; | 11  | [STR 5] |
  ; +-----+---------+
)