# Warren's Abstract Machine

[![Build Status](https://travis-ci.org/rm-hull/wam.svg?branch=master)](http://travis-ci.org/rm-hull/wam)
[![Coverage Status](https://coveralls.io/repos/github/rm-hull/wam/badge.svg?branch=master)](https://coveralls.io/github/rm-hull/wam?branch=master)
[![Dependencies Status](https://versions.deps.co/rm-hull/wam/status.svg)](https://versions.deps.co/rm-hull/wam)
[![Maintenance](https://img.shields.io/maintenance/yes/2018.svg?maxAge=2592000)]()

A gradual WAM implementation in Clojure following Hassan Aït-Kaci's tutorial reconstruction.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Language ℒ₀ – Unification](#language-%E2%84%92%E2%82%80--unification)
  - [Exercise 2.1 (pg. 9)](#exercise-21-pg-9)
  - [EBNF ℒ₀ Grammar & Parser Combinators](#ebnf-%E2%84%92%E2%82%80-grammar--parser-combinators)
  - [Compiling ℒ₀ queries](#compiling-%E2%84%92%E2%82%80-queries)
  - [Compiling ℒ₀ programs](#compiling-%E2%84%92%E2%82%80-programs)
  - [Exercise 2.2 (pg. 14)](#exercise-22-pg-14)
  - [Exercise 2.3 (pg. 14)](#exercise-23-pg-14)
  - [Exercise 2.4 (pg. 14)](#exercise-24-pg-14)
  - [Exercise 2.5 (pg. 14)](#exercise-25-pg-14)
- [Language ℒ₁ – Argument Registers](#language-%E2%84%92%E2%82%81--argument-registers)
  - [Exercise 2.6 (pg. 18)](#exercise-26-pg-18)
  - [Exercise 2.7 (pg. 18)](#exercise-27-pg-18)
  - [Exercise 2.8 (pg. 18)](#exercise-28-pg-18)
  - [Exercise 2.9 (pg. 19)](#exercise-29-pg-19)
- [Language ℒ₂ – Flat Resolution](#language-%E2%84%92%E2%82%82--flat-resolution)
- [Language ℒ₃ – Prolog](#language-%E2%84%92%E2%82%83--prolog)
- [References](#references)
- [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Language ℒ₀ – Unification

### Exercise 2.1 (pg. 9)

> Verify that the effect of executing the sequence of instructions shown in
> Figure 2.3 (starting with `H` = 0) does indeed yield a correct heap
> representation for the term _p(Z, h(Z, W), f(W))_ — the one shown earlier
> as Figure 2.1, in fact.

See [ℳ₀ machine instructions](https://github.com/rm-hull/wam/blob/L0/src/wam/instruction_set.clj) for implementation details

```clojure
  (use 'wam.instruction-set)
  (use 'wam.store)
  (use 'table.core)

  (def context (make-context))

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
    heap
    (table :style :unicode))
```
Produces:
```
┌──────┬────────────┐
│ key  │ value      │
├──────┼────────────┤
│ 1000 ╎ [STR 1001] │
│ 1001 ╎ h|2        │
│ 1002 ╎ [REF 1002] │
│ 1003 ╎ [REF 1003] │
│ 1004 ╎ [STR 1005] │
│ 1005 ╎ f|1        │
│ 1006 ╎ [REF 1003] │
│ 1007 ╎ [STR 1008] │
│ 1008 ╎ p|3        │
│ 1009 ╎ [REF 1002] │
│ 1010 ╎ [STR 1001] │
│ 1011 ╎ [STR 1005] │
└──────┴────────────┘
```

### EBNF ℒ₀ Grammar & Parser Combinators

The simplistic EBNF [grammar rules](https://github.com/rm-hull/wam/blob/master/src/wam/grammar.clj)
for ℒ₀ below have been implemented using a [parser monad](https://github.com/rm-hull/jasentaa).

* _**&lt;Digit&gt;** ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'_

* _**&lt;Number&gt;** ::= &lt;Digit&gt; &lt;Digit&gt;*_

* _**&lt;LowerAlpha&gt;** ::= 'a' .. 'z'_

* _**&lt;UpperAlpha&gt;** ::= 'A' .. 'Z'_

* _**&lt;AlphaNum&gt;** ::= &lt;LowerAlpha&gt; | &lt;UpperAlpha&gt; | &lt;Digit&gt;_

* _**&lt;Predicate&gt;** ::= &lt;LowerAlpha&gt; &lt;AlphaNum&gt;*_

* _**&lt;Constant&gt;** ::= &lt;Number&gt;_

* _**&lt;Variable&gt;** ::= &lt;UpperAlpha&gt; &lt;AlphaNum&gt;* |_ '_'

* _**&lt;Structure&gt;** ::= &lt;Predicate&gt; | &lt;Predicate&gt; '(' &lt;List&gt; ')'_

* _**&lt;List&gt;** ::= &lt;Element&gt; | &lt;Element&gt; ',' &lt;List&gt;_

* _**&lt;Element&gt;** ::= &lt;Variable&gt; | &lt;Constant&gt; | &lt;Structure&gt;_

Parsing the term _p(Z, h(Z, W), f(W))_ with:

```clojure
(use 'wam.grammar)
(use 'jasentaa.parser)
(parse-all structure "p(Z, h(Z, W), f(W))")
```
yields a structure as follows:
```
#Structure{:functor p|3,
           :args (#Variable{:name Z}
                  #Structure{:functor h|2,
                             :args (#Variable{:name Z}
                                    #Variable{:name W}})}
                  #Structure{:functor f|1,
                             :args (#Variable{:name W})})}
```
### Compiling ℒ₀ queries

Now that the term _p(Z, h(Z, W), f(W))_ parses into a hierarchical data
structure, a breadth-first search is employed to allocate registers on a
least available index basis:

```clojure
(use 'wam.compiler)
(use 'wam.grammar)
(use 'jasentaa.parser)
(use 'table.core)

(def term (parse-all structure "p(Z, h(Z, W), f(W))"))
(table (register-allocation term) :style :unicode)
```
evaluates as:
```
┌──────────────────┬───────┐
│ key              │ value │
├──────────────────┼───────┤
│ p(Z h(Z W) f(W)) ╎ X1    │
│ Z                ╎ X2    │
│ h(Z W)           ╎ X3    │
│ f(W)             ╎ X4    │
│ W                ╎ X5    │
└──────────────────┴───────┘
```
Inspecting the structures, and indeed it matches as follows:

* X1 = p(X2, X3, X4)
* X2 = Z
* X3 = h(X2, X5)
* X4 = f(X5)
* X5 = W

Next, given that we have a linear register allocation, walking
the query term structures in depth-first post-order means that
instructions can be assembled as follows:

```clojure
(use 'wam.compiler)
(use 'wam.grammar)
(use 'table.core)

(table
  (cons
    ["instr" "arg1" "arg2"]
    (emit-instructions query-builder term (register-allocation term)))
  :style :unicode)
```

Which returns a list of instructions, which corresponds to Figure 2.3
in the tutorial:
```
┌──────────────────────────────────────────────┬──────┬──────┐
│ instr                                        │ arg1 │ arg2 │
├──────────────────────────────────────────────┼──────┼──────┤
│ #function[wam.instruction-set/put-structure] ╎ h|2  ╎ X3   │
│ #function[wam.instruction-set/set-variable]  ╎ X2   ╎      │
│ #function[wam.instruction-set/set-variable]  ╎ X5   ╎      │
│ #function[wam.instruction-set/put-structure] ╎ f|1  ╎ X4   │
│ #function[wam.instruction-set/set-value]     ╎ X5   ╎      │
│ #function[wam.instruction-set/put-structure] ╎ p|3  ╎ X1   │
│ #function[wam.instruction-set/set-value]     ╎ X2   ╎      │
│ #function[wam.instruction-set/set-value]     ╎ X3   ╎      │
│ #function[wam.instruction-set/set-value]     ╎ X4   ╎      │
└──────────────────────────────────────────────┴──────┴──────┘
```
The instructions are not directly executable as yet, as a context
must be supplied in the first argument to each instruction, but
they are however in a suitable format for returning a function
that can execute them given a context:

```clojure
(use 'wam.compiler)
(use 'wam.grammar)
(use 'wam.store)
(use 'table.core)

(def context (make-context))

(def query0
  (->>
    "p(Z, h(Z, W), f(W))"
    (parse-all structure)
    (compile-term query-builder)))

(-> context query0 heap table)
```
This produces the same heap representation as earlier, but significantly, was
instead generated automatically from executing emitted WAM instructions,
which were derived from hierarchical data structures, which in turn were
parsed from a string representation **"p(Z, h(Z, W), f(W))"**.
```
┌──────┬────────────┐
│ key  │ value      │
├──────┼────────────┤
│ 1000 ╎ [STR 1001] │
│ 1001 ╎ h|2        │
│ 1002 ╎ [REF 1002] │
│ 1003 ╎ [REF 1003] │
│ 1004 ╎ [STR 1005] │
│ 1005 ╎ f|1        │
│ 1006 ╎ [REF 1003] │
│ 1007 ╎ [STR 1008] │
│ 1008 ╎ p|3        │
│ 1009 ╎ [REF 1002] │
│ 1010 ╎ [STR 1001] │
│ 1011 ╎ [STR 1005] │
└──────┴────────────┘
```
### Compiling ℒ₀ programs

Compiling a program term follows a similar vein to query term construction:
registers are allocated breadth-first, but instead of walking the tree in
post-order, a program is walked in pre-order. The rules for emitting instructions
are also subtly different. Assuming the same helper methods as before:

```clojure
(use 'wam.compiler)
(use 'wam.grammar)
(use 'table.core)

; Assume the same helper functions as before

(def term (parse-all structure "p(f(X), h(Y, f(a)), Y)"))

(table
  (cons
    ["instr" "arg1" "arg2"]
    (emit-instructions program-builder term (register-allocation term)))
  :style :unicode)

```
Which returns a list of instructions, which corresponds to Figure 2.4
in the tutorial:
```
┌───────────────────────────────────────────────┬──────┬──────┐
│ instr                                         │ arg1 │ arg2 │
├───────────────────────────────────────────────┼──────┼──────┤
│ #function[wam.instruction-set/get-structure]  ╎ p|3  ╎ X1   │
│ #function[wam.instruction-set/unify-variable] ╎ X2   ╎      │
│ #function[wam.instruction-set/unify-variable] ╎ X3   ╎      │
│ #function[wam.instruction-set/unify-variable] ╎ X4   ╎      │
│ #function[wam.instruction-set/get-structure]  ╎ f|1  ╎ X2   │
│ #function[wam.instruction-set/unify-variable] ╎ X5   ╎      │
│ #function[wam.instruction-set/get-structure]  ╎ h|2  ╎ X3   │
│ #function[wam.instruction-set/unify-value]    ╎ X4   ╎      │
│ #function[wam.instruction-set/unify-variable] ╎ X6   ╎      │
│ #function[wam.instruction-set/get-structure]  ╎ f|1  ╎ X6   │
│ #function[wam.instruction-set/unify-variable] ╎ X7   ╎      │
│ #function[wam.instruction-set/get-structure]  ╎ a|0  ╎ X7   │
└───────────────────────────────────────────────┴──────┴──────┘
```
### Exercise 2.2 (pg. 14)

> Give heap representations for the terms _f(X, g(X, a))_ and _f(b, Y)_.
> Let _a<sub>1</sub>_ and _a<sub>2</sub>_ be their respective heap addresses,
> and let _a<sub>x</sub>_ and _a<sub>y</sub>_ be the heap addresses
> corresponding to variables _X_ and _Y_, respectively. Trace the effects of
> executing _unify(a<sub>1</sub>, a<sub>2</sub>)_, verifying that it terminates
> with the eventual dereferenced bindings from _a<sub>x</sub>_ and
> _a<sub>y</sub>_ corresponding to _X = b_ and _Y = g(b, a)_.

By applying the query terms to an empty context,

```clojure
(use 'wam.compiler)
(use 'wam.store)
(use 'table.core)

(->
  (make-context)
  (query "f(X, g(X, a))")
  (query "f(b, Y)")
  diag)
```
Gives the following heap structure. Note that the heap addresses for
_a<sub>1</sub>_, _a<sub>2</sub>_, _a<sub>x</sub>_ and _a<sub>y</sub>_
have been annotated at locations 1006, 1012, 1008 and 1015 respectively.
```
Heap                   Registers             Variables
------------------------------------------------------------
┌──────┬────────────┐  ┌─────┬────────────┐  ┌─────┬───────┐
│ key  │ value      │  │ key │ value      │  │ key │ value │
├──────┼────────────┤  ├─────┼────────────┤  ├─────┼───────┤
│ 1000 ╎ [STR 1001] │  │ X1  ╎ [STR 1013] │  │ X   ╎ X2    │
│ 1001 ╎ a|0        │  │ X2  ╎ [STR 1011] │  │ Y   ╎ X3    │
│ 1002 ╎ [STR 1003] │  │ X3  ╎ [REF 1015] │  └─────┴───────┘
│ 1003 ╎ g|2        │  │ X4  ╎ [STR 1001] │
│ 1004 ╎ [REF 1004] │  └─────┴────────────┘
│ 1005 ╎ [STR 1001] │
│ 1006 ╎ [STR 1007] │    ← a1
│ 1007 ╎ f|2        │
│ 1008 ╎ [REF 1004] │    ← aX
│ 1009 ╎ [STR 1003] │
│ 1010 ╎ [STR 1011] │
│ 1011 ╎ b|0        │
│ 1012 ╎ [STR 1013] │    ← a2
│ 1013 ╎ f|2        │
│ 1014 ╎ [STR 1011] │
│ 1015 ╎ [REF 1015] │    ← aY
└──────┴────────────┘
```
Now, calling _unify(a<sub>1</sub>, a<sub>2</sub>)_, the changed context store
is displayed below.

```clojure
(use 'wam.anciliary)

(defn tee [v func]
  (func v)
  v)

(->
  (make-context)
  (query "f(X, g(X, a))")
  (query "f(b, Y)")
  (unify 1012 1006)
  diag
  (tee #(println "X =" (resolve-struct % (register-address 'X2))))
  (tee #(println "Y =" (resolve-struct % (register-address 'X3)))))
```
Note that the context failed flag returns as false (not shown), indicating
unification was successful.
```
Heap                   Registers             Variables
------------------------------------------------------------
┌──────┬────────────┐  ┌─────┬────────────┐  ┌─────┬───────┐
│ key  │ value      │  │ key │ value      │  │ key │ value │
├──────┼────────────┤  ├─────┼────────────┤  ├─────┼───────┤
│ 1000 ╎ [STR 1001] │  │ X1  ╎ [STR 1013] │  │ X   ╎ X2    │
│ 1001 ╎ a|0        │  │ X2  ╎ [STR 1011] │  │ Y   ╎ X3    │
│ 1002 ╎ [STR 1003] │  │ X3  ╎ [REF 1015] │  └─────┴───────┘
│ 1003 ╎ g|2        │  │ X4  ╎ [STR 1001] │
│ 1004 ╎ [STR 1011] │  └─────┴────────────┘
│ 1005 ╎ [STR 1001] │
│ 1006 ╎ [STR 1007] │
│ 1007 ╎ f|2        │
│ 1008 ╎ [REF 1004] │
│ 1009 ╎ [STR 1003] │
│ 1010 ╎ [STR 1011] │
│ 1011 ╎ b|0        │
│ 1012 ╎ [STR 1013] │
│ 1013 ╎ f|2        │
│ 1014 ╎ [STR 1011] │
│ 1015 ╎ [STR 1003] │
└──────┴────────────┘

X = b
Y = g(b, a)
```
Inspecting the heap, and it becomes clear that:

* dereferencing _a<sub>x</sub>_, `STR 1011` → `b|0`, so _X = b_
* dereferencing _a<sub>y</sub>_, `STR 1015` → `STR 1003` → `g|2`, so _Y = g(X, a) = g(b, a)_

### Exercise 2.3 (pg. 14)

> Verify that the effect of executing the sequence of instructions shown in
> Figure 2.4 right after that in Figure 2.3 produces the MGU of the terms
> _p(Z, h(Z, W), f(W))_ and _p(f(X), h(Y, f(a)), Y)_. That is, the
> (dereferenced) bindings corresponding to _W = f(a)_, _X = f(a)_,
> _Y = f(f(a))_, _Z = f(f(a))_.

_MGU_ = Most General Unifier

```clojure
(->
  (make-context)

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
  (get-structure 'a|0, 'X7)

  diag

  (tee #(println "W =" (resolve-struct % (register-address 'X5))))
  (tee #(println "X =" (resolve-struct % (register-address 'X5))))
  (tee #(println "Y =" (resolve-struct % (register-address 'X4))))
  (tee #(println "Z =" (resolve-struct % (register-address 'X2)))))
```
Prints:
```
Heap                   Registers             Variables
------------------------------------------------------
┌──────┬────────────┐  ┌─────┬────────────┐  ┌───────┐
│ key  │ value      │  │ key │ value      │  │ value │
├──────┼────────────┤  ├─────┼────────────┤  ├───────┤
│ 1000 ╎ [STR 1001] │  │ X1  ╎ [STR 1008] │  └───────┘
│ 1001 ╎ h|2        │  │ X2  ╎ [REF 1002] │
│ 1002 ╎ [STR 1013] │  │ X3  ╎ [STR 1001] │
│ 1003 ╎ [STR 1016] │  │ X4  ╎ [STR 1005] │
│ 1004 ╎ [STR 1005] │  │ X5  ╎ [REF 1014] │
│ 1005 ╎ f|1        │  │ X6  ╎ [REF 1003] │
│ 1006 ╎ [REF 1003] │  │ X7  ╎ [REF 1017] │
│ 1007 ╎ [STR 1008] │  └─────┴────────────┘
│ 1008 ╎ p|3        │
│ 1009 ╎ [REF 1002] │
│ 1010 ╎ [STR 1001] │
│ 1011 ╎ [STR 1005] │
│ 1012 ╎ [STR 1013] │
│ 1013 ╎ f|1        │
│ 1014 ╎ [REF 1003] │
│ 1015 ╎ [STR 1016] │
│ 1016 ╎ f|1        │
│ 1017 ╎ [STR 1019] │
│ 1018 ╎ [STR 1019] │
│ 1019 ╎ a|0        │
└──────┴────────────┘

W = f(a)
X = f(a)
Y = f(f(a))
Z = f(f(a))
```

### Exercise 2.4 (pg. 14)

> What are the respective sequences of ℳ₀ instructions for ℒ₀ _query_
> term ?-_p(f(X), h(Y, f(a)), Y)_ and _program_ term _p(Z, h(Z, W), f(W))_?

Setting the execution trace to `true` and running the two terms:

```clojure
(->
  (make-context)
  (assoc :trace true)
  (query "p(Z, h(Z, W), f(W))")
  (program "p(f(X), h(Y, f(a)), Y)"))
```

Gives the following instruction list:
```
put_structure h|2, X3
set_variable X2
set_variable X5
put_structure f|1, X4
set_value X5
put_structure p|3, X1
set_value X2
set_value X3
set_value X4
get_structure p|3, X1
unify_variable X2
unify_variable X3
unify_variable X4
get_structure f|1, X2
unify_variable X5
get_structure h|2, X3
unify_value X4
unify_variable X6
get_structure f|1, X6
unify_variable X7
get_structure a|0, X7
```

### Exercise 2.5 (pg. 14)

> After doing Exercise 2.4, verify that the effects of executing the sequence
> you produced yields the same solution as that of [Exercise 2.3](#exercise-23-pg-14).


Executing:

```clojure
(->
  (make-context)
  (assoc :trace true)
  (query "p(Z, h(Z, W), f(W))")
  (program "p(f(X), h(Y, f(a)), Y)")
  diag

  (tee #(println "W =" (resolve-struct % (register-address 'X5))))
  (tee #(println "X =" (resolve-struct % (register-address 'X5))))
  (tee #(println "Y =" (resolve-struct % (register-address 'X4))))
  (tee #(println "Z =" (resolve-struct % (register-address 'X2)))))
```
This gives the same output as [exercise 2.3](#exercise-23-pg-14) (albeit with extra register allocations):
```
Heap                   Registers             Variables
------------------------------------------------------------
┌──────┬────────────┐  ┌─────┬────────────┐  ┌─────┬───────┐
│ key  │ value      │  │ key │ value      │  │ key │ value │
├──────┼────────────┤  ├─────┼────────────┤  ├─────┼───────┤
│ 1000 ╎ [STR 1001] │  │ X1  ╎ [STR 1008] │  │ W   ╎ X5    │
│ 1001 ╎ h|2        │  │ X2  ╎ [REF 1002] │  │ X   ╎ X5    │
│ 1002 ╎ [STR 1013] │  │ X3  ╎ [STR 1001] │  │ Y   ╎ X4    │
│ 1003 ╎ [STR 1016] │  │ X4  ╎ [STR 1005] │  │ Z   ╎ X2    │
│ 1004 ╎ [STR 1005] │  │ X5  ╎ [REF 1014] │  └─────┴───────┘
│ 1005 ╎ f|1        │  │ X6  ╎ [REF 1003] │
│ 1006 ╎ [REF 1003] │  │ X7  ╎ [REF 1017] │
│ 1007 ╎ [STR 1008] │  └─────┴────────────┘
│ 1008 ╎ p|3        │
│ 1009 ╎ [REF 1002] │
│ 1010 ╎ [STR 1001] │
│ 1011 ╎ [STR 1005] │
│ 1012 ╎ [STR 1013] │
│ 1013 ╎ f|1        │
│ 1014 ╎ [REF 1003] │
│ 1015 ╎ [STR 1016] │
│ 1016 ╎ f|1        │
│ 1017 ╎ [STR 1019] │
│ 1018 ╎ [STR 1019] │
│ 1019 ╎ a|0        │
└──────┴────────────┘

W = f(a)
X = f(a)
Y = f(f(a))
Z = f(f(a))
```

## Language ℒ₁ – Argument Registers

### Exercise 2.6 (pg. 18)

> Verify that the effect of executing the sequence of ℳ₁ instructions
> shown in Figure 2.9 produces the same heap representation as that produced by
> the ℳ₀ code of Figure 2.3 (see [Exercise 2.1](#exercise-21-pg-9)).

Assuming the same imports and initial context as perviously:

```clojure
(->
  (make-context)
  (put-variable 'X4, 'A1)
  (put-structure 'h|2, 'A2)
  (set-value 'X4)
  (set-variable 'X5)
  (put-structure 'f|1, 'A3)
  (set-value 'X5)
  heap
  table)
```
gives:
```
┌──────┬────────────┐
│ key  │ value      │
├──────┼────────────┤
│ 1000 ╎ [REF 1000] │
│ 1001 ╎ [STR 1002] │
│ 1002 ╎ h|2        │
│ 1003 ╎ [REF 1000] │
│ 1004 ╎ [REF 1004] │
│ 1005 ╎ [STR 1006] │
│ 1006 ╎ f|1        │
│ 1007 ╎ [REF 1004] │
└──────┴────────────┘
```
Apart from the term root, the heap is layed out _similarly_ to that of
Figure 2.3 as below, albeit with different references:
```
                         ┌──────┬────────────┐
┌──────┬────────────┐    │ key  │ value      │
│ key  │ value      │    ├──────┼────────────┤
├──────┼────────────┤    │ 1000 ╎ [REF 1000] │
│ 1000 ╎ [STR 1001] │    │ 1001 ╎ [STR 1002] │
│ 1001 ╎ h|2        │    │ 1002 ╎ h|2        │
│ 1002 ╎ [REF 1002] │    │ 1003 ╎ [REF 1000] │
│ 1003 ╎ [REF 1003] │    │ 1004 ╎ [REF 1004] │
│ 1004 ╎ [STR 1005] │    │ 1005 ╎ [STR 1006] │
│ 1005 ╎ f|1        │    │ 1006 ╎ f|1        │
│ 1006 ╎ [REF 1003] │    │ 1007 ╎ [REF 1004] │
│ 1007 ╎ [STR 1008] │    └──────┴────────────┘
│ 1008 ╎ p|3        │
│ 1009 ╎ [REF 1002] │
│ 1010 ╎ [STR 1001] │
│ 1011 ╎ [STR 1005] │
└──────┴────────────┘

```

### Exercise 2.7 (pg. 18)

> Verify that the effect of executing the sequence of ℳ₁ instructions
> shown in Figure 2.10 right after that in Figure 2.9 produces the MGU of the
> terms _p(Z, h(Z, W), f(W))_ and _p(f(X), h(Y, f(a)), Y)_. That is, the binding
> _W = f(a)_, _X = f(a)_, _Y = f(f(a))_, _Z = f(f(a))_.

Defining _p/3_ as:

```clojure
(def p|3
  (list
    [get-structure 'f|1, 'A1]
    [unify-variable 'X4]
    [get-structure 'h|2, 'A2]
    [unify-variable 'X5]
    [unify-variable 'X6]
    [get-value 'X5, 'A3]
    [get-structure 'f|1, 'X6]
    [unify-variable 'X7]
    [get-structure 'a|0, 'X7]
    [proceed]))
```
Then, executing the program term directly after the query term:

```clojure
(->
  ctx
  (put-variable 'X4, 'A1)
  (put-structure 'h|2, 'A2)
  (set-value 'X4)
  (set-variable 'X5)
  (put-structure 'f|1, 'A3)
  (set-value 'X5)
  (load 'p|3 p|3)
  (call 'p|3)
  diag
  (tee #(println "W =" (resolve-struct % (register-address 'X4))))
  (tee #(println "X =" (resolve-struct % (register-address 'X4))))
  (tee #(println "Y =" (resolve-struct % (register-address 'A3))))
  (tee #(println "Z =" (resolve-struct % (register-address 'X5)))))
```

gives:

```
Heap                   Registers             Variables
------------------------------------------------------
┌──────┬────────────┐  ┌─────┬────────────┐  ┌───────┐
│ key  │ value      │  │ key │ value      │  │ value │
├──────┼────────────┤  ├─────┼────────────┤  ├───────┤
│ 1000 ╎ [STR 1009] │  │ X1  ╎ [REF 1000] │  └───────┘
│ 1001 ╎ [STR 1002] │  │ X2  ╎ [STR 1002] │
│ 1002 ╎ h|2        │  │ X3  ╎ [STR 1006] │
│ 1003 ╎ [REF 1000] │  │ X4  ╎ [REF 1010] │
│ 1004 ╎ [STR 1012] │  │ X5  ╎ [REF 1000] │
│ 1005 ╎ [STR 1006] │  │ X6  ╎ [REF 1004] │
│ 1006 ╎ f|1        │  │ X7  ╎ [REF 1013] │
│ 1007 ╎ [REF 1004] │  └─────┴────────────┘
│ 1008 ╎ [STR 1009] │
│ 1009 ╎ f|1        │
│ 1010 ╎ [REF 1004] │
│ 1011 ╎ [STR 1012] │
│ 1012 ╎ f|1        │
│ 1013 ╎ [STR 1015] │
│ 1014 ╎ [STR 1015] │
│ 1015 ╎ a|0        │
└──────┴────────────┘

W = f(a)
X = f(a)
Y = f(f(a))
Z = f(f(a))
```
### Exercise 2.8 (pg. 18)

> What are the respective sequences of ℳ₁ instructions for ℒ₁ _query_
> term ?-_p(f(X), h(Y, f(a)), y)_ and ℒ₁ _program_ term _p(Z, h(Z, W), f(W))_?

There is a bit of a leap here in the tutorial, and I'm not sure if I fully
understand, but the query term ?-_p(f(X), h(Y, f(a)), y)_ is now build from
the following instructions:

```
put-structure f|1, A1
set-variable X4)
put-structure h|2, A2
set-variable A3)
put-structure f|1, X5
put-structure a|0, X6
set-value A3
call p|3
```

And the program term _p(Z, h(Z, W), f(W))_ is comprised of:

```
unify-variable A1
get-structure h|2, A2
unify-value A1
unify-variable X4
get-structure f|1, A3
unify-value X4
proceed
```

### Exercise 2.9 (pg. 19)

> After doing [Exercise 2.8](#exercise-28-pg-18), verify that the effect of executing the
> sequence you produced yields the same solution as that of [Exercise 2.7](#exercise-27-pg-18).


Executing the program against the query term does give the same unification
result as previously:

```clojure
(def p|3
  (list
    [unify-variable 'A1]
    [get-structure 'h|2, 'A2]
    [unify-value 'A1]
    [unify-variable 'X4]
    [get-structure 'f|1, 'A3]
    [unify-value 'X4]
    [proceed]))

(->
  ctx
  (put-structure 'f|1, 'A1)
  (set-variable 'X4)
  (put-structure 'h|2, 'A2)
  (set-variable 'A3)
  (put-structure 'f|1, 'X5)
  (put-structure 'a|0, 'X6)
  (set-value 'A3)
  (load 'p|3 p|3)
  (call 'p|3)
  (diag)
  (tee #(println "W =" (resolve-struct % (register-address 'X4))))
  (tee #(println "X =" (resolve-struct % (register-address 'X4))))
  (tee #(println "Y =" (resolve-struct % (register-address 'A3))))
  (tee #(println "Z =" (resolve-struct % (register-address 'A1)))))
```

Outputs:

```
Heap                   Registers             Variables
------------------------------------------------------
┌──────┬────────────┐  ┌─────┬────────────┐  ┌───────┐
│ key  │ value      │  │ key │ value      │  │ value │
├──────┼────────────┤  ├─────┼────────────┤  ├───────┤
│ 1000 ╎ [STR 1001] │  │ X1  ╎ [STR 1001] │  └───────┘
│ 1001 ╎ f|1        │  │ X2  ╎ [STR 1004] │
│ 1002 ╎ [STR 1007] │  │ X3  ╎ [REF 1005] │
│ 1003 ╎ [STR 1004] │  │ X4  ╎ [STR 1007] │
│ 1004 ╎ h|2        │  │ X5  ╎ [STR 1007] │
│ 1005 ╎ [STR 1001] │  │ X6  ╎ [STR 1009] │
│ 1006 ╎ [STR 1007] │  └─────┴────────────┘
│ 1007 ╎ f|1        │
│ 1008 ╎ [STR 1009] │
│ 1009 ╎ a|0        │
│ 1010 ╎ [REF 1005] │
└──────┴────────────┘

W = f(a)
X = f(a)
Y = f(f(a))
Z = f(f(a))
```

## Language ℒ₂ – Flat Resolution

TODO

## Language ℒ₃ – Prolog

TODO

## References

* http://www.ai.sri.com/pubs/files/641.pdf
* http://wambook.sourceforge.net/wambook.pdf
* http://stefan.buettcher.org/cs/wam/wam.pdf
* http://www.cs.ox.ac.uk/jeremy.gibbons/publications/wam.pdf
* https://gist.github.com/kachayev/b5887f66e2985a21a466

## License

The MIT License (MIT)

Copyright (c) 2015 Richard Hull

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
