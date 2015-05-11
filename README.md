# Warren's Abstract Machine
A gradual WAM implementation in Clojure following Hassan Aït-Kaci's tutorial reconstruction.

## Language ℒ₀

#### Exercise 2.1 (pg. 9)

> Verify that the effect of executing the sequence of instructions shown in 
> Figure 2.3 (starting with `H` = 0) does indeed yield a correct heap 
> representation for the term _p(Z, h(Z, W), f(W))_ — the one shown earlier
> as Figure 2.1, in fact.

See [ℳ₀ machine instructions](https://github.com/rm-hull/wam/blob/master/L0/src/wam/l0/instruction_set.clj) for implementation details

```clojure
  (use 'wam.l0.instruction-set)
  (use 'wam.l0.store)
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
    table)
```
Produces:
```
   +-----+---------+
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
   +-----+---------+
```

#### EBNF ℒ₀ Grammar & Parser Combinators

The simplistic EBNF [grammar rules](https://github.com/rm-hull/wam/blob/master/L0/src/wam/l0/grammar.clj) 
for ℒ₀ below have been implemented using a [parser monad](https://github.com/rm-hull/wam/blob/master/L0/src/wam/l0/parser.clj).

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
(use 'wam.l0.grammar)
(parse-all structure "p(Z, h(Z, W), f(W))")
```
yields:
```
#Structure{:functor p|3, 
           :args (#Variable{:name Z} 
                  #Structure{:functor h|2, 
                             :args (#Variable{:name Z} 
                                    #Variable{:name W}})} 
                  #Structure{:functor f|1, 
                             :args (#Variable{:name W})})}
```
#### Compiling ℒ₀ queries

Now that the term _p(Z, h(Z, W), f(W))_ parses into a hierarchical data
structure, a breadth-first search is employed to allocate registers on a
least available index basis:

```clojure
(use 'wam.l0.compiler)
(use 'wam.l0.grammar)
(use 'table.core)

(def term (parse-all structure "p(Z, h(Z, W), f(W))"))
(table (register-allocation term))
```
evaluates as:
```
+-----------------------------------+-------+
| key                               | value |
+-----------------------------------+-------+
| wam.l0.grammar.Structure@d2562d9f | X1    |
| wam.l0.grammar.Variable@d77f6b5c  | X2    |
| wam.l0.grammar.Structure@c8c464ec | X3    |
| wam.l0.grammar.Structure@b1308ecc | X4    |
| wam.l0.grammar.Variable@d77f7490  | X5    |
+-----------------------------------+-------+  
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
(use 'wam.l0.compiler)
(use 'wam.l0.grammar)
(use 'table.core)

; Some helper functions to get round limitations in table
(defn inflate [table]
  (let [max-cols (reduce max 0 (map count table))]
    (map #(take max-cols (lazy-cat % (repeat nil))) table)))

(defn headers [& headers]
  (fn [table] (cons headers table)))

(def table' (comp table inflate (headers "instr" "arg1" "arg2")))

(def term (parse-all structure "p(Z, h(Z, W), f(W))"))
(table' (emit-instructions query-builder term))
```

Which returns a list of instructions, which corresponds to Figure 2.3
in the tutorial:
```
+----------------------------------------------+------+------+
| instr                                        | arg1 | arg2 |
+----------------------------------------------+------+------+
| wam.l0.instruction_set$put_structure@14f613e | h|2  | X3   |
| wam.l0.instruction_set$set_variable@94c1c0   | X2   |      |
| wam.l0.instruction_set$set_variable@94c1c0   | X5   |      |
| wam.l0.instruction_set$put_structure@14f613e | f|1  | X4   |
| wam.l0.instruction_set$set_value@45176e      | X5   |      |
| wam.l0.instruction_set$put_structure@14f613e | p|3  | X1   |
| wam.l0.instruction_set$set_value@45176e      | X2   |      |
| wam.l0.instruction_set$set_value@45176e      | X3   |      |
| wam.l0.instruction_set$set_value@45176e      | X4   |      |
+----------------------------------------------+------+------+
```
The instructions are not directly executable as yet, as a context
must be supplied in the first argument to each instruction, but 
they are however in a suitable format for returning a function 
that can execute them given a context:

```clojure
(use 'wam.l0.compiler)
(use 'wam.l0.grammar)
(use 'wam.l0.store)
(use 'table.core)

(def term (parse-all structure "p(Z, h(Z, W), f(W))"))
(table' (emit-instructions query-builder term))

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
+-----+---------+
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
+-----+---------+
```
#### Compiling ℒ₀ programs

Compiling a program term follows a similar vein to query term construction:
registers are allocated breadth-first, but instead of walking the tree in 
post-order, a program is walked in pre-order. The rules for emitting instructions
are also subtly different. Assuming the same helper methods as before:

```clojure
(use 'wam.l0.compiler)
(use 'wam.l0.grammar)
(use 'table.core)

; Assume the same helper functions as before

(def term (parse-all structure "p(f(X), h(Y, f(a)), Y)"))
(table' (emit-instructions program-builder term))
```
Which returns a list of instructions, which corresponds to Figure 2.4
in the tutorial:
```
+-----------------------------------------------+------+------+
| instr                                         | arg1 | arg2 |
+-----------------------------------------------+------+------+
| wam.l0.instruction_set$get_structure@1458d55  | p|3  | X1   |
| wam.l0.instruction_set$unify_variable@1c40c01 | X2   |      |
| wam.l0.instruction_set$unify_variable@1c40c01 | X3   |      |
| wam.l0.instruction_set$unify_variable@1c40c01 | X4   |      |
| wam.l0.instruction_set$get_structure@1458d55  | f|1  | X2   |
| wam.l0.instruction_set$unify_variable@1c40c01 | X5   |      |
| wam.l0.instruction_set$get_structure@1458d55  | h|2  | X3   |
| wam.l0.instruction_set$unify_value@f92e0d     | X4   |      |
| wam.l0.instruction_set$unify_variable@1c40c01 | X6   |      |
| wam.l0.instruction_set$get_structure@1458d55  | f|1  | X6   |
| wam.l0.instruction_set$unify_variable@1c40c01 | X7   |      |
| wam.l0.instruction_set$get_structure@1458d55  | a|0  | X7   |
+-----------------------------------------------+------+------+
```
#### Exercise 2.2 (pg. 14)

> Give heap representations for the terms _f(X, g(X, a))_ and _f(b, Y)_.
> Let _a<sub>1</sub>_ and _a<sub>2</sub>_ be their respective heap addresses, 
> and let _a<sub>x</sub>_ and _a<sub>y</sub>_ be the heap addresses 
> corresponding to variables _X_ and _Y_, respectively. Trace the effects of 
> executing _unify(a<sub>1</sub>, a<sub>2</sub>)_, verifying that it terminates
> with the eventual dereferenced bindings from _a<sub>x</sub>_ and 
> _a<sub>y</sub>_ corresponding to _X = b_ and _Y = g(b, a)_.

By applying the query terms to an empty context,

```clojure
(use 'wam.l0.compiler)
(use 'wam.l0.store)
(use 'table.core)

(def context (make-context))

(def new-context
  (->
    context
    (query "f(X, g(X, a))")
    (query "f(b, Y)")))

(->
  new-context
  heap
  table)
```
Gives the following heap structure. Note that the heap addresses for 
_a<sub>1</sub>_, _a<sub>2</sub>_, _a<sub>x</sub>_ and _a<sub>y</sub>_
have been annotated at locations 6,12, 8 and 15 respectively.
```
+-----+----------+
| key | value    |
+-----+----------+
| 0   | [STR 1]  |
| 1   | a|0      |
| 2   | [STR 3]  |
| 3   | g|2      |
| 4   | [REF 4]  |
| 5   | [STR 1]  |
| 6   | [STR 7]  |   <-- a1
| 7   | f|2      |
| 8   | [REF 4]  |   <-- aX
| 9   | [STR 3]  |
| 10  | [STR 11] |
| 11  | b|0      |
| 12  | [STR 13] |   <-- a2
| 13  | f|2      |
| 14  | [STR 11] |
| 15  | [REF 15] |   <-- aY
+-----+----------+
```
Now, calling _unify(a<sub>1</sub>, a<sub>2</sub>)_, the changed context store
is displayed below. 

```clojure
(->
  new-context
  (unify 6 12)
  heap
  table)
```
Note that the context failed flag returns as false (not shown), indicating 
unification was successful.
```
+-----+----------+
| key | value    |
+-----+----------+
| 0   | [STR 1]  |
| 1   | a|0      |
| 2   | [STR 3]  |
| 3   | g|2      |
| 4   | [REF 4]  |
| 5   | [STR 1]  |
| 6   | [STR 7]  |   <-- a1
| 7   | f|2      |
| 8   | [REF 14] |   <-- aX
| 9   | [STR 3]  |
| 10  | [STR 11] |
| 11  | b|0      |
| 12  | [STR 13] |   <-- a2
| 13  | f|2      |
| 14  | [STR 11] |
| 15  | [REF 9]  |   <-- aY
+-----+----------+
```
Inspecting the heap, and it becomes clear that:

* dereferencing _a<sub>x</sub>_, `REF 14` → `STR 11` → `b|0`, so _X = b_
* dereferencing _a<sub>y</sub>_, `REF 9` → `STR 3` → `g|2`, so _Y = g(X, a) = g(b, a)_

#### Exercise 2.3 (pg. 14)

> Verify that the effect of executing the sequence of instructions shown in
> Figure 2.4 right after that in Figure 2.3 produces the MGU of the terms
> _p(Z, h(Z, W), f(W))_ and _p(f(X), h(Y, f(a)), Y)_. That is, the 
> (dereferenced) bindings corresponding to _W = f(a)_, _X = f(a)_, 
> _Y = f(f(a))_, _Z = f(f(a))_.

_MGU_ = Most General Unifier

TODO

#### Exercise 2.4 (pg. 14)

> What are the respective sequences of ℳ₀ instructions for ℒ₀ _query_
> term ?-_p(f(X), h(Y, f(a)), Y)_ and _program_ term _p(Z, h(Z, W), f(W))_?

TODO

#### Exercise 2.5 (pg. 14)

> After doing Exercise 2.4, verify that the effects of executing the sequence
> you produced yields the same solution as that of Exercise 2.3.


TODO

## Language ℒ₁

TOOD

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
