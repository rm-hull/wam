# Warren's Abstract Machine
A gradual WAM implementation in Clojure following Hassan Aït-Kaci's tutorial reconstruction.

## Language ℒ₀

### Exercise 2.1 (pg. 9)
Verify that the effect of executing the sequence of instructions shown in Figure 2.3
(starting with `H` = 0) does indeed yield a correct heap representation for the term
_p(Z, h(Z, W), f(W))_ — the one shown earlier as Figure 2.1, in fact.

See [ℳ₀ machine instructions]() for implementation details

```clojure
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
```

Produces:

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

TBC...

## Language ℒ₁

TOOD

## References

* http://www.ai.sri.com/pubs/files/641.pdf
* http://wambook.sourceforge.net/wambook.pdf
* http://stefan.buettcher.org/cs/wam/wam.pdf
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
