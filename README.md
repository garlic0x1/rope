Immutable Ropes for Common Lisp

Have you ever wanted to make yet another Emacs clone?  Well now you
can use ropes to do it so you can tell everyone on Twitter and
Hackernews that your editor is blazingly fast and super serious and
uses fancy text structures.

None of the exported symbols mutate ropes, they return new ones.  This
lets us reuse as many leaves and branches as possible to reduce memory
usage, as well as enable a functional style of programming.

# Installation

With [Ultralisp](https://ultralisp.org/) installed:

```lisp
(ql:quickload :rope)
```

# Usage

Convert a string to a rope with `make-rope`, and turn it back into a
string with `write-rope` passing `nil` as the output.

You can also pass an input stream or pathname to `make-rope` to
efficiently read files.

```lisp
(let* ((rope (rope:make-rope "Immutable Ropes for Common Lisp"))
       (super (rope:insert-rope rope 10 "Super "))
       (superer (rope:kill-rope super 0 10)))
  (list
   (rope:write-rope rope nil)
   (rope:write-rope super nil)
   (rope:write-rope superer nil)))
```

Split a rope at an index:

```lisp
(let ((rope (rope:make-rope "Immutable Super Ropes for Common Lisp")))
  (multiple-value-bind (ante post) (rope:split-rope rope 16)
    (list ante post)))
```

Here is what the split ropes look like graphed with `:cl-dot`.  I
decreased `rope::*long-leaf*` to 8 to better visualize such a small
string.

![Split Rope](screenshots/split.png)

Concatenate ropes together:

```lisp
(let ((rope-a (rope:make-rope "Immutable Super Ropes"))
      (rope-b (rope:make-rope " for Common Lisp")))
  (rope:concat-rope rope-a rope-b))
```

![Concatenated Rope](screenshots/concat.png)

Kill a segment of a rope:

```lisp
(let ((rope (rope:make-rope "Immutable Ropes for Common Lisp")))
  (rope:write-rope (rope:kill-rope rope 20 27) t))
```

Get chars or strings at a position:

```lisp
(let ((rope (rope:make-rope "Immutable Ropes for Common Lisp")))
  (print (rope:index-rope rope 2))
  (print (rope:substr-rope rope 10 15)))
```

# Performance

Time to insert is a good measure since it splits and concatenates.
This graph demonstrates O(log(n)) performance:

![Insert Benchmark](screenshots/insert-benchmark.png)

Inserting into a gigabyte rope takes approximately 26 microseconds on
my i5-6200U.

# Dev Utils

If you want to generate graphs as shown above, you will need to
install `:cl-dot` (not in Quicklisp), and load `:rope/dev`.  Then you
can make a rope and run this:

```lisp
(defparameter rope::*long-leaf* 24)
(defparameter rope::*short-leaf* 8)

(let ((rope (rope::make-rope rope/dev::*string*)))
  (multiple-value-bind (ante post) (rope::split-rope rope 30)
    (rope/dev:graph-ropes (list rope ante post))))
```

If you are using Sly in Emacs, it will automatically open up a PNG
buffer.