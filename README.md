Lisp interpreter
================

A lisp interpreter written for the functional programming class at EPFL.

This is a copy of an assignment I was given. I thought it was an interesting one
so I shared it

How to use
----------
Simply run `sbt` in the project root and then `run` to get to the interpreter.
This will run a (supposedly) Scheme-- interpreter. We were given specifications from the
course and as such I don't know if it follow actual Scheme-- specs.

You can declare variables like so

    (val v 3)    //This will bound v to 3

You can declare functions like so

    (def (plusOne x) (+ x 1))

This is syntactic sugar for

    (def plusOne (lambda (x) (+ x 1)))

The interpreter supports reccursive definitions so you can write

    (def (factorial x) (if (= x 1)
      1
      (* x (factorial (- x 1)))))

The course asked us to add 'switch-like' constructs with the keyword `case`

    (case 3
      (1 1)
      (2 1)
      (3 2)
      (else 0)
    )// returns 2


Disclaimer
----------
Most of the code in Lisp.scala isn't my doing (some is). Credits goes to whoever
made the Lisp.scala class in our assignment
