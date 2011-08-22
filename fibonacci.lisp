;;;; Copyright (c) 2011, Christopher Mark Gore,
;;;; All rights reserved.
;;;;
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.


(load "utilities")
(use-package :utilities)


(defun recursive-fibonacci (n)

  "This is a super-simple recursive definition of the Fibonacci sequence.  This
  function definition is basically an exact translation of the standard form of
  the definition of the Fibonacci sequence itself.  This approach becomes quite
  slow for me around Fib(50) on an Intel Core 2 Duo."

  (assert (nonnegative-integer? n))
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 1)
        (t (+ (recursive-fibonacci (- n 1))
              (recursive-fibonacci (- n 2))))))


;; This is a simple invariant specifying hard-coded values for F(0) ... F(20).
(loop for n from 0 to 20
      and fn in '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584
                  4181 6765)
      do (assert (= fn (recursive-fibonacci n))))


(let ((memory (loop for n from 0 to 10 collect (recursive-fibonacci n))))
  (defun memoized-recursive-fibonacci (n)

    "This is a simple improvement over the recursive-fibonacci function in that
    it memoizes the numbers in the sequence.  This should turn the time
    complexity from exponential to nearly linear.  I can quickly calculate to
    well past Fib(50,000) with this function on an Intel Core 2 Duo."

    (assert (nonnegative-integer? n))
    (while (< (1- (length memory)) n)
           (nconc memory
                  (list (+ (nth (- (length memory) 1) memory)
                           (nth (- (length memory) 2) memory)))))
    (nth n memory)))


;; This is a simple invariant for Fib(0) ... Fib(20).
(loop for n from 0 to 20
      do (assert (= (recursive-fibonacci n)
                    (memoized-recursive-fibonacci n))))


(defun fibonacci (n)

  "This function points to the current best general implementation of a
  generator for the Fibonacci sequence."

  (memoized-recursive-fibonacci n))
