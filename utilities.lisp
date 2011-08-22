;;;; Copyright (c) 2005 -- 2011, Christopher Mark Gore,
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

(unless (find-package 'utilities)
  (defpackage :utilities
    (:nicknames :util)
    (:use :common-lisp #+cmu :extensions #+sbcl :sb-ext)
    (:export
      :? :[?] :aand :a?and :ablock :acond :aif :a?if :aif-otherwise-nil
      :alambda :arefable? :array-raster-line :array-values :awhen :a?when
      :awhile :a?while :best :bit? :character-range :character-ranges :compose
      :conjoin :curry :decaying-probability? :deletef :disjoin :distance :divf
      :do-until :do-while :duplicate :empty-sequence? :escape-tildes :for
      :forever :fractional-part :fractional-value :function-alias
      :function-aliases :integer-range :it :list-to-vector :maximum :maximum?
      :minimum :minimum?  :multf :multicond :nconcf :next-point :nonnegative?
      :nonnegative-float :nonnegative-integer :nonnegative-integer? :norm
      :nshuffle :nthable?  :nth-from-end :operator-to-function :opf
      :otherwise-nil :positive-float :positive-integer :positive-integer?
      :prepackage :probability :probability? :product :random-array
      :random-element :random-in-range :random-in-ranges :randomize-array
      :random-range :raster-line :rcompose :rcurry :read-lines :replace-char
      :sequence? :set-equal :set-nthcdr :shuffle :similar-points?
      :simple-vector-to-list :slice :snap-index :sort-on :sort-order :split
      :strcat :stringify :string-join :strmult :sum :swap :swap-unless
      :swap-when :the-last :time-multiseries :time-multiseries? :time-series?
      :tms-dimensions :tmsref :tms-values :toggle :unsigned-integer :until
      :vector-to-list :while :worst)))
(in-package :utilities)

(defun function-alias (function &rest aliases)
  "This produces one or more aliases (alternate names) for a function.
For example, you might do something like:
> (function-alias 'that-guy-doesnt-know-when-to-stop-typing 'shorter)"
  (loop for alias in aliases
        do (setf (fdefinition alias) (fdefinition function))))

(function-alias 'function-alias 'function-aliases) ; This line seemed fitting.

(defun fractional-value (number)
  "This is the fractional value formula most familiar to most mathematicians.
Note that the result of this is always positive, forming a sawtooth."
  (assert (numberp number))
  (- number (floor number)))

(defun fractional-part (number)
  "This is the fractional part formula most familiar to computer scientists.
It possesses the useful feature that frac(x)+int(x)=x, but may be negative."
  (assert (numberp number))
  (if (minusp number)
    (- number (floor number) 1)
    (- number (floor number))))

(defun nth-from-end (n list)
  "This macro is similar to NTH, but counting from the back."
  (assert (integerp n))
  (assert (<= 0 n))
  (assert (listp list))
  (maplist #'(lambda (a b)
               (when (null (rest b))
                 (return-from nth-from-end (first a))))
           list (nthcdr n list)))

(let ((0-to-10 '(0 1 2 3 4 5 6 7 8 9 10)))
  (assert (equal (nth-from-end 0 0-to-10)
                 10))
  (assert (equal (nth-from-end 3 0-to-10)
                 7))
  (assert (equal (nth-from-end 10 0-to-10)
                 0))
  (assert (equal (nth-from-end 11 0-to-10)
                 nil)))

(defmacro swap (x y)
  "A simple SWAP macro.  The values of the first form and the second form are
swapped with each other."
  `(psetf ,x ,y
          ,y ,x))

(let ((x 15)
      (y 37))
  (swap x y)
  (assert (= y 15))
  (assert (= x 37)))

(defmacro swap-when (predicate x y)
  "This macro calls SWAP only when the predicate evaluates to true."
  `(when (funcall ,predicate ,x ,y)
     (swap ,x ,y)))

(defmacro swap-unless (predicate x y)
  "This macro calls SWAP unless the predicate evaluates to true."
  `(unless (funcall ,predicate ,x ,y)
     (swap ,x ,y)))

(defgeneric ? (x))

(defmethod ? (x)
  "This turns a generalized truth value (NIL, anything else) into a traditional
Lisp-style simplistic truth value (NIL, T)."
  (if x t nil))

(defun toggle (x)
  (if x nil t))

(defun [?] (x)
  "This is Knuth's truth function.  It converts it's input to 1 for true and 0
for false based upon its truth value.  In other words, NIL -> 0 and everything
else -> 1."
  (if x 1 0))

(defun bit? (b)
  (typep b 'bit))

(defun nonnegative? (x)
  (not (minusp x)))

(deftype positive-integer ()
  '(integer (0) *))

(defun positive-integer? (positive-integer)
  (typep positive-integer 'positive-integer))

(deftype nonnegative-integer ()
  '(integer 0 *))

(defun nonnegative-integer? (nonnegative-integer)
  (typep nonnegative-integer 'nonnegative-integer))

(deftype positive-float ()
  '(float (0.0) *))

(deftype nonnegative-float ()
  '(float 0.0 *))

(deftype probability ()
  '(or (float 0.0 1.0)
       (integer 0 1)
       bit))

(defun probability? (probability)
  "This is a simple probabilistic testing function."
  (assert (typep probability 'probability))
  (<= (random 1.0) probability))

(defmacro decaying-probability? (probability &optional (factor 1/2))
  `(if (probability? ,probability)
     (progn (multf ,probability ,factor)
            t)
     nil))

(defmacro while (conditional &rest body)
  "A WHILE macro, operating in a matter similar to the while loop in C."
  `(do ()
     ((not ,conditional))
     ,@body))

(defmacro until (conditional &rest body)
  "An UNTIL loop construct."
  `(while (not ,conditional)
     ,@body))

(defmacro do-while (conditional &rest body)
  "The DO-WHILE macro operates like a do {BODY} while (CONDITIONAL) in the C
  programming language."
  `(progn ,@body
          (while ,conditional
                 ,@body)))

(defmacro do-until (conditional &rest body)
  "A DO-UNTIL loop construct; it operates like do {BODY} while (! CONDITIONAL)
construct in the C programming language."
  `(do-while (not ,conditional)
     ,@body))

(defmacro for (initial conditional step-action &rest body)
  "A FOR macro, much like the ``for'' in the C programming language.
A simple example:
  (for ((i 0))
       (< i 10)
       (incf i)
    (format t \"~%~A\" i))
prints the numbers from 0 through 9, each on their own lines.
Generally this should not be used, but instead the native looping methods."
  `(let ,initial
     (while ,conditional
       (prog1 (progn ,@body) ,step-action))))

(defmacro a?if (anaphor conditional t-action &optional nil-action)
  "This is an anaphoric IF that allows for specification of the anaphor."
  `(let ((,anaphor ,conditional))
     (if ,anaphor ,t-action ,nil-action)))

(defmacro aif (conditional t-action &optional nil-action)
  "This is anaphoric IF, from Paul Graham's ``On Lisp'' page 190."
  `(let ((it ,conditional))
     (if it ,t-action ,nil-action)))

(defmacro a?when (anaphor test-form &body body)
  "This is an anaphoric WHEN that allows for the specification of the anaphor."
  `(a?if ,anaphor ,test-form (progn ,@body)))

(defmacro awhen (test-form &body body)
  "This is anaphoric WHEN, from Paul Graham's ``On Lisp'' page 191."
  `(aif ,test-form (progn ,@body)))

(defmacro a?while (anaphor expression &body body)
  "This is an anaphoric WHILE that allows for the specification of the anaphor."
  `(do ((,anaphor ,expression ,expression))
       ((not ,anaphor))
     ,@body))

(defmacro awhile (expression &body body)
  "This is anaphoric WHILE, from Paul Graham's ``On Lisp'' page 191."
  `(do ((it ,expression ,expression))
       ((not it))
     ,@body))

(defmacro a?and (anaphor &rest arguments)
  "This is an anaphoric AND that allows for the specification of the anaphor."
  (cond ((null arguments) t)
        ((null (rest arguments)) (first arguments))
        (t `(a?if ,anaphor ,(first arguments)
                  (a?and ,anaphor ,@(rest arguments))))))

(defmacro aand (&rest arguments)
  "This is anaphoric AND, from Paul Graham's ``On Lisp'' page 191."
  (cond ((null arguments) t)
        ((null (rest arguments)) (first arguments))
        (t `(aif ,(first arguments)
                  (aand ,@(rest arguments))))))

(defmacro acond (&rest clauses)
  "This is anaphoric COND, from Paul Graham's ``On Lisp'' page 191."
  (if (null clauses)
    nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
            (if ,sym
              (let ((it ,sym)) ,@(cdr cl1))
              (acond ,@(cdr clauses)))))))

(defmacro alambda (parms &body body)
  "This is anaphoric LAMBDA, from Paul Graham's ``On Lisp'' page 193."
  `(labels ((self ,parms ,@body))
           #'self))

(defmacro ablock (tag &rest args)
  "This is anaphoric COND, from Paul Graham's ``On Lisp'' page 193."
  `(block ,tag
          ,(funcall (alambda (args)
                             (case (length args)
                               (0 nil)
                               (1 (car args))
                               (t `(let ((it ,(car args)))
                                        ,(self (cdr args))))))
                    args)))

(defmacro set-nthcdr (n list new-value)
  `(progn (assert (nonnegative-integer? ,n))
          (if (zerop ,n)
            (setf ,list ,new-value)
            (setf (cdr (nthcdr (1- ,n) ,list)) ,new-value))))

#+cmu (defsetf nthcdr set-nthcdr)
#+sbcl (sb-ext:without-package-locks (defsetf nthcdr set-nthcdr))
#+clisp (ext:without-package-lock () (defsetf nthcdr set-nthcdr))

(defmacro opf (operator variable &rest arguments)
  "OPF is a generic operate-and-store macro, along the lines of INCF and DECF,
but allowing for any operation.  For example:
  (opf #'+ foo 42)
does something like
  (incf foo 42)
but you could also do
  (opf #'+ foo 1 2 3 4 5)
with it doing the obvious thing, whereas you cannot do
  (incf foo 1 2 3 4 5)
in any Common Lisp I have used."
  `(setf ,variable
         (funcall ,operator ,variable ,@arguments)))

#|
(ext:without-package-locks
  (defmacro incf (variable &rest addends)
    `(if (null ,addends)
       (opf #'+ ,variable 1)
       (opf #'+ ,variable ,@addends))))

(ext:without-package-locks
  (defmacro decf (variable &rest subtrahends)
    `(if (null ,addends)
       (opf #'- ,variable 1)
       (opf #'- ,variable ,@subtrahends))))
|#

(defmacro multf (variable &rest multiplicands)
  `(opf #'* ,variable ,@multiplicands))

(defmacro divf (variable &rest divisors)
  `(opf #'/ ,variable ,@divisors))

(defun sequence? (sequence)
  (typep sequence 'sequence))

(defun empty-sequence? (sequence)
  (and (sequence? sequence)
       (or (null sequence)
           (and (arrayp sequence)
                (some #'zerop (array-dimensions sequence))))))

(defun the-last (list)
  (assert (listp list))
  (car (last list)))

(defun sum (sequence &key (key 'identity) (start 0) (end nil))
  (assert (sequence? sequence))
  (reduce #'+ sequence :key key :start start :end end :initial-value 0))

(defun product (sequence &key (key 'identity) (start 0) (end nil))
  (assert (sequence? sequence))
  (reduce #'* sequence :key key :start start :end end :initial-value 1))

(defgeneric duplicate (item))

(defmethod duplicate ((list list))
  "This returns a deeply new duplicate of the list."
  (mapcar 'duplicate list))

(defmethod duplicate ((array array))
  "This returns a deeply new duplicate of the array."
  (let ((result (make-array (array-dimensions array)
                            :element-type (array-element-type array)
                            :adjustable (adjustable-array-p array))))
    (when (array-dimensions array)
      (dotimes (index (array-total-size array))
        (setf (row-major-aref result index)
              (duplicate (row-major-aref array index)))))
    result))

(defmethod duplicate ((number number))
  number)

(defmethod duplicate ((symbol symbol))
  symbol)

(defmethod duplicate ((function function))
  ;; XXX: I believe this is correct, but I am not really sure.
  function)

(defmacro multicond (&rest clauses)
  "A macro much like COND, but where multiple clauses may be evaluated."
  `(mapcar #'(lambda (clause)
               (when (first clause)
                 (mapcar #'eval (rest clause))))
           ',clauses))


(defun list-to-vector (list)
  "This takes in a list and returns an equivalent vector."
  (assert (listp list))
  (coerce list 'vector))

(defun vector-to-list (vector)
  "This takes in a vector and returns an equivalent list."
  (assert (vectorp vector))
  (coerce vector 'list))

(defun simple-vector-to-list (vector)
  "This takes in a vector and returns an equivalent list."
  (assert (vectorp vector))
  (loop for index from 0 to (1- (length vector))
        collect (svref vector index)))

(defgeneric random-element (sequence))

(defmethod random-element ((list list))
  "This method returns a random element from a list."
  (when list
    (nth (random (length list)) list)))

(defmethod random-element ((array array))
  "This method returns a random element from an array."
  (when (plusp (array-total-size array))
    (row-major-aref array (random (array-total-size array)))))

(defgeneric minimum (sequence &key key start end))

(defmethod minimum ((sequence sequence)
                    &key (key #'identity) (start 0) (end nil))
  "This reduces MIN onto the sequence provided."
  (reduce #'min sequence :key key :start start :end end))

(defgeneric maximum (sequence &key key start end))

(defmethod maximum ((sequence sequence)
                    &key (key #'identity) (start 0) (end nil))
  "This reduces MAX onto the sequence provided."
  (reduce #'max sequence :key key :start start :end end))

(defgeneric minimum? (sequence &key position key start end))

(defmethod minimum? ((sequence sequence)
                      &key (position nil) (key #'identity) (start 0) (end nil))
  (when (null position)
    (setf position (1- (length sequence))))
  (<= (funcall key (elt sequence position))
      (minimum sequence :key key :start start :end end)))

(defgeneric maximum? (sequence &key position key start end))

(defmethod maximum? ((sequence sequence)
                     &key (position nil) (key #'identity) (start 0) (end nil))
  (when (null position)
    (setf position (1- (length sequence))))
  (>= (funcall key (elt sequence position))
      (maximum sequence :key key :start start :end end)))

(defun random-in-range (lower upper)
  "This function returns a random number in the range [lower, upper).  Lower
and upper may both be sequences, in which case their most extreme members."
  (when (sequence? lower)
    (setf lower (maximum lower)))
  (when (sequence? upper)
    (setf upper (minimum upper)))
  (assert (numberp lower))
  (assert (numberp upper))
  (cond ((< lower upper)
         (+ lower (random (- upper lower))))
        ((= lower upper)
         lower)
        ((> lower upper)
         (random-in-range upper lower))))
   
(defun random-in-ranges (&rest ranges)
  "This function, given many restricting ranges all as two-element lists, will
return a random number in the range that is a common subset to all of them."
  (let ((lower (minimum ranges :key #'minimum))
        (upper (maximum ranges :key #'maximum)))
    (random-in-range lower upper)))

(defun random-range (lower upper &key (containing nil))
  (when (null containing)
    (let ((a (random-in-range lower upper))
          (b (random-in-range lower upper)))
      (return-from random-range
                   (list (min a b)
                         (max a b)))))
  (let ((low-max (if (sequence? containing)
                   (minimum containing)
                   containing))
        (high-min (if (sequence? containing)
                    (maximum containing)
                    containing)))
    (list (random-in-range lower low-max)
          (random-in-range high-min upper))))

(defgeneric best (sequence predicate &key key))

(defmethod best ((list list) predicate &key (key #'identity))
  "This returns the ``best'' element in a list.  This is equivalent to, but
faster than (O(n) vs. O(n*lg(n))), taking the first element after sorting the
sequence with the same predicate and key."
  (when (null list)
    (return-from best nil))
  (let ((best (first list)))
    (dolist (i list best)
      (when (funcall predicate
                     (funcall key i)
                     (funcall key best))
        (setf best i)))
    best))

(defmethod best ((vector vector) predicate &key (key #'identity))
  "This returns the ``best'' element in a vector.  This is equivalent to, but
faster than (O(n) vs. O(n*lg(n))), taking the first element after sorting the
sequence with the same predicate and key."
  (when (zerop (length vector))
    (return-from best nil))
  (let ((best (aref vector 0)))
    (dotimes (i (length vector) best)
      (when (funcall predicate
                     (funcall key (aref vector i))
                     (funcall key best))
        (setf best (aref vector i))))
    best))

(defgeneric worst (sequence predicate &key key))

(defmethod worst ((list list) predicate &key (key #'identity))
  "This returns the ``worst'' element in a list.  This is equivalent to, but
faster than (O(n) vs. O(n*lg(n))), taking the last element after sorting the
sequence with the same predicate and key."
  (when (null list)
    (return-from worst nil))
  (let ((worst (first list)))
    (dolist (i list worst)
      (when (funcall predicate
                     (funcall key worst)
                     (funcall key i))
        (setf worst i)))
    worst))

(defmethod worst ((vector vector) predicate &key (key #'identity))
  "This returns the ``worst'' element in a vector.  This is equivalent to, but
faster than (O(n) vs. O(n*lg(n))), taking the last element after sorting the
sequence with the same predicate and key."
  (when (zerop (length vector))
    (return-from worst nil))
  (let ((worst (aref vector 0)))
    (dotimes (i (length vector) worst)
      (when (funcall predicate
                     (funcall key worst)
                     (funcall key (aref vector i)))
        (setf worst (aref vector i))))
    worst))

(defun integer-range (x &optional y z)
  "This function generates lists of integer ranges of the form [start, stop].
It has three forms:

First form: (integer-range stop)
> (integer-range 10)
=> '(0 1 2 3 4 5 6 7 8 9 10)

Second form: (integer-range start stop)
> (integer-range 5 10)
=> '(5 6 7 8 9 10)

Third form: (integer-range start stop step)
> (integer-range 5 10 2)
=> '(5 7 9)

Negative numbers are allowed, and operate in a logical manner.
> (integer-range -5 0)
=> '(-5 -4 -3 -2 -1 0)
> (integer-range 10 5 -1)
=> '(10 9 8 7 6 5) "
  (let (start stop step)
    (flet ((step-function ()
             (if (< start stop) 1 -1)))
      (cond ((and y z)             (setf start x
                                         stop  y
                                         step  z))
            ((and y (not z))       (setf start x
                                         stop  y
                                         step  (step-function)))
            ((and (not y) (not z)) (setf start 0
                                         stop  x
                                         step  (step-function))))
      (do ((i     start (+ i step))
           (range nil   (cons i range))) 
        ((or (and (plusp step)
                  (> i stop))
             (and (minusp step)
                  (< i stop)))
         (reverse range))))))

(assert (equal (integer-range 5)
               '(0 1 2 3 4 5)))
(assert (equal (integer-range 5 10)
               '(5 6 7 8 9 10)))
(assert (equal (integer-range 5 10 2)
               '(5 7 9)))
(assert (equal (integer-range -5)
               '(0 -1 -2 -3 -4 -5)))
(assert (equal (integer-range -5 0)
               '(-5 -4 -3 -2 -1 0)))
(assert (equal (integer-range 10 5)
               '(10 9 8 7 6 5)))
(assert (equal (integer-range 10 5 1)
               nil))
(assert (equal (integer-range 5 10 -1)
               nil))
(assert (equal (integer-range -5 5)
               '(-5 -4 -3 -2 -1 0 1 2 3 4 5)))
(assert (equal (integer-range -5 5 2)
               '(-5 -3 -1 1 3 5)))

(defmacro forever (&rest body)
  `(while t ,@body))

(defmacro nconcf (list-1 list-2)
  `(setf ,list-1 (nconc ,list-1 ,list-2)))

(defun set-equal (list-1 list-2 &key (key #'identity) test test-not)
  (assert (listp list-1))
  (assert (listp list-2))
  (assert (not (and test test-not)))
  (cond (test (and (not (set-difference list-1 list-2 :key key :test test))
                   (not (set-difference list-2 list-1 :key key :test test))))
        (test-not  (and (not (set-difference list-1 list-2
                                             :key key :test-not test-not))
                        (not (set-difference list-2 list-1
                                             :key key :test-not test-not))))
        (t (and (not (set-difference list-1 list-2 :key key))
                (not (set-difference list-2 list-1 :key key))))))

(defun rcompose (&rest functions)
  "A version of COMPOSE in reverse order."
  (dolist (function functions)
    (assert (or (functionp function)
                (symbolp function))))
  (destructuring-bind (function-1 . rest)
    functions
    #'(lambda (&rest arguments)
        (reduce #'(lambda (v f)
                    (funcall f v))
                rest
                :initial-value (apply function-1 arguments)))))

(defun compose (&rest functions)
  "This function composes a single function from a list of several functions
such that the new function is equivalent to calling the functions in
succession.  This is based upon a COMPOSE function in Paul Graham's ``ANSI
Common Lisp'' which is  based upon the compose function from Dylan, a
programming language which he describes as a ``cross between Scheme and Common
Lisp, with a syntax like Pascal.''"
  (apply #'rcompose (reverse functions)))

(defun disjoin (predicate &rest predicates)
  "This function takes in one or more predicates, and returns a predicate that
returns true whenever any of the predicates return true.  This is from Paul
Graham's ``ANSI Common Lisp'' and is based upon the disjoin function from
Dylan, a programming language which he describes as a ``cross between Scheme
and Common Lisp, with a syntax like Pascal.''"
  (assert (or (functionp predicate)
              (symbolp predicate)))
  (dolist (predicate predicates)
    (assert (or (functionp predicate)
                (symbolp predicate))))
  (if (null predicates)
    predicate
    (let ((disjoinment (apply #'disjoin predicates)))
      #'(lambda (&rest arguments)
          (or (apply predicate arguments)
              (apply disjoinment arguments))))))

(defun conjoin (predicate &rest predicates)
  "This function takes in one or more predicates, and returns a predicate that
returns true whenever all of the predicates return true.  This is from Paul
Graham's ``ANSI Common Lisp'' and is based upon the conjoin function from
Dylan, a programming language which he describes as a ``cross between Scheme
and Common Lisp, with a syntax like Pascal.''"
  (assert (or (functionp predicate)
              (symbolp predicate)))
  (dolist (predicate predicates)
    (assert (or (functionp predicate)
                (symbolp predicate))))
  (if (null predicates)
    predicate
    (let ((conjoinment (apply #'conjoin predicates)))
      #'(lambda (&rest arguments)
          (and (apply predicate arguments)
               (apply conjoinment arguments))))))

(defun curry (function &rest arguments)
  "This function takes in a function and some of its arguments, and returns a
function that expects the rest of the required arguments.  This is from Paul
Graham's ``ANSI Common Lisp'' and is based upon the curry function from
Dylan, a programming language which he describes as a ``cross between Scheme
and Common Lisp, with a syntax like Pascal.''"
  (assert (or (functionp function)
              (symbolp function)))
  #'(lambda (&rest more-arguments)
      (apply function (append arguments more-arguments))))

(defun rcurry (function &rest arguments)
  "This function takes in a function and some of its ending arguments, and
returns a function that expects the rest of the required arguments.  This is
from Paul Graham's ``ANSI Common Lisp'' and is based upon the rcurry function
from Dylan, a programming language which he describes as a ``cross between
Scheme and Common Lisp, with a syntax like Pascal.''"
  (assert (or (functionp function)
              (symbolp function)))
  #'(lambda (&rest more-arguments)
      (apply function (append more-arguments arguments))))

(defgeneric split (sequence separators &key key test remove-separators?))

(defmethod split ((list list) 
                   separators
                   &key
                   (key #'identity)
                   (test #'eql)
                   (remove-separators? t))
  "This splits LIST on the SEPERATORS, returning a list of all the fields.
The optional KEY and TEST arguments are for the comparison of items in the
SEQUENCE for membership in the SEPERATORS."
  (assert (not (null list)))
  (assert (not (null separators)))
  (unless (listp separators)
    (setf separators (list separators)))
  (let ((result nil)
        (current-list nil))
    (mapc #'(lambda (item)
              (if (member item separators :key key :test test)
                (progn (unless remove-separators?
                         (push item current-list))
                       (push (reverse current-list) result)
                       (setf current-list nil))
                (push item current-list)))
          list)
    (push (reverse current-list) result)
    (reverse result)))

(defmethod split ((string string) 
                   separators
                   &key
                   (key #'identity)
                   (test #'string=)
                   (remove-separators? t))
  (mapcar (rcurry #'coerce 'string)
          (split (coerce string 'list) separators
                 :key key :test test :remove-separators? remove-separators?)))

(defun unsigned-integer? (x)
  (and (integerp x)
       (not (minusp x))))

(defmacro snap-index (index bound)
  "This wraps the value of index between 0 and bound."
  `(progn
     (when (< ,index 0)
       (setf ,index (+ ,index ,bound)))
     (when (>= ,index ,bound)
       (setf ,index (- ,index ,bound)))))

(defun randomize-array (array argument-for-random)
  "This function randomizes the contents of the array."
  (assert (arrayp array))
  (dotimes (index (array-total-size array) array)
    (setf (row-major-aref array index)
          (random argument-for-random))))

(defun nthable? (n list)
  (and (listp list)
       (typep n `(integer 0 ,(1- (length list))))))

(defun arefable? (array position)
  (and (arrayp array)
       (listp position)
       (= (length (array-dimensions array))
          (length position))
       (every #'(lambda (position dimension)
                  (typep position `(integer 0 ,(1- dimension))))
              position
              (array-dimensions array))))

(defun random-array (dimensions argument-for-random)
  "This function returns a new array with randomized contents."
  (randomize-array (make-array dimensions) argument-for-random))

(defgeneric sort-on (sequence-to-sort ordering-sequence predicate &key key))

(defmethod sort-on ((sequence-to-sort list)
                    (ordering-sequence list)
                    predicate
                    &key
                    (key #'identity))
  "This function sorts the sequence-to-sort based upon the ordering-sequence."
  (assert (listp sequence-to-sort))
  (assert (listp ordering-sequence))
  (assert (<= (length sequence-to-sort)
              (length ordering-sequence)))
  (mapcar #'cdr (sort (mapcar #'cons ordering-sequence sequence-to-sort)
                      predicate
                      :key (compose key #'car))))

(defmethod sort-on ((sequence-to-sort vector)
                    (ordering-sequence list)
                    predicate
                    &key (key #'identity))
  (list-to-vector (sort-on (vector-to-list sequence-to-sort)
                           ordering-sequence
                           predicate
                           :key key)))

(defmethod sort-on (sequence-to-sort
                    (ordering-sequence vector)
                    predicate
                    &key (key #'identity))
  (sort-on sequence-to-sort
           (vector-to-list ordering-sequence)
           predicate
           :key key))

(defgeneric shuffle (container))

(defmethod shuffle ((array array))
  "This randomly shuffles the contents of an array."
  (let ((result (duplicate array)))
    (do* ((j (1- (array-total-size result)) (1- j))
          (k (random-in-range 1 (1+ j)) (random-in-range 1 (1+ j))))
      ((minusp j) result)
      (swap (row-major-aref result j)
            (row-major-aref result k)))))

(defmethod shuffle ((list list))
  "This randomly shuffles a list."
  (vector-to-list (shuffle (list-to-vector list))))

(defmacro nshuffle (argument)
  "This randomly shuffles the argument in place."
  `(setf ,argument (shuffle ,argument)))

(defun sort-order (sequence predicate &key (key #'identity))
  "This function returns the indices in the order for the sorted sequence."
  (sort-on (integer-range (1- (length sequence)))
           sequence
           predicate
           :key key))

(defun similar-points? (p q &optional (coordinate-assertion #'numberp))
  "This predicate determines if the points P and Q are similar."
  (and (listp p)
       (listp q)
       (= (length p) (length q))
       (every coordinate-assertion p)
       (every coordinate-assertion q)))

(defun raster-line (start-point
                     end-point
                     &key
                     (coordinate-assertion #'integerp)
                     (from-start 0)
                     (from-end 0))
  "This is derived from the algorithm for raster conversion of a 3D line as
found in ``3D Scan-Conversion Algorithms for Voxel-Based Graphics'' by
Arie Kaufman and Eyal Shimony, 1986 Workshop on Interactive 3D Graphics.
Here it should work for any any n-dimensional space where n is non-negative."
  (assert (similar-points? start-point end-point coordinate-assertion))
  (assert (integerp from-end))
  (when (equalp start-point end-point)
    (return-from raster-line start-point))
  ;;; We perform the only logical thing in the case of the two points being
  ;;; equal, that is to view it as a degenerate condition and return a line
  ;;; consisting of only that point.
  (let* ((dimensions (length start-point))
         (deltas (mapcar (compose #'abs #'-) start-point end-point))
         ;; We need to work on the coordinates such that
         ;; delta x >= delta y >= delta z >= ... >= 0.
         ;; NB: this mapping is reflexive.
         (coordinate-order (sort-order deltas #'>))
         ;; This is the point we start at, ordered.
         (from-point (mapcar (rcurry #'nth start-point) coordinate-order))
         ;; This is the point we end at, ordered.
         (to-point (mapcar (rcurry #'nth end-point) coordinate-order))
         ;; We want x2 > x1, and swap the points if necessary.
         (swap-points? (let ((swap-points? (< (first to-point)
                                              (first from-point))))
                         (when swap-points?
                           (swap to-point from-point))
                         swap-points?))
         ;; This is the point we are currently at in the loop.
         (current-point from-point)
         ;; We recalculate the deltas because that is O(n), but the lookup as
         ;; we used for from-point and to-point is O(n^2).
         (deltas (mapcar (compose #'abs #'-) from-point to-point))
         ;; The basic length of the raster line is the number of points that
         ;; would exist in the raster line without any FROM-START or FROM-END
         ;; arguments used by the algorithm.
         (basic-length (apply #'max deltas))
         ;; These are all in '(+1 0 -1), and they are the direction that the
         ;; line is travelling in for that particular dimension.
         (signums (mapcar (compose #'signum #'-) to-point from-point))
         ;; We don't use the decision variable for the first coordinate, but we
         ;; calculate it anyway to ease variable addressing later on.
         (deciders (mapcar #'(lambda (delta)
                               (- (* 2 delta)
                                  (first deltas)))
                           deltas))
         ;;; These two are the two increments for the deciders.
         (if-increments (mapcar (curry #'* 2) deltas))
         (else-increments (mapcar #'(lambda (delta)
                                      (* 2 (- delta (first deltas))))
                                  deltas))
         ;; We now update CURRENT-POINT so that it accurately reflects any
         ;; offset from the FROM-START argument.
         (current-point (mapcar #'(lambda (x delta signum)
                                    (round (+ x (* signum
                                                   (/ delta basic-length)
                                                   from-start))))
                                current-point deltas signums))
         (result (list (duplicate current-point))))
    (when (equal start-point end-point)
      (return-from raster-line (list start-point)))
    (when (not (plusp (+ basic-length (- from-start) from-end)))
      (return-from raster-line nil))
    (while (< (first current-point)
              (+ from-end (first to-point)))
      (incf (first current-point))
      (loop for i from 1 to (1- dimensions) do
            (if (minusp (nth i deciders))
              ;; If no change in the current coordinate.
              (incf (nth i deciders)
                    (nth i if-increments))
              ;; If a change in the current coordinate.
              (progn (incf (nth i deciders)
                           (nth i else-increments))
                     (incf (nth i current-point)
                           (nth i signums)))))
      (push (duplicate current-point) result))
    (map-into result 
              #'(lambda (point)
                  (mapcar (rcurry #'nth point)
                          (sort-order coordinate-order #'<)))
              result)
    ;;; We need to reverse the list of points before returning from this
    ;;; function unless we reversed the end points originally.
    (unless swap-points?
      (opf #'reverse result))
  result))

(defun norm (sequence &optional (power 2))
  "This function returns the mathematical vector norm of a sequence.  For the
infinity norm, use :INFINITY for the power."
  (cond ((equalp power :infinity)
         (apply #'max sequence))
        ((numberp power)
         (expt (sum sequence :key (rcurry #'expt power))
               (/ power)))
        ;; We don't currently understand any other sort of norm.
        (t nil)))

(defun distance (initial-point final-point &optional (power 2))
  "This calculates the distance between two points."
  (norm (mapcar #'- initial-point final-point) power))

(defun array-values (array positions)
  "This function returns a list of the values in array found at the specified
positions."
  (assert (arrayp array))
  (assert (listp positions))
  (mapcar #'(lambda (position)
              (assert (and (listp position)
                           (= (length position)
                              (length (array-dimensions array)))))
              (apply #'aref array position))
          positions))

(defun time-series? (time-series &optional (element-type t))
  "The TIME-SERIES? predicate returns true if the argument could be a time
series."
  (and (listp time-series)
       (not (null time-series))
       (every (rcurry #'typep element-type) time-series)))

(defun time-multiseries? (time-multiseries)
  "This predicate returns true if the argument is a time multiseries
(multivariate time series), which we represent as a list of arrays of equal
dimensions, where each array represents data from a single time step."
  (and (listp time-multiseries)
       (not (null time-multiseries))
       (every #'arrayp time-multiseries)
       (let ((dimensions (array-dimensions (first time-multiseries))))
         (every #'(lambda (array)
                    (equalp dimensions (array-dimensions array)))
                time-multiseries))))

(deftype time-multiseries ()
  '(satisfies time-multiseries?))

(defun tmsref (time-multiseries time &rest position)
  "This function works like AREF, but for a time series or multiseries.  The
time multiseries is represented as a list of arrays, where there is an array
for each time step representing all of the data for that step in time.
A time series is represented as a list."
  (assert (or (time-multiseries? time-multiseries)
              (and (listp time-multiseries)
                   (null position))))
  (if (null position)
    ;; A (one-dimensional) time series.
    (nth time time-multiseries)
    ;; A (multi-dimensional) time multiseries.
    (apply #'aref (nth time time-multiseries) position)))

(defun tms-values (time-multiseries positions)
  "This function returns a list of the values in a time series or multiseries
at the specified positions.  A time multiseries is represented as a list of
arrays with identical dimensions, where each array represents a single time
step's entire data.  A time series is represented as a list.  The first value
in each position is the time position."
  (assert (listp positions))
  (setf positions (mapcar #'(lambda (position)
                              (if (listp position)
                                position
                                (list position)))
                          positions))
  (assert (or (and (time-multiseries? time-multiseries)
                   (every #'listp positions))
              (and (listp time-multiseries)
                   (every #'(lambda (position)
                              (or (nonnegative-integer? position)
                                  (and (listp position)
                                       (= 1 (length position))
                                       (nonnegative-integer?
                                         (first position)))))
                          positions))))
  (mapcar #'(lambda (position)
              (apply #'tmsref time-multiseries position))
          positions))

(defun tms-dimensions (time-multiseries)
  "This works like the ARRAY-DIMENSIONS function, but for a time multiseries.
  The first dimension listed is the time dimension."
  (assert (or (time-multiseries? time-multiseries)
              (listp time-multiseries)))
  (if (time-multiseries? time-multiseries)
    ;; A (multi-dimensional) time multi-series.
    (cons (length time-multiseries)
          (array-dimensions (first time-multiseries)))
    ;; A (one-dimensional) time series.
    (list (length time-multiseries))))

(defun array-raster-line (array
                           start-point
                           end-point
                           &key
                           (coordinate-assertion #'positive-integer?)
                           (from-start 0)
                           (from-end 0))
  "This function returns a one-dimensional list of values from the array which
  starts at the start-point and ends at the end-point, as taken from the
  straight-line path between the two points.  The array may be of any rank."
  (array-values array
                (raster-line start-point end-point
                             :coordinate-assertion coordinate-assertion
                             :from-start from-start
                             :from-end from-end)))

(defun tms-raster-line (time-multiseries
                         start-point
                         end-point
                         &key
                         (coordinate-assertion #'positive-integer?)
                         (from-start 0)
                         (from-end 0))
  "This function returns a one-dimensional list of values from a time
  multiseries which starts at the start-point and ends at the end-point, as
  taken from the straight-line path between the two points.  The time
  multiseries may be of any rank."
  (tms-values time-multiseries
              (raster-line start-point
                           end-point
                           :coordinate-assertion coordinate-assertion
                           :from-start from-start
                           :from-end from-end)))

(defgeneric slice (sequence &optional slice))

(defmethod slice ((vector vector) &optional (slice 1))
  "This method returns a slice from a one-dimensional vector; that is, a modular
subset of the vector.  For example,
> (slice #(1 2 3 4 5 6 7 8 9) 2)
=> #(1 3 5 7 9)
The slice argument may be any positive rational number."
  (assert (and (rationalp slice)
               (plusp slice)))
  (let ((index 0)
        (result nil))
    (while (< index (length vector))
      (when (integerp index)
        (push (svref vector index) result))
      (incf index slice))
    (make-array (list (length result))
                :initial-contents (reverse result))))

(defmethod slice ((list list) &optional (slice 1))
  "This method returns a slice from a one-dimensional list; that is, a modular
subset of the list.  For example,
> (slice '(1 2 3 4 5 6 7 8 9) 2)
=> '(1 3 5 7 9)
The slice argument may be any positive rational number."
  (assert (and (rationalp slice)
               (plusp slice)))
  (let ((index 0)
        (vector (list-to-vector list))
        (result nil))
    (while (< index (length vector))
      (when (integerp index)
        (push (svref vector index) result))
      (incf index slice))
    (reverse result)))

(defun read-file (filename)
  "This reads in the entire file FILENAME, and returns a string."
  (with-open-file (input-file filename :direction :input)
    (do* ((current-char (read-char input-file nil)
                        (read-char input-file nil))
          (result (list current-char)
                  (cons current-char result)))
      ((null current-char)
       (concatenate 'string (nreverse (rest result)))))))

(defun read-lines (filename)
  "This reads in the entire file FILENAME, and returns a list of its lines."
  (with-open-file (input-file filename :direction :input)
    (do* ((current-line (read-line input-file nil)
                        (read-line input-file nil))
          (result (list current-line)
                  (cons current-line result)))
      ((null current-line)
       (reverse (rest result))))))

(defun strcat (&rest rest)
  (apply #'concatenate 'string rest))

(defun strmult (count &rest strings)
  (apply #'strcat (loop for i from 1 to count collect (apply #'strcat strings))))

#-cmu
(defmacro deletef (item sequence &rest rest)
  `(setf ,sequence
         (delete ,item ,sequence ,@rest)))

(defun operator-to-function (operator)
  (lambda (&rest rest)
    (eval `(,operator ,@rest))))

(defun replace-char (string from-char to-char)
  "Replaces every instance of FROM-CHAR with TO-CHAR."
  (assert (stringp string))
  (loop for i from 0 to (1- (length string)) do
        (if (char= (char string i) from-char)
          (setf (char string i) to-char)))
  string)

(defun stringify (argument)
  (format nil "~A" argument))

(defmacro otherwise-nil (conditional t-action)
  `(if ,conditional ,t-action nil))

(defmacro aif-otherwise-nil (conditional t-action)
  `(aif ,conditional ,t-action nil))

(defun character-range (start end)
  (loop for i from (char-code start) to (char-code end) collect (code-char i)))

(defun character-ranges (&rest rest)
  (cond ((<= (length rest) 1)
         rest)
        ((= 2 (length rest))
         (character-range (car rest) (cadr rest)))
        ((< 2 (length rest))
         (concatenate 'list
                      (character-range (car rest) (cadr rest))
                      (apply #'character-ranges (cddr rest))))))

(defun string-join (strings &optional (connecting-string ""))
  (assert (or (stringp strings)
              (and (listp strings)
                   (every #'stringp strings))))
  (if (stringp strings)
    (string-join (list strings))
    (apply #'concatenate 'string
           (first strings)
           (mapcar (lambda (string)
                     (concatenate 'string connecting-string string))
                   (rest strings)))))

(defun escape-tildes (string)
  (let ((input (vector-to-list string))
        (result nil)
        (current nil))
    (while (not (null input))
           (setf current (pop input))
           (when (eq current #\~)
             (push #\~ result))
           (push current result))
    (strcat (reverse result))))