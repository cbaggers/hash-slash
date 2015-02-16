;;;; hash-slash.lisp

(in-package #:hash-slash)

;;   All credit goes to the following post by Erik Naggum
;;   https://groups.google.com/forum/#!topic/comp.lang.lisp/VrcXtRUx43M
;;
;;   I have taken to define #/ as a reader-macro akin to the format control,
;;   to read _registered_ types with associated functions, as I ran out of
;;   convenient ways to specify classes and such.

;;   In this particular case, #/point/(1 2 3), would require a registered type
;;   named point with an associated reader function that that would read as
;;   many arguments as it needs using whatever printer controls and reader
;;   functions it pleased, but it would of course be the question of more or
;;   less reasoanble usages, as with every other powerful mechanism.  E.g.,
;;   #/ip/192.168.150.155/26 could mean the address and a 26-bit mask for
;;   routing or matching purposes, whild #/ip/192.168.150.155:5555 could mean
;;   an address and a port number.  And #/md5/0123456789abcdeffedcba9876543210
;;   could mean the hexadecimal representation of an MD5 sum.

;;   Given this framework, would be a very simple task to supply macros that
;;   defined and registered reader functions and corresponding print-object
;;   methods for new classes.

(defparameter *form-types* '(:object :space :paren))
(defparameter *read-modes* '(:standard :safe))
(defparameter *registered-readers* nil)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defmacro def-hashslash-reader ((name form-type &optional (form-var 'this))
                                &body body)
  `(progn (push (list ',name nil ,form-type (lambda (,form-var) ,@body))
                *registered-readers*)
          ',name))

(defun fetch-registered-reader (macro-type)
  (or (cdr (assoc macro-type *registered-readers*))
      (error "#/ reader for type /~a/ has not been registered" macro-type)))

(defun parse-form (form-type stream)
  (case form-type
    (:object (read stream t nil t))
    (:space (concatenate 'string
                         (remove #\newline
                                 (remove #\return
                                         (read-to #\space stream nil)))))
    (:paren (concatenate 'string (read-to-matching #\( #\) stream t nil)))))

(defun delegate-form-reading (reader stream)
  (destructuring-bind (parse-mode form-type func) reader
    (declare (ignore parse-mode))
    (funcall func (parse-form form-type stream))))

(defun read-type (stream)
  (read stream t nil t))

(defun read-to (end-char &optional (stream *standard-input*) (eol-error t))
  (loop :for char = (read-char stream eol-error nil t)
     :while (and (characterp char) (char/= char end-char))
     :collect char))

(defun read-to-matching (open-char close-char
                         &optional (stream *standard-input*) (read-one t)
                           (eol-error t))
  (when read-one (read-char stream t nil t))
  (let ((depth 0)
        (escape nil))
    (loop :for char = (read-char stream eol-error nil t) :do
       (when (characterp char)
         (let ((open (char= char open-char))
               (close (char= char close-char)))
           (when (and open (not escape)) (incf depth))
           (when (and close (not escape)) (decf depth))
           (setf escape (char= char #\\))))
       :while (and (>= depth 0) (characterp char)) :collect char)))

(defun extract-slash-form (stream)
  (symb (string-upcase (concatenate 'string (read-to #\/ stream)))))

(defun read-hashslash-form (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((type (extract-slash-form stream)))
    (delegate-form-reading
     (fetch-registered-reader type)
     stream)))

(named-readtables:defreadtable hash-slash
  (:merge :standard)
  (:dispatch-macro-char #\# #\/ #'read-hashslash-form))

;;(named-readtables:in-readtable hash-slash)


;; #/ip/10.0.0.1:4005/#

;; (def-hashslash-reader (glsl :paren)
;;   `(progn '(:glsl ,(subseq this 1 (- (length this) 2)))))

;; #/glsl/
;; (
;;  void main ()
;;  {
;;     int a = 1;
;;  }
;; )
