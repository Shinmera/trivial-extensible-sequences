#|
 This file is a part of trivial-extensible-sequences
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.trivial-extensible-sequences.fallback
  (:use #:cl)
  (:shadow #:defclass #:define-condition #:defgeneric #:defmethod))

;;; Wrapper stuff as I don't want to depend on PLNs
(defun sequence-symbol (name)
  (or (find-symbol (string name) #:org.shirakumo.trivial-extensible-sequences)
      (error "No such symbol ~s" name)))

(defmacro defgeneric (name lambda-list &body options)
  `(cl:defgeneric ,(sequence-symbol name) ,lambda-list
     ,@options))

(defmacro defmethod (name &rest args)
  `(cl:defmethod ,(sequence-symbol name)
     ,@args))

(defmacro defclass (name direct-superclasses direct-slots &body options)
  `(cl:defclass ,(sequence-symbol name) ,direct-superclasses
     ,direct-slots
     ,@options))

(defmacro define-condition (name direct-conditions direct-slots &body options)
  `(cl:define-condition ,name ,direct-conditions
     ,direct-slots
     ,@options))

;;;; Core Protocol
(defclass sequence ()
  ())

(defgeneric length (sequence)
  (:method (thing)
    (protocol-unimplemented 'length thing))
  (:method ((sequence list))
    (length sequence))
  (:method ((sequence vector))
    (length sequence)))

(defgeneric elt (sequence index)
  (:method (thing index)
    (protocol-unimplemented 'elt thing))
  (:method ((sequence list) index)
    (elt sequence index))
  (:method ((sequence vector) index)
    (elt sequence index)))

(defgeneric (setf elt) (new-value sequence index)
  (:method (new-value thing index)
    (protocol-unimplemented '(setf elt) thing))
  (:method (new-value (sequence list) index)
    (setf (elt sequence index) new-value))
  (:method (new-value (sequence vector) index)
    (setf (elt sequence index) new-value)))

(defgeneric adjust-sequence (sequence length &key initial-element initial-contents)
  (:method (thing length &key initial-element initial-contents)
    (protocol-unimplemented 'adjust-sequence thing))
  (:method ((sequence vector) length &rest args)
    (apply #'adjust-array sequence length args))
  (:method ((sequence list) length &key initial-element (initial-contents NIL contents-p))
    (when (< 0 length)
      (loop for cons = sequence then (cdr cons)
            for i from 1 below length
            while (cdr cons)
            finally (cond ((< i length)
                           (setf (cdr cons) (make-list (- length i) :initial-element initial-element)))
                          ((= i length)
                           (setf (cdr cons) NIL))))
      (when contents-p
        (replace sequence initial-contents))
      sequence)))

(defgeneric make-sequence-like (sequence length &key initial-element initial-contents)
  (:method (thing length &key initial-element initial-contents)
    (protocol-unimplemented 'make-sequence-like thing))
  (:method ((sequence vector) length &rest args)
    (if (loop for cons on args by #'cddr
              thereis (or (eql :initial-element (car cons))
                          (eql :initial-contents (car cons))))
        (apply #'make-array length args)
        (replace (make-array length) sequence)))
  (:method ((sequence list) length &key initial-element (initial-contents NIL contents-p))
    (if contents-p
        (replace (make-list length) initial-contents)
        (make-list length :initial-element initial-element))))

(define-condition protocol-unimplemented (type-error)
  ((protocol :initarg :protocol :reader protocol))
  (:report (lambda (c s) (format s "The sequence protocol function ~s is not implemented for~%  ~a"
                                 (protocol c) (type-error-datum c)))))

(defun protocol-unimplemented (protocol datum)
  (error (sequence-symbol 'protocol-unimplemented)
         :protocol (if (listp protocol)
                       `(setf ,(sequence-symbol (second protocol)))
                       (sequence-symbol protocol))
         :datum datum
         :epected-type '(or sequence #.(sequence-symbol 'sequence))))

;;;; Default Functions
(defgeneric emptyp ())
(defgeneric count ())
(defgeneric count-if ())
(defgeneric count-if-not ())
(defgeneric find ())
(defgeneric find-if ())
(defgeneric find-if-not ())
(defgeneric position ())
(defgeneric position-if ())
(defgeneric position-if-not ())
(defgeneric subseq ())
(defgeneric copy-seq ())
(defgeneric fill ())
(defgeneric map ())
(defgeneric nsubstitute ())
(defgeneric nsubstitute-if ())
(defgeneric nsubstitute-if-not ())
(defgeneric substitute ())
(defgeneric substitute-if ())
(defgeneric substitute-if-not ())
(defgeneric replace ())
(defgeneric nreverse ())
(defgeneric reverse ())
(defgeneric concatenate ())
(defgeneric reduce ())
(defgeneric mismatch ())
(defgeneric search ())
(defgeneric delete ())
(defgeneric delete-if ())
(defgeneric delete-if-not ())
(defgeneric remove ())
(defgeneric remove-if ())
(defgeneric remove-if-not ())
(defgeneric delete-duplicates ())
(defgeneric remove-duplicates ())
(defgeneric sort ())
(defgeneric stable-sort ())
(defgeneric merge ())
(defgeneric dosequence ())

;;; Iterator Protocol
(defgeneric make-sequence-iterator ())
(defgeneric with-sequence-iterator ())
(defgeneric with-sequence-iterator-functions ())

;;; Simple Iterator Protocol
(defgeneric iterator-step ())
(defgeneric iterator-endp ())
(defgeneric iterator-element ())
(defgeneric iterator-index ())
(defgeneric iterator-copy ())
(defgeneric make-simple-sequence-iterator ())
