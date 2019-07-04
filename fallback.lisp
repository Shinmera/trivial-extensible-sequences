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

(defgeneric length (sequence))
(defgeneric elt (sequence index))
(defgeneric (setf elt) (new-value sequence index))
(defgeneric adjust-sequence (sequence length &key initial-element initial-contents))
(defgeneric make-sequence-like (sequence length &key initial-element initial-contents))

(define-condition protocol-unimplemented (type-error)
  ())

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
