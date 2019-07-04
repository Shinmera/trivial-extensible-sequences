#|
 This file is a part of trivial-extensible-sequences
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.trivial-extensible-sequences.fallback
  (:use #:cl)
  (:shadow #:defclass #:define-condition #:defgeneric #:defmethod))

;;; Wrapper stuff as I don't want to depend on PLNs (sigh)
(defun name (name)
  (if (listp name)
      `(setf ,(name (second name)))
      (or (find-symbol (string name) #:org.shirakumo.trivial-extensible-sequences)
          (error "No such extensible sequences protocol name ~s" name))))

(defmacro defgeneric (name lambda-list &body options)
  `(cl:defgeneric ,(name name) ,lambda-list
     ,@options))

(defmacro defmethod (name &rest args)
  `(cl:defmethod ,(name name)
     ,@args))

(defmacro defclass (name direct-superclasses direct-slots &body options)
  `(cl:defclass ,(name name) ,direct-superclasses
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
  (error (name 'protocol-unimplemented)
         :protocol (name protocol)
         :datum datum
         :epected-type '(or sequence #.(name 'sequence))))

;;;; Default Functions
(defgeneric emptyp (sequence))
(defgeneric count (item sequecne &key key test test-not start end from-end))
(defgeneric count-if (pred sequence &key key start end from-end))
(defgeneric count-if-not (pred sequence &key key start end from-end))
(defgeneric find (item sequence &key key test test-not start end from-end))
(defgeneric find-if (pred sequence &key key start end from-end))
(defgeneric find-if-not (pred sequence &key key start end from-end))
(defgeneric position (item sequence &key key test test-not start end from-end))
(defgeneric position-if (pred sequence &key key start end from-end))
(defgeneric position-if-not (pred sequence &key key start end from-end))
(defgeneric subseq (sequence start &optional end))
(defgeneric copy-seq (sequence))
(defgeneric fill (sequence item &key start end))
(defgeneric map (prototype function sequence &rest sequences))
(defgeneric nsubstitute (new old sequence &key key test test-not start end from-end count))
(defgeneric nsubstitute-if (new pred sequence &key key start end from-end count))
(defgeneric nsubstitute-if-not (new pred sequence &key key start end from-end count))
(defgeneric substitute (new old sequence &key key test test-not start end from-end))
(defgeneric substitute-if (new pred sequence &key key start end from-end count))
(defgeneric substitute-if-not (new pred sequence &key key start end from-end count))
(defgeneric replace (sequence1 sequence2 &key start1 end1 start2 end2))
(defgeneric nreverse (sequence))
(defgeneric reverse (sequence))
(defgeneric concatenate (result-prototype &rest sequences))
(defgeneric reduce (function sequence &key initial-value key start end from-end))
(defgeneric mismatch (sequence1 sequence2 &key key test test-not start1 end1 start2 end2 from-end))
(defgeneric search (sequence1 sequence2 &key key test test-not start1 end1 start2 end2 from-end))
(defgeneric delete (item sequence &key key test test-not start end from-end))
(defgeneric delete-if (pred sequence &key key start end from-end))
(defgeneric delete-if-not (pred sequence &key key start end from-end))
(defgeneric remove (item sequence &key key test test-not start end from-end))
(defgeneric remove-if (pred sequence &key key start end from-end))
(defgeneric remove-if-not (pred sequence &key key start end from-end))
(defgeneric delete-duplicates (sequence &key key test test-not start end from-end))
(defgeneric remove-duplicates (sequence &key key test test-not start end from-end))
(defgeneric sort (sequence pred &key key))
(defgeneric stable-sort (sequence pred &key key))
(defgeneric merge (prototype sequence1 sequence2 predicate &key key))

(defmacro #.(name 'dosequence) ((element sequence &optional return) &body body)
  (let ((step (gensym "STEP")) (endp (gensym "ENDP")) (elt (gensym "ELT"))
        (setf (gensym "SETF")) (index (gensym "INDEX")) (copy (gensym "COPY"))))
  `(with-sequence-iterator-functions (,step ,endp ,elt ,setf ,index ,copy) (,sequence)
     (declare (ignore #',setf #',index #',copy))
     (loop until (,endp)
           for ,element = (,elt)
           do (progn
                ,@body
                (,step))
           finally (return ,return))))

;;; Iterator Protocol
(defgeneric make-sequence-iterator (sequence &key start end from-end)
  (:method (sequence &rest args)
    (multiple-value-bind (iterator limit from-end) (apply #'make-simple-sequence-iterator sequence args)
      (values iterator limit from-end
              #'#.(name 'iterator-step)
              #'#.(name 'iterator-endp)
              #'#.(name 'iterator-element)
              #'#.(name '(setf iterator-element))
              #'#.(name 'iterator-index)
              #'#.(name 'iterator-copy)))))

(defmacro #.(name 'with-sequence-iterator) ((&whole vars
                                                    &optional iterator limit from-end-p
                                                    step endp element set-element index copy)
                                            (sequence &key from-end (start 0) end) &body body)
  (let* ((ignored ())
         (vars (loop for var in vars
                     far gensym = (gensym)
                     collect (or var
                                 (prog1 gensym
                                   (push gensym ignored))))))
    `(multiple-value-bind ,vars (make-sequence-iterator ,sequence :start ,start :end ,end :from-end ,from-end)
       (declare (ignore ,@ignored))
       ,@body body)))

(defmacro #.(name 'with-sequence-iterator-functions) ((step endp elt setf index copy)
                                                                (sequence &rest args &key from-end start end)
                                                                &body body)
  (let ((nstate (gensym "STATE")) (nlimit (gensym "LIMIT"))
        (nfrom-end (gensym "FROM-END-")) (nstep (gensym "STEP"))
        (nendp (gensym "ENDP")) (nelt (gensym "ELT"))
        (nsetf (gensym "SETF")) (nindex (gensym "INDEX"))
        (ncopy (gensym "COPY")) (new-value (gensym "NEW-VALUE")))
    (#.(name 'with-sequence-iterator)
         (,nstate ,nlimit ,nfrom-end ,nstep ,nendp ,nelt ,nsetf ,nindex ,ncopy)
       (,sequence ,@args)
       (flet ((,step () (setq ,nstate (funcall ,nstep ,sequence ,nstate ,nfrom-end)))
              (,endp () (funcall ,nendp ,sequence ,nstate ,nlimit ,nfrom-end))
              (,elt () (funcall ,nelt ,sequence ,nstate))
              (,setf (,new-value) (funcall ,nsetf ,new-value ,sequence ,nstate))
              (,index () (funcall ,nindex ,sequence ,nstate))
              (,copy () (funcall ,ncopy ,sequence ,nstate)))
         ,@body))))

;;; Simple Iterator Protocol
;; Taken from SBCL's extensible sequences implementation
(defgeneric make-simple-sequence-iterator
    (sequence &key from-end start end)
  (:method ((s list) &key from-end (start 0) end)
    (if from-end
        (let* ((termination (if (= start 0) *exhausted* (nthcdr (1- start) s)))
               (init (if (<= (or end (length s)) start)
                         termination
                         (if end (last s (- (length s) (1- end))) (last s)))))
          (values init termination t))
        (cond
          ((not end) (values (nthcdr start s) nil nil))
          (t (let ((st (nthcdr start s)))
               (values st (nthcdr (- end start) st) nil))))))
  (:method ((s vector) &key from-end (start 0) end)
    (let ((end (or end (length s))))
      (if from-end
          (values (1- end) (1- start) t)
          (values start end nil))))
  (:method ((s sequence) &key from-end (start 0) end)
    (let ((end (or end (length s))))
      (if from-end
          (values (1- end) (1- start) from-end)
          (values start end nil)))))

(defgeneric iterator-step (sequence iterator from-end)
  (:method ((s list) iterator from-end)
    (if from-end
        (if (eq iterator s)
            *exhausted*
            (do* ((xs s (cdr xs)))
                 ((eq (cdr xs) iterator) xs)))
        (cdr iterator)))
  (:method ((s vector) iterator from-end)
    (if from-end
        (1- iterator)
        (1+ iterator)))
  (:method ((s sequence) iterator from-end)
    (if from-end
        (1- iterator)
        (1+ iterator))))

(defgeneric iterator-endp (sequence iterator limit from-end)
  (:method ((s list) iterator limit from-end)
    (eq iterator limit))
  (:method ((s vector) iterator limit from-end)
    (= iterator limit))
  (:method ((s sequence) iterator limit from-end)
    (= iterator limit)))

(defgeneric iterator-element (sequence iterator)
  (:method ((s list) iterator)
    (car iterator))
  (:method ((s vector) iterator)
    (aref s iterator))
  (:method ((s sequence) iterator)
    (sequence:elt s iterator)))

(defgeneric (setf iterator-element) (new-value sequence iterator)
  (:method (o (s list) iterator)
    (setf (car iterator) o))
  (:method (o (s vector) iterator)
    (setf (aref s iterator) o))
  (:method (o (s sequence) iterator)
    (setf (sequence:elt s iterator) o)))

(defgeneric iterator-index (sequence iterator)
  (:method ((s list) iterator)
    ;; FIXME: this sucks.  (In my defence, it is the equivalent of the
    ;; Apple implementation in Dylan...)
    (loop for l on s for i from 0 when (eq l iterator) return i))
  (:method ((s vector) iterator) iterator)
  (:method ((s sequence) iterator) iterator))

(defgeneric iterator-copy (sequence iterator)
  (:method ((s list) iterator) iterator)
  (:method ((s vector) iterator) iterator)
  (:method ((s sequence) iterator) iterator))
