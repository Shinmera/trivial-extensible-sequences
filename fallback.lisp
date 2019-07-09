#|
 This file is a part of trivial-extensible-sequences
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.trivial-extensible-sequences.fallback
  (:use #:cl)
  #+(or abcl ccl clasp ecl sbcl)
  (:local-nicknames (#:sequences #:org.shirakumo.trivial-extensible-sequences))
  (:shadow #:step #:endp))
(in-package #:org.shirakumo.trivial-extensible-sequences.fallback)

;;;; Core Protocol
(defclass sequences:sequence ()
  ())

(defgeneric sequences:length (sequence)
  (:method (thing)
    (protocol-unimplemented 'sequences:length thing))
  (:method ((sequence list))
    (length sequence))
  (:method ((sequence vector))
    (length sequence)))

(defgeneric sequences:elt (sequence index)
  (:method (thing index)
    (declare (ignore index))
    (protocol-unimplemented 'sequences:elt thing))
  (:method ((sequence list) index)
    (elt sequence index))
  (:method ((sequence vector) index)
    (elt sequence index)))

(defgeneric (setf sequences:elt) (new-value sequence index)
  (:method (new-value thing index)
    (declare (ignore new-value index))
    (protocol-unimplemented '(setf sequences:elt) thing))
  (:method (new-value (sequence list) index)
    (setf (elt sequence index) new-value))
  (:method (new-value (sequence vector) index)
    (setf (elt sequence index) new-value)))

(defgeneric sequences:adjust-sequence (sequence length &key initial-element initial-contents)
  (:method (thing length &key initial-element initial-contents)
    (declare (ignore initial-element initial-contents))
    (protocol-unimplemented 'sequences:adjust-sequence thing))
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

(defgeneric sequences:make-sequence-like (sequence length &key initial-element initial-contents)
  (:method (thing length &key initial-element initial-contents)
    (declare (ignore initial-element initial-contents))
    (protocol-unimplemented 'sequences:make-sequence-like thing))
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

(define-condition sequences:protocol-unimplemented (type-error)
  ((protocol :initarg :protocol :reader sequences:protocol-unimplemented-operation))
  (:report (lambda (c s) (format s "The sequence protocol function ~s is not implemented for~%  ~a"
                                 (sequences:protocol-unimplemented-operation c) (type-error-datum c)))))

(defun protocol-unimplemented (protocol datum)
  (error 'sequences:protocol-unimplemented
         :protocol protocol
         :datum datum
         :epected-type '(or sequence sequences:sequence)))

;;; Iterator Protocol
(defgeneric sequences:make-sequence-iterator (sequence &key start end from-end)
  (:method (sequence &rest args)
    (multiple-value-bind (iterator limit from-end) (apply #'sequences:make-simple-sequence-iterator sequence args)
      (values iterator limit from-end
              #'sequences:iterator-step
              #'sequences:iterator-endp
              #'sequences:iterator-element
              #'(setf sequences:iterator-element)
              #'sequences:iterator-index
              #'sequences:iterator-copy))))

(defmacro sequences:with-sequence-iterator ((&whole vars
                                             &optional iterator limit from-end-p
                                                       step endp element set-element index copy)
                                            (sequence &key from-end (start 0) end) &body body)
  (declare (ignore iterator limit from-end-p step endp element set-element index copy))
  (let* ((ignored ())
         (vars (loop for var in vars
                     for gensym = (gensym)
                     collect (or var
                                 (prog1 gensym
                                   (push gensym ignored))))))
    `(multiple-value-bind ,vars (sequences:make-sequence-iterator ,sequence :start ,start :end ,end :from-end ,from-end)
       (declare (ignore ,@ignored))
       ,@body)))

(defmacro sequences:with-sequence-iterator-functions ((step endp elt setf index copy)
                                                      (sequence &rest args &key from-end start end)
                                                      &body body)
  (declare (ignore from-end start end))
  (let ((nstate (gensym "STATE")) (nlimit (gensym "LIMIT"))
        (nfrom-end (gensym "FROM-END-")) (nstep (gensym "STEP"))
        (nendp (gensym "ENDP")) (nelt (gensym "ELT"))
        (nsetf (gensym "SETF")) (nindex (gensym "INDEX"))
        (ncopy (gensym "COPY")) (new-value (gensym "NEW-VALUE")))
    `(sequences:with-sequence-iterator
         (,nstate ,nlimit ,nfrom-end ,nstep ,nendp ,nelt ,nsetf ,nindex ,ncopy)
         (,sequence ,@args)
       (flet ((,step () (setq ,nstate (funcall ,nstep ,sequence ,nstate ,nfrom-end)))
              (,endp () (funcall ,nendp ,sequence ,nstate ,nlimit ,nfrom-end))
              (,elt () (funcall ,nelt ,sequence ,nstate))
              (,setf (,new-value) (funcall ,nsetf ,new-value ,sequence ,nstate))
              (,index () (funcall ,nindex ,sequence ,nstate))
              (,copy () (funcall ,ncopy ,sequence ,nstate)))
         (declare (ignorable #',setf #',index #',copy))
         ,@body))))

;;; Simple Iterator Protocol
;; Taken from SBCL's extensible sequences implementation
(defvar *exhausted* (make-symbol "EXHAUSTED"))

(defgeneric sequences:make-simple-sequence-iterator
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
  (:method ((s sequences:sequence) &key from-end (start 0) end)
    (let ((end (or end (length s))))
      (if from-end
          (values (1- end) (1- start) from-end)
          (values start end nil)))))

(defgeneric sequences:iterator-step (sequence iterator from-end)
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
  (:method ((s sequences:sequence) iterator from-end)
    (if from-end
        (1- iterator)
        (1+ iterator))))

(defgeneric sequences:iterator-endp (sequence iterator limit from-end)
  (:method ((s list) iterator limit from-end)
    (eq iterator limit))
  (:method ((s vector) iterator limit from-end)
    (= iterator limit))
  (:method ((s sequences:sequence) iterator limit from-end)
    (= iterator limit)))

(defgeneric sequences:iterator-element (sequence iterator)
  (:method ((s list) iterator)
    (car iterator))
  (:method ((s vector) iterator)
    (aref s iterator))
  (:method ((s sequences:sequence) iterator)
    (sequence:elt s iterator)))

(defgeneric (setf sequences:iterator-element) (new-value sequence iterator)
  (:method (o (s list) iterator)
    (setf (car iterator) o))
  (:method (o (s vector) iterator)
    (setf (aref s iterator) o))
  (:method (o (s sequences:sequence) iterator)
    (setf (sequence:elt s iterator) o)))

(defgeneric sequences:iterator-index (sequence iterator)
  (:method ((s list) iterator)
    ;; FIXME: this sucks.  (In my defence, it is the equivalent of the
    ;; Apple implementation in Dylan...)
    (loop for l on s for i from 0 when (eq l iterator) return i))
  (:method ((s vector) iterator) iterator)
  (:method ((s sequences:sequence) iterator) iterator))

(defgeneric sequences:iterator-copy (sequence iterator)
  (:method ((s list) iterator) iterator)
  (:method ((s vector) iterator) iterator)
  (:method ((s sequences:sequence) iterator) iterator))

;;;; Default Functions
(defgeneric sequences:emptyp (sequence)
  (:method ((sequence sequences:sequence))
    (= 0 (sequences:length sequence)))
  (:method ((null null)) T)
  (:method ((cons cons)) NIL)
  (:method ((vector vector)) (= 0 (length vector))))

(defun test (test test-not)
  (cond (test-not
         (complement test-not))
        (test
         test)
        (T
         #'eql)))

(defun coerce-to (type sequence)
  (case type
    (list (sequences:make-sequence-like '() (sequences:length sequence) :initial-contents sequence))
    (vector (sequences:make-sequence-like #() (sequences:length sequence) :initial-contents sequence))
    (T (sequences:make-sequence-like (allocate-instance type) (sequences:length sequence) :initial-contents sequence))))

(defmacro with-sequence-arguments ((&rest defs) &body body)
  `(let ,(loop for def in defs
               collect (case def
                         (test `(,def (test test test-not)))
                         (start `(,def (or start 0)))
                         (end `(,def (or end (sequences:length sequence))))
                         (key `(,def (or key #'identity)))
                         (count `(,def (or count most-positive-fixnum)))
                         (T def)))
     ,@body))

(defmacro do-iteration ((sequence &optional result) &body body)
  `(sequences:with-sequence-iterator-functions (step endp selt (setf selt) idx copy)
       (,sequence :from-end from-end :start (or start 0) :end (or end (sequences:length ,sequence)))
     (declare (ignorable #'selt))
     (loop until (endp)
           do (progn ,@body)
              (step)
           finally (return ,result))))

(defgeneric sequences:count (item sequence &key key test test-not start end from-end)
  (:method (item (sequence sequences:sequence) &key key test test-not start end from-end)
    (with-sequence-arguments ((count 0) test key)
      (do-iteration (sequence count)
        (when (funcall test item (funcall key (selt)))
          (incf count)))))
  (:method (item (sequence sequence) &rest args)
    (apply #'count item sequence args)))

(defgeneric sequences:count-if (pred sequence &key key start end from-end)
  (:method (pred (sequence sequences:sequence) &key key start end from-end)
    (with-sequence-arguments ((count 0) key) 
      (do-iteration (sequence count)
        (when (funcall pred (funcall key (selt)))
          (incf count)))))
  (:method (pred (sequence sequence) &rest args)
    (apply #'count-if pred sequence args)))

(defgeneric sequences:count-if-not (pred sequence &key key start end from-end)
  (:method (pred (sequence sequences:sequence) &key key start end from-end)
    (with-sequence-arguments ((count 0) key)
      (do-iteration (sequence count)
        (unless (funcall pred (funcall key (selt)))
          (incf count)))))
  (:method (pred (sequence sequence) &rest args)
    (apply #'count-if-not pred sequence args)))

(defgeneric sequences:find (item sequence &key key test test-not start end from-end)
  (:method (item (sequence sequences:sequence) &key key test test-not start end from-end)
    (with-sequence-arguments (test key)
      (do-iteration (sequence)
        (let ((elt (selt)))
          (when (funcall test item (funcall key elt))
            (return elt))))))
  (:method (item (sequence sequence) &rest args)
    (apply #'find item sequence args)))

(defgeneric sequences:find-if (pred sequence &key key start end from-end)
  (:method (pred (sequence sequences:sequence) &key key start end from-end)
    (with-sequence-arguments (key)
      (do-iteration (sequence)
        (let ((elt (selt)))
          (when (funcall pred (funcall key elt))
            (return elt))))))
  (:method (pred (sequence sequence) &rest args)
    (apply #'find-if pred sequence args)))

(defgeneric sequences:find-if-not (pred sequence &key key start end from-end)
  (:method (pred (sequence sequences:sequence) &key key start end from-end)
    (with-sequence-arguments (key)
      (do-iteration (sequence)
        (let ((elt (selt)))
          (unless (funcall pred (funcall key elt))
            (return elt))))))
  (:method (pred (sequence sequence) &rest args)
    (apply #'find-if-not pred sequence args)))

(defgeneric sequences:position (item sequence &key key test test-not start end from-end)
  (:method (item (sequence sequences:sequence) &key key test test-not start end from-end)
    (with-sequence-arguments (test key)
      (do-iteration (sequence)
        (when (funcall test item (funcall key (selt)))
          (return (idx))))))
  (:method (pred (sequence sequence) &rest args)
    (apply #'position pred sequence args)))

(defgeneric sequences:position-if (pred sequence &key key start end from-end)
  (:method (pred (sequence sequences:sequence) &key key start end from-end)
    (with-sequence-arguments (key)
      (do-iteration (sequence)
        (when (funcall pred (funcall key (selt)))
          (return (idx))))))
  (:method (pred (sequence sequence) &rest args)
    (apply #'position-if pred sequence args)))

(defgeneric sequences:position-if-not (pred sequence &key key start end from-end)
  (:method (pred (sequence sequences:sequence) &key key start end from-end)
    (with-sequence-arguments (key)
      (do-iteration (sequence)
        (unless (funcall pred (funcall key (selt)))
          (return (idx))))))
  (:method (pred (sequence sequence) &rest args)
    (apply #'position-if-not pred sequence args)))

(defgeneric sequences:subseq (sequence start &optional end)
  (:method ((sequence sequences:sequence) start &optional end)
    (let* ((from-end NIL)
           (head (cons NIL NIL))
           (tail head))
      (do-iteration (sequence)
        (setf tail (setf (cdr tail) (cons (selt) NIL))))
      (let ((contents (cdr head)))
        (sequences:make-sequence-like sequence (length contents) :initial-contents contents))))
  (:method ((sequence sequence) start &optional end)
    (subseq sequence start end)))

(defgeneric sequences:copy-seq (sequence)
  (:method ((sequence sequences:sequence))
    (sequences:make-sequence-like sequence (sequences:length sequence)))
  (:method ((sequence sequence))
    (copy-seq sequence)))

(defgeneric sequences:fill (sequence item &key start end)
  (:method ((sequence sequences:sequence) item &key start end)
    (let ((from-end NIL))
      (do-iteration (sequence sequence)
        (setf (selt) item))))
  (:method ((sequence sequence) item &rest args)
    (apply #'fill sequence item args)))

(defgeneric sequences:map (prototype function sequence &rest sequences)
  (:method ((prototype sequences:sequence) function sequence &rest sequences)
    (let* ((sequences (list* sequence sequences))
           (iterators (loop for sequence in sequences
                            collect (list* sequence (multiple-value-list (sequences:make-sequence-iterator sequence)))))
           (mapped (loop while (loop for iterator in iterators
                                     for (sequence it limit from-end step endp) = iterator
                                     never (funcall endp sequence it limit from-end))
                         collect (let ((els (loop for iterator in iterators
                                                  for (sequence it limit from-end step endp elt) = iterator
                                                  collect (funcall elt sequence it))))
                                   (apply function els))
                         do (loop for iterator in iterators
                                  for (sequence it limit from-end step) = iterator
                                  do (setf (second iterator) (funcall step sequence it from-end))))))
      (sequences:make-sequence-like prototype (length mapped) :initial-contents mapped)))
  (:method ((prototype sequence) function sequence &rest sequences)
    (apply #'map (type-of prototype) function sequence sequences)))

(defgeneric sequences:nsubstitute (new old sequence &key key test test-not start end from-end count)
  (:method (new old (sequence sequences:sequence) &key key test test-not start end from-end count)
    (with-sequence-arguments (key test count)
      (do-iteration (sequence sequence)
        (when (funcall test old (funcall key (selt)))
          (setf (selt) new)
          (when (= 0 (decf count)) (loop-finish))))))
  (:method (new old (sequence sequence) &rest args)
    (apply #'nsubstitute new old sequence args)))

(defgeneric sequences:nsubstitute-if (new pred sequence &key key start end from-end count)
  (:method (new pred (sequence sequences:sequence) &key key start end from-end count)
    (with-sequence-arguments (key count)
      (do-iteration (sequence sequence)
        (when (funcall pred (funcall key (selt)))
          (setf (selt) new)
          (when (= 0 (decf count)) (loop-finish))))))
  (:method (new pred (sequence sequence) &rest args)
    (apply #'nsubstitute-if new pred sequence args)))

(defgeneric sequences:nsubstitute-if-not (new pred sequence &key key start end from-end count)
  (:method (new pred (sequence sequences:sequence) &key key start end from-end count)
    (with-sequence-arguments (key count)
      (do-iteration (sequence sequence)
        (unless (funcall pred (funcall key (selt)))
          (setf (selt) new)
          (when (= 0 (decf count)) (loop-finish))))))
  (:method (new pred (sequence sequence) &rest args)
    (apply #'nsubstitute-if-not new pred sequence args)))

(defgeneric sequences:substitute (new old sequence &key key test test-not start end from-end count)
  (:method (new old (sequence sequences:sequence) &key key test test-not start end from-end count)
    (with-sequence-arguments (key test count)
      (let ((sequence (sequences:copy-seq sequence)))
        (do-iteration (sequence sequence)
          (when (funcall test old (funcall key (selt)))
            (setf (selt) new)
            (when (= 0 (decf count)) (loop-finish)))))))
  (:method (new old (sequence sequence) &rest args)
    (apply #'substitute new old sequence args)))

(defgeneric sequences:substitute-if (new pred sequence &key key start end from-end count)
  (:method (new pred (sequence sequences:sequence) &key key start end from-end count)
    (with-sequence-arguments (key count)
      (let ((sequence (sequences:copy-seq sequence)))
        (do-iteration (sequence sequence)
          (when (funcall pred (funcall key (selt)))
            (setf (selt) new)
            (when (= 0 (decf count)) (loop-finish)))))))
  (:method (new pred (sequence sequence) &rest args)
    (apply #'substitute-if new pred sequence args)))

(defgeneric sequences:substitute-if-not (new pred sequence &key key start end from-end count)
  (:method (new pred (sequence sequences:sequence) &key key start end from-end count)
    (with-sequence-arguments (key count)
      (let ((sequence (sequences:copy-seq sequence)))
        (do-iteration (sequence sequence)
          (unless (funcall pred (funcall key (selt)))
            (setf (selt) new)
            (when (= 0 (decf count)) (loop-finish)))))))
  (:method (new pred (sequence sequence) &rest args)
    (apply #'substitute-if-not new pred sequence args)))

(defgeneric sequences:replace (sequence1 sequence2 &key start1 end1 start2 end2)
  (:method (sequence1 sequence2 &key start1 end1 start2 end2)
    (let* ((start1 (or start1 0))
           (start2 (or start2 0))
           (end1 (or end1 (sequences:length sequence1)))
           (end2 (or end2 (sequences:length sequence2)))
           (length (min (- end1 start1) (- end2 start2)))
           (start start1)
           (end (+ start length))
           (from-end NIL))
      (sequences:with-sequence-iterator-functions (step2 endp2 elt2 (setf elt2) idx2 copy2)
          (sequence2 :start start2 :end (+ start2 length))
        (declare (ignore #'endp2))
        (do-iteration (sequence1 sequence1)
          (setf (selt) (elt2))
          (step2)))))
  (:method ((a sequence) (b sequence) &rest args)
    (apply #'replace a b args)))

(defgeneric sequences:nreverse (sequence)
  (:method ((sequence sequences:sequence))
    (sequences:with-sequence-iterator-functions (step endp selt (setf selt) idx copy)
        (sequence :from-end T)
      (loop for length from 0
            until (endp)
            collect (selt) into contents
            do (step)
            finally (return (sequences:replace sequence contents)))))
  (:method ((sequence sequence))
    (nreverse sequence)))

(defgeneric sequences:reverse (sequence)
  (:method ((sequence sequences:sequence))
    (sequences:with-sequence-iterator-functions (step endp selt (setf selt) idx copy)
        (sequence :from-end T)
      (loop for length from 0
            until (endp)
            collect (selt) into contents
            do (step)
            finally (return (sequences:make-sequence-like sequence length :initial-contents contents)))))
  (:method ((sequence sequence))
    (reverse sequence)))

(defgeneric sequences:concatenate (result-prototype &rest sequences)
  (:method ((prototype sequences:sequence) &rest sequences)
    (let ((new (make-array 0 :adjustable T :fill-pointer T)))
      (dolist (sequence sequences)
        (sequences:with-sequence-iterator-functions (step endp selt (setf selt) idx copy) (sequence)
          (loop until (endp)
                do (vector-push-extend (selt) new)
                   (step))))
      (sequences:make-sequence-like prototype (length new) :initial-contents new)))
  (:method ((prototype sequence) &rest sequences)
    (apply #'concatenate (type-of prototype) sequences)))

(defgeneric sequences:reduce (function sequence &key initial-value key start end from-end)
  (:method (func (sequence sequences:sequence) &key (initial-value NIL ip) key start end from-end)
    (with-sequence-arguments ((result initial-value) key start end)
      (unless ip
        (cond (from-end
               (decf end)
               (setf result (funcall key (sequences:elt sequence end))))
              (T
               (setf result (funcall key (sequences:elt sequence start)))
               (incf start))))
      (if (and (not ip) (sequences:emptyp sequence))
          (funcall func)
          (do-iteration (sequence result)
            (setf result (funcall func result (funcall key (selt))))))))
  (:method (func (sequence sequence) &rest args)
    (apply #'reduce func sequence args)))

(defgeneric sequences:mismatch (sequence1 sequence2 &key key test test-not start1 end1 start2 end2 from-end)
  (:method (sequence1 sequence2 &rest args)
    ;; FIXME: Implement without copying
    (apply #'mismatch (coerce-to 'vector sequence1) (coerce-to 'vector sequence2) args))
  (:method ((a sequence) (b sequence) &rest args)
    (apply #'mismatch a b args)))

(defgeneric sequences:search (sequence1 sequence2 &key key test test-not start1 end1 start2 end2 from-end)
  (:method (sequence1 sequence2 &rest args)
    ;; FIXME: Implement without copying
    (apply #'search (coerce-to 'vector sequence1) (coerce-to 'vector sequence2) args))
  (:method ((a sequence) (b sequence) &rest args)
    (apply #'search a b args)))

(defgeneric sequences:delete (item sequence &key key test test-not start end from-end)
  (:method (item (sequence sequences:sequence) &key key test test-not start end from-end)
    (let ((new (make-array (sequences:length sequence) :fill-pointer 0)))
      (with-sequence-arguments (key test)
        (do-iteration (sequence)
          (unless (funcall test item (funcall key (selt)))
            (vector-push (selt) new))))
      (sequences:adjust-sequence sequence (length new) :initial-contents new)))
  (:method (item (sequence sequence) &rest args)
    (apply #'delete item sequence args)))

(defgeneric sequences:delete-if (pred sequence &key key start end from-end)
  (:method (pred (sequence sequences:sequence) &key key start end from-end)
    (let ((new (make-array (sequences:length sequence) :fill-pointer 0)))
      (with-sequence-arguments (key)
        (do-iteration (sequence)
          (unless (funcall pred (funcall key (selt)))
            (vector-push (selt) new))))
      (sequences:adjust-sequence sequence (length new) :initial-contents new)))
  (:method (pred (sequence sequence) &rest args)
    (apply #'delete-if pred sequence args)))

(defgeneric sequences:delete-if-not (pred sequence &key key start end from-end)
  (:method (pred (sequence sequences:sequence) &key key start end from-end)
    (let ((new (make-array (sequences:length sequence) :fill-pointer 0)))
      (with-sequence-arguments (key)
        (do-iteration (sequence)
          (when (funcall pred (funcall key (selt)))
            (vector-push (selt) new))))
      (sequences:adjust-sequence sequence (length new) :initial-contents new)))
  (:method (pred (sequence sequence) &rest args)
    (apply #'delete-if-not pred sequence args)))

(defgeneric sequences:remove (item sequence &key key test test-not start end from-end)
  (:method (item (sequence sequences:sequence) &key key test test-not start end from-end)
    (let ((new (make-array (sequences:length sequence) :fill-pointer 0)))
      (with-sequence-arguments (key test)
        (do-iteration (sequence)
          (unless (funcall test item (funcall key (selt)))
            (vector-push (selt) new))))
      (sequences:make-sequence-like sequence (length new) :initial-contents new)))
  (:method (item (sequence sequence) &rest args)
    (apply #'remove item sequence args)))

(defgeneric sequences:remove-if (pred sequence &key key start end from-end)
  (:method (pred (sequence sequences:sequence) &key key start end from-end)
    (let ((new (make-array (sequences:length sequence) :fill-pointer 0)))
      (with-sequence-arguments (key)
        (do-iteration (sequence)
          (unless (funcall pred (funcall key (selt)))
            (vector-push (selt) new))))
      (sequences:make-sequence-like sequence (length new) :initial-contents new)))
  (:method (pred (sequence sequence) &rest args)
    (apply #'remove-if pred sequence args)))

(defgeneric sequences:remove-if-not (pred sequence &key key start end from-end)
  (:method (pred (sequence sequences:sequence) &key key start end from-end)
    (let ((new (make-array (sequences:length sequence) :fill-pointer 0)))
      (with-sequence-arguments (key)
        (do-iteration (sequence)
          (when (funcall pred (funcall key (selt)))
            (vector-push (selt) new))))
      (sequences:make-sequence-like sequence (length new) :initial-contents new)))
  (:method (pred (sequence sequence) &rest args)
    (apply #'remove-if-not pred sequence args)))

(defgeneric sequences:delete-duplicates (sequence &key key test test-not start end from-end)
  (:method ((sequence sequences:sequence) &rest args)
    ;; FIXME: Implement without copying
    (let ((new (coerce-to 'list sequence)))
      (setf new (apply #'remove-duplicates new args))
      (sequences:adjust-sequence sequence (length new) :initial-contents new)))
  (:method ((sequence sequence) &rest args)
    (apply #'delete-duplicates sequence args)))

(defgeneric sequences:remove-duplicates (sequence &key key test test-not start end from-end)
  (:method ((sequence sequences:sequence) &rest args)
    ;; FIXME: Implement without copying
    (let ((new (coerce-to 'list sequence)))
      (setf new (apply #'remove-duplicates new args))
      (sequences:make-sequence-like sequence (length new) :initial-contents new)))
  (:method ((sequence sequence) &rest args)
    (apply #'remove-duplicates sequence args)))

(defgeneric sequences:sort (sequence pred &key key)
  (:method ((sequence sequences:sequence) pred &rest args)
    ;; FIXME: Implement without copying
    (let ((sorted (apply #'sort (coerce-to 'list sequence) pred args)))
      (sequences:adjust-sequence sequence (sequences:length sequence) :initial-contents sorted)))
  (:method ((sequence sequence) pred &rest args)
    (apply #'sort sequence pred args)))

(defgeneric sequences:stable-sort (sequence pred &key key)
  (:method ((sequence sequences:sequence) pred &rest args)
    ;; FIXME: Implement without copying
    (let ((sorted (apply #'stable-sort (coerce-to 'list sequence) pred args)))
      (sequences:adjust-sequence sequence (sequences:length sequence) :initial-contents sorted)))
  (:method ((sequence sequence) pred &rest args)
    (apply #'stable-sort sequence pred args)))

(defgeneric sequences:merge (prototype sequence1 sequence2 predicate &key key)
  (:method (prototype sequence1 sequence2 predicate &rest args)
    ;; FIXME: Implement without copying
    (let ((new (apply #'merge 'vector (coerce-to 'vector sequence2) (coerce-to 'vector sequence2) predicate args)))
      (sequences:make-sequence-like prototype (length new) :initial-contents new)))
  (:method (prototype (a sequence) (b sequence) pred &rest args)
    (apply #'merge prototype a b pred args)))

(defmacro sequences:dosequence ((element sequence &optional return) &body body)
  (let ((step (gensym "STEP")) (endp (gensym "ENDP")) (elt (gensym "ELT"))
        (setf (gensym "SETF")) (index (gensym "INDEX")) (copy (gensym "COPY")))
    `(sequences:with-sequence-iterator-functions (,step ,endp ,elt ,setf ,index ,copy) (,sequence)
       (declare (ignore #',setf #',index #',copy))
       (loop until (,endp)
             for ,element = (,elt)
             do (progn
                  ,@body
                  (,step))
             finally (return ,return)))))
