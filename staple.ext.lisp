(asdf:load-system :staple-markless)

(defpackage #:org.shirakumo.trivial-extensible-sequences.doc
  (:use #:cl)
  (:local-nicknames (#:sequences #:org.shirakumo.trivial-extensible-sequences)))

(defclass simple-page* (staple:simple-page) ())

(defmethod staple:document-package ((page simple-page*))
  (find-package '#:org.shirakumo.trivial-extensible-sequences.doc))

(defmethod staple:packages ((_ (eql (asdf:find-system :trivial-extensible-sequences))))
  (list (find-package '#:org.shirakumo.trivial-extensible-sequences)))

(defmethod staple:page-type ((_ (eql (asdf:find-system :trivial-extensible-sequences))))
  'simple-page*)

#+sbcl
(defmethod staple:definition-wanted-p ((_ definitions:source-transform) page) NIL)
