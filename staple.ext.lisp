(asdf:load-system :staple-markless)

(defpackage #:org.shirakumo.trivial-extensible-sequences.doc
  (:use #:cl)
  (:local-nicknames (#:sequences #:org.shirakumo.trivial-extensible-sequences)))

(defmethod staple:packages ((_ (eql (asdf:find-system :trivial-extensible-sequences))))
  (list (find-package '#:org.shirakumo.trivial-extensible-sequences.doc)))

#+sbcl
(defmethod staple:definition-wanted-p ((_ definitions:source-transform) page) NIL)
