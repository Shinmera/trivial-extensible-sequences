(defpackage #:org.shirakumo.trivial-extensible-sequences
  #-(or abcl ccl clasp ecl sbcl)
  (:nicknames #:sequence)
  (:use #+sbcl #:sb-sequence
        #+(or abcl clasp) #:sequence)
  #+(or sbcl abcl clasp)
  (:import-from #:cl #:sequence)
  (:export
   #:sequence
   #:length
   #:elt
   #:adjust-sequence
   #:make-sequence-like
   #:protocol-unimplemented
   #:protocol-unimplemented-operation
   #:emptyp
   #:count
   #:count-if
   #:count-if-not
   #:find
   #:find-if
   #:find-if-not
   #:position
   #:position-if
   #:position-if-not
   #:subseq
   #:copy-seq
   #:fill
   #:map
   #:nsubstitute
   #:nsubstitute-if
   #:nsubstitute-if-not
   #:substitute
   #:substitute-if
   #:substitute-if-not
   #:replace
   #:nreverse
   #:reverse
   #:concatenate
   #:reduce
   #:mismatch
   #:search
   #:delete
   #:delete-if
   #:delete-if-not
   #:remove
   #:remove-if
   #:remove-if-not
   #:delete-duplicates
   #:remove-duplicates
   #:sort
   #:stable-sort
   #:merge
   #:dosequence
   #:make-sequence-iterator
   #:with-sequence-iterator
   #:with-sequence-iterator-functions
   #:iterator-step
   #:iterator-endp
   #:iterator-element
   #:iterator-index
   #:iterator-copy
   #:make-simple-sequence-iterator))
