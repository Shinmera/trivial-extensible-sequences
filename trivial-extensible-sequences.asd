#|
 This file is a part of trivial-extensible-sequences
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defsystem trivial-extensible-sequences
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Portability library for the extensible sequences protocol."
  :homepage "https://shinmera.github.io/trivial-extensible-sequences/"
  :bug-tracker "https://github.com/Shinmera/trivial-extensible-sequences/issues"
  :source-control (:git "https://github.com/Shinmera/trivial-extensible-sequences.git")
  :serial T
  :components ((:file "api")
               (:file "fallback" :if-feature (:not (:or :sbcl :abcl))))
  :depends-on ((:feature :abcl (:require :extensible-sequences))))
