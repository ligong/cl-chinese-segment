;;;; package.lisp

(defpackage #:cl-chinese-segment
  (:nicknames :chseg)
  (:use #:cl #:cl-ppcre)
  (:export
   #:segment
   #:*language-model*
   #:*language-model-pathname*
   ))

