;;;; cl-chinese-segment.asd

(asdf:defsystem #:cl-chinese-segment
  :description "Chinese segment utility based on Viterbi algorithm"
  :version "0.0.1"
  :author "Li Gong"
  :serial t
  :depends-on (#:cl-ppcre
	       #:arnesi)
  :components ((:file "package")
	       (:file "parameter")
	       (:file "utils")
	       (:file "lm")
	       (:file "segment")))

