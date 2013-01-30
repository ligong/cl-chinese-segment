(in-package :cl-chinese-segment)

(defparameter *language-model* 'bigram
  "Choose 'unigram or 'bigram as language model
bigram language model takes more memory and provide better results")

(defvar *language-model-pathname*
  (merge-pathnames (pathname "data/lm_sc.t3g.arpa")
		   (asdf:system-definition-pathname
		    (asdf:find-system :cl-chinese-segment)))
  "open-gram's language model pathname")
