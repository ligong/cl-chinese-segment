(in-package :cl-chinese-segment)

;; Language Model

(defvar *language-model-pathname*
  (merge-pathnames (pathname "data/lm_sc.t3g.arpa")
		   (asdf:system-definition-pathname
		    (asdf:find-system :cl-chinese-segment))))

(assert (probe-file *language-model-pathname*))

(defun parse-lm(line model-name)
  "Parse datafile's line based on current model name, returns (new-model-name words probability)."

  (if (eql (char line 0) #\\)
      (cond ((startswith line "\\1-gram\\") (values 'unigram nil nil))
	    ((startswith line "\\2-gram\\") (values 'bigram nil nil))
	    ((startswith line "\\3-gram\\") (values 'trigram nil nil))
	    (t (format t "~&Ignore ~a~%" line)))
      (if model-name
	(let ((tokens (split-by-space line)))
	  (case model-name
	    (unigram (values model-name (nth 0 tokens) (arnesi:parse-float (nth 1 tokens))))
	    (bigram (values model-name (join-string-list (subseq tokens 0 2)) (arnesi:parse-float (nth 2 tokens))))
	    (trigram (values model-name (join-string-list (subseq tokens 0 3)) (arnesi:parse-float (nth 3 tokens))))))
	(values nil nil nil))))

(defun load-language-model(model-type)
  "Load and return the specified language model: 'unigram, 'bigram"

  (format t "~&Load language model ~a" model-type)
  (with-open-file (in *language-model-pathname*)
    (let ((model (make-hash-table :test 'equal)))
      (loop
	 with model-name = nil
	 for line = (read-line in nil)
	 for line-num = 0 then (incf line-num)
	 while line do
	   (if (= (mod line-num 10000) 0)
	       (format t "."))
	   (multiple-value-bind (new-model-name words probability) (parse-lm line model-name)
	     (cond ((null model-name) (when (eql new-model-name model-type)
				   (setf model-name new-model-name)))
		   ((eql model-name new-model-name) (setf (gethash words model) probability))
		   (t (return model))))
	 finally (return model))
      (format t "~&done~%")
      model)))

(defvar *unigram-model* (load-language-model 'unigram))  ; unigram is needed even by bigram
(defvar *bigram-model* (if (eql *language-model* 'bigram) (load-language-model 'bigram)))

(defvar *words-num* 10000000
  "number of tokens in corpus") ; don't know original corpus word number, guess it

(defun pdist (word &optional (missing-fn #'(lambda (k n) (declare (ignore k n)) (/ 1. *words-num*))))
  "A probability distribution estimated from counts in datafile"
  (or (gethash word *unigram-model*)
      (funcall missing-fn word *words-num*)))

(defun avoid-long-words (word n)
  "Estimate probability of unknown word"
  (/ 10.0 (* n (expt 10 (length word)))))

(defun pw (word)
  "Return probability of word in unigram language model"
  (pdist word #'avoid-long-words))

(defun cpw (word prev)
  "Return probability of word given its previous word, i.e. P(word | prev).
If no previous word, return probability in unigram language model"
  (if (null prev)
      (pw word)
      (multiple-value-bind (probability present)
	  (gethash (join-string-list (list prev word)) *bigram-model*)
	(if present
	    probability
	    (pw word)))))
      
  