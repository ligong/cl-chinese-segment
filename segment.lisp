(in-package #:cl-chinese-segment)

(defparameter +delimiter+
	     "(http://[a-zA-Z.]+?/)|[ ,，.。?？!！''@#\"‘“/:：]+|([a-zA-Z0-9]+)"
	     "delimiter splits text into word list")

(defun segment (text)
  "segment chinese text into word list"
  (loop
     for s in (cl-ppcre:split +delimiter+ text
			      :with-registers-p t :omit-unmatched-p t)
     unless (string= s "")
     if (cl-ppcre:scan "[a-zA-Z0-9]+" s)
       append (list s)
     else
       append (if (eql *language-model* 'unigram)
		  (segment1 s)
		  (segment2 s))))

(defun splits(text &optional (max-text-len 4))
  "Return a list of all possible (first-word . remain) pairs with
restriction that first-word's length <= max-text-len"
  (loop for i from 1 to (min (length text) max-text-len)
     collect (cons (subseq text 0 i) (subseq text i))))


(def-memo-fun segment2(text &optional prev)
  "Given prefix word, return (words log10(probability)). words is the best segment of text"
  (if (string= text "")
      (values nil 0.0)
      (let ((candidates (mapcar #'(lambda (lst)
				    (destructuring-bind (first . rem) lst
				      (multiple-value-bind (rem-words rem-probability) (segment2 rem first)
					(combine (log (cpw first prev) 10) first rem-probability rem-words))))
				(splits text))))
	(values-list (maximum candidates :key #'second)))))


(defun combine (first-probability first-word rem-probability rem-words)
  (list (cons first-word rem-words)
	(+ first-probability rem-probability)))
      
    
(defun segment-probability (&rest words)
  "debug utility"
  (loop with prev = nil
       and p = 0
       for word in words
       do
       (format t "~&P(~a|~a) = ~a, log10(P) = ~a" word prev (cpw word prev) (log (cpw word prev) 10))
       (incf p (log (cpw word prev) 10))
       (setf prev word)
       finally (format t "~&Total P = ~a" p)))
  
    
;; a simpler version 
(def-memo-fun segment1(text)
  "Return a list of words that is the best segmentation of text"
  
  (if (string= text "")
      '()
      (let ((candidates (mapcar #'(lambda(lst)
				    (destructuring-bind (first . rem) lst
				      (cons first (segment1 rem))))
				(splits text))))
	(maximum candidates :key #'pwords))))
  
(defun pwords(words)
  "The Naive Bayes probability of a sequence of words"
  (reduce #'* words :key #'pw))
	