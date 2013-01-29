(in-package :cl-chinese-segment)

(defun maximum (list &key (key #'identity))
  "Return maximum element of list based on key value"
  (loop
     with max-elt = (first list)
     with max-key = (funcall key max-elt)
     for elt in (rest list)
     for elt-key = (funcall key elt)
     do
       (when (> elt-key max-key)
	 (setf max-elt elt
	       max-key elt-key))
     finally (return max-elt)))

(defun memo (fn)
  "Return a memorized version of fn"
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (value present) (gethash args cache)
	  (if present
	      (values-list value)
	      (let ((result (multiple-value-list (apply fn args))))
		(setf (gethash args cache) result)
		(values-list result)))))))

(defun memorize (fn-name)
  "Given the function name, change it to a memorized function"
  (when (symbol-function fn-name)
    (setf (symbol-function fn-name)
	  (memo (symbol-function fn-name)))
    (symbol-function fn-name)))
  
(defmacro def-memo-fun (name args &rest body)
  "define a memorized version function"
  `(progn
     (defun ,name ,args ,@body)
     (memorize ',name)
     ',name))

(defun startswith (string prefix)
  "Is string starts with prefix?"
  (eql (search prefix string) 0))

(defun endswith (string suffix)
  "Is string ends with suffix?"
  (eql (search suffix string :from-end t)
       (- (length string) (length suffix))))

(defun position-not (obj seq &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'position obj seq :test-not (complement test-not) keyword-args)
      (apply #'position obj seq :test (complement test) keyword-args)))

(defun split-by-char (string char)
  (loop for i = (position-not char string) then (position-not char string :start (1+ j))
       as j = (position char string :start i)
       collect (subseq string i j)
       while j))

(defun split-by-space(string)
  (split-by-char string #\Space))

(defun join-string-list (string-list)
  "Concatenates the a list of string, seperated by space"
  (format nil "~{~A~^ ~}" string-list))

(defvar *debug-ids* '())

(defun debugon (&rest ids)
  (setf *debug-ids* (union *debug-ids* ids)))

(defun undebug (&rest ids)
  (if (null ids)
      (setf *debug-ids* '())
      (setf *debug-ids* (set-difference *debug-ids* ids))))

(defun dbg (id format-string &rest args)
  (when (member id *debug-ids*)
    (apply #'format t format-string args)))

