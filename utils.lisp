(in-package :make-grapher)

(defun string-replace (new old text)
  (let ((index (search old text)))
    (if (null index)
	text
	(concatenate 'string 
		     (subseq text 0 index) 
		     new 
		     (subseq text (+ index (length old)))))))



(defun split (string &key (char #\Space) (accept-empty nil))
    "Returns a list of substrings of string
divided by the character given.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
       as j = (position char string :start i)
       as s = (subseq string i j)
       when (or accept-empty (not (eq 0 (length s)))) collect s
       while j))

(defun copy-hash-table (hash &key (test 'eql)) 
  (declare (hash-table hash))
  (let ((h (make-hash-table :test test)))
    (maphash #'(lambda (key x)
		(setf (gethash key h) x))
	     hash)
    h))

(defmacro with-syms (syms &rest body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

(defmacro hashash (key hash)
  (with-syms (value entry-p)
    `(multiple-value-bind (,value ,entry-p) (gethash ,key ,hash)
       (declare (ignore ,value))
       ,entry-p)))
		

(defmacro hash-table-update! (key hash var &rest body)
  (with-syms (k h)
    `(let ((,k ,key)
	   (,h ,hash))
       (setf (gethash ,k ,h)
	     (let ((,var (gethash ,k ,h)))
		,@body)))))

(defmacro when-bind ((var test) &body body)
  `(let ((,var ,test))
     (when ,var
       ,@body)))

(defmacro if-bind ((var condition) then &optional else)
  (etypecase var
    (cons `(multiple-value-bind ,var ,condition
             (if ,(car var) ,then ,else)))
    (symbol `(let ((,var ,condition)) 
               (if ,var ,then ,else)))))


(defmacro hash-table-update!/default (key hash var default &rest body)
  (with-syms (k h d)
    `(let ((,k ,key)
	   (,h ,hash)
	   (,d ,default))
       (if (not (nth-value 1 (gethash ,k ,h)))
	   (setf (gethash ,k ,h) ,d))
       (setf (gethash ,k ,h) 
	     (let ((,var (gethash ,k ,h)))
		,@body)))))

(defun hash-table-set-if-no-value (key hash default)
  (hash-table-update!/default key hash value default
			      value))

(defun hash-table-ref/default (key hash default)
  (if (not (nth-value 1 (gethash key hash)))
      (setf (gethash key hash) default)
    (gethash key hash)))

(defun hash-values (hash)
  (let ((values nil))
    (with-hash-table-iterator
     (my-iterator hash)
     (loop
      (multiple-value-bind
          (entry-p key value)
          (my-iterator)
        (declare (ignore key))
        (if entry-p
            (setf values (cons value values))
          (return)))))
    values))

(defun hash->alist (hash)
  (let ((values nil))
    (with-hash-table-iterator
     (my-iterator hash)
     (loop
      (multiple-value-bind
          (entry-p key value)
          (my-iterator)
        (if entry-p
            (setf values (cons (cons key value) values))
          (return)))))
    values))

(defun hash-keys (hash)
  (let ((keys nil))
    (with-hash-table-iterator
     (my-iterator hash)
     (loop
      (multiple-value-bind 
          (entry-p key value)
          (my-iterator)
        (declare (ignore value))
        (if entry-p
            (setf keys (cons key keys))
          (return)))))
    keys))


(defun equal-set (rhs lhs)
  (and (eql (list-length lhs)
	    (list-length rhs))
       (reduce (lambda (ok node)
		 (if ok
		     (not (null (member node rhs :test 'equal)))))
	       lhs
	       :initial-value t)))

(defun uniqueness-set (set)
  (if (null set)
      nil
    (if (member (car set) (cdr set))
	(uniqueness-set (cdr set))
      (cons (car set) (uniqueness-set (cdr set))))))
	    

(defun generate-name (index)
  (format nil "q~A" index))

;; (defun for-each-line-in-file (file func)
;;   (declare (function func))
;;   (with-open-file (p ,file :direction :input)
;; 		  (do ((line (read-line p nil 'eof)
;; 			     (read-line p nil 'eof)))
;; 		      ((eql line 'eof))
;; 		      (funcall func line))))


(defmacro for-each-line-in-file ((var file) &body body)
  (with-syms (stream)
    `(with-open-file (,stream ,file :direction :input)
       (do ((,var (read-line ,stream nil 'eof) (read-line ,stream nil 'eof)))
	   ((eql ,var 'eof))
	 ,@body))))

(defmacro for-each-line-in-stream ((var stream) &body body)
  (with-syms (s)
    `(let ((,s ,stream))
       (do ((,var (read-line ,s nil 'eof) (read-line ,s nil 'eof)))
	   ((eql ,var 'eof))
	 ,@body))))

		      
(defmacro vector-walk ((index value vector) &rest body)
  (with-syms (vec)
    `(let ((,vec ,vector))
       (dotimes (,index (array-dimension ,vec 0) nil)
	 (let ((,value (aref ,vec ,index)))
	   ,@body)))))

