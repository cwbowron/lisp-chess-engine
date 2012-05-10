;; This file is part of Boo's Chess Engine
;; Copyright 1999 By Christopher Bowron

; Book Opening routines

(provide 'bce-book)

(defun load-book (filename)
  (declare (optimize (speed 3)))
  (let ((new-list nil)
	(c 1) (j 0))
    (declare (type fixnum c j))
    (format t "Loading Book~%")
    (with-open-file (input filename :direction :input)
      (loop
        (incf j)
	(let ((move-str (read-line input nil nil))
	       (moves nil))
	   (when (equalp move-str nil)
	     (return nil))
	   (unless (equalp (schar move-str 0) #\#)
	     (setq moves (parse-move-string move-str))
	     ;;(format t "moves : ~a~%" moves)
	     (initialize)
	     (setq c *white*)
	     (dolist (str moves)
		     (let ((m (decode-user-move str c)))
		       (do-move m)
		       (setq c (opp c))))
	     (if (find *dummy-square* *game-move-list* :test #'eq
			   :key #'move-start)
		 (progn 
		   (format t "Book Ambigous : Line ~d~%" j)
		   (format t "~a~%" (nreverse (list-string *game-move-list*))))
	       (progn
		 (push (reverse (the list *game-move-list*)) new-list)
		 (push nil (car new-list))))
	     ))))
    (setq *book* new-list))
  (initialize)
  (format t "Book Loaded (~a openings)~%" (length *book*)))

;; From: stevelong@isomedia.com
;; from comp.lang.lisp newsgroup
(defun parse-move-string (str &key (token #\space))
  (do* ((p       (position token str)
                 (position token str))
        (strings (list (if p (subseq str 0 p) str))
                 (cons (if p (subseq str 0 p) str) strings)))
      ((not p) (remove-if
                #'(lambda(obj)(string= obj ""))
                (nreverse strings)))
    (setf str (subseq str (1+ p) (length str)))))


(defun bookp ()
  (not (null *book*)))

(defun book-opening ()
  (let ((i (length *book*)))
    (cadr (nth  (random i) *book*))))

(defun trim-book ()
  (let* ((m (car *game-move-list*)))
    ;;(format t "~a" m)
    (dolist (open *book*)
	    (if (equalp (cadr open) m)
		(setf (cdr open) (cddr open))
	      (setq *book* (delete open *book*))))
    (setq *book* (delete '(nil) *book* :test #'tree-equal))))
