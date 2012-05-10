;; This file is part of Boo's Chess Engine
;; Copyright 1999 By Christopher Bowron

;; user interaction functions

;; Boo's Chess Engine

(provide 'bce-user-moves)

;; get a move from user (or robofics)
(defun get-user-move (color)
  (loop
   (let* ((str (read-line nil nil nil))
	  (command (user-command str color)))
     (cond
      ((null str) *dummy-move*)		;; no input
      ((eq command 'exit)
       (return t))
      ((not command)
       (return
	(decode-user-move str color)))))))

(defun decode-alg-square (str)
  (let ((f (- (char-int (aref str 0)) (char-int #\a)))
	(r (- (char-int (aref str 1)) (char-int #\1))))
    (make-square f  r)))

;; parse user commands (backup, exit, etc)
(defun user-command (str color)
  (cond
   ((or
     (string-equal str "exit")
     (string-equal str "quit"))
    'exit)
   ((string-equal str "redraw")
    (print-board)
    t)
   ((string-equal str "big")
    (defun print-board ()
      (print-board-big))
    (print-board)    
    t)
   ((string-equal str "small")
    (defun print-board ()
      (print-board-small))
    (print-board)    
    t)
   ((string-equal str "tiny")
    (defun print-board ()
      (print-board-tiny))
    (print-board)
    t)
   ((string-equal str "save")
    (save-board "bce.saved")
    t)
   ((string-equal str "load")
    (load-board "bce.saved")
    (setq *game-move-list* nil)
    (print-board)
    t)
   ((string-equal str "reset")
    (start-up)
    (print-board)
    t)
   ((and (>= (length str) 6)
	 (string-equal (subseq str 0 6) "backup"))
    (let ((count 2))
      (when (> (length str) 6)
	(setq count (parse-integer (subseq str 6))))
      (back-up count))
    t)
   ((and (>= (length str) 7)
	 (string-equal (subseq str 0 7) "forward"))
    (let ((count 2))
      (when (> (length str) 7)
	(setq count (parse-integer (subseq str 7))))
      (forward count))
    t)
   ((string-equal str "suggest")
    (setq *computer* t)
    (bce-search *lose* *win* color *search-depth*)
    (setq *computer* nil)
    (format t "Suggested Move: ~a~%" (move-string
				      (get-saved-move *search-depth*)))
    t)
   ((string-equal str "moves")
    (format t "~a~%" (list-string (gen-def color)))
    t)
   ((string-equal str "b")
    (back-up 2))
   ((string-equal str "f")
    (forward 2))
   (t nil)))

;; find the starting square that can reach endsquare
(defun find-start-square (piece endsquare)
  (let ((candidates nil))
    (do ((f 7 (- f 1)))
	((< f 0))
	(do ((r 0 (+ r 1)))
	    ((> r 7))
	    (when (equalp (get-piece f r) piece)
	      (dolist (move (get-moves f r piece))
		      (when (equalp (move-end move) endsquare)
			(push (make-square f r) candidates))))))
    (if (= (length candidates) 1)
	(car candidates)
      (progn
	(when (> (length candidates) 1)
	  (format t "ambiguous move~%"))
	*dummy-square*))))

(defun find-start-rank (piece f endsquare)
  ;;(format t "finding rank for ~a ~a ~a~%" piece f endsquare)
  (let ((candidates nil))
    (dotimes (r 8)
	     (when (equalp (get-piece f r) piece)
	       (dolist (move (get-moves f r piece))
		       (when (equalp (move-end move) endsquare)
			 (push (make-square f r) candidates)))))
    ;;(format t "~a~%" candidates)
    (if (= (length candidates) 1)
	(car candidates)
      (progn
	(when (> (length candidates) 1)
	  (format t "ambiguous move~%"))
	*dummy-square*))))

(defun find-start-file (piece r endsquare)
  ;;(format t "finding file for ~a ~a ~a~%" piece r endsquare)
  (let ((candidates nil))
    (do ((f 7 (- f 1)))
	((< f 0))
	(when (equalp (get-piece f r) piece)
	  (dolist (move (get-moves f r piece))
		  (when (equalp (move-end move) endsquare)
		    (push (make-square f r) candidates)))))
    (if (= (length candidates) 1)
	(car candidates)
      (progn
	(when (> (length candidates) 1)
	  (format t "ambiguous move~%"))
	*dummy-square*))))

(defun find-square-smart (piece f r endsquare)
  (cond
   (f (find-start-rank piece f endsquare))
   (r (find-start-file piece r endsquare))
   (t *dummy-square*)))

;; forms to understand
;; a3 Na3 a2-a3 o-o o-o-o O-O O-O-O a8=Q a7-a8=Q Nbd7 N8d7 bxc6
;; todo:
;; BxN xN
(defun decode-user-move (str color)
  (let ((p (position #\= str)))
    (cond
     (p 
      (setq *choosen-piece* (make-chess-piece
			     :piece (gethash (aref str (1+ p)) *piece-hash*)
			     :color color))
      (format t "Decoding Promotion to ~a~%" (rep *choosen-piece*))
      (setq str (subseq str 0 p)))
     (t (setq *choosen-piece* *empty*))))
  
  (setq str (delete #\+ str))
  (setq str (delete #\x str))

  (when (and (= (length str) 3)
	     (not (equalp str "o-o"))
	     (not (equalp str "0-0"))		  
	     (not (gethash (aref str 0) *piece-hash*)))
    (setq str (concatenate 'string "P" str)))
  
  (cond
   ;; castling
   ((and (or (string-equal "o-o" str)
	     (string-equal "o" str)
	     (string= "0-0" str))
	 (= color *white*))
    *white-king-side-castle*)
   ((and (or (string-equal "o-o" str)
	     (string-equal "o" str)
	     (string= "0-0" str))
	 (= color *black*))
    *black-king-side-castle*)
   ((and (or (string-equal "o-o-o" str)
	     (string-equal "o-" str)
	     (string= "0-0-0" str))
	 (= color *white*))
    *white-queen-side-castle*)
   ((and (or (string-equal "o-o-o" str)
	     (string-equal "o-" str)	     
	     (string= "0-0-0" str))
	 (= color *black*))
    *black-queen-side-castle*)

   ;; format a7-a8
   ((= (length str) 5)
    (make-move  (decode-alg-square (subseq str 0 2))
	        (decode-alg-square (subseq str 3 5))))

   ;; format a7a8
   ((and (= (length str) 4) (valid-square-p
			     (decode-alg-square (subseq str 0 2))))
    (make-move  (decode-alg-square (subseq str 0 2))
		(decode-alg-square (subseq str 2 4))))
   ((= (length str) 4)
    ;; format Nbd7 N8d7
    (let ((end (decode-alg-square (subseq str 2 4)))
	  (piece (make-chess-piece :piece (gethash (aref str 0) *piece-hash*)
				   :color color)))
      (make-move 
       (find-square-smart piece
			  (position (char-downcase (char str 1)) "abcdefgh")
			  (position (char str 1) "12345678")
			  end)
       end)))
   ;; format Kd3
   ((= (length str) 3)
    (let ((m *dummy-move*)
	  (p (make-chess-piece
	      :piece (gethash (aref str 0) *piece-hash*)
	      :color color))
	  (end-square (decode-alg-square (subseq str 1 3))))
      (setq m (make-move  (find-start-square p end-square) end-square))
      ;; solve the problem of bc6 (pawn move not bishop)
      (let ((f (square-file end-square))
	    (r (square-rank end-square))
	    (dr (- (dir color))))
      (cond
       ((and (char= (char str 0) #\b)
	     (= (+ f 1) fileb)
	     (= (chess-piece-piece (get-piece (+ f 1) (+ dr r))) pawn-value)
	     (= (chess-piece-color (get-piece (+ f 1) (+ dr r))) color))
	(make-move (make-square (+ f 1) (+ dr r)) end-square))
       ((and (char= (char str 0) #\b)
	     (= (- f 1) fileb)	     
	     (= (chess-piece-piece (get-piece (- f 1) (+ dr r))) pawn-value)
	     (= (chess-piece-color (get-piece (- f 1) (+ dr r))) color))
	(make-move (make-square (- f 1) (+ dr r)) end-square))
       (t m)))))
   ;; format a8
   ((= (length str) 2)
    (let ((p (make-chess-piece
	      :piece 1
	      :color color))
	  (end-square (decode-alg-square str)))
      (make-move  (find-start-square p end-square)
		  end-square)))
   (t (make-move  *dummy-square*  *dummy-square*))))
  
;; is the users move valid?
(defun validp (move color)
  (find move (gen-def color) :test #'equalp))

(defun valid-square-p (square)
  (not (offboardp (square-file square) (square-rank square))))

(defun get-promotion (color)
  (cond
   (*computer*
    (setq *choosen-piece* *empty*)
    (make-chess-piece :color color :piece queen-value))
   ((not (equalp (chess-piece-piece *choosen-piece*) *empty*))
    *choosen-piece*)
   (t
    (fresh-line)
    (format t "Choose Piece (QRBN):")
    (force-output)
    (let ((str (read-line)))
      (make-chess-piece :piece (gethash (char str 0) *piece-hash*)
			:color color)))))
  
