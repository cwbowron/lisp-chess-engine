;; This file is part of Boo's Chess Engine
;; Copyright 1999 By Christopher Bowron

;; Boo's Chess Engine AI routines

(provide 'bce-ai)

(proclaim fast)

(defconstant *ai-array-size* 10)
(defvar *ai-moves*
  (make-array *ai-array-size* :initial-element *dummy-move*
	      :element-type 'move))

(deftype ai-move-type ()
  (type-of *ai-moves*))

(proclaim '(type ai-move-type *ai-moves*))

(defun save-move (m depth)
  (declare (type move m)
	   (type fixnum depth))
  (when (= depth *search-depth*)
    (format t "saving ~a @ depth ~a~%" (move-string m) depth))
  (setf (aref *ai-moves* depth) m))

(defun get-saved-move (depth)
  (declare (type fixnum depth))
  (aref *ai-moves* depth))

(defun computer-move (color depth)
  (setq *computer* t)
  (let ((start-time (get-internal-real-time))
	(advantage 0)
	(m *dummy-move*))
    (declare (type fixnum advantage)
	     (type move m))
    (if (bookp)
	(progn
	  (format t "Book Opening~%")
	  (setq advantage 0)
	  (setq m (book-opening)))
      (progn 	
	(setq advantage (bce-search *lose* *win* color depth))
	(setq m (get-saved-move depth))))
    
    (cond
     ((= m *dummy-move*)
      (cond
       ((incheckp color) -1)
       (t -2)))
     (t 
      (let* ((start (move-start m))
	     (end (move-end m))
	     (p (get-piece (square-file start) (square-rank start))))
	
	(push (do-move m) *game-undo-list*)
	(fresh-line)
	(format t "move ~a~A~%" (move-string m)
		(cond
		 ((and (= (chess-piece-piece p) pawn-value)
		       (or (= (square-rank end) rank8)
			   (= (square-rank end) rank1)))
		  "=Q")
		 (t "")))
	(format t "BCE search : ~a~%" advantage)
	(- (get-internal-real-time) start-time))))))


;; search routines
(proclaim '(ftype (function (fixnum fixnum fixnum fixnum) fixnum)
		  ab-search q-search))

(defvar minq *q-depth*)
(declaim (type fixnum minq))

;; quiesent search
(defun q-search (alpha beta color depth)
  (declare (type fixnum alpha beta color depth))

  (when (< depth minq)
    (setq minq depth))
  (cond
   ((<= depth 0) (evaluate color))
   (t 
    (let ((moves (gen-moves color))
	  (searched nil)
	  (incheck (incheckp color)))
      (declare (type boolean searched incheck))

      (when (and (not (capture-p))
		 (not incheck))
	(return-from q-search
		     (evaluate color)))
      
      (loop
       (when (endp moves) (return nil))
       (when (>= alpha beta) (return t))
       
       (let* ((m (pop moves))
	      (end (move-end m))
	      (f (square-file end))
	      (r (square-rank end)))
	 
	 (declare (type move m)
		  (type square end)
		  (type fixnum f r))

	 ;; continue search if in check or the move is a take
	 (when (or incheck
		   (not (emptyp f r)))
	   
	   (let ((undo (do-move m))
		 (intocheck nil)
		 (value 0))
	     (declare (type undo-type undo)
		      (type boolean intocheck)
		      (type fixnum value))
	   
	     (setq intocheck (incheckp color))
	 
	     (cond
	      (intocheck (setq value *lose*))
	      (t
	       (setq searched t)
	       (setq value (- (bce-search (- beta)
					  (- alpha)
					  (opp color)
					  (1- depth))))))
	     (undo-move undo)
	 
	     (when (> value alpha)
	       (setq alpha value))))))
      
      (if searched
	  alpha
	(evaluate color))))))

(defun bce-search (alpha beta color depth)
  (declare (type fixnum alpha beta color depth))
  (cond
   ((<= depth 0) (q-search alpha beta color *q-depth*))
   (t 
    (let ((moves (gen-moves color))
	  (searched nil))
      (declare (type boolean searched))
      
      (loop
       (when (endp moves) (return nil))
       (when (>= alpha beta) (return t))
       
       (let* ((m (pop moves))
	      (undo (do-move m))
	      (intocheck nil)
	      (value 0))
	 
	 (declare (type move m)
		  (type undo-type undo)
		  (type boolean intocheck)
		  (type fixnum value))
	 
	 (setq intocheck (incheckp color))
		
	 (cond
	  (intocheck (setq value *lose*))
	  (t
	   (setq searched t)
	   (setq value (- (bce-search (- beta)
				      (- alpha)
				      (opp color)
				      (1- depth))))))
	 (undo-move undo)
	 
	 (when (> value alpha)
	   (setq alpha value)
	   (save-move m depth))))
      
      (if searched
	  alpha
	(evaluate color))))))

;; static evaluation functions
(proclaim '(ftype (function (fixnum) fixnum) evaluate material mobility
		  doubled-pawns))

(proclaim '(ftype (function (file rank chess-piece) fixnum) square-value))

(defun evaluate (color)
  (declare (type fixnum color))
  (let ((result 0)
	(opp (opp color))
	(us 0)
	(them 0))
    (declare (type fixnum result opp us them))
    (setq us
	  (+ (material color)
	     (the fixnum (- (the fixnum
				 (* 8 (the fixnum (doubled-pawns color))))))
	     ))
    (setq them (+ (material opp)
		  (the fixnum (- (the fixnum
				      (* 8 (the fixnum (doubled-pawns opp))))))
		  ))
    (setq result (- us them))
    result))

(defun material (color)
  (declare (type fixnum color)
	   (inline))
  (let ((index (color-index color)))
    (declare (type fixnum index))
    (aref *material* index)))

(defun mobility (color)
  (declare (type fixnum color)
	   (inline))
  (let ((index (color-index color)))
    (declare (type fixnum index))
    (aref *mobility* index)))


(defun square-value (f r p)
  (declare (type fixnum f r)
	   (type chess-piece p))
  (let* ((array
	  (aref *square-values*
		(color-index (chess-piece-color p))))
	 (this-board (aref array (chess-piece-piece p))))
    (declare (type (simple-vector 7) array))
    (get-board this-board f r)))

(defun evaluate-2 (color)
  (declare (type fixnum color))
  (the fixnum
       (+ (* 8 (the (integer -32000 32000) (- (material color)
					      (material (opp color)))))
	  (the fixnum (- (mobility color) (mobility (opp color)))))))

(defun doubled-pawns (color)
  (declare (type fixnum color))
  (let ((pawn-vec (aref *pawns* (color-index color)))
	(doubled 0))
    (declare (type fixnum doubled)
	     (type pawn-struct pawn-vec))
    (dotimes (c 8)
	     (declare (type fixnum c))
	     (when (> (the fixnum (svref pawn-vec c)) 1)
	       (incf doubled)))
    doubled))
	    
(defun count-material ()
  (setf (aref *material* 0) 0)
  (setf (aref *material* 1) 0)
  (fill (the pawn-struct (aref *pawns* 0)) 0)
  (fill (the pawn-struct (aref *pawns* 1)) 0)
  (loopboard
   (let* ((p (get-piece f r)))
     (declare (type chess-piece p))
     (update-material (make-square f r) p t))))

;;(defun bce-search-2 (alpha beta color depth)
;;  (declare (type fixnum alpha beta color depth))
;;  (cond
;;   ((= depth 0) (q-search alpha beta color *q-depth*))
;;   (t 
;;    (let ((best *lose*) (value 0))
;;      (declare (type fixnum best value))
      
;;      (dolist (m (gen-moves color))
;;	      (declare (type move m))

;;	      (when (< best beta)
;;		(when (> best alpha)
;;		  (setq alpha best)))

;;	      (let ((undo (do-move m))
;;		    (check nil))
;;		(declare (type undo-type undo)
;;			 (type boolean check))

;;		(setq check (incheckp color))
		
;;		(unless check
;;		  (setq value
;;			(- (bce-search-2 (- beta)
;;					 (- alpha)
;;					 (opp color)
;;					 (1- depth)))))
;;		(undo-move undo)
	      
;;		(when (and (not check) (>= value best))
;;		  (setq best value)
;;		  (save-move m depth))))
;;      best))))

