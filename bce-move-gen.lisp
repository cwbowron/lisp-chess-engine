;; This file is part of Boo's Chess Engine
;; Copyright 1999 By Christopher Bowron

;; Move Generation

;; Boo's Chess Engine

;; Christopher Bowron - bowronch@cse.msu.edu

(provide 'bce-move-gen)

(proclaim fast)

(defun get-sliders (f r df dr color)
  (declare (type fixnum f r df dr color))
  (let ((move-list nil)
	(nf f)
	(nr r)
	(square1 (make-square  f  r))
	(opp (opp color)))
    (declare (type list move-list)
	     (type fixnum nf nr opp)
	     (type square square1))
    (loop
     (incf nf df)
     (incf nr dr)
     (if (offboardp nf nr)
	 (return move-list)
       (let* ((p (get-piece nf nr))
	      (c (chess-piece-color p)))
	 (cond 
	  ((eql p *empty*)
	   (push (make-move square1 (make-square nf nr)) move-list))
	  ((= c opp)
	   (return (push (make-move square1 (make-square nf nr)) move-list)))
	  ((= c color) (return move-list))))))))
		     
(defun gen-moves (color)
  (declare (type fixnum color))
  (let ((move-list nil))
    (declare (type list move-list))
    (dotimes (f 8)
	     (declare (type fixnum f))
	     (dotimes (r 8)
		      (declare (type fixnum r))
		      (let ((p (get-piece f r)))
			(declare (type chess-piece p))
			(when (= (chess-piece-color p) color)
			  (setq
			   move-list (nconc (get-moves f r p) move-list))))))
    
    (setf (aref *mobility* (color-index color))  (length move-list))
    move-list))

(defun gen-attacks (color)
  (declare (type fixnum color))
  (let ((move-list nil)
	(attacks nil))
    (declare (type list move-list attacks))
    (dotimes (f 8)
	     (declare (type fixnum f))
	     (dotimes (r 8)
		      (declare (type fixnum r))
		      (let ((p (get-piece f r)))
			(declare (type chess-piece p))
			(when (= (chess-piece-color p) color)
			  (setq
			   move-list (nconc (get-moves f r p) move-list))))))
    
    (dolist (m move-list)
	    (declare (type move m))
	    (let ((f (square-file (move-end m)))
		  (r (square-rank (move-end m))))
	      (declare (type fixnum f r))
	      (when (= (chess-piece-color (get-piece f r)) (opp color))
		(push m attacks))))
    attacks))

;; generate move to get out of check
(defun gen-def (color)
  (declare (type fixnum color))
  (let ((tmplist (gen-moves color))
	(movelist nil)
	(computer *computer*))
    (declare (type list tmplist movelist)
	     (type boolean computer))
    (setq *computer* t)
    (dolist (m tmplist)
	    (declare (type move m))
	    (let ((undo (do-move m)))
	      (declare (type undo-type undo))
	      (unless (incheckp color)
		(push m movelist))
	      (undo-move undo)))
    (setq *computer* computer)
    movelist))

(defun get-moves-p (f r p)
  (declare (type fixnum f r)
	   (type chess-piece p))
  (let* ((c (chess-piece-color p))
	 (loc (make-square f r))
	 (move-list nil)
	 (forwards 1)
	 (startr 0)
	 (r2 0))
    (declare (type fixnum c forwards startr r2)
	     (type square loc))

    (cond
     ((= c *white*) (setq startr 1))
     ((= c *black*) (setq startr 6)))
    
    (setq forwards (dir c))
    (setq r2 (+ r forwards))

    ;; diagonal attacks
    (when (opponentp (1+ f) r2 c)
      (push (make-move loc (make-square (1+ f) r2))
	    move-list))
    
    (when (opponentp (1- f) r2 c)
      (push (make-move loc (make-square (1- f) r2))
	    move-list))

    ;; forward moves
    (when (emptyp f r2)
      (push (make-move loc (make-square f r2)) move-list)
      ;; two moves at the start
      (incf r2 forwards)
      (when (and (eq r startr)
		 (emptyp f r2))
	(push (make-move loc (make-square f r2))
	      move-list)))

    ;; can we en passante
    (when (double-push-p)
      (let ((last (car *game-move-list*)))
	(declare (type move last))
	(when (and 
	       (= (square-rank (move-end last)) r)
	       (or (= (1+ f)
		      (square-file (move-start last)))
		   (= (1- f)
		      (square-file (move-start last)))))
	  (push (make-move loc
			   (make-square
			    (square-file
			     (move-start last))
			    (+ forwards (square-rank
					 (move-end last)))))
		move-list))))
    move-list))
      
(defun get-moves-n (f r p)
  (declare (type fixnum f r)
	   (type chess-piece p))
  (let ((c (chess-piece-color p))
	(move-list nil)
	(loc (make-square  f  r))
	(df #( 2 2 -2 -2 -1  1 -1 1))
	(dr #(-1 1 -1  1  2 -2 -2 2)))
    (dotimes (i (length df))
	     (let ((file (+ f (the fixnum (svref df i))))
		   (rank (+ r (the fixnum (svref dr i)))))
	       (declare (type fixnum file rank))
	       (unless
		   (or
		    (offboardp file rank)
		    (and
		     (not (eql (get-piece file rank) *empty*))
		     (= (chess-piece-color (get-piece file rank)) c)))
		 (push (make-move loc (make-square file rank)) move-list))))
    move-list))

(defun get-moves-b (f r p)
  (let ((c (chess-piece-color p)))
    (nconc
     (get-sliders f r  1  1 c)
     (get-sliders f r  1 -1 c)
     (get-sliders f r -1  1 c)
     (get-sliders f r -1 -1 c))))

(defun get-moves-r (f r p)
  (let ((c (chess-piece-color p)))
    (nconc
     (get-sliders f r  1  0 c)
     (get-sliders f r -1  0 c)
     (get-sliders f r  0  1 c)
     (get-sliders f r  0 -1 c))))

(defun get-moves-q (f r p)
  (nconc (get-moves-r f r p)
	 (get-moves-b f r p)))

(defun get-moves-k (f r p)
  (declare (type fixnum f r)
	   (type chess-piece p))
  (let ((c (chess-piece-color p))
	(move-list nil)
	(loc (make-square f r))
	(w-q-thru
	 (make-move (make-square filee rank1) (make-square filed rank1)))
	(w-k-thru
	 (make-move (make-square filee rank1) (make-square filef rank1)))
	(b-q-thru
	 (make-move (make-square filee rank8) (make-square filed rank8)))
	(b-k-thru
	 (make-move (make-square filee rank8) (make-square filef rank8))))
    (declare (type move w-q-thru w-k-thru b-q-thru b-k-thru))
    
    (do ((file (1- f) (1+ file)))
	((> file (1+ f)) nil)
	(declare (type fixnum file))
	(do ((rank (1- r) (1+ rank)))
	    ((> rank (1+ r)) nil)
	    (declare (type fixnum rank))
	    
	    (unless
		(or
		 (and (= file rank) (= rank 0))
		 (offboardp file rank)
		 (and
		  (not (eql (get-piece file rank) *empty*))
		  (= (chess-piece-color (get-piece file rank)) c)))
	      (push (make-move loc (make-square file rank)) move-list))))

    (if (= c *white*)
	(progn
	  (when (and
		 (w-q-castle-p)
		 (emptyp filed 0)
		 (emptyp filec 0)		  
		 (emptyp fileb 0)
		 (not (incheckp c))
		 (not (would-be-incheckp w-q-thru *white*))
		 )
	    (push *white-queen-side-castle* move-list))
	  (when (and
		 (w-k-castle-p)
		 (emptyp filef 0)
		 (emptyp fileg 0)
		 (not (incheckp c))
		 (not (would-be-incheckp w-k-thru *white*))
		 )
	    (push *white-king-side-castle* move-list)))
      (progn
	(when (and
	       (b-q-castle-p)
	       (emptyp filed 7)
	       (emptyp filec 7)		  
	       (emptyp fileb 7)
	       (not (incheckp c))
	       (not (would-be-incheckp b-q-thru *black*)))
	     (push *black-queen-side-castle* move-list))
	(when (and
	       (b-k-castle-p)
	       (emptyp filef 7)
	       (emptyp fileg 7)
	       (not (incheckp c))
	       (not (would-be-incheckp b-k-thru *black*)))
	  (push *black-king-side-castle* move-list))))
    move-list))

(defun get-moves (f r p)
  (case (chess-piece-piece p)
	(1 (get-moves-p f r p))
	(2 (get-moves-n f r p))
	(3 (get-moves-b f r p))
	(4 (get-moves-r f r p))
	(5 (get-moves-q f r p))
	(6 (get-moves-k f r p))))

(defun incheckp (color)
  (declare (type fixnum color))
  (let* ((king (if (= color *white*) *wk* *bk*))
	 (f (square-file king))
	 (r (square-rank king)))
    (declare (type square king)
	     (type fixnum f r))
    (or 
     (bishop-attacked-p f r  1  1 color)
     (bishop-attacked-p f r  1 -1 color)
     (bishop-attacked-p f r -1  1 color)
     (bishop-attacked-p f r -1 -1 color)
     (rook-attacked-p f r  0  1 color)
     (rook-attacked-p f r  0 -1 color)
     (rook-attacked-p f r  1  0 color)
     (rook-attacked-p f r -1  0 color)
     (knight-attacked-p f r color)
     (pawn-attacked-p f r color)
     (king-attacked-p f r color)
     )))

(defun king-attacked-p (f r color)
  (declare (type fixnum f r color))
  (let ((result nil))
    (do ((df -1 (1+ df)))
	((> df 1) result)
	(declare (type fixnum df))
	(do ((dr -1 (1+ dr)))
	    ((> dr 1) nil)
	    (declare (type fixnum dr))	    
	    (let* ((p (get-piece (+ f df) (+ r dr)))
		   (c (chess-piece-color p))
		   (v (chess-piece-piece p)))
	      (when (and (= c (opp color))
			 (= v king-value))
		(return-from king-attacked-p t)))))))
		    
  
(defun knight-attacked-p (f r color)
  (declare (type fixnum f r color))
  (let ((offsetf #( 2  2 -2 -2 1  1 -1 -1))
	(offsetr #(-1  1  1 -1 2 -2  2 -2))
	(opp (opp color)))
    (declare (type vector offsetf offsetr)
	     (type fixnum opp))
    (dotimes (i (length offsetr) nil)
	     (let* ((df (svref offsetf i))
		    (dr (svref offsetr i))
		    (p (get-piece (+ f df) (+ r dr))))
	       (declare (type fixnum df dr)
			(type chess-piece p))
	       (when (and
		      (= (chess-piece-color p) opp)
		      (= (chess-piece-piece p) knight-value))
		 (return t))))))
		 
(defun pawn-attacked-p (f r color)
  (declare (type fixnum f r color))
  (let* ((deltar color)
	 (p1 (get-piece (1- f) (+ r deltar)))
	 (p2 (get-piece (1+ f) (+ r deltar)))
	 (opp (opp color)))
    (declare (type fixnum deltar opp)
	     (type chess-piece p1 p2))
    (or (and (= (chess-piece-color p1) opp)
	     (= (chess-piece-piece p1) pawn-value))
	(and (= (chess-piece-color p2) opp)
	     (= (chess-piece-piece p2) pawn-value)))))

(defun bishop-attacked-p (f r df dr color)
  (declare (type fixnum f r df dr color))
  (let ((nf f)
	(nr r)
	(opp (opp color)))
    (declare (type fixnum nf nr opp))
    (loop
     (incf nf df)
     (incf nr dr)
     (if (offboardp nf nr)
	 (return nil)
       (let* ((p (get-piece nf nr))
	      (c (chess-piece-color p))
	      (v (chess-piece-piece p)))
	 (cond 
	  ((eq *empty* p) nil)		;do nothing
	  ((= c opp)
	   (return (or (= v bishop-value)	;bishop
		       (= v queen-value))))	;queen
	  ((= c color) (return nil))))))))

(defun rook-attacked-p (f r df dr color)
  (declare (type fixnum f r df dr color))
  (let ((nf f)
	(nr r)
	(opp (opp color)))
    (declare (type fixnum nf nr opp))
    (loop
     (incf nf df)
     (incf nr dr)
     (if (offboardp nf nr)
	 (return nil)
       (let* ((p (get-piece nf nr))
	      (c (chess-piece-color p))
	      (v (chess-piece-piece p)))
	 (cond
	  ((eq *empty* p) nil)		;do nothing
	  ((= c opp)
	   (return (or (= v rook-value)		;rook
		       (= v queen-value))))	;queen
	  ((= c color) (return nil))))))))


(defun game-over-p (color)
  (let ((check (incheckp color))
	(has-move (can-move-p color)))
    (cond
     ((and (not has-move) check) 'Check-Mate)
     ((not has-move) 'Stale-Mate)
     (t nil))))
    

(defun can-move-p (color)
  (declare (type fixnum color))
  (let ((result nil)
	(computer *computer*))
    (declare (type (or t nil) result)
	     (type boolean computer))
    (setq *computer* t)
    (dotimes (f 8)
	     (declare (type fixnum f))
	     (dotimes (r 8)
		      (declare (type fixnum r))
		      (let ((p (get-piece f r)))
			(declare (type chess-piece p))
			(when (= (chess-piece-color p) color)
			  (dolist (m (get-moves f r p))
				  (let ((undo (do-move m)))
				    (when (not (incheckp color))
				      (undo-move undo)
				      (return (setq result t)))
				    (undo-move undo)))))))
    (setq *computer* computer)
    result))
  
(defun would-be-incheckp (m c)
  (declare (type move m)
	   (type fixnum c))
  (let ((computer *computer*))
    (setq *computer* t)
    (let ((undo (do-move m))
	  (result nil))
      (declare (type undo-type undo)
	       (type boolean result))
      (setq result (incheckp c))
      (undo-move undo)
      (setq *computer* computer)
      result)))
    