;; This file is part of Boo's Chess Engine
;; Copyright 1999 By Christopher Bowron

;; do / undo moves and helpers

(provide 'bce-do-move)

(proclaim fast)

(defun save-variables ()
  (declare (inline))
  (push *variables* *var-stack*))

(defun restore-variables ()
  (declare (inline))
  (setq *variables* (pop *var-stack*)))

(proclaim '(ftype (function () list) save-variables))
(proclaim '(ftype (function () fixnum) restore-variables))

(defun do-move (m)
  (declare (type move m))
  (let* ((start (move-start m))
	 (end (move-end m))
	 (piece (get-piece (square-file start) (square-rank start)))
	 (captured (get-piece (square-file end) (square-rank end)))
	 (undo (make-undo-type m piece captured))
	 (v (chess-piece-piece piece))
	 (c (chess-piece-color piece)))
    (declare (type square start end)
	     (type chess-piece piece captured)
	     (type undo-type undo)
	     (type fixnum c v))
    
    (save-variables)
    (push m *game-move-list*)
    
    (reset-en-passanted)
    (reset-double-push)
    (reset-castled)
    (reset-capture)

    (unless (= captured *empty*)
      (set-capture))
    
    (set-piece (square-file end) (square-rank end) piece)
    (set-piece (square-file start) (square-rank start) *empty*)

    ;; update material on the board
    (update-material end captured nil)
    (update-material-move m piece)

    (case (square-rank start)
	  (7
	  ;; check for black castle
 	   (when (and (= c *black*)
		      (= v king-value)
		      (move-equal m *black-queen-side-castle*))
	     (set-castled)
	     (set-piece filed 7 (get-piece filea 7))
	     (set-piece filea 7 *empty*))
	   (when  (and (= c *black*)
		       (= v king-value)
		       (move-equal m *black-king-side-castle*))
	     (set-castled)
	     (set-piece filef 7 (get-piece fileh 7))
	     (set-piece fileh 7 *empty*))
	   ;; turn of castling if rook moved
	   (when (= (square-file start) filea)
	     (reset-b-q-castle))
	   (when (= (square-file start) fileh)
	     (reset-b-k-castle)))
	  (6
	   ;; check for black double-push
	   (if (= c *black*)
	       (when (and (= v pawn-value)
			  (= (square-rank end) 4))
		 (set-double-push))
	     ;; white promotion
	     (when (= v pawn-value)
	       (set-piece (square-file end) (square-rank end)
			  (get-promotion (chess-piece-color piece))))))
	  (1
	   ;; check for white double-push
	   (if (= c *white*)
	       (when (and (= v pawn-value)
			  (= (square-rank end) 3))
		 (set-double-push))
	     ;; black promotion
	     (when (= v pawn-value)
	       (set-piece (square-file end) (square-rank end)
			  (get-promotion (chess-piece-color piece))))))

	  (0
	   ;; white castling
	   (when (and (= c *white*)
		      (= v king-value)
		      (move-equal m *white-queen-side-castle*))
	     (set-castled)
	     (set-piece filed 0 (get-piece filea 0))
	     (set-piece filea 0 *empty*))
	   (when (and (= c *white*)
		      (= v king-value)
		      (move-equal m *white-king-side-castle*))
	     (set-castled)
	     (set-piece filef 0 (get-piece fileh 0))
	     (set-piece fileh 0 *empty*))
	   ;; turn of castling if rook moved
	   (when (= (square-file start) filea)
	     (reset-w-q-castle))
	   (when (= (square-file start) fileh)
	     (reset-w-k-castle))))

    (case (square-rank end)
	  (0
	   ;; turn of castling if rook captured
	   (when (= (square-file end) filea)
	     (reset-w-q-castle))
	   (when (= (square-file end) fileh)
	     (reset-w-k-castle)))
	  (7
	   ;; turn of castling if rook captured
	   (when (= (square-file end) filea)
	     (reset-b-q-castle))
	   (when (= (square-file end) fileh)
	     (reset-b-k-castle))))
    
    (case v
	  (1 
	   ;; did we enpassant?
	   (when (and 
		  (not (= (square-file start) (square-file end)))
		  (eq captured *empty*))
	     (set-piece (square-file end)
			(- (square-rank end)
			   (dir (chess-piece-color piece)))
			*empty*)
	     (set-en-passanted)))
	       
	  (6 
	   ;; keep track of kings
	   (if (= c *white*)
	       (progn
		 (setq *wk* end)
		 (reset-w-k-castle)
		 (reset-w-q-castle))
	     (progn
	       (setq *bk* end)
	       (reset-b-k-castle)
	       (reset-b-q-castle)))))
    
    undo))

(proclaim '(ftype (function (move) undo-type) do-move))

(defun undo-move (undo)
  (declare (type undo-type undo))
  (let* ((m (undo-type-move undo))
	 (moved (undo-type-moved undo))
	 (captured (undo-type-captured undo))
	 (end (move-end m))
	 (start (move-start m)))
    
    (declare (type move m)
	     (type chess-piece moved captured)
	     (type square end start))
    
    (set-piece (square-file start) (square-rank start) moved)
    (set-piece (square-file end) (square-rank end) captured)
    
    ;; update material on the board
    (update-material end captured t)
    (update-material-move (move-flip m) moved)
    
    (cond
     ;; restore pawn on en-passante
     ((en-passanted-p)
      (let* ((c (chess-piece-color moved)))
	(set-piece (square-file (move-end m))
		   (square-rank (move-start m))
		   (make-chess-piece :color (opp c) :piece pawn-value))))

     ;; restore castling
     ((castled-p)
      (cond
       ((move-equal *white-queen-side-castle* m)
	(set-piece filea 0 (make-chess-piece :color *white* :piece rook-value))
	(set-piece filed 0 *empty*)
	(setq *wk* (make-square filee rank1)))
       ((move-equal *white-king-side-castle* m)
	(set-piece fileh 0 (make-chess-piece :color *white* :piece rook-value))
	(set-piece filef 0 *empty*)
	(setq *wk* (make-square filee rank1)))
       ((move-equal *black-queen-side-castle* m)
	(set-piece filea 7 (make-chess-piece :color *black* :piece rook-value))
	(set-piece filed 7 *empty*)
	(setq *bk* (make-square filee rank8)))
       ((move-equal *black-king-side-castle* m)
	;; should be same as (get-piece file f 7)
	(set-piece fileh 7 (make-chess-piece :color *black* :piece rook-value))
	(set-piece filef 7 *empty*)
	(setq *bk* (make-square filee rank8)))))

     ;; keep track of kings
     ((= (chess-piece-piece moved) king-value)
      (if (= (chess-piece-color moved) *white*)
	  (setq *wk* (move-start m))
	(setq *bk* (move-start m))))))
  
  (restore-variables)
  (pop *game-move-list*))

(proclaim '(ftype (function (undo-type) move) undo-move))

(defun back-up (&optional (count 1))
  (declare (type fixnum count))  
  (let ((m *dummy-move*))
    (dotimes (c count)
	     (declare (type fixnum c))
	     (setq m (undo-move (pop *game-undo-list*)))
	     (push m *back-up-list*)
	     (print-board))
    m))

(defun forward (&optional (count 1))
  (declare (type fixnum count))  
  (dotimes (c count)
	   (declare (type fixnum c))
	   (push
	    (do-move (pop *back-up-list*))
	    *game-undo-list*)
	   (print-board)))


(defun update-material (square replaced add)
  (declare (type chess-piece replaced)
	   (type boolean add))
  (cond
   ((eql *empty* replaced) nil)
   (t
    (let* ((c (chess-piece-color replaced))
	   (v (chess-piece-piece replaced))
	   (index (color-index c))
	   (file (square-file square))
	   (rank (square-rank square))
	   (sv (square-value file rank replaced))
	   (v1 (aref piece-values v))
	   (total (the fixnum (+ sv v1))))
      (declare (type fixnum c v index file rank sv v1 total))
      (cond
       (add
	(incf (aref *material* index) total)
	;; update pawn structure
	(when (= v pawn-value)
	  (incf (the fixnum (aref
			     (the pawn-struct (aref *pawns* index)) file)))))
       (t
	(decf (aref *material* index) total)
	;; update pawn structure	  
	(when (= v pawn-value)
	  (decf (the fixnum (aref
			     (the pawn-struct (aref *pawns* index))
				  file))))))))))


(defun update-material-move (move piece)
  (declare (type chess-piece piece)
	   (type move move))
  (cond
   ((eq *empty* piece) nil)
   (t
    (let* ((index (color-index (chess-piece-color piece)))
	   (s (move-start move))
	   (e (move-end move))
	   (sf (square-file s))
	   (sr (square-rank s))
	   (ef (square-file e))
	   (er (square-rank e))
	   (v1 (square-value sf sr piece))
	   (v2 (square-value ef er piece))
	   (diff (- v1 v2)))
      (incf (aref *material* index) diff)
      ;; update our pawn structure
      (when (= (chess-piece-piece piece) pawn-value)
	(decf (the fixnum
		   (aref (the pawn-struct (aref *pawns* index)) sf)))
	(incf (the fixnum
		   (aref (the pawn-struct (aref *pawns* index)) ef))))))))

	   
	      
      
      
	   