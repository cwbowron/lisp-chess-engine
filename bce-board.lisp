;; This file is part of Boo's Chess Engine
;; Copyright 1999 By Christopher Bowron

;; board routines
(provide 'bce-board)
(proclaim fast)

(defvar *board*
  (make-array '(16 16) :element-type 'fixnum
	      :initial-element *empty*))

(proclaim '(type (simple-array fixnum (16 16)) *board*))
;;(proclaim '(type (simple-array chess-piece (16 16)) *board*))

(defun set-piece (file rank piece)
  (declare (type (integer 0 7) file rank)
	   (type chess-piece piece)
	   (inline))
  (setf (aref *board* (+ rank 4) (+ file 4)) piece))

(defun get-piece (file rank)
  (declare (type fixnum file rank)
	   (inline))
  (aref *board* (+ rank 4) (+ file 4)))

(defun offboardp (f r)
  (declare (type fixnum f r)
	   (inline)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (not (zerop
	(logior (logand f #xf8)
		(logand r #xf8)))))

(defun emptyp (f r)
  (declare (type fixnum f r)
	   (inline)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (eq (get-piece f r) *empty*))

;; get the oppenents color
;;(defun opp (c)
;;  (declare (type fixnum c)
;;	   (inline))
;;  (the fixnum (- c)))

;;;; what direction is forward for this color
;;(defun dir (c)
;;  (declare (type fixnum c)
;;	   (inline))
;;  c)

;; if white = 0 and black = 1
(defun dir (c)
  (declare (type fixnum c)
	   (inline))
  (if (= c *white*) 1 -1))

(defun opp (c)
  (declare (type fixnum c)
	   (inline))
  (logxor c 1))

(proclaim '(ftype (function (fixnum) fixnum) opp dir))

(defun opponentp (f r color)
  (declare (type fixnum f r color)
	   (inline)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (cond
   ((emptyp f r) nil)
   (t (= (chess-piece-color (get-piece f r)) (opp color)))))

(proclaim slow)

(defun clear-board ()
  (loopboard
   (set-piece f r *empty*))
  'Clear)

(defun print-board-big ()
  (fresh-line)
  (format t "    a   b   c   d   e   f   g   h  ~%")
  (format t "  +---+---+---+---+---+---+---+---+~%")
  (let ((last-moved *dummy-square*))
    (when (car *game-move-list*)
      (setq last-moved (move-end (car *game-move-list*))))
    (do ((r 7 (- r 1)))
	((< r 0) nil)
	(format t "~a |" (1+ r))
	(do ((f 0 (+ f 1)))
	    ((> f 7) nil)
	    (cond
	     ((and
	       (car *game-move-list*)
	       (= f (square-file last-moved))
	       (= r (square-rank last-moved))
	       *highlight*)
	      (format t "*~a*|" (rep (get-piece f r))))
	     (t
	      (format t " ~a |" (rep (get-piece f r))))))
	
	(format t " ~a~%" (1+ r))
	(format t "  +---+---+---+---+---+---+---+---+~%"))
    (format t "    a   b   c   d   e   f   g   h  ~%")))

;; setup board from robofics commands
(defun robofics-setup-board ()
  (let ((color *white*))
    (loop
     (let ((str (read-line)))
       (cond
	((string= str "#") (clear-board))
	((string= str "c") (setq color (opp color)))
	((string= str ".") (return 'done))
	(t (robofics-set-piece str color))))))
  (print-board))

(defun robofics-set-piece (str color)
  (let ((f (- (char-int (aref str 1)) (char-int #\a)))
	(r (- (char-int (aref str 2)) (char-int #\1)))
	(p (make-chess-piece
	    :piece (gethash (aref str 0) *piece-hash*)
	    :color color)))
    (format t "~a ~a ~a ~%" f r p)
    (set-piece f r p)))

;; board-setup could use robofics-setup-board
;; and send appropriate commands - e.g. Pa2 Pb2 etc
(defun setup-board ()
  (let ((wp (make-chess-piece :piece 1 :color *white*))
	(wn (make-chess-piece :piece 2 :color *white*))
	(wb (make-chess-piece :piece 3 :color *white*))
	(wr (make-chess-piece :piece 4 :color *white*))
	(wq (make-chess-piece :piece 5 :color *white*))
	(wk (make-chess-piece :piece 6 :color *white*))
	(bp (make-chess-piece :piece 1 :color *black*))
	(bn (make-chess-piece :piece 2 :color *black*))
	(bb (make-chess-piece :piece 3 :color *black*))
	(br (make-chess-piece :piece 4 :color *black*))
	(bq (make-chess-piece :piece 5 :color *black*))
	(bk (make-chess-piece :piece 6 :color *black*)))

    (clear-board)
    (set-piece filea 1 wp) (set-piece fileb 1 wp)
    (set-piece filec 1 wp) (set-piece filed 1 wp)
    (set-piece filee 1 wp) (set-piece filef 1 wp)
    (set-piece fileg 1 wp) (set-piece fileh 1 wp)
    (set-piece filea 0 wr) (set-piece fileh 0 wr)
    (set-piece fileb 0 wn) (set-piece fileg 0 wn)
    (set-piece filec 0 wb) (set-piece filef 0 wb)
    (set-piece filed 0 wq) (set-piece filee 0 wk)
    (set-piece filea 6 bp) (set-piece fileb 6 bp)
    (set-piece filec 6 bp) (set-piece filed 6 bp)
    (set-piece filee 6 bp) (set-piece filef 6 bp)
    (set-piece fileg 6 bp) (set-piece fileh 6 bp)
    (set-piece filea 7 br) (set-piece fileh 7 br)
    (set-piece fileb 7 bn) (set-piece fileg 7 bn)
    (set-piece filec 7 bb) (set-piece filef 7 bb)
    (set-piece filed 7 bq) (set-piece filee 7 bk)    
    "Board Setup"))
	

(defun print-board-small ()
  (fresh-line)
  (format t "   a b c d e f g h~%")
  (format t "  +-+-+-+-+-+-+-+-+~%")
  (do ((r 7 (1- r)))
      ((< r 0) nil)
      (format t "~a |" (1+ r))
      (dotimes (f 8)
	       (if (= *empty* (get-piece f r))
		   (format t "-|")
		 (format t "~a|" (rep (get-piece f r)))))
      (format t " ~a~%" (1+ r)))
  (format t "  +-+-+-+-+-+-+-+-+~%")
  (format t "   a b c d e f g h~%"))

(defun print-board-tiny ()
  (fresh-line)
  (format t "   abcdefgh~%")
  (do ((r 7 (1- r)))
      ((< r 0) nil)
      (format t "~a |" (1+ r))
      (dotimes (f 8)
	       (format t "~a" (rep (get-piece f r))))
      (format t "|~a~%" (1+ r)))
  (format t "   abcdefgh~%"))

(defun print-board ()
  (print-board-big))

		  

(defmacro loopboard (block)
  `(dotimes (f 8)
	    (declare (type fixnum f))
	    (dotimes (r 8)
		     (declare (Type fixnum r))
		     ,block)))

(defun save-board (file)
  (with-open-file (out file :direction :output)
    (dotimes (r 8)
       (dotimes (f 8)
	 (write-char (char (rep (get-piece f r)) 0) out)))
    (fresh-line out)
    (write *variables* :stream out)))

(defun load-board (file)
  (with-open-file (in file :direction :input)
    (dotimes (r 8)
       (dotimes (f 8)
	  (let ((char (read-char in)))
	    (set-piece f r (getvalue char)))))
    (setq *variables* (read in))))

(defun getvalue (c)
  (let ((v (gethash c *piece-hash*)))
    (cond
     ((null v) *empty*)
     ((upper-case-p c)	(make-chess-piece :color *WHITE* :piece v))
     (t (make-chess-piece :color *BLACK* :piece v)))))
