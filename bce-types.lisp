;; This file is part of Boo's Chess Engine
;; Copyright 1999 By Christopher Bowron

;; Data Types and helpers

;; Boo's Chess Engine

(provide 'bce-types)
(proclaim fast)

(deftype boolean ()
  '(member t nil))

(defun rep (piece)
  (declare (type chess-piece piece))
  (let ((v (chess-piece-piece piece))
	(c (chess-piece-color piece))
	(w-pieces (list " " "P" "N" "B" "R" "Q" "K"))
	(b-pieces (list " " "p" "n" "b" "r" "q" "k")))
    (declare (type fixnum v c))
    (cond
     ((= c *white*) (nth v w-pieces))
     ((= c *black*) (nth v b-pieces))
     (t " "))))

(deftype chess-piece ()
  `(integer 0 15))

(defun make-chess-piece (&key
			 (piece 0)
			 (color 0))
  (declare
   (inline)
   (type (integer 0 6) piece)
   (type (integer 0 1) color))
  (logior (ash piece 1) color))

(defun chess-piece-piece (p)
  (declare (type chess-piece p)
	   (inline))
  (ash p -1))

(defun chess-piece-color (p)
  (declare (type chess-piece p)
	   (inline))
  (logand 1 p))

(defun color-index (color)
  (declare (type fixnum color)
	   (inline)
	   (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  ;;(if (= color *white*) 0 1))
  color)


;;(defun make-chess-piece (&key
;;			 (piece 0)
;;			 (color 0))
;;  (declare
;;   (inline)
;;   (type (integer 0 6) piece)
;;   (type (integer -1 1) color))
;;  (if (= color *black*)
;;      (logior piece 8)
;;    piece))

;;(defun chess-piece-piece (p)
;;  (declare (type chess-piece p)
;;	   (inline))
;;  (logand p (lognot 8)))
  
;;(defun chess-piece-color (p)
;;  (declare (type chess-piece p)
;;	   (inline))
;;  (cond
;;   ((zerop p) 0)
;;   ((logtest p 8) *black*)
;;   (t *white*)))

(proclaim '(ftype (function (chess-piece) (integer 0 15)) chess-piece-piece))
(proclaim '(ftype (function (chess-piece) (integer -1 1)) chess-piece-color))

(deftype file ()
  `(integer 0 15))
(deftype rank ()
  `(integer 0 15))
(deftype square ()
  `(integer 0 255))

(defun make-square (file rank)
  (declare (type file file)
	   (type rank rank)
	   (inline))
  (the square (logior (ash file 4) rank)))

(defun square-file (square)
  (declare (type square square))
  (ash square -4))

(defun square-rank (square)
  (declare (type square square)
	   (inline))
  (logand square 15))

(deftype move ()
  `(integer 0 ,(1- (expt 2 16))))

(defun make-move (start end)
  (declare (type square start end)
	   (inline))
  (the move (logior (ash start 8) end)))

(defun move-start (move)
  (declare (type move move)
	   (inline))
  (the square (ash move -8)))

(defun move-end (move)
  (declare (type move move)
	   (inline))
  (the square (logand move 255)))

;; flip end and start in a move
(defun move-flip (move)
  (declare (type move move)
	   (inline))
  (make-move (move-end move) (move-start move)))
	      
(proclaim '(ftype (function (move) move) move-flip))

(defun move-equal (m1 m2)
  (declare (inline))
  (eql m1 m2))

(defun square-equal (s1 s2)
  (declare (inline))
  (eql s1 s2))

(proclaim '(ftype (function (move move) boolean) move-equal))
(proclaim '(ftype (function (square square) boolean) square-equal))


(defun square-string (sq)
  (declare (type square sq))
  (let ((alpha-strings `(#\a #\b #\c #\d #\e #\f #\g #\h #\i)))
    (format nil "~a~a" (nth (square-file sq) alpha-strings)
	    (1+ (square-rank sq)))))

(defun list-string (list)
  (map 'list #'move-string list))

(defun print-game (list)
  (declare (type list list))
  (setq list (reverse list))
  (setup-board)
  (let ((c *white*)
	(count 1))
    (declare (type fixnum c count))
    (do* ((l list (cdr l))
	  (m (car l) (car l)))
	 ((endp l) t)
	 (format t "~a~2d : ~a~a  " (if (= c *white*) "W" "B") count
		 (move-string m)
		 (cond
		  ((eq (game-over-p (opp c)) 'Check-Mate) "++")
		  ((incheckp (opp c)) "+ ")
		  (t "  ")))
	 (do-move m)
	 (when (= c *black*)
	   (fresh-line)
	   (incf count))
	 (setq c (opp c))))
  (fresh-line)
  (print-board))
	 
(defun move-string (m)
  (declare (type move m))
  (format nil "~a-~a"
	  (square-string (move-start m))
	  (square-string (move-end m))))


(deftype undo-type ()
  ;;  4 bits moved
  ;;  4 bits captured
  ;; 16 bits move = 8 bits per square = 4 bits per rank/file
  ;; --
  ;; 24 bits total = fits into a cmucl fixnum ... yay!
  `(integer 0 ,(1- (expt 2 24))))

(defun make-undo-type (move moved captured)
  (declare (type chess-piece captured moved)
	   (type move move))
  (the undo-type
       (logior (ash captured 20)
	       (ash moved 16)
	       move)))

(defun undo-type-move (undo)
  (declare (type undo-type undo))
  (the move (logand (- (expt 2 16) 1) undo)))

(defun undo-type-moved (undo)
  (declare (type undo-type undo))
  (the chess-piece
       (logand (ash undo -16) 15)))
       
(defun undo-type-captured (undo)
  (declare (type undo-type undo))
  (the chess-piece (ash undo -20)))

(defconstant w-q-c (expt 2 0))
(defconstant w-k-c (expt 2 1))
(defconstant b-q-c (expt 2 2))
(defconstant b-k-c (expt 2 3))
(defconstant en-p  (expt 2 4))
(defconstant dbl-p (expt 2 5))
(defconstant cast  (expt 2 6))
(defconstant cap   (expt 2 7))

(defun make-var-type ()
  (logior w-q-c w-k-c b-q-c b-k-c))

(deftype var-type ()
  (type-of (make-var-type)))

(defun w-q-castle-p ()
  (declare (inline))
  (logtest *variables* w-q-c))
(defun w-k-castle-p ()
  (declare (inline))
  (logtest *variables* w-k-c))
(defun b-q-castle-p ()
  (declare (inline))
  (logtest *variables* b-q-c))
(defun b-k-castle-p ()
  (declare (inline))
  (logtest *variables* b-k-c))
(defun en-passanted-p ()
  (declare (inline))
  (logtest *variables* en-p))
(defun double-push-p ()
  (declare (inline))
  (logtest *variables* dbl-p))
(defun castled-p ()
  (declare (inline))
  (logtest *variables* cast))
(defun capture-p ()
  (declare (inline))
  (logtest *variables* cap))

(proclaim '(ftype (function () boolean) w-q-castle-p w-k-castle-p
		  b-q-castle-p b-k-castle-p en-passanted-p double-push-p
		  castled-p capture-p))

(defun set-w-q-castle ()
  (declare (inline))
  (setq *variables* (logior *variables* w-q-c)))
(defun set-w-k-castle ()
  (declare (inline))
  (setq *variables* (logior *variables* w-k-c)))
(defun set-b-q-castle ()
  (declare (inline))
  (setq *variables* (logior *variables* b-q-c)))
(defun set-b-k-castle ()
  (declare (inline))
  (setq *variables* (logior *variables* b-k-c)))
(defun set-en-passanted ()
  (declare (inline))
  (setq *variables* (logior *variables* en-p)))
(defun set-double-push ()
  (declare (inline))
  (setq *variables* (logior *variables* dbl-p)))
(defun set-castled ()
  (declare (inline))
  (setq *variables* (logior *variables* cast)))
(defun set-capture ()
  (declare (inline))
  (setq *variables* (logior *variables* cap)))

(defun reset-w-q-castle ()
  (declare (inline))
  (setq *variables* (logand *variables* (lognot w-q-c))))
(defun reset-w-k-castle ()
  (declare (inline))
  (setq *variables* (logand *variables* (lognot w-k-c))))
(defun reset-b-q-castle ()
  (declare (inline))
  (setq *variables* (logand *variables* (lognot b-q-c))))
(defun reset-b-k-castle ()
  (declare (inline))
  (setq *variables* (logand *variables* (lognot b-k-c))))
(defun reset-en-passanted ()
  (declare (inline))
  (setq *variables* (logand *variables* (lognot en-p))))
(defun reset-double-push ()
  (declare (inline))
  (setq *variables* (logand *variables* (lognot dbl-p))))
(defun reset-castled ()
  (declare (inline))
  (setq *variables* (logand *variables* (lognot cast))))
(defun reset-capture ()
  (declare (inline))
  (setq *variables* (logand *variables* (lognot cap))))


(proclaim '(ftype (function () var-type) 
		  set-w-q-castle
		  set-w-k-castle
		  set-b-q-castle
		  set-b-k-castle
		  set-en-passanted
		  set-double-push
		  set-castled
		  reset-w-q-castle
		  reset-w-k-castle
		  reset-b-q-castle
		  reset-b-k-castle
		  reset-en-passanted
		  reset-double-push
		  reset-castled))

(defun make-fixnum-board ()
  (make-array '(8 8) :element-type 'fixnum
		   :initial-element 0))

(deftype fixnum-board ()
  (type-of (make-fixnum-board)))

(defun make-bit-board ()
  (make-array 64 :element-type 'bit))

;; should be simple-bit-vector 64
(deftype bit-board ()
  (type-of (make-bit-board)))

(defun set-bit-board (b f r)
  (declare (type bit-board b)
	   (type rank f r))
  (setf (bit b (+ (* f 8) r)) 1))
	      
(defun reset-bit-board (b f r)
  (declare (type bit-board b)
	   (type rank f r))
  (setf (bit b (+ (* f 8) r)) 0))

(defun get-bit-board (b f r)
  (declare (type bit-board b)
	   (type rank f r))
  (bit b (+ (* f 8) r)))

(defun clear-bit-board (b)
  (declare (type bit-board b))
  (fill b 0))

(defun print-bit-board (b)
  (declare (type bit-board b))
  (dotimes (f 8)
	   (declare (type fixnum f))
	   (dotimes (r 8)
		    (declare (type fixnum r))
		    (format t "~a" (get-bit-board b f r)))
	   (fresh-line)))

;;(defun bit-int (b)
;;  (declare (type bit-board b))
;;  (let ((int 0))
;;    (declare (type (unsigned-byte 64) int))
;;    (dotimes (count 64)
;;	     (setq int (logior int (ash (sbit b count) count))))
;;    int))

;;(defun int-bit (i)
;;  (declare (type (unsigned-byte 64) i))
;;  (let ((bit (make-bit-board)))
;;    (dotimes (count 64)
;;	     (setf (sbit bit count) (ash (logand i (ash 1 count)) (- count))))
;;    bit))

(defun get-board (board file rank)
  (declare (type fixnum-board board)
	   (type fixnum file rank))
  (aref board rank file))

(defun set-board (board file rank n)
  (declare (type fixnum-board board)
	   (type fixnum file rank n))
  (setf (aref board rank file) n))

(proclaim '(ftype (function (fixnum-board fixnum fixnum) fixnum) get-board))

(defun bit-on (b)
  (declare (type fixnum b)
	   (inline)
	   (optimize (speed 3) (safety 0)))
  (not (zerop b)))

(defun bit-off (b)
  (declare (type fixnum b)
	   (inline)
	   (optimize (speed 3) (safety 0)))
  (zerop b))

(proclaim '(ftype (function (fixnum) boolean) bit-on bit-off))

;; using 0 15
;;071D3D40:       .ENTRY CHESS-PIECE-COLOR()   ; FUNCTION
;;      58:       ADD   -18, %CODE
;;      5C:       ADD   %CFP, 32, %CSP
;;      60:       CMP   %A0, 0                 ; No-arg-parsing entry point
;;      64:       BEQ   L2
;;      68:       NOP
;;      6C:       AND   32, %A0
;;      70:       CMP   %A0, 0
;;      74:       BEQ   L1
;;      78:       NOP
;;      7C:       ADD   %ZERO, -4, %A0
;;      80: L0:   MOVE  %CFP, %CSP
;;      84:       MOVE  %OCFP, %CFP
;;      88:       J     %LRA+5
;;      8C:       MOVE  %LRA, %CODE
;;      90: L1:   B     L0
;;      94:       ADD   %ZERO, 4, %A0
;;      98: L2:   B     L0
;;      9C:       MOVE  %ZERO, %A0

;; conditionals

;;071D3C68:       .ENTRY CHESS-PIECE-PIECE()   ; FUNCTION
;;      80:       ADD   -18, %CODE
;;      84:       ADD   %CFP, 32, %CSP
;;      88:       AND   -36, %A0               ; No-arg-parsing entry point
;;      8C:       MOVE  %CFP, %CSP
;;      90:       MOVE  %OCFP, %CFP
;;      94:       J     %LRA+5
;;      98:       MOVE  %LRA, %CODE
;;      9C:       UNIMP 0

;; 6 instructions

;; using -6 6

;;07265E38:       .ENTRY CHESS-PIECE-COLOR()   ; FUNCTION
;;      50:       ADD   -18, %CODE
;;      54:       ADD   %CFP, 32, %CSP
;;      58:       MOVE  %OCFP, %NFP
;;      5C:       MOVE  %LRA, %A2              ; No-arg-parsing entry point
;;      60:       LD    [%CODE+13], %CNAME
;;      64:       ADD   %ZERO, 4, %NARGS
;;      68:       LD    [%CNAME+5], %A1
;;      6C:       MOVE  %NFP, %OCFP
;;      70:       MOVE  %A2, %LRA
;;      74:       J     %A1+23
;;      78:       MOVE  %A1, %CODE
;;      7C:       UNIMP 0

;; 8

;;07265D58:       .ENTRY CHESS-PIECE-PIECE()   ; FUNCTION
;;      70:       ADD   -18, %CODE
;;      74:       ADD   %CFP, 32, %CSP
;;      78:       CMP   %A0, 0                 ; No-arg-parsing entry point
;;      7C:       BGE   L1
;;      80:       NOP
;;      84:       NEG   %A0
;;      88: L0:   MOVE  %CFP, %CSP
;;      8C:       MOVE  %OCFP, %CFP
;;      90:       J     %LRA+5
;;      94:       MOVE  %LRA, %CODE
;;      98: L1:   B     L0
;;      9C:       NOP

;; 10

;; using white = 0 black = 1

;;07134DE8:       .ENTRY CHESS-PIECE-COLOR-2() ; FUNCTION
;;     E00:       ADD   -18, %CODE
;;     E04:       ADD   %CFP, 32, %CSP
;;     E08:       AND   -8, %A0                ; No-arg-parsing entry point
;;     E0C:       MOVE  %CFP, %CSP
;;     E10:       MOVE  %OCFP, %CFP
;;     E14:       J     %LRA+5
;;     E18:       MOVE  %LRA, %CODE
;;     E1C:       UNIMP 0

;; 6 instructions

;;07134D00:       .ENTRY CHESS-PIECE-PIECE-2() ; FUNCTION
;;      18:       ADD   -18, %CODE
;;      1C:       ADD   %CFP, 32, %CSP
;;      20:       SRA   %A0, 2, %NL0           ; No-arg-parsing entry point
;;      24:       SRA   %NL0, 1, %NL1
;;      28:       SLL   %NL1, 2, %A0
;;      2C:       MOVE  %CFP, %CSP
;;      30:       MOVE  %OCFP, %CFP
;;      34:       J     %LRA+5
;;      38:       MOVE  %LRA, %CODE
;;      3C:       UNIMP 0

; 8 instructions
