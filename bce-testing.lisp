;; This file is part of Boo's Chess Engine
;; Copyright 1999 By Christopher Bowron

(proclaim fast)

;;(defvar *testboard* (make-array '(16 16) :element-type 'chess-piece
;;				:initial-element *empty*))

;;(proclaim '(type (simple-array chess-piece (16 16)) *testboard*))

;;(defun get-piece-2 (file rank)
;;  (declare (type fixnum file rank)
;;	   (inline)
;;	   (optimize (speed 3) (safety 0) (debug 0)))
;;  (aref *testboard* (+ rank 4) (+ file 4)))


;;(defun get-bit-board-2 (b f r)
;;  (declare (type bit-board b)
;;	   (type fixnum f r))
;;  (logand b (ash 1 (logior r (ash f 3)))))

;;(defun get-bb-2 (b f r)
;;  (declare (type bit-board b)
;;	   (type fixnum f r))
;;  (ldb (byte 1 (logior (ashf f 3) r)) b))

;;(deftype bb-2 ()
;;  `(simple-array (signed-byte 8) 8))
  
;;(defun get-bla (b f r)
;;  (declare (type bb-2 b)
;;	   (fixnum f r))
;;  (logand (aref b f) r))


;;(proclaim fast)

;;(defun make-bb ()
;;  (make-array 64 :element-type 'bit))

;;(deftype bb ()
;;  (type-of (make-bb)))

;;(defun get-bb (b f r)
;;  (declare (type (simple-bit-vector 64) b)
;;	   (type fixnum f r))
;;  (bit (the simple-bit-vector b) (the fixnum (+ (the fixnum (* 8 f)) r))))

;;(defun set-bb (b f r)
;;  (declare (type (simple-bit-vector 64) b)
;;	   (type fixnum f r))
;;  (setf (bit (the simple-bit-vector b)
;;	     (the fixnum (+ (the fixnum (* 8 f)) r))) 1))

;;(defun reset-bb (b f r)
;;  (declare (type (simple-bit-vector 64) b)
;;	   (type fixnum f r))
;;  (setf (bit (the simple-bit-vector b)
;;	     (the fixnum (+ (the fixnum (* 8 f)) r))) 0))

;;(defvar testboard
;;#2A((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;    (0 0 0 0 0 4 0 0 0 0 0 0 0 0 0 0)
;;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;    (0 0 0 0 14 0 0 0 0 0 6 0 0 0 0 0)
;;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;    (0 0 0 0 0 0 4 0 0 0 0 0 0 0 0 0)
;;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;;(defvar tb (make-array 64 :element-type 'chess-piece :initial-element 0))

;;(defun get-tb (f r)
;;  (declare (type file f r))
;;  (aref tb (+ (* r 8) f)))

(defun sign-extend (b shift)
  (declare (type fixnum b shift)
	   (inline))
  (the fixnum (ash (the fixnum (ash b shift)) (- shift))))

(defun make-chess-piece-2 (&key
			 (piece 0)
			 (color 0))
  (declare
   (inline)
   (type (integer 0 6) piece)
   (type (integer 0 1) color))
  (logior (ash piece 1) color))

(defun chess-piece-piece-2 (p)
  (declare (type chess-piece p)
	   (inline))
  (ash p -1))

(defun chess-piece-color-2 (p)
  (declare (type chess-piece p)
	   (inline))
  (logand (lognot 1) p))



