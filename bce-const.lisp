;; This file is part of Boo's Chess Engine
;; Copyright 1999 By Christopher Bowron

;; Constants

;; Boo's Chess Engine

(provide 'bce-constants)

(defconstant *white* 0)
(defconstant *black* 1)

;;(defconstant *white* 1)
;;(defconstant *black* -1)

(defconstant *empty* (make-chess-piece :piece 0 :color 0))

(defconstant filea 0)
(defconstant fileb 1)
(defconstant filec 2)
(defconstant filed 3)
(defconstant filee 4)
(defconstant filef 5)
(defconstant fileg 6)
(defconstant fileh 7)

(defconstant rank1 0)
(defconstant rank2 1)
(defconstant rank3 2)
(defconstant rank4 3)
(defconstant rank5 4)
(defconstant rank6 5)
(defconstant rank7 6)
(defconstant rank8 7)

(defconstant pawn-value   1)
(defconstant knight-value 2)
(defconstant bishop-value 3)
(defconstant rook-value   4)
(defconstant queen-value  5)
(defconstant king-value   6)

(defconstant piece-values
  #(0 100 325 350 500 1100 10000000))

(defconstant *white-mate* (- most-positive-fixnum 100))
(defconstant *black-mate* (+ most-negative-fixnum 100))
(defconstant *stale-mate* 0)
(defconstant *win*  *white-mate*)
(defconstant *lose* *black-mate*)

(defconstant *white-king-side-castle*
  (make-move (make-square filee 0) (make-square fileg 0)))

(defconstant *white-queen-side-castle*
  (make-move (make-square filee 0) (make-square filec 0)))

(defconstant *black-king-side-castle*
  (make-move (make-square filee 7) (make-square fileg 7)))

(defconstant *black-queen-side-castle*
  (make-move (make-square filee 7) (make-square filec 7)))


(defconstant *dummy-square*
  (make-square 8 8))

(defconstant *dummy-move*
  (make-move *dummy-square* *dummy-square*))

(defconstant *pawn-squares*
  #2A(
      (  0   0   0   0   0  0  0  0)
      (  0   0  -5 -10 -10 10  0  0)
      ( 10  15  25  35  45 25 15 10)
      ( 20  25  45  65  80 45 25 20)
      ( 30  45  55  75  85 55 35 30)
      ( 40  55  65  85  90 65 45 40)
      ( 50  65  75  90  95 75 55 50)
      ( 60  70  90 100 100 90 75 60)
      ))

(defconstant *black-pawn-squares*
  #2A(
      ( 60  70  90 100 100 90 75 60)
      ( 50  65  75  90  95 75 55 50)
      ( 40  55  65  85  90 65 45 40)
      ( 30  45  55  75  85 55 35 30)
      ( 20  25  45  65  80 45 25 20)
      ( 10  15  25  35  45 25 15 10)
      (  0   0  -5 -10 -10 10  0  0)
      (  0   0   0   0   0  0  0  0)))

(defconstant *knight-squares*
  #2A( (  0 10 20 30 30 20 10  0)
     ( 10 20 30 40 40 30 20 10)
     ( 20 30 40 50 50 40 30 20)
     ( 30 40 50 60 60 50 40 30)
     ( 30 40 50 60 60 50 40 30)
     ( 20 30 40 50 50 40 30 20)
     ( 10 20 30 40 40 30 20 10)
     (  0 10 20 30 30 20 10  0) ))
(defconstant *bishop-squares*
  #2A( ( 20 10  5  0  0  5 10 20)
     ( 10 20 10  5  5 10 20 10)
     (  5 10 20 10 10 20 10  5)
     (  0  5 10 20 20 10  5  0)
     (  0  5 10 20 20 10  5  0)
     (  5 10 20 10 10 20 10  5)
     ( 10 20 10  5  5 10 20 10)
     ( 20 10  5  0  0  5 10 20) ))
(defconstant *rook-squares*
  #2A(
      ( 00 00 30 00 00 30 00 00)
      ( 00 00 00 00 00 00 00 00)
      ( 00 00 00 00 00 00 00 00)
      ( 00 00 00 00 00 00 00 00)
      ( 00 00 00 00 00 00 00 00)
      ( 00 00 00 00 00 00 00 00)
      ( 00 00 00 00 00 00 00 00)
      ( 00 00 30 00 00 30 00 00) ))
(defconstant *queen-squares*
  #2A( (  0  5 10 20 15 10  5  0)
     (  5 10 15 20 20 15 10  5)
     ( 10 15 20 25 25 20 15 10)
     ( 15 20 25 30 30 25 20 15)
     ( 15 20 25 30 30 25 20 15)
     ( 10 15 20 25 25 20 15 10)
     (  5 10 15 20 20 15 10  5)
     (  0  5 10 20 15 10  5  0) ))
(defconstant *king-squares*
  #2A(
      ( -3  0 10  0 10  0 20 -3)
      ( -5 -3 -5 -5 -5 -5  0 -1)
      ( -5 -5 -5 -5 -5 -5 -5 -3)
      ( -5 -5 -5 -5 -5 -5 -5 -5)
      ( -5 -5 -5 -5 -5 -5 -5 -5)
      ( -5 -5 -5 -5 -5 -5 -5 -3)
      ( -5 -3 -5 -5 -5 -5  0 -1)
      ( -3  0 10  0 10  0 20 -3) ))

(defconstant *empty-squares*
  (make-array '(8 8) :initial-element 0))

(defconstant *square-values*
  (make-array 2 :initial-contents
   (list
    (make-array
     7 :initial-contents
     (list *empty-squares*
	   *pawn-squares*
	   *knight-squares*
	   *bishop-squares*
	   *rook-squares*
	   *queen-squares*
	   *king-squares*))
    (make-array
     7 :initial-contents
     (list *empty-squares*
	   *black-pawn-squares*
	   *knight-squares*
	   *bishop-squares*
	   *rook-squares*
	   *queen-squares*
	   *king-squares*)))))

