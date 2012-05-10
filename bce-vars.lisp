;; variables

;; Boo's Chess Engine
;; This file is part of Boo's Chess Engine
;; Copyright 1999 By Christopher Bowron


(provide 'bce-vars)

(defvar *white-clock* 0)
(defvar *black-clock* 0)

(defvar *search-depth* 3
  "General Search Depth")
(defvar *q-depth* 2
  "Quiescent Search Depth")
(defvar *highlight* t
  "Should We highlight last move when printing the board?")

(proclaim '(type fixnum *search-depth* *q-depth*))
(proclaim '(type fixnum *white-clock* *black-clock*))
(proclaim '(type boolean *hightlight*))

(defvar *bk* (make-square filee rank8)
  "Where is the Black King?")

(defvar *wk* (make-square filee rank1)
  "Where is the White King?")

(proclaim '(type square *wk* *bk*))

(defvar *choosen-piece* *empty*)

(proclaim '(type chess-piece *empty*))

(defvar *piece-hash*
  (make-hash-table :test #'eql :size 12))

(defun setup-piece-hash ()
  (setf (gethash #\p *piece-hash*) 1)
  (setf (gethash #\n *piece-hash*) 2)
  (setf (gethash #\b *piece-hash*) 3)
  (setf (gethash #\r *piece-hash*) 4)
  (setf (gethash #\q *piece-hash*) 5)
  (setf (gethash #\k *piece-hash*) 6)
  (setf (gethash #\P *piece-hash*) 1)
  (setf (gethash #\N *piece-hash*) 2)
  (setf (gethash #\B *piece-hash*) 3)
  (setf (gethash #\R *piece-hash*) 4)
  (setf (gethash #\Q *piece-hash*) 5)
  (setf (gethash #\K *piece-hash*) 6))
  
(defvar *game-move-list* nil
  "list of all moves in game")

(defvar *game-undo-list* nil
  "list of all undos to backtrack through game")

(defvar *back-up-list* nil)

(proclaim '(type list *game-undo-list* *game-move-list* *back-up-list*))

(defvar *variables* (make-var-type))
(proclaim '(type var-type *variables*))
 
(defvar *var-stack* nil)
(proclaim '(type list *var-stack*))

;;(defvar *attacked*
;;  (make-array 2 :initial-contents (list (make-bit-board) (make-bit-board))))

;;(defvar *controlled*
;;  (make-array 2 :initial-contents (list (make-bit-board) (make-bit-board))))

(defvar *mobility*
  (make-array 2 :element-type 'fixnum))

(defvar *material*
  (make-array 2 :element-type 'fixnum))

(proclaim `(type ,(type-of *material*) *material* *mobility*))


(defvar *pawns*
  #( #(0 0 0 0 0 0 0 0) #(0 0 0 0 0 0 0 0)))

(proclaim `(type ,(type-of *pawns*) *pawns*))

(deftype pawn-struct ()
  (type-of (aref *pawns* 0)))
  
(defvar *book* nil)

(defvar *computer* nil)

(proclaim '(type list *book*))
(proclaim '(type boolean *computer*))
(proclaim '(ftype (function (fixnum) bit) color-index))
