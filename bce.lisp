;; bce.lisp
;; boo's chess engine

;; copyright 1999

;;  christopher bowron - bowronch@cse.msu.edu
;;      -=* digital dreamland 1999 *=- 
;; ... we came, we saw, we played chess ...

;; this program or any derivative work may not be distributed for
;; *any* commercial purpose without specific consent of
;; Christopher Bowron - bowronch@cse.msu.edu

;; TODO:
;; GENERAL:
;;    get fully operational on fics
;;    decode input in the form BxN (Bishop Takes Knight) 
;;    mysterious invalid moves - POSSIBLY (save-move?)
;;    solve the mystery of the disappearing pieces - DONE
;;    cannot castle thru check - DONE
;;    user commands (backup, exit, etc) - DONE
;;    fix promotion - DONE (error in gen-def, can-move-p, etc set *computer* t)

;; AI:
;;    re-incorporate square-values into material value - DONE
;;    search further ahead towards the end
;;    get meaningful rating on fics
;;    find a stupid checkmate when its right in front of your damn nose
;;    better static evaluator
;;    end game mode / mate finder
;;    bit boards
;;    transposition tables
;;    move ordering (get best moves to top of list)
;;    cannot move into check EVER - DONE ?

(provide 'bce)

(defconstant fast '(optimize
		    (speed 3)
		    (safety 0)
		    (debug 0) 
		    (compilation-speed 0)
		    ;;(extensions::inhibit-warnings 0)
		    ))

(defconstant slow '(optimize
		    (safety 3)
		    (debug 3)
		    (speed 1)
		    (compilation-speed 0)
		    ;;(extensions::inhibit-warnings 3)		    
		    ))


;; (defconstant *bce-dir* "/user/bowronch/lisp/chess/BCE/")
;; (setq *default-pathname-defaults* (pathname *bce-dir*))
;; (setq ext:*gc-verbose* nil)

(load "bce-types.lisp")
(load "bce-const.lisp")
(load "bce-vars.lisp")
(load "bce-board.lisp")
(load "bce-move-gen.lisp")
(load "bce-user-moves.lisp")
(load "bce-do-move.lisp")
(load "bce-ai.lisp")
(load "bce-book.lisp")

(defun initialize (&key
		   (whitetime (* 6000 10))
		   (blacktime (* 6000 10)))
  (setup-board)
  (setq *var-stack* nil)
  (setq *variables* (make-var-type))
  (setq *game-move-list* nil)
  (setq *game-undo-list* nil)
  (setq *wk* (make-square  filee  0))
  (setq *bk* (make-square  filee  7))
  (setq *random-state* (make-random-state t))
  (setq *white-clock* whitetime)
  (setq *black-clock* blacktime)
  'Initialized)
  
(defun internal-hundredth (difference)
  (round (/ (* 100 difference) internal-time-units-per-second)))

(defun decode-time (h-secs)
  (format nil "~2,'0d:~2,'0d.~2,'0d"
	  (floor (/ h-secs 6000))
	  (floor (/ (mod h-secs 6000) 100))
	  (mod h-secs 100)))
  
(defun next-human-move (color)
  (setq *computer* nil)
  (let ((start-time (get-internal-real-time))
	(end-time 0)
	(move (get-user-move color)))

    (cond
     ((eq move t) -3)
     (t
      ;;(format t "move: ~a~%" (move-string move))
      (push
       (loop
	(when (validp move color)
	  (return (do-move move)))
	(format t "invalid move: ~a~%" (move-string move))
	(setq move (get-user-move color)))
       
       *game-undo-list*)
      (setq end-time (get-internal-real-time))
      (- end-time start-time)))))

(defvar *file-list* (list
		     "bce.lisp"
		     "bce-types.lisp"
		     "bce-const.lisp"
		     "bce-vars.lisp"
		     "bce-board.lisp"
		     "bce-move-gen.lisp"
		     "bce-user-moves.lisp"
		     "bce-do-move.lisp"
		     "bce-ai.lisp"
		     "bce-book.lisp"))

(defvar *compiled-file-list*
  (list "bce"
	"bce-types"
	"bce-const"
	"bce-vars"
	"bce-board"
	"bce-move-gen"
	"bce-user-moves"
	"bce-do-move"
	"bce-ai"
	"bce-book"))

(defun load-source ()
  (dolist (file *file-list*)
	  (load file)))

(defun make (&optional (load nil))
  (dolist (file *file-list*)
	  (compile-file file :load load)))

(defun load-compiled ()
  (dolist (file *compiled-file-list*)
	  (load file)))

(defun demo ()
  (play-chess 3 3))

(defun play-chess (white-depth black-depth)
  (let ((status nil))
    (count-material)
    (loop
     (setq status (game-over-p *white*))
     (when status
       (format t "~a~%" status)
       (print-board)       
       (return -1))
     (unless (next-move *white* white-depth)
       (return -1))
   
     (trim-book)
     (setq status (game-over-p *black*))
     (when status
       (format t "~a~%" status)
       (print-board)
       (return 1))
     (unless (next-move *black* black-depth)
       (return 1))
     (trim-book))))
   

(defun next-move (color depth)
   (print-board)
   (when (incheckp color)
     (format t "~a in check~%" (if (= color *white*) "white" "black")))
   (format t "w ~a : b ~a~%"
	   (decode-time *white-clock*) (decode-time *black-clock*))
   (format t "w ~a : b ~a~%"
	   (evaluate *white*) (evaluate *black*))
   (format t "w ~a : b ~a~%"
	   (material *white*) (material *black*))

   (let ((time 
	  (if depth
	      (computer-move color depth)
	    (next-human-move color))))
     (cond
      ((= time -1)
       (format t "resignation~%")
       nil)
      ((= time -2)
       (format t "stalemate/draw~%")
       nil)
      ((= time -3)
       (format t "exit~%")
       nil)
      (t
       (if (= color *white*)
	   (setq *white-clock* (- *white-clock* time))
	 (setq *black-clock* (- *black-clock* time)))
       t))))
   
(defun go-robofics-worker ()
  (let ((black-depth nil)
	(white-depth nil))
    (format t "Starting New Robofics Game~%")
    (load-compiled)
    ;; we dont need to print that big board
    ;; but the small board may be helpful for
    ;; debugging
    (defun print-board ()
      (print-board-tiny))
    (start-up)
    (loop
     (let ((str (read-line nil nil nil))
	   (status nil))
     
       (format t "str : ~a~%" str)
       (when (equalp "edit" str)
	 (robofics-setup-board))
       (when (equalp "force" str)
	 ;; to do
	 ;;    setup board from adjourned game
	 ;;
	 )
       (when (equalp "white" str)
	 (format t "received white~%")
	 (setq str (read-line nil nil nil))
	 (cond
	  ((equalp str "go")
	   (format t "setting white depth~%")
	   (setq white-depth *search-depth*)
	   (return t))
	  (t				;time xxxxx
	   (read-line nil nil nil)	;otim xxxxx
	   (format t "setting black depth~%")
	   (setq black-depth *search-depth*)
	   (return t))))))
    
    (format t "starting game~%")
    (count-material)
    (let ((first t))
      (loop
       (when (and (not first) (not white-depth))
;;	 (format t "reading times~%")
;;	 (format t "first :  ~a~%" first)
;;	 (format t "white-depth : ~a~%" white-depth)
;;	 (format t "black : ~a~%" black-depth)
	 (let ((str1 (read-line nil nil nil))	;time xxx
	       (str2 (read-line nil nil nil)))	;otim xxx
	   (when (or (< (length str1) 5) (< (length str2) 5))
	     (format t "Error reading clock times~%")
	     (quit))
	   (setq *black-clock* (parse-integer (subseq str1 5)))
	   (setq *white-clock* (parse-integer (subseq str2 5)))))
	 
       (setq first nil)
       (unless (next-move *white* white-depth)
	 (when white-depth
	   (format t "tellics resign~%"))
	 (return -1))
       (trim-book)
     
       (unless black-depth
;;;      (format t "reading times~%")
;;;	 (format t  "first :  ~a~%" first)
;;;	 (format t "white-depth : ~a~%" white-depth)
;;;	 (format t  "black : ~a~%" black-depth)
;;;	 (format t "reading times~%")	 
	 (let ((str1 (read-line nil nil nil))	;time xxx
	       (str2 (read-line nil nil nil)))	;otim xxx
	   (when (or (< (length str1) 5) (< (length str2) 5))
	     (format t "Error reading clock times~%")
	     (quit))
	   (setq *white-clock* (parse-integer (subseq str1 5)))
	   (setq *black-clock* (parse-integer (subseq str2 5)))))
       
       (unless (next-move *black* black-depth)
	 (when black-depth
	   (format t "tellics resign~%"))
	 (return 1))
       (trim-book)))))


(defun int-handler (&optional
		    (signal nil)
		    (code nil)
		    (sp nil))
  (format t "received signal: ~a ~a ~a~%" signal code sp)
  (quit))

(defun go-robofics ()
  ;;(system:enable-interrupt Unix:SIGINT  #'int-handler)
  ;;(system:enable-interrupt Unix:SIGTSTP #'int-handler)
  ;;(system:enable-interrupt Unix:SIGTERM #'int-handler)
  ;;(system:enable-interrupt Unix:SIGPIPE #'int-handler)
  (go-robofics-worker)
  (quit))

(defun start-up ()
  (setup-piece-hash)
  (initialize)
  (load-book (merge-pathnames *bce-dir* "book.txt")))

(defun gcl-play (w b)
  (read-line)
  (play-chess w b))

(defun play ()
  (let ((w nil)
	(b nil))
  (start-up)
  (cond
   ((= (random 2) 0)
    (setq b *search-depth*)
    (format t "You will be playing white~%")
    )
   (t
    (setq w *search-depth*)
    (format t "You will be playing black~%")    
    ))
  (play-chess w b)
  ))

