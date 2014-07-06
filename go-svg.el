;;; go-svg.el --- A go gtp interface with svg image support

;; Copyright (C) 2012 William Stevenson

;; Author: William Stevenson <yhvh2000@gmail.com>
;; Version: 0.1
;; Package-Requires: ((xml-gen "0.4"))

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A go gtp interface with svg image support.

;;; Code:

(require 'cl-lib)

(defgroup go-svg nil
  "Top level for go-svg customization.")

(defcustom go-program "gnugo"
  "String containing Go program name."
  :type '(string)
  :group 'go-svg)

(defcustom go-program-args (list "--mode" "gtp")
  "String containing Go program command line arguments."
  :type '(string)
  :group 'go-svg)

(defvar go-boardsize 19 "Holds the current boardsize")
(defvar go-level 1 "Holds the current computer level, the amount
of time gnugo will spend thinking about the next move")
(defvar go-img-size 300 "The size of the svg image of the game")
(defvar go-process-buffer "*gnugo*" "The buffer that the go
process is associated with")

(defconst go-position-regex "\\([A-T][0-9]+\\)"
"Regex matching go board position")

(defvar go-game-over nil
  "Non-nil if game is over.")
(defvar go-last-move-was-pass nil
  "Non-nil if last move was a pass.")

(defvar go-next-color 'black
  "Holds the next stone color to be played.")

(defun go-toggle-next-color ()
  "Toggles next stone color to be played."
  (if (eq go-next-color 'black)
      (setq go-next-color 'white)
    (setq go-next-color 'black)))

(defvar go-position-map
  (let (result)
    (dotimes (j 19)
      (dotimes (i 19)
	(setq
	 result
	 (cons
	  `(,(intern
	      (concat
	       (char-to-string (if (> 8 i) (+ 65 i) (+ 66 i)))
	       (number-to-string (+ 1 j))))
	    ,i ,j)
	  result))))
    result)
  "Holds board symbol map, '((A1 1 1) (T19 19 19))")

(defun go-symbol-position (symbol)
  "Returns position associated with symbol."
  (cdr (assoc symbol go-position-map)))

(defvar go-process nil
  "Holds the process associated with this buffer")

(defvar go-process-result nil
  "Holds a string of successful process output.
Set to nil after result has been used.  ")
(defvar go-process-reply nil)

(defun go-filter-function (proc string)
  "Filter function for go gtp process. "
  (let* ((result-tmp (concat go-process-reply string))
	 (end-of-result (string-match "\n\n" result-tmp)))
    (if (and end-of-result
	     (numberp end-of-result)
	     (> end-of-result 0))
	(progn
	  (setq go-process-reply nil)
	  (setq go-process-result result-tmp))
      (setq go-process-reply result-tmp))))

(defun go-error ()
  "Handles gtp errors"
  (message (replace-regexp-in-string "\n" "" go-process-result))
  (go-board-update))

(defun go-start-process ()
  "Starts the go gtp process"
  (setq go-process nil)
  (setq go-process-reply nil)
  (setq go-stones-alist '((black) (white)))
  (setq go-process
	(apply 'start-process "gnugo" "*gnugo*" go-program go-program-args))
  (set-process-filter go-process 'go-filter-function))

(defun go-kill-process ()
  "Kills the go-process associated with this game."
  (delete-process go-process))

(defun go-gtp-command (command &optional arg)
  "Sends gtp COMMAND with optional ARG, waits for successful
output."
  (setq go-process-reply nil)
  (setq go-process-result nil)
  (process-send-string go-process-buffer
		       (concat command " " arg "\n"))
  (while (not go-process-result)
    (accept-process-output go-process)))

(defun go-boardsize-set (size)
  "Set boardsize to SIZE and clear the board"
  (interactive "nSet boardsize to: ")
  (setq go-game-over nil)
  (go-gtp-command "boardsize" (number-to-string size))
  (cond
   ((string-match "^?" go-process-result)
    (go-error))
   (go-process-result
    (setq go-boardsize size)
    (go-board-update))
   (t nil)))

(defun go-level-set (level)
  "Set level to LEVEL."
  (interactive "nSet Go level to: ")
  (go-gtp-command "level" (number-to-string level))
  (cond
   ((string-match "^?" go-process-result)
    (go-error))
   (go-process-result
    (setq go-level level))
   (t nil)))

(defun go-load-sgf (file)
  "Load sgf FILE, set next color to play."
  (interactive "fFile: ")
  (go-gtp-command (concat "loadsgf " (expand-file-name file)))
  (cond ((string-match "^?" go-process-result)
	 (go-error))
	((string-match "^=" go-process-result)
	 (let ((sgf (go-save-sgf)))
	   ;; read the boardsize and set
	   (string-match "SZ\\[\\([0-9]+\\)\\]" sgf)
	   (setq go-boardsize (string-to-number (match-string 1 sgf)))
	   ;; read next color to play and set
	   (string-match "PL\\[\\(B\\|W\\)\\]" sgf)
	   (setq go-next-color (if (equal "B" (match-string 1 sgf))
				   'black 'white))
	   (go-board-update)))))

(defun go-save-sgf (file)
  "Save the current position as sgf. FILE specifies filename to
save in."
  (interactive "FFile to save as: ")
  (let ((arg (if file (expand-file-name file) "-")))
    (go-gtp-command (concat "printsgf " arg))
    (if go-process-result
	(if (equal arg "-")
	    (substring go-process-result 1 -2)
	  (message (concat "Saved game at: " arg ))))))

(defun go-estimate-score ()
  "Estimate score, gtp command"
  (interactive)
  (go-gtp-command "estimate_score")
  (if go-process-result
      (message (substring go-process-result 1 -2))))

(defun go-final-score ()
  "Show final score."
  (interactive)
  (setq go-game-over t)
  (go-gtp-command "final_score")
  (if (string-match "\\(B\\|W\\)\\+\\([0-9]+\\.[0-9]+\\)" go-process-result)
      (let ((winner (if (equal (match-string 1 go-process-result) "B")
			'black
		      'white))
	    (score (match-string 2 go-process-result)))
	(message
	 (format "%s wins with +%s points" winner score)))
    (go-error)))

(defun go-final-status ()
  "Get final status of verticies as an alist like
'((dead G7 K9) (white_territory D4 H1) (black_territory J9 T2) "
  (interactive)
  (mapcar
   (lambda (status)
     (with-temp-message (concat "gnugo calculating final status… "
				(symbol-name status))
       (setq go-process-reply nil
	     go-process-result nil)
       (go-gtp-command "final_status_list" (symbol-name status)))
     (cond ((string-match "^?" go-process-result)
	    (go-error))
	   ((string-match go-position-regex go-process-result)
	    (let ((i) (r))
	      (while (string-match go-position-regex go-process-result i)
		(setq i (match-end 0)
		      r (cons (intern (match-string 1 go-process-result)) r)))
	      (cons status r)))))
   '(dead white_territory black_territory)))

(defun go-play-pass ()
  "Calls `go-play-stone' with pass for current color.
If the last move was also a pass the game is over and the final
score is shown."
  (interactive)
  (if go-last-move-was-pass
      (go-final-score)
    (go-play-stone 'pass)))

(defun go-play-stone-mouse (pos)
  "Calls `go-play-stone' from mouse click on board."
  (interactive "e")
  (go-play-stone  (nth 1 (cadr pos))))

(defun go-play-stone (pos)
  "Plays a stone of COLOR at position POS"
  (interactive "SPosition to play: ")
  (setq go-process-reply nil)
  (setq go-process-result nil)
  (go-gtp-command "play "
		  (concat (upcase (symbol-name go-next-color))
			  " " (symbol-name pos)))
  (cond
   ((string-match "^?" go-process-result)
    (go-error))
   (go-process-result
    (go-toggle-next-color)
    (go-board-update)
    ;; (sit-for 0.1)
    (go-genmove))
   (t nil)))

(defun go-undo ()
  "Undoes one move."
  (interactive)
  (go-gtp-command "undo")
  (while (not go-process-result)
    (accept-process-output go-process))
  (if go-process-result
      (go-toggle-next-color))
  (go-board-update))

(defun go-genmove (&optional color)
  "Generate and play the supposed best move for COLOR."
  (interactive)
  (let ((col (or color go-next-color)))
    (with-temp-message "gnugo is thinking…"
      (go-gtp-command "genmove " (symbol-name col)))
    (cond
     ((string-match "^?" go-process-result)
      (go-error))
     ((string-match "RESIGN" go-process-result)
      (message (concat (symbol-name col) "RESIGN"))
      (go-final-score))
     ((string-match "PASS" go-process-result)
      (go-toggle-next-color)
      (if go-last-move-was-pass
	  (go-final-score)
	(progn
	  (setq go-last-move-was-pass t)
	  (message "PASS"))))
     ((string-match "[A-T][0-9]+" go-process-result)
      (progn
	(setcdr
	 (assoc col go-stones-alist)
	 (cons
	  (intern (match-string 0 go-process-result))
	  (cdr (assoc col go-stones-alist))))
	(go-toggle-next-color)
	(go-board-update))))))

(defun go-last-move ()
  "Return color and vertex of last move. "
  (interactive)
  (go-gtp-command "last_move")
  (cond
   ((string-match "^?" go-process-result)
    nil)
   ((string-match "\\(black\\|white\\) \\([A-T][0-9]+\\)" go-process-result)
    `(,(intern (match-string 2 go-process-result))
      ,(intern (match-string 1 go-process-result))))
   (t nil)))

(defvar go-stones-alist nil
  "Stores the moves so far.")

(defun go-list-stones (color)
  "Returns a list of positions for COLOR"
  (go-gtp-command "list_stones " (symbol-name color))
  (cond
   ((string-match "^?" go-process-result)
    (go-error))
   ((string-match go-position-regex go-process-result)
    (mapcar
     'intern
     (split-string (substring go-process-result 1))))
   (t nil)))

(defun go-stones-refresh-alist ()
  "Returns a list of all stones on board in the form
'((black D5 E7) (white D6 F3)) "
  (setq go-stones-alist
	`((black ,@(go-list-stones 'black))
	  (white ,@(go-list-stones 'white)))))

(defun go-stones ()
  "Returns a list of circle S-expressions for splicing into svg."
  (let ((black-positions (cdr (assoc 'black go-stones-alist)))
	(white-positions (cdr (assoc 'white go-stones-alist)))
	(stone-radius (number-to-string
		       (/ (- (go-pos-pixel-offset 1)
			     (go-pos-pixel-offset 0)) 2))))
    (append
     (mapcar
      (lambda (el)
	`(circle :cx ,(number-to-string
		       (go-pos-pixel-offset
			(car (go-symbol-position el))))
		 :cy ,(number-to-string
		       (go-pos-pixel-offset
			(cadr (go-symbol-position el))))
		 :r ,stone-radius
		 :fill "url(#rg)"))
      black-positions)
     (mapcar
      (lambda (el)
	`(circle :cx ,(number-to-string
		       (go-pos-pixel-offset
			(car (go-symbol-position el))))
		 :cy ,(number-to-string
		       (go-pos-pixel-offset
			(cadr (go-symbol-position el))))
		 :r ,stone-radius
		 :fill "url(#wh)"))
      white-positions))))

(defun go-last-move-marker ()
  "Returns a marker for last played stone."
  (let ((last-move (go-last-move))
	(marker-radius (number-to-string
			(/ (- (go-pos-pixel-offset 1)
			      (go-pos-pixel-offset 0)) 5))))
    (if last-move
	`((circle :cx ,(number-to-string
			(go-pos-pixel-offset
			 (car (go-symbol-position (car last-move)))))
		  :cy ,(number-to-string
			(go-pos-pixel-offset
			 (cadr (go-symbol-position (car last-move)))))
		  :r ,marker-radius
		  :stroke ,(if (eq (car (cdr last-move)) 'black)
			       "url(#wh)"
			     "url(#rg)")
		  :stroke-width "2" :fill "none")))))

(defun go-vertex-labels ()
  "Returns a list of vertex labels for go board"
  (append
   (mapcar
    (lambda (el)
      `(text :x ,(number-to-string (- (go-pos-pixel-offset el) 5))
	     :y ,(number-to-string
		  (+ (/ (- (go-pos-pixel-offset 1) (go-pos-pixel-offset 0)) 2)
		     (go-pos-pixel-offset (- go-boardsize 1))))
	     :font-size "11"
	     :font-family "Verdana"
	     ,(char-to-string (if (> 8 el) (+ 65 el) (+ 66 el)))))
    (number-sequence 0 (- go-boardsize 1)))

   (mapcar
    (lambda (el)
      `(text :x ,(if (> 9 el) "6" "1")
	     :y ,(number-to-string (go-pos-pixel-offset el))
	     :font-size "11"
	     :font-family "Verdana"
	     ,(number-to-string (1+ el))))
    (number-sequence 0 (- go-boardsize 1)))))

(defun go-board-svg ()
  "Returns the svg to draw the board"
  (let ((padding (go-pos-pixel-offset 0)))
    `(;; Draw the board background
      (rect :width ,go-img-size :height ,go-img-size :fill "#DCB35C")
      ;; Draw the outer square
      (path :stroke "#000" :stroke-width "0.7" :fill "none"
	    :d ,(concat
		 (format "M %d,%d H%d " padding padding
			 (go-pos-pixel-offset (- go-boardsize 1)))
		 (format "M %d,%d V%d " padding padding
			 (go-pos-pixel-offset (- go-boardsize 1)))
		 (format "M %d,%d H%d " padding
			 (go-pos-pixel-offset (- go-boardsize 1))
			 (go-pos-pixel-offset (- go-boardsize 1)))
		 (format "M %d,%d V%d " (go-pos-pixel-offset (- go-boardsize 1))
			 padding (go-pos-pixel-offset (- go-boardsize 1)))
		 ))
      ;; Draw the grid
      (path :stroke "#000" :stroke-width "0.3" :fill "none"
	    :d ,(mapconcat
		 (lambda (el)
		   (concat
		    (format "M %d,%d H%d " padding (go-pos-pixel-offset el)
			    (go-pos-pixel-offset (- go-boardsize 1)))
		    (format "M %d,%d V%d " (go-pos-pixel-offset el) padding
			    (go-pos-pixel-offset (- go-boardsize 1)))))
		 (number-sequence 1 (- go-boardsize 1))
		 ""))
      ;; Draw Star points
      (path :stroke "#000" :stroke-width "5"
	    :stroke-linecap "round"
	    :d ,(let ((left-star (if (< 11 go-boardsize)  3 2)))
		  (concat
		   ;; corner stars
		   (format "M%d,%d l0,0" (go-pos-pixel-offset left-star)
			   (go-pos-pixel-offset left-star))
		   (format "M%d,%d l0,0" (go-pos-pixel-offset
					  (- go-boardsize (+ 1 left-star)))
			   (go-pos-pixel-offset left-star))
		   (format "M%d,%d l0,0" (go-pos-pixel-offset
					  (- go-boardsize (+ 1 left-star)))
			   (go-pos-pixel-offset
			    (- go-boardsize (+ 1 left-star))))
		   (format "M%d,%d l0,0" (go-pos-pixel-offset left-star)
			   (go-pos-pixel-offset
			    (- go-boardsize (+ 1 left-star))))
		   (if (cl-oddp go-boardsize)
		       (concat
			;; centre star
			(format "M%d,%d l0,0" (go-pos-pixel-offset
					       (/ go-boardsize 2))
				(go-pos-pixel-offset  (/ go-boardsize 2)))
			;; four edge stars
			(format "M%d,%d l0,0" (go-pos-pixel-offset left-star)
				(go-pos-pixel-offset  (/ go-boardsize 2)))
			(format "M%d,%d l0,0" (go-pos-pixel-offset
					       (/ go-boardsize 2))
				(go-pos-pixel-offset  left-star))
			(format "M%d,%d l0,0" (go-pos-pixel-offset
					       (- go-boardsize (+ 1 left-star)))
				(go-pos-pixel-offset (/ go-boardsize 2)))
			(format "M%d,%d l0,0" (go-pos-pixel-offset
					       (/ go-boardsize 2))
				(go-pos-pixel-offset
				 (- go-boardsize (+ 1 left-star))))))))))))

(defun go-final-status-svg ()
  "Returns the svg to draw the final status of the verticies."
  (let* ((r) (width
	      (/ (- (go-pos-pixel-offset 1) (go-pos-pixel-offset 0)) 5))
	 (radius (/ width  2)))
    (mapcar
     (lambda (status)
       (mapcar
	(lambda (pos)
	  (setq r
		(cons
		 (cond
		  ((eq (car status) 'white_territory)
		   `(rect :x ,(number-to-string (- (go-pos-pixel-offset
						    (car (go-symbol-position pos)))
						   (/ width 2)))
			  :y ,(number-to-string (- (go-pos-pixel-offset
						    (cadr (go-symbol-position pos)))
						   (/ width 2)))
			  :width ,width :height ,width :fill "#FFF"))
		  ((eq (car status) 'black_territory)
		   `(rect :x ,(number-to-string (- (go-pos-pixel-offset
						    (car (go-symbol-position pos)))
						   (/ width 2)))
			  :y ,(number-to-string (- (go-pos-pixel-offset
						    (cadr (go-symbol-position pos)))
						   (/ width 2)))
			  :width ,width :height ,width :fill "#000"))
		  ((eq (car status) 'dead)
		   `(circle :cx ,(number-to-string (go-pos-pixel-offset
						    (car (go-symbol-position pos))))
			    :cy ,(number-to-string (go-pos-pixel-offset
						    (cadr (go-symbol-position pos))))
			    :r ,radius
			    :fill ,(if (member pos
					       (cdr (assoc 'black go-stones-alist)))
				       "#FFF"
				     "#000"))))
		 r)))
	(cdr status)))
     (go-final-status))
    r))

(defun go-img-string ()
  "Returns a svg string for game image"
  (xmlgen
   `(svg :xmlns "http://www.w3.org/2000/svg"
	 :width ,(number-to-string go-img-size)
	 :height ,(number-to-string go-img-size)
	 :encoding "UTF-8"
	 ,@(go-board-svg)
	 ,@(go-vertex-labels)
	 (defs
	   (radialGradient :id "rg" :cx ".3" :cy ".3" :r ".8"
			   (stop :offset "0" :stop-color "#777")
			   (stop :offset ".3" :stop-color "#222")
			   (stop :offset "1" :stop-color "#000"))
	   (radialGradient :id "wh" :cx ".3" :cy ".3" :r ".8"
			   (stop :offset "0" :stop-color "#FEE")
			   (stop :offset ".3" :stop-color "#DDD")
			   (stop :offset "1" :stop-color "#FFF")))
	 ,@(go-stones)
	 ,@(go-last-move-marker)
	 ,@(if go-game-over (go-final-status-svg)))))

(defun go-pos-pixel-offset (number)
  "Returns a pixel position for go board position"
  (let ((padding (/ (/ go-img-size go-boardsize) 2)))
    (+ (+ padding (/ padding 2))
       (* number
	  (/ (- go-img-size padding)
	     go-boardsize)))))

(defun go-mouse-event-circles ()
  "Returns a plist of circles at positions which are free of
stones."
  (mapcar
   (lambda (el)
     (if (not
	  (or
	   (member (car el) (cdr (assoc 'black go-stones-alist)))
	   (member (car el) (cdr (assoc 'white go-stones-alist)))))
	 `((circle . (( ,(* (go-pos-pixel-offset (cadr el))) .
			,(* (go-pos-pixel-offset (car (cddr el))))) .
			,(/ (/ go-img-size go-boardsize) 2)))
	   ,(car el) ; event name eg. [D4 mouse-1]
	   (pointer hand))))
   go-position-map))

(defun go-board-insert ()
  "Insert go board svg image."
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert-image
   (create-image (go-img-string) 'svg t
		 :map (go-mouse-event-circles)))
  (setq buffer-read-only t))

(defun go-board-update ()
  "Updates the go board image"
  (interactive)
  (go-stones-refresh-alist)
  (go-board-insert))

(defvar gosvg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'go-board-update)
    (define-key map "k" 'gosvg-cleanup)
    (define-key map "p" 'go-play-stone)
    (define-key map "P" 'go-play-pass)
    (define-key map "m" 'go-genmove)
    (define-key map "l" 'go-level-set)
    (define-key map "b" 'go-boardsize-set)
    (define-key map "u" 'go-undo)
    (define-key map "e" 'go-estimate-score)
    (define-key map "F" 'go-final-score)
    (define-key map "L" 'go-load-sgf)
    (define-key map "S" 'go-save-sgf)
    ;; The svg image has a map of circles which show the pointer as
    ;; hand and fire an event like: <D4 mouse-1>. Here I bind all these
    ;; events to `go-play-stone-mouse'. This results in a great many
    ;; entries in the keymap.
    (dolist (pos go-position-map)
       (define-key map (vector (car pos)  'mouse-1) 'go-play-stone-mouse))
    map)
  "Keymap for `gosvg-mode'")

(define-derived-mode gosvg-mode special-mode "gosvg"
  "Major mode for playing Go with SVG display."
  (make-local-variable 'go-stones-alist)
  (make-local-variable 'go-process-result)
  (make-local-variable 'go-process-reply)
  (make-local-variable 'go-next-color)
  (make-local-variable 'go-boardsize)
  (make-local-variable 'go-level)
  (make-local-variable 'go-last-move-was-pass)
  (make-local-variable 'go-level)
  (make-local-variable 'go-game-over)
  (let ((win-size (window-inside-absolute-pixel-edges)))
    (setq go-img-size (min (- (nth 2 win-size) (nth 0 win-size))
			   (- (nth 3 win-size) (nth 1 win-size)))))
  (setq cursor-type nil))

(defun gosvg ()
  "Play the game of Go with SVG display"
  (interactive)
  (switch-to-buffer (get-buffer-create "gosvg"))
  (gosvg-mode)
  (go-start-process)
  (go-board-update))

(defun gosvg-cleanup ()
  "Kills processes and buffers used"
  (interactive)
  (go-kill-process)
  (kill-buffer go-process-buffer)
  (kill-buffer "gosvg"))

(provide 'go-svg)
;;; go-svg.el ends here
