;;; go-svg.el --- A go gtp interface with svg image support

;; Copyright (C) 2012 William Stevenson

;; Author: William Stevenson <yhvh2000@gmail.com>
;; Version: 0.4

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
;; depends on xmlgen https://github.com/philjackson/xmlgen
(require 'xmlgen)

(defgroup go-svg nil
  "Top level for go-svg customization.")

(defcustom go-program "/usr/bin/gnugo"
  "String containing Go program name."
  :type '(string)
  :group 'go-svg)

(defcustom go-program-args "--mode gtp"
  "String containing Go program command line arguments."
  :type '(string)
  :group 'go-svg)

(defvar go-boardsize 19)
(defvar go-img-size 300)
(defvar go-process-buffer "*gnugo*" )

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

(defun go-start-process ()
  "Starts the go gtp process"
  (setq go-process nil)
  (setq go-process-reply nil)
  (setq go-stones-alist '((black) (white)))
  (setq go-process
	(start-process "gnugo" "*gnugo*" "gnugo" "--mode" "gtp"))
  (set-process-filter go-process 'go-filter-function))

(defun go-kill-process ()
  (setq go-stones-alist '((black) (white)))
  (delete-process go-process))

(defun go-boardsize-set (size)
  "Set boardsize to SIZE and clear the board"
  (setq go-process-reply nil)
  (setq go-process-result nil)
  (process-send-string
   go-process-buffer
   (concat "boardsize " (number-to-string size) "\n"))
  (accept-process-output go-process)
  (if go-process-reply
      (setq go-boardsize size)
    (setq go-boardsize nil)))

(defun go-play-stone (color pos)
  "Plays a stone of COLOR at position POS"
  (setq go-process-reply nil)
  (setq go-process-result nil)
  (process-send-string
   go-process-buffer
   (concat "play " (symbol-name color) " " (symbol-name pos) "\n"))
  (accept-process-output go-process)
  (if go-process-result
      (progn
	(setcdr
	 (assoc color go-stones-alist)
	 (cons
	  pos
	  (cdr (assoc color go-stones-alist))))
	(go-board-update))
    nil))

(defun go-genmove (color)
  "Generate and play the supposed best move for COLOR."
  (setq go-process-reply nil)
  (setq go-process-result nil)
  (process-send-string
   go-process-buffer
   (concat "genmove " (symbol-name color) "\n"))
  (while (not go-process-result)
      (accept-process-output go-process 30))
  (if (string-match "[A-T]+[0-9]+" go-process-result)
      (progn
	(setcdr
	 (assoc color go-stones-alist)
	 (cons
	  (intern (match-string 0 go-process-result))
	  (cdr (assoc color go-stones-alist))))
	(go-board-update))
    (message (concat "Fail\|" go-process-result "\|"))))

(defvar go-stones-alist nil
  "Stores the moves so far.")

(defun go-list-stones (color)
  "Returns a list of positions for COLOR"
  (setq go-process-reply nil)
  (setq go-process-result nil)
  (process-send-string
   go-process-buffer
   (concat "list_stones " (symbol-name color) " \n"))
  (accept-process-output go-process)
  (if go-process-result
      (mapcar				; wrong
       'intern
       (split-string (substring go-process-result 1)))
    nil))

(defun go-stones-refresh-alist ()
  "Returns a list of all stones on board in the form
'((black (D5 E7) (white (D6 F3)))"
  (setq go-stones-alist
	`((black ,@(go-list-stones 'black))
	  (white ,@(go-list-stones 'white)))))

(defun go-stones ()
  "Returns a list of circle S-expressions for splicing into svg."
  (let ((black-positions (cdr (assoc 'black go-stones-alist)))
	(white-positions (cdr (assoc 'white go-stones-alist))))
    (append
     (mapcar
      (lambda (el)
	`(circle :cx ,(number-to-string
		       (+ 2.5
			  (* 5
			     (car
			      (go-symbol-position el)))))
		 :cy ,(number-to-string
		       (+ 2.5
			  (* 5
			     (car (cdr
				   (go-symbol-position el))))))
		 :r "2.4"
		 :fill ,(concat "url(#rg)")))
      black-positions)
     (mapcar
      (lambda (el)
	`(circle :cx ,(number-to-string
		       (+ 2.5
			  (* 5
			     (car
			      (go-symbol-position el)))))
		 :cy ,(number-to-string
		       (+ 2.5
			  (* 5
			     (car (cdr
				   (go-symbol-position el))))))
		 :r "2.4"
		 :fill ,(concat "url(#wh)")))
      white-positions))))

(defun go-img-string ()
  "Returns a svg string for game image"
  (xmlgen
   `(svg :xmlns "http://www.w3.org/2000/svg"
	 :width ,(number-to-string go-img-size)
	 :height ,(number-to-string go-img-size)
	 :encoding "UTF-8" :viewBox "0 0 96 96"
	 (rect :width "114" :height "114" :fill "#DCB35C")
	 (path :stroke "#000" :stroke-width ".2" :fill "none"
	       :d "M2.9,93 h90.2 m-.2-5 H3 m0-5
h90 m0-5 H3 m0-5 h90 m0-5 H3 m0-5 h90 m0-5 H3
m0-5 h90 m0-5 H3 m0-5 h90 m0-5 H3 m0-5 h90 m0-5
H3 m0-5 h90 m0-5 H3 m0-5 h90 m0-5 H3 m-.1-5 h90.2
M3,3 V93 m5,0 V3 m5,0 V93 m5,0 V3 m5,0 V93 m5,0
V3 m5,0 V93 m5,0 V3 m5,0 V93 m5,0 V3 m5,0 V93
m5,0 V3 m5,0 V93 m5,0 V3 m5,0 V93 m5,0 V3 m5,0
V93 m5,0 V3 m5,0 V93 m5,0 V3")
	 (path :stroke "#000" :stroke-width "2"
	       :stroke-linecap "round"
	       :d "M18,78 l0,0 m30,0 l0,0
m30,0 l0,0 m0-30 l0,0 m-30,0 l0,0 m-30,0 l0,0
m0-30 l0,0 m30,0 l0,0 m30,0 l0,0")
	 (defs
	   (radialGradient :id "rg" :cx ".3" :cy ".3" :r ".8"
			   (stop :offset "0" :stop-color "#777")
			   (stop :offset ".3" :stop-color "#222")
			   (stop :offset "1" :stop-color "#000"))
	   (radialGradient :id "wh" :cx ".3" :cy ".3" :r ".8"
			   (stop :offset "0" :stop-color "#FEE")
			   (stop :offset ".3" :stop-color "#DDD")
			   (stop :offset "1" :stop-color "#FFF")))
	 ,@(go-stones))))

(defun go-board-insert ()
  "Insert go board svg image at cursor pos"
  (setq buffer-read-only nil)
  (insert-image
   (create-image (go-img-string) 'svg t
		 :map '(((circle . ((100 . 100) . 20))
			 area1
			 (pointer hand)))))
  (setq buffer-read-only t))

(defun go-board-update ()
  "Updates the go board image"
  (setq buffer-read-only nil)
  (erase-buffer)
  (go-board-insert))

(defvar gosvg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'go-board-update)
    (define-key map "k" 'gosvg-cleanup)
    map)
  "Keymap for `gosvg-mode'")

(define-derived-mode gosvg-mode special-mode "gosvg"
  "Major mode for playing Go with SVG display
\\{gosvg-mode-map}"
  (make-variable-buffer-local 'go-stones-alist)
  (make-variable-buffer-local 'go-process-result)
  (make-variable-buffer-local 'go-process-reply)
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
  (go-board-insert)
  )

(defun gosvg-cleanup ()
  "Kills processes and buffers used"
  (interactive)
  (go-kill-process)
  (kill-buffer go-process-buffer)
  (kill-buffer "gosvg"))

(defun go-test ()
  "Test go-svg"
    (go-genmove 'black)
    (go-genmove 'white))

(defun go-test-continue ()
  "Test go-svg"
    (go-genmove 'black)
    (go-genmove 'white))
