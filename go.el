;; depends xmlgen https://github.com/philjackson/xmlgen

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
(defvar go-img-size 500)
(defvar go-process-buffer "*gnugo*" ) ;; make local?


(defun go-process ()
  "Starts the go gtp process"
  (start-process "gnugo" "*gnugo*" "gnugo" "--mode" "gtp"))

(defun go-boardsize-set (size)
  "Set boardsize to SIZE and clear the board"
  (process-send-string go-process-buffer "boardsize 19\n"))

(defun go-play-stone (color pos)
  "Plays a stone of COLOR at position POS"
  (process-send-string go-process-buffer (concat "play " color " " pos "\n")))

(defun go-genmove (color)
  "Generate and play the supposedly best move for COLOR."
  (process-send-string go-process-buffer (concat "genmove " color "\n")))

(defun go-list-stones (color)
  "Returns a list of positions for COLOR"
  (process-send-string go-process-buffer "list_stones " color " \n"))

(defun go-stone-alist ()
  "Returns a list of all stones on board in the form
'((black ((D . 4) (E . 5))) (white ((G . 3) (H . 4 ))))"
  
)

(defun go-stones (stones-alist)
  "Returns a list of circle S-expressions for splicing into svg.  
Random atm, should read in board state"
  (let 
      (value)
    (dolist (i black)
      (cons 
       `(circle :cx ,(number-to-string (cons i))
		:cy ,(number-to-string (cdr i))
		:r "2.4" 
		:fill "url(#rg)")
       value))
    (dolist (i white)
      (cons 
       (circle :cx ,(number-to-string (cons i))
		:cy ,(number-to-string (cdr i))
		:r "2.4" 
		:fill "url(#rg)")
       value))))

(defun go-img-string ()
  "Returns a svg string for game image
Green?
...Super Green."
  (xmlgen 
   `(svg :xmlns "http://www.w3.org/2000/svg" 
	 :width ,(number-to-string go-img-size)
	 :height ,(number-to-string go-img-size)
	 :encoding "UTF-8" :viewBox "0 0 96 96"
	 (rect :width "114" :height "114" :fill "#DCB35C")
	 (path :stroke "#000" :stroke-width ".2" :fill "none" 
	       :d "M2.9,93h90.2m-.2-5H3m0-5h90m0-5H3m0-5h90m0
-5H3m0-5h90m0-5H3m0-5h90m0-5H3m0-5h90m0-5H3m0-5h90m0-5H3m0-5h90m0
-5H3m0-5h90m0-5H3m-.1-5h90.2M3,3V93m5,0V3m5,0V93m5,0V3m5,0V93m5,
0V3m5,0V93m5,0V3m5,0V93m5,0V3m5,0V93m5,0V3m5,0V93m5,0V3m5,0V93m5,
0V3m5,0V93m5,0V3m5,0V93m5,0V3")
	 (path :stroke "#000" :stroke-width "2" :stroke-linecap "round" 
	       :d "M18,78l0,0m30,0l0,0m30,0l0,0m0-30l0,0m-30,0l0,
0m-30,0l0,0m0-30l0,0m30,0l0,0m30,0l0,0")
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
  (insert-image
   (create-image (go-img-string) 'svg t
		 :map '(((circle . ((100 . 100) . 20)) 
			 area1 
			 (pointer hand))))))


 
