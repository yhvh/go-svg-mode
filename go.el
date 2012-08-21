(require 'xmlgen) ;; https://github.com/philjackson/xmlgen

(defvar boardsize 19)
(defvar go-img-size 500)

(defun go-stones ()
  "Returns a list of circle S-expressions for splicing into svg
Random atm, should read in board state"
  (let 
      (value)
    (dotimes (i 19 value)
      (dotimes (j 19 value)
	(if (> (random 2) 0)
	    (setq value (cons `(circle :cx ,(number-to-string (+ 2.9 (* i 5)))
				       :cy ,(number-to-string (+ 2.9 (* j 5)))
				       :r "2.4" 
				       :fill ,(concat "url(#" (if (> (random 2) 0) "rg" "wh") ")")) value)))))))
  
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
	       :d "M2.9,93h90.2m-.2-5H3m0-5h90m0-5H3m0-5h90m0-5H3m0-5h90m0-5H3m0-5h90m0-5H3m0-5h90m0-5H3m0-5h90m0-5H3m0-5h90m0-5H3m0-5h90m0-5H3m-.1-5h90.2M3,3V93m5,0V3m5,0V93m5,0V3m5,0V93m5,0V3m5,0V93m5,0V3m5,0V93m5,0V3m5,0V93m5,0V3m5,0V93m5,0V3m5,0V93m5,0V3m5,0V93m5,0V3m5,0V93m5,0V3")
	 (path :stroke "#000" :stroke-width "2" :stroke-linecap "round" 
	       :d "M18,78l0,0m30,0l0,0m30,0l0,0m0-30l0,0m-30,0l0,0m-30,0l0,0m0-30l0,0m30,0l0,0m30,0l0,0")
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


(insert-image
 (create-image (go-img-string) 'svg t
	       :map '(((circle . ((100 . 100) . 20)) 
		       area1 
		       (pointer hand)))))
 
