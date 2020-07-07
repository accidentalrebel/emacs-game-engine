;; Load the game engine

(setq x-pos 0)

(when (file-exists-p "emacs-game-engine.el")
  (load-file "./emacs-game-engine.el"))

(require 'emacs-game-engine)

(ege:init "Example Game" 80 24)

(defun draw_header()
  (ege:place-string-at-area 1 1 "##    
##    
##    
##    
######")
  (ege:place-string-at-area 8 1 "######
  ##  
  ##  
  ##  
######")
  (ege:place-string-at-area 15 1 " #####
##    
 #### 
    ##
##### ")
  (ege:place-string-at-area 22 1 "##### 
##  ##
##### 
##    
##    ")
  (ege:place-string-at-area 32 1 "    ##
    ##
    ##
##  ##
 #### ")
  (ege:place-string-at-area 39 1 "  ##  
 #### 
##  ##
######
##  ##")
  (ege:place-string-at-area 46 1 "##    ##
###  ###
## ## ##
## ## ##
##    ##"))

(defun update()
  (ege:clear_buffer) ; Clear the window at the start of the frame
  (message "here1")

  (let ((inhibit-read-only t))
	(setq x-pos (+ x-pos 1)) ; Move the xposition forward
	(draw_header)
	(ege:place-char-at x-pos 7 "x")))

;; (run-with-timer 1 1 'update)
(ege:register_update 'update 2)
