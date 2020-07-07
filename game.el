;;; game.el --- Emacs Game Engine example game    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

;; Load the game engine library
;; If the file is in another direcotry then change it here
(when (file-exists-p "emacs-game-engine.el")
  (load-file "./emacs-game-engine.el"))

(require 'emacs-game-engine)

;; Initialize the engine with the buffer name and number of cols and rows
(ege:init "Example Game" 80 24)

;; VARIABLES
;; =========
(setq x-pos 0)

;; HELPERS
;; =======
(defun draw_header()
  "Draw the header texts."
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
  "Game update function."
  (ege:clear_buffer) ; Clear the window at the start of the frame
  (message "here1")

  (let ((inhibit-read-only t)
	(x (+ x-pos 1)))
    (if (>= x ege:buffer-cols)
	(setq x-pos 0)
	(setq x-pos x))
    (draw_header)
    (ege:place-char-at x-pos 7 "x")))

(ege:register_update 'update 24)

;;; game.el ends here
