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
(setq player-sprite
      "   o   
 o x o 
o xxx o
 o x o
   o   ")

;; HELPERS
;; =======
(defun draw-header()
  "Draw the header texts."
  (ege:place-string-at-area 1 1 "##    
##    
##    
##    
######" '(:background "blue" :foreground "white"))
  (ege:place-string-at-area 8 1 "######
  ##  
  ##  
  ##  
######" '(:background "yellow" :foreground "black"))
  (ege:place-string-at-area 15 1 " #####
##    
 #### 
    ##
##### " '(:background "green" :foreground "black"))
  (ege:place-string-at-area 22 1 "##### 
##  ##
##### 
##    
##    " '(:background "#ff00ff" :foreground "black"))
  (ege:place-string-at-area 32 1 "    ##
    ##
    ##
##  ##
 #### " '(:foreground "red"))
  (ege:place-string-at-area 39 1 "  ##  
 #### 
##  ##
######
##  ##" '(:foreground "green"))
  (ege:place-string-at-area 46 1 "##    ##
###  ###
## ## ##
## ## ##
##    ##" '(:foreground "blue")))

(defun update()
  "Game update function."
  ;;(ege:clear-buffer) ; Clear the window at the start of the frame

  (ege:place-string-at-area 10 10 player-sprite)
  
  (ege:place-string-at-area x-pos 10 "---
---
---
---
---")
  (let ((inhibit-read-only t)
	(x (+ x-pos 1)))
    (if (>= x (- ege:buffer-cols 3))
	(setq x-pos 0)
	(setq x-pos x))
    (ege:place-string-at-area x-pos 10 ">->>--
->->>-
-->->>
->->>-
>->>--")))

;; INITIALIZATION
;; ==============
(draw-header)
(ege:draw-rect 56 1 22 5 "x");

(ege:register-update 'update 24)

;;; game.el ends here
