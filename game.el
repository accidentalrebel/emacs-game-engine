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
  (ege:draw-letter 1 1 ege:key-l "X" '(:background "blue" :foreground "white"))
  (ege:draw-letter 8 1 ege:key-i "X" '(:background "yellow" :foreground "black"))
  (ege:draw-letter 15 1 ege:key-s "X" '(:background "green" :foreground "black"))
  (ege:draw-letter 22 1 ege:key-p "X" '(:background "#ff00ff" :foreground "black"))
  (ege:draw-letter 32 1 ege:key-j "X" '(:foreground "red"))
  (ege:draw-letter 39 1 ege:key-a "X" '(:foreground "green"))
  (ege:draw-letter 46 1 ege:key-m "X" '(:foreground "blue")))

(defun update()
  "Game update function."
  ;;(ege:clear-buffer) ; Clear the window at the start of the frame

  (ege:place-string-at-area 10 10 player-sprite)
  
  (ege:place-string-at-area x-pos 10 "---
---
---
---
---")
  (let ((x (+ x-pos 1)))
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
