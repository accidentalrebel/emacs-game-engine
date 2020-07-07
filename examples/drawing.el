;;; drawing.el --- Emacs Game Engine example game    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

;; Load the game engine library
;; If the file is in another direcotry then change it here
(when (file-exists-p "../../emacs-game-engine.el")
  (load-file "../../emacs-game-engine.el"))

(require 'emacs-game-engine)

;; Initialize the engine with the buffer name and number of cols and rows
(ege:init "Example Game" 80 24)

(ege:draw-text " Draw rect (8x4) " 1 1 
	       '(:background "gray" :foreground "black"))

(ege:draw-rect 2 3 8 4 "X")

(ege:draw-rect 13 3 8 4 "X"
	       '(:foreground "red"))

(ege:draw-rect 24 3 8 4 "X"
	       '(:background "red"))
