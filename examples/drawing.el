;;; drawing.el --- Emacs Game Engine example game    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

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

(ege:draw-border 38 3 8 4 "X")

(ege:draw-border 49 3 8 4 "X"
	       '(:foreground "red"))

(ege:draw-border 60 3 8 4 "X"
	       '(:background "red"))

(ege:draw-text " Draw letters " 1 9
	       '(:background "gray" :foreground "black"))

(ege:draw-letter 2 11 ege:key-l "L"
		 '(:foreground "red"))
(ege:draw-letter 10 11 ege:key-i "I"
		 '(:foreground "green"))
(ege:draw-letter 18 11 ege:key-s "S"
		 '(:foreground "blue"))
(ege:draw-letter 26 11 ege:key-p "P"
		 '(:foreground "#ff00ff"))

