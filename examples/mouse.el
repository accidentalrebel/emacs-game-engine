;;; keyboard.el --- Emacs Game Engine example game    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(require 'emacs-game-engine)

;; Initialize the engine with the buffer name and number of cols and rows
(ege:init "Example Game" 80 24)

(setq button-pos '(2 2 8 1))

(ege:draw-button " > ON " button-pos "=" " "
		 '(:background "orange" :foreground "black"))
