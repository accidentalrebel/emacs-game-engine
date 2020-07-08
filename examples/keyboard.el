;;; keyboard.el --- Emacs Game Engine example game    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(require 'emacs-game-engine)

;; Initialize the engine with the buffer name and number of cols and rows
(ege:init "Example Game" 80 24)

(setq player-pos-x 40)
(setq player-pos-y 12)

(ege:draw-text " Is any key pressed: " 2 1
	       '(:background "gray" :foreground "black"))

(ege:draw-text " Last pressed key: " 2 3
	       '(:background "gray" :foreground "black"))

(defun update()
  "Game update function."

  (when ege:key-pressed

    (ege:draw-rect 21 3 8 1 "-")
    (ege:draw-text (concat " " ege:key-pressed " ")
		   21 3
		   '(:background "red" :foreground "white"))
		   
    (ege:draw-char "*" player-pos-x player-pos-y)
    
    (cond ((or (string= ege:key-pressed "s")
	       (string= ege:key-pressed "<down>"))
	   (setq player-pos-y
		 (+ player-pos-y 1)))
	  ((or (string= ege:key-pressed "w")
	       (string= ege:key-pressed "<up>"))
	   (setq player-pos-y
		 (- player-pos-y 1))))
    (cond ((or (string= ege:key-pressed "d")
	       (string= ege:key-pressed "<right>"))
	   (setq player-pos-x
		 (+ player-pos-x 1)))
	  ((or (string= ege:key-pressed "a")
	       (string= ege:key-pressed "<left>"))
	   (setq player-pos-x
		 (- player-pos-x 1))))

    (ege:draw-char "@" player-pos-x player-pos-y))
  )

(ege:register-update 'update 24)
