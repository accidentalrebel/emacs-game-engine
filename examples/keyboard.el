;;; keyboard.el --- Emacs Game Engine example game    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(require 'emacs-game-engine)

;; Initialize the engine with the buffer name and number of cols and rows
(ege:init "Example Game" 80 24)

(setq player-pos-x 0)
(setq player-pos-y 0)

(defun update()
  "Game update function."

  (ege:draw-char "*" player-pos-x player-pos-y)

  (when ege:last-key-pressed
    (cond ((or (string= ege:last-key-pressed "s")
	       (string= ege:last-key-pressed "<down>"))
	   (setq player-pos-y
		 (+ player-pos-y 1)))
	  ((or (string= ege:last-key-pressed "w")
	       (string= ege:last-key-pressed "<up>"))
	   (setq player-pos-y
		 (- player-pos-y 1))))
    (cond ((or (string= ege:last-key-pressed "d")
	       (string= ege:last-key-pressed "<right>"))
	   (setq player-pos-x
		 (+ player-pos-x 1)))
	  ((or (string= ege:last-key-pressed "a")
	       (string= ege:last-key-pressed "<left>"))
	   (setq player-pos-x
		 (- player-pos-x 1)))))

  (ege:draw-char "@" player-pos-x player-pos-y)

  (message "Hello");
  )

(ege:register-update 'update 24)
