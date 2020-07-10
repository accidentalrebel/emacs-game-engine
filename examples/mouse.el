;;; keyboard.el --- Emacs Game Engine example game    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(require 'emacs-game-engine)

;; Initialize the engine with the buffer name and number of cols and rows
(ege:init "Example Game" 80 24)

(setq button-rect '(2 2 10 1))
(setq clicked-count 0)

(ege:draw-button " > BUTTON " button-rect " " nil
		 '(:background "orange" :foreground "black"))

(defun update-clicked-count()
  (ege:draw-text (concat " Clicked count: " (number-to-string clicked-count) " ")
		 13 2
		 '(:background "gray" :foreground "black")))

(defun update()
  (when (ege:rect-pressed-p button-rect 1)
    (setq clicked-count (+ clicked-count 1))
    (update-clicked-count)
    )
)

(update-clicked-count)

(ege:register-update 'update 24)
