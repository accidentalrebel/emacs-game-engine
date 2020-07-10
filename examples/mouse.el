;;; keyboard.el --- Emacs Game Engine example game    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(require 'emacs-game-engine)

;; Initialize the engine with the buffer name and number of cols and rows
(ege:init "Example Game" 80 24)

(setq button-rect '(2 6 10 1))
(setq clicked-count 0)

(defun update-clicked-count()
  (ege:draw-text (concat " Clicked count: " (number-to-string clicked-count) " ")
		 13 6
		 '(:background "gray" :foreground "black"))
  (let ((mouse-pressed (if ege:mouse-pressed
			   ege:mouse-pressed
			 0)))
  (ege:draw-text (concat " " (number-to-string mouse-pressed) " ")
		 30 3
		 '(:background "red" :foreground "white"))))

(defun update()
  (when (ege:rect-pressed-p button-rect 1)
    (setq clicked-count (+ clicked-count 1))
    (update-clicked-count))

  (let ((mouse-pos (ege:mouse-position)))
    (ege:draw-text (concat " ("
			   (number-to-string (car mouse-pos)) ","
			   (number-to-string (cdr mouse-pos)) ") ")
		   19 1
		   '(:background "red" :foreground "white")))
  )

;; INITIALIZATION
;; ==============
(ege:draw-text " Mouse position: " 2 1
	       '(:background "gray" :foreground "black"))

(ege:draw-text " Last pressed mouse button: " 2 3
	       '(:background "gray" :foreground "black"))

(ege:draw-button " > BUTTON " button-rect " " nil
		 '(:background "orange" :foreground "black"))

(update-clicked-count)

(ege:register-update 'update 24)
