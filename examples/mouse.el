;;; keyboard.el --- Emacs Game Engine example game    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(require 'emacs-game-engine)

;; Initialize the engine with the buffer name and number of cols and rows
(ege:init "Example Game" 80 24)

(setq button-rect '(2 2 8 1))

(ege:draw-button " > ON " button-rect " " nil
		 '(:background "orange" :foreground "black"))

(defun ege:point-in-rect-p (coordinate rect)
  (let ((col (nth 0 rect))
	(row (nth 1 rect))
	(width (nth 2 rect))
	(height (nth 3 rect))
	(point-x (car coordinate))
	(point-y (cdr coordinate)))
    (and (>= point-x col)
	 (<= point-x (+ col width))
	 (>= point-y row)
	 (<= point-y (+ row height))))
  )

(defun update()
  (when (ege:point-in-rect-p
  	 (ege:mouse-position) button-rect)
    (message "BUTTON CLICKED")
    )
  )

(ege:register-update 'update 24)
