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


(defun update()
  (when (ege:rect-pressed-p button-rect 1)
    (message "BUTTON CLICKED")
    )
  )

(ege:register-update 'update 24)
