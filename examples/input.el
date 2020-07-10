;;; input.el --- Emacs Game Engine example game    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(require 'emacs-game-engine)

;; Initialize the engine with the buffer name and number of cols and rows
(ege:init "EGE Input Example" 80 24)

(setq button-rect '(42 5 10 1))
(setq clicked-count 0)
(setq player-pos-x 40)
(setq player-pos-y 15)

(defun update-clicked-count()
  (ege:draw-text (concat " " (number-to-string clicked-count) " ")
		 69 5
		 '(:background "red" :foreground "white"))
  (let ((mouse-pressed (if ege:mouse-pressed
			   ege:mouse-pressed
			 0)))
  (ege:draw-text (concat " " (number-to-string mouse-pressed) " ")
		 70 3
		 '(:background "red" :foreground "white"))))

(defun update()
  "Game update function."

  ;; KEYBOARD
  ;; ========

  (if ege:key-pressed
      (progn
	(ege:draw-text " yes " 23 1
		       '(:background "red" :foreground "white"))
	
	(ege:draw-rect 21 3 10 1 "-")
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
		     (- player-pos-x 1)))))
    (progn
      (ege:draw-text " no " 23 1
		     '(:background "red" :foreground "white"))
      ))

  (ege:draw-char "@" player-pos-x player-pos-y)

  ;; MOUSE
  ;; =====
  (when (ege:rect-pressed-p button-rect 1)
    (setq clicked-count (+ clicked-count 1))
    (update-clicked-count))

  (ege:draw-rect 59 1 10 1 "-")
  
  (let ((mouse-pos (ege:mouse-position)))
    (ege:draw-text (concat " ("
			   (number-to-string (car mouse-pos)) ","
			   (number-to-string (cdr mouse-pos)) ") ")
		   59 1
		   '(:background "red" :foreground "white")))
  )


;; INITIALIZATION
;; ==============
(ege:draw-text " Is any key pressed: " 2 1
	       '(:background "gray" :foreground "black"))

(ege:draw-text " Last pressed key: " 2 3
	       '(:background "gray" :foreground "black"))

(ege:draw-text " Mouse position: " 42 1
	       '(:background "gray" :foreground "black"))

(ege:draw-text " Last pressed mouse button: " 42 3
	       '(:background "gray" :foreground "black"))

(ege:draw-text " Clicked count: " 53 5
	       '(:background "gray" :foreground "black"))

(ege:draw-button " > BUTTON " button-rect " " nil
		 '(:background "orange" :foreground "black"))

(ege:draw-border 4 10 72 11 "x")

(update-clicked-count)

(ege:register-update 'update 24)
