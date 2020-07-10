;;; ege-mouse.el --- Emacs Game Engine    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

;; TODO: Double clicking highlights the line. Find a way to disable this.
;; TODO: For buttons to work properly, should make sure that the buffer is on the upper leftmost part of the frame

(defun ege:_setup-mouse-events ()
  (local-set-key [mouse-1] 'ege:_on-mouse-pressed))

(defun ege:mouse-init ()
  (ege:_setup-mouse-events))

(defun ege:_on-mouse-pressed ()
  ;; TODO; remove interactive
  (interactive) 
  (message "Clicked")
  (let ((pos (ege:mouse-position)))
    (message (concat "Pos " (number-to-string (car pos)) ", " (number-to-string (cdr pos))))
    (coordinate-position-point-at (car pos) (cdr pos))))

(defun ege:mouse-position ()
  (cdr (mouse-position)))

(provide 'ege-mouse)
;;; ege-mouse.el ends here
