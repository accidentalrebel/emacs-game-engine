;;; ege-draw.el --- Emacs Game Engine    -*- lexical-binding: t -*-

;;; Commentary:
;; 

;;; Code:

(defvar ege:key-a "  ##  
 #### 
##  ##
######
##  ##")
(defvar ege:key-i "######
  ##  
  ##  
  ##  
######")
(defvar ege:key-j "    ##
    ##
    ##
##  ##
 #### ")
(defvar ege:key-l "##    
##    
##    
##    
######")
(defvar ege:key-m "##    ##
###  ###
## ## ##
## ## ##
##    ##")
(defvar ege:key-p "##### 
##  ##
##### 
##    
##    ")
(defvar ege:key-s " #####
##    
 #### 
    ##
##### ")

(defun ege:draw-letter(col row letter char &optional attributes)
  (when (ege:in-game-buffer-p)
    (let ((inhibit-read-only t))
      (coordinate-place-string-at-area col row
				       (subst-char-in-string ?#
							     (string-to-char char)
							     letter)
				       attributes))))

(defun ege:draw-rect(col row width height char &optional attributes)
  "Draw a rectangle at COL and ROW with WIDTH and HEIGHT.

Displays CHAR with optional ATTRIBUTES.
This is just a wrapper for 'coordinate-place-char-at-area'."
  (when (ege:in-game-buffer-p)
    (let ((inhibit-read-only t))
      (coordinate-place-char-at-area col row width height char attributes))))

(defun ege:draw-border(col row width height char &optional attributes)
  "Draw a rectangle at COL and ROW with WIDTH and HEIGHT.

Displays CHAR with optional ATTRIBUTES."
  (when (ege:in-game-buffer-p)
    (let ((inhibit-read-only t))
      (coordinate-place-char-as-border col row width height char attributes))))

(defun ege:draw-text(str col row &optional attributes)
  "Draws the STR at COL and ROW with optional ATTRIBUTES."
  (when (ege:in-game-buffer-p)
    (let ((inhibit-read-only t))
      (coordinate-place-string-at-area col row str attributes))))

(defun ege:draw-char(char col row &optional attributes)
  "Draws the CHAR at COL and ROW with optional ATTRIBUTES."
  (when (ege:in-game-buffer-p)
    (let ((inhibit-read-only t))
      (coordinate-place-char-at col row char attributes))))

(defun ege:draw-button(str rect bg-char border-char &optional attributes)
  "A convenience function that draws a button.
Draws a button with STR as text and RECT as dimensions.

BG-CHAR for the character for the background.

BORDER-CHAR for drawing the border.
If BORDER-CHAR is nil, don't show a border.

Uses ATTRIBUTES on the button rectangle including the text.
Uses the other draw functions."
  (let ((col (nth 0 rect))
	(row (nth 1 rect))
	(width (nth 2 rect))
	(height (nth 3 rect)))
    (when border-char
      (ege:draw-border col row width height border-char attributes))
    (ege:draw-rect col row width height bg-char attributes)
    (ege:draw-text str col row attributes))
  )

(provide 'ege-draw)
;;; ege-draw.el ends here
