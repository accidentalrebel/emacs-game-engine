;;; rougemacs-jam.el --- Roguelike example game for Emacs Game engine    -*- lexical-binding: t -*-

;;; Commentary:
;; Roguemacs-Jam is a roguelike example game for Emacs Game engine.
;; This example was made during the Handmade Network #lispjam.
;; I had to do shortcuts to finish on time.

;;; Code:

(require 'emacs-game-engine)

;; Initialize the engine with the buffer name and number of cols and rows
;; (ege:init "Example Game" 80 24)


(defvar rog:map
"################################################################################
################################################################################
ABCDE           #########           ###############################          ###
###                  ####           ####           ##           ###          ###
###             ###  ####           ####           ##                        ###
###             ###  ####           ####                        ###          ###
#### ##############                 ####           ##           #########  #####
###      #################### ############  ########### ###########          ###
###      ######         ##### ######           ########        ####          ###
###############                                ########        ####          ###
###############         ############           #################################
################################################################################"
"Non-procedurally generated map.")

(defun rog:get-map-tiles-from-position (position count)
  "Gets the tiles from the given map POSITION.
From that position copies the characters up to COUNT."
  (let* ((col (nth 0 position))
	 (row (nth 1 position))
	 (index (+ (* row 80) (+ col 1)))
	 (adjusted-index (+ index (- row 1))))
    (substring rog:map
	       adjusted-index
	       (+ adjusted-index count))
    )
  )

(defun rog:draw-map (draw-position copy-rect)
  (let ((copy-col (nth 0 copy-rect))
	(copy-row (nth 1 copy-rect))
	(copy-width (nth 2 copy-rect))
	(copy-height (nth 3 copy-rect)))
    
    )
  )

(message (rog:get-map-tiles-from-position '(2 2) 40))
