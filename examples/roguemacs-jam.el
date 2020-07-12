;;; rougemacs-jam.el --- Roguelike example game for Emacs Game engine    -*- lexical-binding: t -*-

;;; Commentary:
;; Roguemacs-Jam is a roguelike example game for Emacs Game engine.
;; This example was made during the Handmade Network #lispjam.
;; I had to do shortcuts to finish on time.

;;; Code:

(require 'emacs-game-engine)

;; Initialize the engine with the buffer name and number of cols and rows
(ege:init "Example Game" 80 24)


(defvar rog:map
"################################################################################
################################################################################
###             #########           ###############################          ###
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

(defun rog:get-tile-value-at-position (position)
  (let ((col (nth 0 copy-rect))
	(row (nth 1 copy-rect))

	)
  )

(defun rog:draw-map (draw-position copy-rect)
  (let ((copy-col (nth 0 copy-rect))
	(copy-row (nth 1 copy-rect))
	(copy-width (nth 2 copy-rect))
	(copy-height (nth 3 copy-rect)))
    
    )
  )
