;;; rougemacs-jam.el --- Roguelike example game for Emacs Game engine    -*- lexical-binding: t -*-

;;; Commentary:
;; Roguemacs-Jam is a roguelike example game for Emacs Game engine.
;; This example was made during the Handmade Network #lispjam.
;; I had to do shortcuts to finish on time.

;;; Code:

(require 'emacs-game-engine)

;; Initialize the engine with the buffer name and number of cols and rows
(ege:init "Example Game" 80 24)

(defvar rog:map-dimensions '(80 25) "Dimensions of the map")
(defvar rog:map-viewport '(4 5 40 15) "Rect viewport of the map")
(defvar rog:player-pos-x 8)
(defvar rog:player-pos-y 8)

(defvar rog:map
  "                                                                                
                                                                                
  ###############       #############                             ############  
  #.............#####   #...........#  ############ ############# #..........#  
  #.................#   #...........#  #..........# #...........###..........#  
  #.............###.#   #...........#  #..........###........................#  
  #.............# #.#####...........#  #........................###..........#  
  ##.############ #.................#  #..........###...........# #######.####  
   #.#        ###############.#############.####### ###.######### #..........#  
  ##.#####    #.........#   #.#     #...........#     #.........# #..........#  
  #......#    #.........#####.#######...........#     #.........# #..........#  
  #......#    #.................................#####################.########  
  #......#    #.........######################### #...................#         
  ########    ###########                         #.#####.###########.#####
                                                  #.#   #.#         #.#   #
    ###############################################.#####.###    ####.#####     
    #...............................................#.......#    #........#     
    #.###############################################.......#    #........#     
    #..............#       #...............#        #.......#    #........#     
    ##############.#########...............#        #########    #........#     
                 #.........................#                     #........#     
                 ###########...............#                     ##########     
                           #################                                    
                                                                                
                                                                                "
"Non-procedurally generated map.")

(defun rog:get-map-tiles-from-position (position count)
  "Gets the tiles from the given map POSITION.
From that position copies the characters up to COUNT."
  (let* ((col (nth 0 position))
	 (row (nth 1 position))
	 (index (+ (* row (car rog:map-dimensions)) (+ col 1)))
	 (adjusted-index (+ index (- row 1))))
    (substring rog:map
	       adjusted-index
	       (+ adjusted-index count))
    )
  )

(defun rog:draw-map (draw-position copy-rect)
  (let ((draw-col (nth 0 draw-position))
	(draw-row (nth 1 draw-position))
	(copy-col (nth 0 copy-rect))
	(copy-row (nth 1 copy-rect))
	(copy-width (nth 2 copy-rect))
	(copy-height (nth 3 copy-rect))
	(y-index 0))
    (while (< y-index copy-height)
      (message (number-to-string y-index))
      (ege:draw-text (rog:get-map-tiles-from-position (list copy-col (+ copy-row y-index)) copy-width)
		     draw-col
		     (+ draw-row y-index))
      (setq y-index (+ y-index 1))
      )
    )
  )

(defun rog:update()
  "Game update function."

  ;; Erase the player character
  (ege:draw-char "."
		 rog:player-pos-x
		 rog:player-pos-y)

  ;; Check if any key was pressend and move the player
  (cond ((string= ege:key-pressed "<down>")
	 (setq rog:player-pos-y
	       (+ rog:player-pos-y 1)))
	((string= ege:key-pressed "<up>")
	 (setq rog:player-pos-y
	       (- rog:player-pos-y 1))))
  (cond ((string= ege:key-pressed "<right>")
	 (setq rog:player-pos-x
	       (+ rog:player-pos-x 1)))
	((string= ege:key-pressed "<left>")
	 (setq rog:player-pos-x
	       (- rog:player-pos-x 1))))

  ;; Draw player 
  (ege:draw-char "@"
		 rog:player-pos-x
		 rog:player-pos-y
		 '(:foreground "green"))
  )

;; INITIALIZATION
;; ==============

(setq rog:player-pos-x 8)
(setq rog:player-pos-y 8)

;; Draw background
(ege:draw-rect 0 0 80 24 "-"
	       '(:background "#11" :foreground "#666"))

(ege:draw-text " -= ROUGEMACS-JAM =- " 13 1
	       '(:background "#4444FF" :foreground "#ffffff"))
(ege:draw-text " -Version 0.1- " 16 2
	       '(:background "#2222AA" :foreground "#ffffff"))

;; Draw viewport border
(ege:draw-border (- (nth 0 rog:map-viewport) 1)
		 (- (nth 1 rog:map-viewport) 1)
		 (+ (nth 2 rog:map-viewport) 2)
		 (+ (nth 3 rog:map-viewport) 2)
		 "="
		 '(:background "#222266" :foreground "#8888AA"))

;; Draw player details and border
(ege:draw-rect 52 4 22 6 " ")
(ege:draw-border 52 4 22 6 "="
		 '(:background "#222266" :foreground "#8888AA"))

(ege:draw-text "Player name" 54 5)
(ege:draw-text (concat "Level: " (number-to-string 1))
	       54 6)
(ege:draw-text (concat
		"STR:" (number-to-string 1)
		"|AGI:" (number-to-string 1)
		"|INT:" (number-to-string 1))
	       54 7)
(ege:draw-text (concat "Gold: " (number-to-string 0))
	       54 8)

;; Draw messaging border
(ege:draw-rect 50 9 26 12 " ")
(ege:draw-border 50 9 26 12 "="
		 '(:background "#222266" :foreground "#8888AA"))

;; Draw messaging sample text
(ege:draw-text "> You have awaken..."
	       51 10)

;; Draw the map
(message (rog:get-map-tiles-from-position '(2 2) 40))
(rog:draw-map (list (nth 0 rog:map-viewport)
		    (nth 1 rog:map-viewport))
	      (list 0 0
		    (nth 2 rog:map-viewport)
		    (nth 3 rog:map-viewport)))

(ege:register-update 'rog:update 24)
