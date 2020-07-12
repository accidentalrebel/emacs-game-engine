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

(defvar rog:map
  "                                                                                
                                                                                
  ###############       #############                             ############  
  #.............######  #...........#  ########################## #..........#  
  #..................#  #...........#  #...........##...........###..........#  
  #.............###..#  #...........#  #...........##........................#  
  #.............# #..####...........#  #........................###..........#  
  ##.############ #.................#  #...........##...........# #######..###  
  #......#    ###############.############..###########.######### #..........#  
  #......#    #.........#####.######...........#      #........#  #..........#  
  ########    #................................#      #........#  #..........#  
              #.........############...........######################.########  
              ###########          ################...................#         
                                                  #.##### ########### #####
                                                  #.#   # #         # #   #
    ###############################################.#####.###    ####.#####     
    #...............................................#.......#    #........#     
    #.###############################################.......#    #........#     
    #...............#      #...............#        #.......#    #........#     
    ##############..########...............#        #########    #........#     
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

(ege:draw-border (- (nth 0 rog:map-viewport) 1)
		 (- (nth 1 rog:map-viewport) 1)
		 (+ (nth 2 rog:map-viewport) 2)
		 (+ (nth 3 rog:map-viewport) 2)
		 "x")

(message (rog:get-map-tiles-from-position '(2 2) 40))
(rog:draw-map (list (nth 0 rog:map-viewport)
		    (nth 1 rog:map-viewport))
	      (list 0 0
		    (nth 2 rog:map-viewport)
		    (nth 3 rog:map-viewport)))
