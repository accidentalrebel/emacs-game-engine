;;; roguemacs-jam.el --- Roguelike example game for Emacs Game engine    -*- lexical-binding: t -*-

;;; Commentary:
;; Roguemacs-Jam is a roguelike example game for Emacs Game engine.
;; This example was made during the Handmade Network #lispjam.
;; I had to do shortcuts to finish on time.

;;; Code:

(require 'emacs-game-engine)

(defvar rog:map-dimensions '(80 25) "Dimensions of the map")
(defvar rog:map-viewport '(4 5 40 15) "Rect viewport of the map")
(defvar rog:player-pos-col 0)
(defvar rog:player-pos-row 0)
(defvar rog:camera-offset-x 0)
(defvar rog:camera-offset-y 0)

(defvar rog:map
"                                                                                
                                                                                
  ###############       #############                             ############  
  #.............#####   #...........#  ############ ############# #..........#  
  #.................#   #...........#  #..........# #...........###..........#  
  #.............###.#   #...........#  #..........###........................#  
  #.............# #.#####...........#  #........................###..........#  
  ##.############ #.................#  #..........###...........# #######.####  
   #.#            ###########.#######  ####.####### #############       #.#     
  ##.#####                  #.#           #.#                       #####.#     
  #......#    ###############.#############.#####                   #.....#     
  #......#    #.................................# ###################.#####     
  #......#    #.........###########.############# #...................#         
  ########    ###########         #.#             #.#####.###########.#         
                                  #.#             #.#   #.#         #.#         
    ###############################.###############.# ###.###    ####.#####     
    #...............................................# #.....#    #........#     
    #..............#########...............########## #.....#    #........#     
    #..............#       #...............#          #.....#    #........#     
    #..............#       #...............#          #######    #........#     
    #..............#       #...............#                     #........#     
    ################       #...............#                     ##########     
                           #################                                    
                                                                                
                                                                                "
"Non-procedurally generated map.")

(defun rog:get-map-tiles-from-position (position count)
  "Gets the tiles from the given map POSITION.
From that position copies the characters up to COUNT."
  (when (and (< (nth 0 position) (nth 0 rog:map-dimensions))
	     (< (nth 1 position) (nth 1 rog:map-dimensions)))
    
    (let* ((col (nth 0 position))
	   (row (nth 1 position))
	   (index (+ (* row (car rog:map-dimensions)) (+ col 1)))
	   (adjusted-index (+ index (- row 1)))
	   (adjusted-index+count (+ adjusted-index count)))

      (when (>= adjusted-index+count (length rog:map))
	(setq adjusted-index+count (length rog:map)))

      (message (concat "Getting substring: "
		       (number-to-string col) ","
		       (number-to-string row) " - "
		       (number-to-string adjusted-index) ","
		       (number-to-string adjusted-index+count) " ? "
		       (number-to-string (length rog:map))))
      
      (substring rog:map
		 adjusted-index
		 adjusted-index+count)
      ))
  )

(defun rog:draw-map (draw-position copy-rect)
  "Draw the map according to the DRAW-POSITION and COPY-RECT.

DRAW-POSITION (The position inside the canvas where it would be displayed.
COPY-RECT (The rectangle where the characters inside the map that will be copied."
  (let ((draw-col (nth 0 draw-position))
	(draw-row (nth 1 draw-position))
	(copy-col (nth 0 copy-rect))
	(copy-row (nth 1 copy-rect))
	(copy-width (nth 2 copy-rect))
	(copy-height (nth 3 copy-rect))
	(y-index 0))
    (while (< y-index copy-height)
      (ege:draw-text (rog:get-map-tiles-from-position (list copy-col
							    (+ copy-row y-index))
						      copy-width)
		     draw-col
		     (+ draw-row y-index))
      (setq y-index (+ y-index 1))
      )
    )
  )

(defun rog:update-map ()
  "Update the drawing of the map based on the player's current position.

Note that the map is the one that is being moved and the player just stays
in the middle of the viewport.  This is the reason for the code below."
  (let* ((viewport-center (rog:get-viewport-center))
	 (map-viewport-col (nth 0 rog:map-viewport))
	 (map-viewport-row (nth 1 rog:map-viewport))
	 (map-viewport-width (nth 2 rog:map-viewport))
	 (map-viewport-height (nth 3 rog:map-viewport))
	 (draw-col (- (nth 0 viewport-center) rog:player-pos-col))
	 (draw-row (- (nth 1 viewport-center) rog:player-pos-row))
	 (copy-col-offset 0)
	 (copy-row-offset 0)
	 (copy-width-offset 0)
	 (copy-height-offset 0))
    
    ;; If the computed drawing location is less than the viewport column
    (when (< draw-col map-viewport-col)
      (setq copy-col-offset (- map-viewport-col draw-col))
      (setq draw-col map-viewport-col))

    ;; If the computed drawing location is less than the viewport row
    (when (< draw-row map-viewport-row)
      (setq copy-row-offset (- map-viewport-row draw-row))
      (setq draw-row map-viewport-row))

    (when (>= draw-col map-viewport-col)
      (setq copy-width-offset (- (+ map-viewport-col map-viewport-width) draw-col)))

    (when (>= draw-row map-viewport-row)
      (setq copy-height-offset (- (+ map-viewport-row map-viewport-height) draw-row)))

    (rog:draw-map (list draw-col draw-row)
		  (list copy-col-offset
			copy-row-offset
			copy-width-offset
			copy-height-offset))))

(defun rog:add-message (str)
  "Add a message STR to the message queue UI.

This reads the contents of the message queue and moves 
the rectangle one row down.

Then it adds the STR at the first line."
  (when (ege:in-game-buffer-p)
    (let ((inhibit-read-only t))
      (coordinate-place-string-at-area 51 11 (coordinate-get-string-at-area 51 10 24 9))
      (ege:draw-rect 51 10 24 1 " ")
      (coordinate-place-string-at-area 51 10 str)
      ))
  )

(defun rog:can-player-move (col row)
  "Check if the COL and ROW are valid for the player to move."
  (let ((map-tiles-from-position (rog:get-map-tiles-from-position (list col row) 1)))
    (when map-tiles-from-position
      (not (string= map-tiles-from-position
		    "#")))))

(defun rog:move-player (x-offset y-offset)
  "Move the player by X-OFFSET and Y-OFFSET.

It either moves the position of the player character or
adjusts the camera."
  (let ((col (+ rog:player-pos-col x-offset))
	(row (+ rog:player-pos-row y-offset)))
    (when (rog:can-player-move col row)

      ;; Add a movement message
      (cond ((> x-offset 0)
	     (rog:add-message "> Moved east"))
	    ((< x-offset 0)
	     (rog:add-message "> Moved west"))
	    ((> y-offset 0)
	     (rog:add-message "> Moved south"))
	    ((< y-offset 0)
	     (rog:add-message "> Moved north")))

      ;; Update the player position
      (setq rog:player-pos-col col)
      (setq rog:player-pos-row row)

      ;; Move the camera offsets
      (cond ((not (= x-offset 0))
	     (setq rog:camera-offset-x (+ rog:camera-offset-x x-offset)))
	    ((not (= y-offset 0))
	     (setq rog:camera-offset-y (+ rog:camera-offset-y y-offset))))

      (rog:update-map))))

(defun rog:get-viewport-center ()
    (list (+ (nth 0 rog:map-viewport) (/ (nth 2 rog:map-viewport) 2))
	  (+ (nth 1 rog:map-viewport) (/ (nth 3 rog:map-viewport) 2))))

(defun rog:update()
  "Game update function."

  ;; Check if any key was pressed and move the player
  (cond ((string= ege:key-pressed "<down>")
	 (rog:move-player 0 1))
	((string= ege:key-pressed "<up>")
	 (rog:move-player 0 -1)))
  (cond ((string= ege:key-pressed "<right>")
	 (rog:move-player 1 0))
	((string= ege:key-pressed "<left>")
	 (rog:move-player -1 0)))

  ;; Draw player
  (let ((viewport-center (rog:get-viewport-center)))
    (ege:draw-char "@"
		   (nth 0 viewport-center)
		   (nth 1 viewport-center)
		   '(:foreground "green")))

  ;; Move the cursor away
  (coordinate-position-point-at 0 0)
  )

(defun roguemacs-start ()
  "Start the game."
  (interactive)

  ;; Initialize the engine with the buffer name and number of cols and rows
  (ege:init "ROGUEMACS JAM" 80 24)
  
  (setq rog:init-player-pos-col 4)
  (setq rog:init-player-pos-row 5)

  (setq rog:player-pos-col rog:init-player-pos-col)
  (setq rog:player-pos-row rog:init-player-pos-row)
  (setq rog:camera-offset-x 0)
  (setq rog:camera-offset-y 0)

  ;; Draw background
  (ege:draw-rect 0 0 80 24 "-"
		 '(:background "#11" :foreground "#666"))

  (ege:draw-text " -= ROGUEMACS-JAM =- " 13 1
		 '(:background "#4444FF" :foreground "#ffffff"))
  (ege:draw-text " -Version 0.1- " 16 2
		 '(:background "#2222AA" :foreground "#ffffff"))

  ;; Draw viewport background and border
  (let ((map-viewport-col (nth 0 rog:map-viewport))
	(map-viewport-row (nth 1 rog:map-viewport))
	(map-viewport-width (nth 2 rog:map-viewport))
	(map-viewport-height (nth 3 rog:map-viewport)))
    (ege:draw-border (- map-viewport-col 1)
		     (- map-viewport-row 1)
		     (+ map-viewport-width 2)
		     (+ map-viewport-height 2)
		     "="
		     '(:background "#222266" :foreground "#8888AA"))
    (ege:draw-rect map-viewport-col
		   map-viewport-row
		   map-viewport-width
		   map-viewport-height
		   " "))

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
  (rog:update-map)

  (ege:register-update 'rog:update 10))
