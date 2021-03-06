# emacs-game-engine
A minimal game engine for making games on and for Emacs.

This is my entry for the [Lisp Jam](https://handmade.network/blogs/p/7390-%5Bnews%5D_lisp_jam,_hms_2020,_showcase_streams,_podcast_episodes#23034) at the Handmade Network Discord.

![preview-1](assets/preview.png)

Watch the video [here](https://youtu.be/yuBNyhwqdWQ).

## Features 
The engine offers functions that allows the following features
- [x] Buffer management
- [x] Draw canvas setup
- [x] Basic text-based drawing functions
- [x] Keyboard input 
- [x] Mouse input
- [x] Audio output (Audio playing via external programs)

## Why?
I [made a couple](https://github.com/accidentalrebel/emacs-tic-tac-toe-improved) of [games for emacs](https://github.com/accidentalrebel/emacs-dice-kingdoms). I had a lot of fun making those so I wanted to make a game engine that would allow me to make more.

## Sample Code
A simple example program that detects keyboard movement and moves a player on the screen

```emacs-lisp
(require 'emacs-game-engine)

;; Initialize the engine with the buffer name and number of cols and rows
(ege:init "Example Game" 80 24)

(setq player-pos-x 40)
(setq player-pos-y 12)

(ege:draw-text " Is any key pressed: " 2 1
	       '(:background "gray" :foreground "black"))

(ege:draw-text " Last pressed key: " 2 3
	       '(:background "gray" :foreground "black"))

(defun update()
  "Game update function."

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

    (ege:draw-char "@" player-pos-x player-pos-y))

;; Registers the update function and sets the FPS to 24
(ege:register-update 'update 24)
```

There are more code examples in the [examples folder](https://github.com/accidentalrebel/emacs-game-engine/tree/master/examples). To run the examples, open the `.el` file and run the `eval-bufer` emacs command. Additionally, for the `roguemacs-jam.el` example, you also need to run the `roguemacs-start` command.

## Dependencies

* [coordinate.el](https://github.com/accidentalrebel/coordinate.el) - A separate emacs library I made that provides convenience functions for editing buffers through col and row coondinates.
