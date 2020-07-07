;; Load the game engine

(when (file-exists-p "emacs-game-engine.el")
  (load-file "./emacs-game-engine.el"))

(require 'emacs-game-engine)

(ege:init "Example Game" 80 24)

(coordinate-place-string-at-area 1 1 "##    
##    
##    
##    
######")
(coordinate-place-string-at-area 8 1 "######
  ##  
  ##  
  ##  
######")
(coordinate-place-string-at-area 15 1 " #####
##    
 #### 
    ##
##### ")
(coordinate-place-string-at-area 22 1 "##### 
##  ##
##### 
##    
##    ")
(coordinate-place-string-at-area 32 1 "    ##
    ##
    ##
##  ##
 #### ")
(coordinate-place-string-at-area 39 1 "  ##  
 #### 
##  ##
######
##  ##")
(coordinate-place-string-at-area 46 1 "##    ##
###  ###
## ## ##
## ## ##
##    ##")
