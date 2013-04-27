;;;
; Rob Lass <rob.lass@gmail.com>
; I wrote this using clisp, it may or may not work with other implementations.
; December 2004
;;;

;;;
; Legend: x = blank, ! = shot, s = ship, h = hit
;;;

; your ships
(defparameter *ocean* '(
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)))

; shots you have fired
(defparameter *combat* '(
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)))

; opponent ship layout
(defparameter *opp-view* '(
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)
(_ _ _ _ _ _ _ _ _ _)))

(defparameter *opp-battleship* '())
(defparameter *my-battleship* '())

(defparameter *opp-carrier* '())
(defparameter *my-carrier* '())

(defparameter *opp-cruiser* '())
(defparameter *my-cruiser* '())

(defparameter *opp-submarine* '())
(defparameter *my-submarine* '())

(defparameter *opp-destroyer* '())
(defparameter *my-destroyer* '())

; prints board for the human player
(defun print-board (combat x)
	(cond ((not (null combat)) (progn 
				(format t "~A ~A~%" x (car combat))
				(print-board (cdr combat) (+ x 1))))))

; prints out my seperator for the two views
(defun print-seperator ()
	(format t "--------------------~%"))

; gets the [x,y] element of [grid]
(defun poll (x y grid)
	(nth x (nth y grid)))
	
; sets position [x,y] on [grid] to [value]
(defun setpos (x y grid value)
	(setf (nth x (nth y grid)) value))

; fires at the position x,y on the grid
(defun fire-at-pos (x y)
	(if (eq (poll x y *opp-view*) '_)
		(progn
			(format t "Miss!~%")
			(setpos x y *opp-view* '!)
			(setpos x y *combat* '!))
		(progn ; this should be an 's position
			(format t "Hit!~%")
			(setpos x y *opp-view* 'h)
			(setpos x y *combat* 'h))))

; print the current state of the board
(defun show-status ()
	(format t " -=[COMBAT VIEW]=- ~%")
	(format t "   0 1 2 3 4 5 6 7 8 9~%")
	(print-board *combat* 0)
	(print-seperator)
	(format t " -=[OCEAN VIEW]=- ~%")
	(format t "   0 1 2 3 4 5 6 7 8 9~%")
	(print-board *ocean* 0))

; read input from the user, and make sure it's a spot on the grid
(defun read-grid-location ()
	(format t "Input spot to fire at in the format X Y~%(ctrl-d to exit)>")
	(let ((val (cons (read) (cons (read) nil))))
		(if (and (numberp (car val)) (numberp (car (cdr val))))
			val
			(progn (format t "~A is not a valid spot.~%" val)
				(read-grid-location)))))

; process input from the user, and ensure the spot can be fired upon
(defun get-input ()
	(let ((move (read-grid-location)))
		(if (eq (poll (car move) (car (cdr move)) *combat*) '_)
				(fire-at-pos (car move) (car (cdr move)))
				(progn 	(format t "Invalid location.~%")
						(get-input)))))

(defun place-ship (x y len orientation grid)
	(if (> len 0)
		(progn
			(setpos x y grid 's)
			(if (eq orientation 'horizontal)
				(place-ship (+ x 1) y (- len 1) orientation grid)
				(place-ship x (+ y 1) (- len 1) orientation grid)))
		nil))

(defun good-ship-location (x y len orientation grid)
	(if (= len 0)
		t
		(if (eq orientation 'horizontal)
			(if (eq (poll x y grid) '_)
				(and t 
					(good-ship-location (+ x 1) y (- len 1) orientation grid))
				nil)
			(if (eq (poll x y grid) '_)
				(and t 
					(good-ship-location x (+ y 1) (- len 1) orientation grid))
				nil))))

; stores the ship in the array
(defun make-ship-list (x y len orientation)
	(if (> len 0)
		(if (eq orientation 'horizontal)
			(cons (list x y) 
				(make-ship-list (+ x 1) y (- len 1) orientation))
			(cons (list x y)
				(make-ship-list x (+ y 1) (- len 1) orientation)))
		nil))

; gets a random place to put a ship that is "good" (non-overlapping)
(defun get-random-spot (len grid)
	; get an orientation
	(if (eq (random 2) 0)
		(setq orientation 'horizontal)
		(setq orientation 'vertical))
	; get an x coordinate
	(if (eq orientation 'horizontal)
		(setq x (random (- 11 len)))
		(setq x (random 10)))
	; get a y coordinate
	(if (eq orientation 'horizontal)
		(setq y (random (- 11 len)))
		(setq y (random 10)))
	; make sure it's ok
	(if (not (good-ship-location x y len orientation grid))
		(get-random-spot len grid)
		(cons x (cons y (cons orientation nil)))))

(defun place-ships-random (grid)
	; carrier 5
	(let (( spot (get-random-spot 5 grid)))
		(progn 
			(place-ship (car spot) (car (cdr spot)) 5 (car (cdr (cdr spot)))
					grid)
			; place this is the correct spot
			(if (eq grid *ocean*)
				(setq *my-carrier*  
					(make-ship-list (car spot) (car (cdr spot)) 5  
							(car (cdr (cdr spot)))))
				(setq *opp-carrier*
					(make-ship-list (car spot) (car (cdr spot)) 5  
							(car (cdr (cdr spot))))))))
	; battleship 4
	(let (( spot (get-random-spot 4 grid)))
		(progn 
			(place-ship (car spot) (car (cdr spot)) 4 (car (cdr (cdr spot))) 
					grid)
			(if (eq grid *ocean*)
				(setq *my-battleship*  
					(make-ship-list (car spot) (car (cdr spot)) 4  
							(car (cdr (cdr spot)))))
				(setq *opp-battleship*
					(make-ship-list (car spot) (car (cdr spot)) 4  
							(car (cdr (cdr spot))))))))
	; cruiser 3
	(let (( spot (get-random-spot 3 grid)))
		(progn
			(place-ship (car spot) (car (cdr spot)) 3 (car (cdr (cdr spot))) 
					grid)
			(if (eq grid *ocean*)
				(setq *my-cruiser*  
					(make-ship-list (car spot) (car (cdr spot)) 3  
							(car (cdr (cdr spot)))))
				(setq *opp-cruiser*
					(make-ship-list (car spot) (car (cdr spot)) 3  
							(car (cdr (cdr spot))))))))
	; submarine 3
	(let (( spot (get-random-spot 3 grid)))
		(progn 
			(place-ship (car spot) (car (cdr spot)) 3 (car (cdr (cdr spot))) 
					grid)
			(if (eq grid *ocean*)
				(setq *my-submarine*
					(make-ship-list (car spot) (car (cdr spot)) 3  
							(car (cdr (cdr spot)))))
				(setq *opp-submarine*
					(make-ship-list (car spot) (car (cdr spot)) 3  
							(car (cdr (cdr spot))))))))
	; destroyer 2
	(let (( spot (get-random-spot 2 grid)))
		(progn 
			(place-ship (car spot) (car (cdr spot)) 2 (car (cdr (cdr spot))) 
				grid)
			(if (eq grid *ocean*)
				(setq *my-destroyer*
					(make-ship-list (car spot) (car (cdr spot)) 2  
							(car (cdr (cdr spot)))))
				(setq *opp-destroyer*
					(make-ship-list (car spot) (car (cdr spot)) 2  
							(car (cdr (cdr spot)))))))))

; returns a computer move
(defun computer-move ()
	(setq x (random 10))
	(setq y (random 10))
	(if (not (eq (poll x y *ocean*) '!))
		(if (eq (poll x y *ocean*) '_)
			(setpos x y *ocean* '!)
			(setpos x y *ocean* 'h))
		(computer-move)))

; figure out if the given boat is sunk
(defun sunk-boat (boat grid)
	(if (eq nil boat)
		t
		(and
			(eq (poll (car (car boat)) (car (cdr (car boat))) grid) 'h)
			(sunk-boat (cdr boat) grid))))

; returns true if all human ships are sunk
(defun all-human-sunk ()
	(and 
		(sunk-boat *my-destroyer* *ocean*)
		(sunk-boat *my-submarine* *ocean*)
		(sunk-boat *my-cruiser* *ocean*)
		(sunk-boat *my-battleship* *ocean*)
		(sunk-boat *my-carrier* *ocean*)))

; returns true if all computer ships are sunk
(defun all-computer-sunk ()
	(and 
		(sunk-boat *opp-destroyer* *combat*)
		(sunk-boat *opp-submarine* *combat*)
		(sunk-boat *opp-cruiser* *combat*)
		(sunk-boat *opp-battleship* *combat*)
		(sunk-boat *opp-carrier* *combat*)))

; returns true if the game is over, nil otherwise
(defun is-done ()
	(if (or (all-human-sunk) (all-computer-sunk))
		t
		nil))

(defun play-game ()
	(show-status)
	(get-input)
	(computer-move)
	(if (is-done)
		nil
		(play-game)))

;this function will run when the player wins
(defun player-wins ()
(format t "#   #   ####   #    #           ####   #    #  #    #  #    #~%")
(format t " # #   #    #  #    #          #       #    #  ##   #  #   #~%")
(format t "  #    #    #  #    #           ####   #    #  # #  #  ####~%")
(format t "  #    #    #  #    #               #  #    #  #  # #  #  #~%")
(format t "  #    #    #  #    #          #    #  #    #  #   ##  #   #~%")
(format t "  #     ####    ####            ####    ####   #    #  #    #~%")


(format t "                #    #   #   #~%")
(format t "                ##  ##    # #~%")
(format t "                # ## #     #~%")
(format t "                #    #     #~%")
(format t "                #    #     #~%")
(format t "                #    #     #~%")
(print-battleship))

; this function will run when the computer wins
(defun computer-wins ()
	(format t "            ###~%")
	(format t "             #              ####     ##    #    #  #    #~%")
	(format t "             #             #        #  #   ##   #  #   #~%")
	(format t "             #              ####   #    #  # #  #  ####~%")
	(format t "             #                  #  ######  #  # #  #  #~%")
	(format t "             #             #    #  #    #  #   ##  #   #~%")
	(format t "            ###             ####   #    #  #    #  #    #~%")

	(format t "                #   #   ####   #    #  #####~%")
	(format t "                 # #   #    #  #    #  #    #~%")
	(format t "                  #    #    #  #    #  #    #~%")
	(format t "                  #    #    #  #    #  #####~%")
	(format t "                  #    #    #  #    #  #   #~%")
	(format t "                  #     ####    ####   #    #~%")
	(print-battleship))

; this function prints out the word "battleship" in obscenely big text
(defun print-battleship ()
(format t "#####     ##     #####   #####  #       ######   ####   #    #     #    #####~%")
(format t "#    #   #  #      #       #    #       #       #       #    #     #    #    #~%")
(format t "#####   #    #     #       #    #       #####    ####   ######     #    #    #~%")
(format t "#    #  ######     #       #    #       #            #  #    #     #    #####~%")
(format t "#    #  #    #     #       #    #       #       #    #  #    #     #    #~%")
(format t "#####   #    #     #       #    ######  ######   ####   #    #     #    #~%"))


(setq *random-state* (make-random-state t))
(place-ships-random *ocean*)
(place-ships-random *opp-view*)
(play-game)
(if (all-computer-sunk)
	(player-wins)
	(computer-wins))
