(in-package :pong)
(export 'run)

(defun run ()
  (unwind-protect (glut:display-window (make-instance 'pong))
    (setf glut::*glut-initialized-p* nil)))

;; got balls?
(defclass ball (cairo-game-object)
  ((dx :initform 5)
   (dy :initform 5))
  (:default-initargs :width 40 :height 40 :mass 1))

(defmethod collision-hull ((obj ball))
  (list 
   (squirl:make-circle (/ (width obj) 2)
                       :center (squirl:vec 0 0)
                       :restitution 0.5 :friction 0.5)))

(defmethod redraw ((obj ball))
  (cairo:set-source-rgb 1 1 1)
  (cairo:arc 0.5 0.5 0.5 0 (cairo:deg-to-rad 360))
  (cairo:fill-path)
  (cairo:set-line-width 0.05)
  (cairo:move-to 0.2 0.2)
  (cairo:line-to 0.8 0.8)
  (cairo:move-to 0.2 0.8)
  (cairo:line-to 0.8 0.2)
  (cairo:set-source-rgb 0.3 0.3 0.4)
  (cairo:stroke)
)

(defclass paddle (cairo-game-object) 
  ((rjoint :initform nil :accessor paddle-rjoint))
  (:default-initargs :width 100 :height 10 :prescale nil :mass 1))

;; shameful hack!
(defmethod rotate-paddle ((pad paddle) angle)
  (setf (squirl::rotary-limit-joint-max (paddle-rjoint pad)) (deg->rad angle))
  (setf (squirl::rotary-limit-joint-min (paddle-rjoint pad)) (deg->rad angle)))

(defmethod collision-hull ((obj paddle))
  (list (squirl:make-rectangle (width obj) (height obj) :friction 1 :restitution 0)))

(defmethod redraw ((obj paddle))
  (cairo:set-line-width 2)
  (cairo:set-source-rgb 1 1 1)
  (let ((w (width obj))
        (h (height obj))
        (rad 7))
    (cairo:move-to rad 0)
    (cairo:line-to (- w rad) 0)
    (cairo:curve-to w 0 w 0 w rad)
    (cairo:line-to w (- h rad))
    (cairo:curve-to w h w h (- w rad) h)
    (cairo:line-to rad h) 
    (cairo:curve-to 0 h 0 h 0 (- h rad))
    (cairo:line-to 0 rad)
    (cairo:curve-to 0 0 0 0 rad 0)
    (cairo:stroke)
    ;; dot
    (cairo:arc 5 (/ h 2) 2.5 0 (deg->rad 360))
    (cairo:fill-path)))

(defclass playfield (cairo-game-object) 
  ((opening :initform 235 :reader playfield-opening))
  (:default-initargs :prescale nil))

(defmethod create-paddles ((obj playfield) (game game))
  (with-slots (opening) obj
    (let* ((left-pin  (- (/ (width obj) 2) (/ opening 2)))
           (right-pin (+ left-pin opening))
           (left (make-instance 'paddle :pos-x (+ (pos-x obj) left-pin)
                                        :pos-y (+ (pos-y obj) (height obj))
                                        :rotation 20))
           (right (make-instance 'paddle :pos-x (+ (pos-x obj) right-pin)
                                         :pos-y (+ (pos-y obj) (height obj))
                                         :rotation 180))           
           (pb (physics-body obj))
           (conn-left  (squirl:make-pivot-joint pb (physics-body left) (squirl:vec (- (/ opening 2)) (/ (height obj) 2))
                                                (squirl:vec (- (/ (width left) 2)) 0)))
           (conn-right (squirl:make-pivot-joint pb (physics-body right) (squirl:vec (/ opening 2) (/ (height obj) 2))
                                                (squirl:vec (- (/ (width right) 2)) 0)))
           (rot-left  (squirl:make-rotary-limit-joint pb (physics-body left) (deg->rad 20) (deg->rad 20)))
           (rot-right (squirl:make-rotary-limit-joint pb (physics-body right) (deg->rad 160) (deg->rad 160))))
      (setf (paddle-rjoint left) rot-left)
      (setf (paddle-rjoint right) rot-right)
      (squirl:world-add-constraint (physics-world game) rot-left)
      (squirl:world-add-constraint (physics-world game) conn-left)
      (squirl:world-add-constraint (physics-world game) conn-right)
      (squirl:world-add-constraint (physics-world game) rot-right)
      (values left right))))
(defmethod collide ((obj1 playfield) (obj2 paddle) arb) nil)

(defmethod collision-hull ((obj playfield))
  (with-slots (opening) obj
    (let* ((w/2 (/ (width obj) 2))
           (h/2 (/ (height obj) 2)))
      (list (squirl:make-segment (squirl:vec (- w/2) (- h/2))   (squirl:vec (- (/ opening 2)) h/2) :friction 1 :restitution 1)
            (squirl:make-segment (squirl:vec (/ opening 2) h/2) (squirl:vec w/2 (- h/2)) :friction 1 :restitution 1)))))

(defmethod redraw ((obj playfield))
  (with-slots (opening) obj
    (let* ((open-left (- (/ (width obj) 2) (/ opening 2)))
           (open-right (+ open-left opening)))
      ;; walls
      (cairo:set-source-rgb 0.4 0.5 0.5)
      (cairo:set-line-width 3)
      (cairo:move-to 0 0)
      (cairo:line-to open-left (height obj))
      (cairo:move-to open-right (height obj))
      (cairo:line-to (width obj) 0)
      (cairo:stroke)
      ;; penalty area
      (cairo:set-source-rgb 0.4 0.2 0.2)
      (cairo:set-dash 0 '(10 4))
      (let ((w/8 (/ (width obj) 8))
            (h/2 (/ (height obj) 2)))
        (cairo:move-to (* 2 w/8) h/2)
        (cairo:curve-to (* 3 w/8) 0 (* 5 w/8) 0 (* 6 w/8) h/2)
        (cairo:stroke)))))

(defclass pong (game) 
  ((playfield :initform (make-instance 'playfield :pos-x 60 :pos-y 300 :width 900 :height 200))
   paddle-left paddle-right)
  (:default-initargs :title "pong" :width 800 :height 800 
                     :game-mode nil :framerate 70 :gravity (squirl:vec 0 300)))

(defmethod move-paddle ((game pong) pad dir)
  (with-slots (paddle-left paddle-right) game
    (if (eq pad :left)
        (rotate-paddle paddle-left (if (eq dir :up) -40 20))
        (rotate-paddle paddle-right (if (eq dir :up) 220 160)))))

(defmethod key-up ((game pong) key)
;  (format t "kup: ~a~%" key)
    (case key
      ((#\a) (move-paddle game :left :down))
      ((#\d) (move-paddle game :right :down))))

(defmethod key-down ((game pong) key)
;  (format t "kdown: ~a~%" key)
  (case key
    ((#\a) (move-paddle game :left :up))
    ((#\d) (move-paddle game :right :up))))

(defmethod initial-objects ((game pong))
  (let ((playfield (slot-value game 'playfield)))
    (multiple-value-bind (left right)
        (create-paddles playfield game)
      (setf (slot-value game 'paddle-left) left)
      (setf (slot-value game 'paddle-right) right)
      (list (make-instance 'ball :pos-x 300 :pos-y 40 );:velocity (squirl:vec 0 200))
            left right playfield))))