(in-package :pong)
(export 'run)

(defun run ()
  (glut:display-window (make-instance 'pong)))

(defclass paddle (cairo-game-object) ()
  (:default-initargs :width 20 :height 100 :prescale nil :mass 0))

(defmethod redraw ((obj paddle))
  (cairo:set-source-rgba 1 1 1 1)
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
    (cairo:curve-to 0 0 0 0 rad 0))
  (cairo:fill-path))

;; got balls?
(defclass ball (cairo-game-object)
  ((dx :initform 5)
   (dy :initform 5))
  (:default-initargs :width 40 :height 40 :mass 4))

(defmethod collision-hull ((obj ball))
  (list 
   (squirl:make-circle (/ (width obj) 2)
                       :center (squirl:vec 0 0)
                       :restitution 1 :friction 0.8)))

(defmethod redraw ((obj ball))
  (cairo:set-source-rgb 1 1 1)
  (cairo:arc 0.5 0.5 0.5 0 (cairo:deg-to-rad 360))
  (cairo:fill-path)
  (cairo:set-line-width 0.2)
  (cairo:move-to 0.2 0.2)
  (cairo:line-to 0.8 0.8)
  (cairo:move-to 0.2 0.8)
  (cairo:line-to 0.8 0.2)
  (cairo:set-source-rgb 0.3 0.3 0.4)
  (cairo:stroke)
)

(defclass playfield (cairo-game-object) ()
  (:default-initargs :width 500 :height 500))

(defmethod collision-hull ((obj playfield))
  (let* ((w (width obj))
         (h (height obj))
         (w/2 (/ w 2))
         (h/2 (/ h 2)))
  (list 
     (squirl:make-segment (squirl:vec (- w/2) (- h/2)) (squirl:vec w/2 (- h/2))     :friction 1 :restitution 1 :radius 13)
     (squirl:make-segment (squirl:vec w/2 (- h/2))     (squirl:vec w/2 h/2)         :friction 1 :restitution 1 :radius 13)
     (squirl:make-segment (squirl:vec w/2 h/2)         (squirl:vec (- w/2) h/2)     :friction 1 :restitution 1 :radius 13)
     (squirl:make-segment (squirl:vec (- w/2) h/2)     (squirl:vec (- w/2) (- h/2)) :friction 1 :restitution 1 :radius 13)
     )))

(defmethod redraw ((obj playfield))
  (cairo:set-source-rgb 1 0 0)
  (cairo:set-line-width 0.05)
  (cairo:rectangle 0 0 1 1)
  (cairo:stroke))

(defclass pong (game) 
  ((ball :initform (make-instance 'ball :pos-x 300 :pos-y 200))
   (paddle1 :initform nil)
   (paddle2 :initform (make-instance 'paddle))
   (p1-score :initform 0)
   (p2-score :initform 0))
  (:default-initargs :title "pong" :width 800 :height 800 
                     :game-mode nil :framerate 70 :gravity (squirl:vec 0 100)))

(defmethod key-down ((game pong) key)
  (with-slots (paddle2) game
    (case key
      ((:key-up)   (squirl:body-apply-force (physics-body paddle2) (squirl:vec 0 -3) (squirl:vec 0 0)))
      ((:key-down) (squirl:body-apply-force (physics-body paddle2) (squirl:vec 0 3) (squirl:vec 0 0))))))

(defmethod initial-objects ((game pong))
  (list 
   (slot-value game 'ball)
;   (slot-value game 'paddle2)
   (make-instance 'playfield :pos-x 20 :pos-y 20 :rotation 5)
   ))