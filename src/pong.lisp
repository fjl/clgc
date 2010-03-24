(in-package :pong)
(export 'run)

(defun run ()
  (glut:display-window (make-instance 'pong)))

;; got balls?
(defclass paddle (cairo-game-object) ()
  (:default-initargs :width 20 :height 100 :prescale nil))

(defmethod redraw ((obj paddle))
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
    (cairo:curve-to 0 0 0 0 rad 0))
  (cairo:fill-path))

(defclass ball (cairo-game-object)
  ((dx :initform 5)
   (dy :initform 5))
  (:default-initargs :width 20 :height 20))

(defmethod redraw ((obj ball))
  (cairo:set-source-rgb 1 1 1)
  (cairo:arc 0.5 0.5 0.5 0 (cairo:deg-to-rad 360))
  (cairo:fill-path))

(defclass pong (game) 
  ((ball :initform (make-instance 'ball))
   (paddle1 :initform nil)
   (paddle2 :initform (make-instance 'paddle))
   (p1-score :initform 0)
   (p2-score :initform 0))
  (:default-initargs :title "pong" :width 800 :height 800 :game-mode nil :framerate 70))

(defmethod key-down ((game pong) key)
  (with-slots (paddle2) game
    (case key
      ((:key-up)   (decf (pos-y paddle2) 20))
      ((:key-down) (incf (pos-y paddle2) 20)))))

(defmethod initial-objects ((game pong))
  (list 
   (slot-value game 'ball)
   (slot-value game 'paddle2)))

(defmethod update ((game pong))
  (with-slots (ball) game
    (move ball (mod (+ (pos-x ball) 5) 800) (pos-y ball))))

;; (defmethod draw ((window pong-window) ctx)
;;   (with-slots (ball-x ball-y ball-dx ball-dy glut:width glut:height
;;                p1-score p2-score) window
;;     (cairo:with-context (ctx)
;;       (cairo:set-source-rgb 0 0 0)
;;       (cairo:paint)
     
;;       ;; playfield line
;;       (cairo:set-source-rgb 0.5 0.5 0.5)
;;       (cairo:set-line-width 2)
;;       (cairo:move-to (/ glut:width 2) 0)
;;       (cairo:line-to (/ glut:width 2) glut:height)
;;       (cairo:set-dash 5 (list 5.0 5.0))
;;       (cairo:stroke)

;;       ;; scores
;;       (cairo:set-source-rgb 0.5 0.5 0.5)
;;       (cairo:select-font-face "Monospace" :normal :normal)
;;       (cairo:set-font-size 40)
;;       (cairo:move-to (/ glut:width 4) 40)
;;       (cairo:show-text (format nil "~a" p1-score))
;;       (cairo:move-to (* 3 (/ glut:width 4)) 40)
;;       (cairo:show-text (format nil "~a" p2-score))

;;       ;; scores
;;       (cairo:move-to 100 100)
;;       (cairo:line-to 200 110)
;;       (cairo:line-to 300 550)
;;       (cairo:stroke)

;;       ;; ball
;;       (incf ball-y ball-dy)
;;       (incf ball-x ball-dx)
;;       (when (or (<= ball-x 0) (>= ball-x glut:width))
;;         (setf ball-dx (- ball-dx))
;;         (incf p1-score))
;;       (when (or (<= ball-y 0) (>= ball-y glut:height)) 
;;         (setf ball-dy (- ball-dy))
;;         (incf p2-score))
;;       (cairo:set-source-rgb 1 1 1)
;;       (draw-ball ball-x ball-y))))

;; (defun draw-ball (x y)
;;   (cairo:save)
;;   (cairo:set-source-rgb 1 1 1)
;;   (cairo:translate x y)
;;   (cairo:fill-path)
;;   (cairo:restore))

