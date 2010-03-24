(in-package :pong)

(defclass game-object (squirl:body)
  ((pos-x :initarg :pos-x :initform 0 :reader pos-x)
   (pos-y :initarg :pos-y :initform 0 :reader pos-y)
   (width :initarg :width ::reader width)
   (height :initarg :height :reader height)
   (rotation :initarg :rotation :initform 0 :accessor rotation)
   phy-body))

(defclass game (glut:window)
  ((objects :initform nil :reader game-objects)
   (framerate :initform 60 :initarg :framerate :reader game-framerate)
   phy-world)
  (:default-initargs :mode '(:rgba :double :alpha)))

;; window ------------------------------------------------     
(defmethod initialize-instance ((self game) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (call-next-method))

(defgeneric initial-objects (game)
  (:documentation "Make a list of objects that are visible when the game starts."))
(defmethod initial-objects ((self game)) nil)

(defgeneric update (game)
  (:documentation "Update the game. Called on very frame"))
(defmethod update ((game game)))

(defgeneric key-down (game key)
  (:documentation "Called whenever a key is pressed"))
(defmethod key-down ((game game) key))

(defmethod glut:keyboard ((game game) key x y)
  (key-down game key))
(defmethod glut:special ((game game) key x y)
  (key-down game key))

(defmethod glut:display-window :before ((self game))
  (gl:clear-color 0 0 0 0)
  (gl:disable :depth-test)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :texture-rectangle-arb)
  (glut:enable-tick self (round (* 1000 (/ 1 (game-framerate self)))))
  (glut:enable-event self :special)
  (glut:enable-event self :keyboard)
  (glut:reshape self (glut:width self) (glut:height self))
  (format t "game: gl init done, adding objects~%")
  (mapc (lambda (obj) (add-object self obj)) (initial-objects self)))

(defmethod glut:reshape ((self game) width height)
  (format t "game: reshape (~a,~a)~%" width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 width height 0))

(defmethod glut:display ((self game))
  (with-slots (objects) self
    (gl:clear :color-buffer)
    (mapc #'draw-gl objects)
    (glut:swap-buffers)))

(defmethod glut:tick ((self game))
  (glut:post-redisplay)
  (update self))

(defmethod add-object ((self game) (obj game-object))
  (format t "game: adding object ~a~%" obj)
  (with-slots (objects) self
    (added-to-game obj self)
    (setf objects (cons obj objects))))

(defmethod glut:close :after ((self game))
  (setf glut::*glut-initialized-p* nil))

;; object -----------------------------------------------------
(defgeneric added-to-game (obj game)
  (:documentation "Notify a game object that it has been added to a game"))

(defgeneric removed-from-game (obj game)
  (:documentation "Notify a game object that it has been removed from a game"))

(defgeneric draw-gl (game-obj)
  (:documentation "Draw a game object into an OpenGL context."))
   
(defmethod resize ((obj game-object) new-width new-height)
  (format t "game-object ~a: resize to (~a, ~a)~%" obj new-width new-height)
  (setf (slot-value obj 'width) new-width)
  (setf (slot-value obj 'height) new-height))

(defmethod (setf width) (new-width (obj game-object))
  (resize obj new-width (height obj)))
(defmethod (setf height) (new-height (obj game-object))
  (resize obj (width obj) new-height))

(defmethod move ((obj game-object) new-x new-y)
  (setf (slot-value obj 'pos-x) new-x)
  (setf (slot-value obj 'pos-y) new-y))

(defmethod (setf pos-x) (new-x (obj game-object))
  (move obj new-x (pos-y obj)))
(defmethod (setf pos-y) (new-y (obj game-object))
  (move obj (pos-x obj) new-y))

;; cairo -----------------------------------------------------------
(defclass cairo-game-object (game-object)
  ((gl-texture-id :initform nil :reader gl-texture-id)
   (cairo-surface :initform nil :reader cairo-surface)
   (cairo-context :initform nil :reader cairo-context)
   (prescale :initform t :initarg :prescale :documentation 
             "Whether drawing should happen in a normalized coordinate system."))
  (:documentation "A game object that is drawn by cairo. Subclasses should override the 'redraw' method."))

(defmethod initialize-instance ((obj cairo-game-object) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (call-next-method)
  (resize obj (width obj) (height obj)))

(defmethod resize ((obj cairo-game-object) new-width new-height)
  (call-next-method)
  (when (slot-value obj 'gl-texture-id)
    (init-cairo-surface obj)
    (redraw obj)))

(defmethod init-cairo-surface ((obj cairo-game-object))
  (with-slots (gl-texture-id cairo-surface cairo-context) obj
    (if cairo-surface (cairo:destroy cairo-surface))
    (if cairo-context (cairo:destroy cairo-context))
    (setf cairo-surface (cairo:create-image-surface :argb32 (width obj) (height obj)))
    (setf cairo-context (cairo:create-context cairo-surface))
    (if gl-texture-id
        (gl:delete-textures (list gl-texture-id)))
    (setf (slot-value obj 'gl-texture-id) (car (gl:gen-textures 1)))))
      
(defmethod added-to-game ((obj cairo-game-object) (game game))
  (declare (ignore game))
  (init-cairo-surface obj)
  (redraw obj))

(defmethod removed-from-game ((obj cairo-game-object) (game game))
  (declare (ignore game))
  (with-slots (gl-texture-id) obj
    (when gl-texture-id
      (gl:delete-textures (list gl-texture-id))
      (setf gl-texture-id nil))))

(defmethod draw-gl ((obj cairo-game-object))
  (with-slots (gl-texture-id) obj
    (when gl-texture-id
      (gl:bind-texture :texture-rectangle-arb gl-texture-id)
      (gl:tex-image-2d :texture-rectangle-arb 0 :rgba 
                       (width obj) (height obj) 0 :bgra
                       :unsigned-byte (cairo:image-surface-get-data (cairo-surface obj) :pointer-only t))
      (gl:matrix-mode :modelview)
      (gl:with-pushed-matrix
        (gl:load-identity)
        (gl:rotate (rotation obj) 0 0 1)
        (gl:translate (pos-x obj) (pos-y obj) 0)
        (gl:scale (width obj) (height obj) 1)
        (gl:with-primitive :quads
          (gl:tex-coord 0 0)
          (gl:vertex 0 0)
          (gl:tex-coord (width obj) 0)
          (gl:vertex 1 0)
          (gl:tex-coord (width obj) (height obj))
          (gl:vertex 1 1)
          (gl:tex-coord 0 (height obj))
          (gl:vertex 0 1))))))

(defgeneric redraw (game-object)
  (:documentation "Draw the contents of a game object"))

(defmethod redraw :around ((obj cairo-game-object))
 (format t "cairo-game-object ~a: redraw~%" obj)
  (cairo:with-context ((slot-value obj 'cairo-context))
    (cairo:save)
    (if (slot-value obj 'prescale) (cairo:scale (width obj) (height obj)))
    (call-next-method)
    (cairo:restore)))