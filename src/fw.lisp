(in-package :pong)

(defclass game-object ()
  ((width :reader width)
   (height :reader height)
   (phy-body :initform nil :reader physics-body)))

(defclass game (glut:window)
  ((objects :initform nil :reader game-objects)
   (framerate :initform 60 :initarg :framerate :reader game-framerate)
   (phy-world :initform nil :reader physics-world))
  (:default-initargs :mode '(:rgba :double :alpha)))

;; window ------------------------------------------------     
(defmethod initialize-instance :before ((self game) &rest initargs 
                                        &key (gravity (squirl:vec 0 9.81)) 
                                             (damping 1d0)
                                             (physics-accuracy 3)
                                        &allow-other-keys)
  (declare (ignore initargs))
  (let ((world (squirl:make-world :gravity gravity :damping (coerce damping 'double-float)
                                  :iterations physics-accuracy :elastic-iterations 5)))
    (setf (slot-value self 'phy-world) world)))

(defgeneric initial-objects (game)
  (:documentation "Make a list of objects that are visible when the game starts.")
  (:method ((g game)) nil))

(defgeneric update (game)
  (:documentation "Update the game. Called on very frame")
  (:method ((g game))))

(defgeneric key-down (game key)
  (:documentation "Called whenever a key is pressed")
  (:method ((g game) key)))

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
  (glu:ortho-2d 0 width 0 height))

(defmethod glut:display ((self game))
  (with-slots (objects) self
    (gl:clear :color-buffer)
    (mapc #'draw-gl objects)
    (glut:swap-buffers)))

(defmethod glut:tick ((self game))
  (glut:post-redisplay)
  (squirl:world-step (physics-world self) (/ (game-framerate self)))
  (update self))

(defmethod add-object ((self game) (obj game-object))
  (format t "game: adding object ~a~%" obj)
  (with-slots (objects) self
    (added-to-game obj self)
    (squirl:world-add-body (physics-world self) (physics-body obj))
    (setf objects (cons obj objects))))

(defmethod glut:close :after ((self game))
  (setf glut::*glut-initialized-p* nil))

;; object -----------------------------------------------------
(defmethod initialize-instance :before ((obj game-object) &rest initargs &key 
                                        (width nil) (height nil) ; no defaults
                                        (mass 0) (inertia nil)
                                        (calculate-inertia-p (not inertia)) (pos-x 0) (pos-y 0) &allow-other-keys)
  (declare (ignore initargs))
  (or width height (error "game-object: need initial width and height"))
  (setf (slot-value obj 'width) width)
  (setf (slot-value obj 'height) height)
  (let ((body (squirl:make-body :mass mass :inertia (or inertia most-positive-double-float)
                                :calculate-inertia-p calculate-inertia-p 
                                :position (squirl:vec pos-x pos-y)
                                :actor obj)))
    (setf (slot-value obj 'phy-body) body)
    (squirl:attach-shapes (collision-hull obj) body)))

(defgeneric collision-hull (object)
  (:documentation "Make a list of shapes that define the collision hull of an object")
  (:method ((obj game-object)) nil))

(defgeneric added-to-game (obj game)
  (:documentation "Notify a game object that it has been added to a game")
  (:method ((obj game-object) (game game)))) 

(defgeneric removed-from-game (obj game)
  (:documentation "Notify a game object that it has been removed from a game")
  (:method ((obj game-object) (game game))))

(defgeneric draw-gl (game-obj)
  (:documentation "Draw a game object into an OpenGL context.")
  (:method ((obj game-object))))
   
(defmethod resize ((obj game-object) new-width new-height)
  (format t "game-object ~a: resize to (~a, ~a)~%" obj new-width new-height)
  (setf (slot-value obj 'width) new-width)
  (setf (slot-value obj 'height) new-height)
  (let ((body (physics-body obj)))
    (mapc (lambda (s) (squirl:detach-shape s body)) (squirl:body-shapes body))
    (squirl:attach-shapes (collision-hull obj) body)))

(defmethod (setf width) (new-width (obj game-object))
  (resize obj new-width (height obj)))
(defmethod (setf height) (new-height (obj game-object))
  (resize obj (width obj) new-height))

(defmethod pos-x ((obj game-object))
  (squirl:vec-x (squirl:body-position (physics-body obj))))
(defmethod pos-y ((obj game-object))
  (squirl:vec-y (squirl:body-position (physics-body obj))))

(defmethod rotation ((obj game-object))
  (* (/ 180 pi) (squirl:vec->angle (squirl:body-rotation (physics-body obj)))))

(defmethod (setf rotation) (angle (obj game-object))
  (setf (squirl:body-rotation (physics-body obj)) (squirl:angle->vec (/ angle (/ 180 pi)))))

(defmethod squirl:collide :before ((obj1 t) (obj2 t) (arb t))
;  (format t "COLLISION: ~a, ~a, ~a~%" obj1 obj2 arb))
)

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
        (gl:translate (pos-x obj) (pos-y obj) 0)
        (gl:rotate (rotation obj) 0 0 1)
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
    (cairo:set-source-rgba 0 0 0 0)
    (cairo:paint)
    (if (slot-value obj 'prescale) (cairo:scale (width obj) (height obj)))
    (call-next-method)
    (cairo:restore)))

