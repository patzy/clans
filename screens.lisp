(in-package #:clans)

;;; Some screen stuff

(defclass fading-screen ()
  ((delay :initform 1.0 :initarg :delay :accessor fading-screen-delay)
   (date :accessor fading-screen-date)
   (shape :initform (glaw:create-rectangle-shape 0 0 *width* *height*)
          :reader fading-screen-shape)
   (gradient :initform (glaw:create-color-gradient 0.0 0.0 0.0 1.0
                                                   0.0 0.0 0.0 0.0)
             :initarg :gradient :accessor fading-screen-gradient)))


(defmethod fading-screen-time ((it fading-screen))
  (let* ((elapsed (- (get-internal-real-time) (fading-screen-date it)))
         (time (/ elapsed (* (fading-screen-delay it) internal-time-units-per-second))))
    (if (> time 1.0)
        1.0
        time)))

(defmethod glaw:init-screen :after ((it fading-screen) &key)
  (setf (fading-screen-date it) (get-internal-real-time)))

(defmethod glaw:render-screen :after ((it fading-screen))
  (glaw:select-texture nil) ;; just to be sure
  (glaw:set-color-from-gradient (fading-screen-gradient it)
                                (fading-screen-time it))
  (glaw:render-shape (fading-screen-shape it)))

(defclass input-screen () ())

(defmethod glaw:init-screen :before ((it input-screen) &key)
  (glaw:add-input-handler it))

(defmethod glaw:resume-screen :before ((it input-screen))
  (glaw:add-input-handler it))

(defmethod glaw:suspend-screen :after ((it input-screen))
  (glaw:remove-input-handler it))

(defmethod glaw:shutdown-screen :after ((it input-screen))
  (glaw:remove-input-handler it))
