;;;; clans.lisp

(in-package #:clans)

(defun init ()
  ;;(cl-i18n:load-language #P"data/lang/en")
  (glaw:init-sound)
  (glaw:clear-input-handlers)
  (glaw:init-content-manager :config #P"clans.assets")
  (glaw:load-asset "font")
  (setf *screens* (glaw:make-screen-stack))
  (glaw:push-screen (make-instance 'title-screen) *screens*))

(defun shutdown ()
  (glaw:shutdown-sound)
  (glaw:pop-screen *screens*)
  (glaw:dispose-asset "font")
  (glaw:shutdown-content-manager))

(defun draw (window)
  (glaw:begin-draw)
  (glaw:render-screens *screens*)
  (glaw:end-draw)
  (glop:swap-buffers window))

(defun update (dt)
  (glaw:update-scheduler dt)
  (glaw:update-sound)
  (glaw:update-screens *screens* dt))

;; App skeleton
(defmethod glop:on-close (window)
  (declare (ignore window))
  (shutdown))

(defmethod glop:on-key (window pressed keycode keysym string)
  (declare (ignore window))
  (glaw:dispatch-key-event keysym (if pressed :press :release) keycode string))

(defmethod glop:on-button (window pressed button)
  (declare (ignore window))
  (glaw:dispatch-button-event :mouse (glaw:translate-mouse-button button)
                              (if pressed :press :release)))

(defmethod glop:on-mouse-motion (window x y dx dy)
  (declare (ignore window))
  (glaw:update-mouse-position x y)
  (glaw:dispatch-motion-event :mouse dx dy))

(defmethod glop:on-draw (window)
  (draw window))

(defmethod glop:on-resize (window w h)
  (glaw:reshape w h)
  (draw window))

(glaw:key-handler :global (:escape :press)
   (glaw:pop-screen *screens*)
   (format t "Screens: ~S~%" (mapcar (lambda (item)
                                       (type-of item)) (glaw::screen-stack-screens *screens*))))

(defun run ()
  (setf cl-opengl-bindings:*gl-get-proc-address* 'glop:gl-get-proc-address)
  (glop:with-window (win "Clans" 1024 768)
    (glaw:setup-gl-defaults)
    (glaw:reshape 1024 768)
    (init)
    (let ((last-update-time (get-internal-real-time)))
      (loop while (and (glaw:has-screens *screens*)
                       (glop:dispatch-events win :blocking nil)) do
           (let* ((elapsed-time (- (get-internal-real-time) last-update-time))
                  (dt (float (/ elapsed-time internal-time-units-per-second))))
             (setf last-update-time (get-internal-real-time))
             (glaw:with-timestep (dt 0.01)
               (update dt)
               (draw win)))))
    (shutdown)))
