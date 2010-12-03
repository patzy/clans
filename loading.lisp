;;;; loading.lisp

(in-package #:clans)

;;; Loading screen
(defstruct loading-screen
  view
  assets
  completed
  (total 1.0)
  next-screen
  displayed-p)

(defun loading-screen-nb-remaining (scr)
  (length (loading-screen-assets scr)))

(defun loading-screen-nb-completed (scr)
  (length (loading-screen-completed scr)))

(defun loading-screen-progress (scr)
  (float (/ (loading-screen-nb-completed scr) (loading-screen-total scr))))

(defun loading-screen-complete-p (scr)
  (= (loading-screen-progress scr) 1.0))

(defun loading-screen-load-next (scr)
  (let ((asset (pop (loading-screen-assets scr))))
    (glaw:load-asset asset)
    (push asset (loading-screen-completed scr))))

(defmethod glaw:init-screen ((it loading-screen) &key)
  (setf (loading-screen-view it) (glaw:create-2d-view 0 0 *width* *height*)
        (loading-screen-total it) (loading-screen-nb-remaining it))
  (gl:clear-color 0.0 0.0 0.0 1.0))

(defmethod glaw:render-screen ((it loading-screen))
  (glaw:set-view-2d (loading-screen-view it))
  (glaw:with-resources ((fnt "font"))
    (glaw:set-color/rgb 1.0 1.0 1.0 1.0)
    (glaw:render-wrapped-string 0 (/ *height* 2.0) *width* fnt
                         (format nil "LOADING: ~S%" (floor (* (loading-screen-progress it) 100.0)))
                                :justify :center))
    (unless (loading-screen-displayed-p it)
    (setf (loading-screen-displayed-p it) t)))

(defmethod glaw:update-screen ((it loading-screen) dt)
  (declare (ignore dt))
  (when (loading-screen-displayed-p it)
    (unless (loading-screen-complete-p it)
      (loading-screen-load-next it)))
  (when (loading-screen-complete-p it)
    (glaw:replace-screen *screens* (loading-screen-next-screen it))))