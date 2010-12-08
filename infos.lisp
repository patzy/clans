;;;; infos.lisp

(in-package #:clans)

;;; Infos screen
(defclass infos-screen ()
  ((view :accessor infos-screen-view :initarg :view)
   (background :accessor infos-screen-background
               :initform (glaw:create-rectangle-shape 0 0 *width* *height*))
   (background-color :accessor infos-screen-background-color
                     :initform #(0.0 0.0 0.0 0.5)
                     :initarg :background-color)
   (player :reader infos-screen-player :initarg :player)
   (color :reader infos-screen-player-color :initarg :player-color)
   (bonus-terrain :reader infos-screen-bonus-terrain :initarg :bonus-terrain)
   (malus-terrain :reader infos-screen-malus-terrain :initarg :malus-terrain)
   (bonus-points :reader infos-screen-bonus-points :initarg :bonus-points)))

(defmethod glaw:init-screen ((it infos-screen) &key)
  (setf (infos-screen-view it) (glaw:create-2d-view 0 0 *width* *height*))
  (glaw:push-input-handlers)
  (glaw:add-input-handler it))

(defmethod glaw:resume-screen ((it infos-screen))
  (glaw:add-input-handler it))

(defmethod glaw:suspend-screen ((it infos-screen))
  (glaw:remove-input-handler it))

(defmethod glaw:shutdown-screen ((it infos-screen))
  (glaw:remove-input-handler it)
  (glaw:pop-input-handlers))

(defmethod glaw:render-screen ((it infos-screen))
  (glaw:set-view-2d (infos-screen-view it))
  (glaw:select-texture nil)
  (glaw:set-color (infos-screen-background-color it))
  (glaw:render-shape (infos-screen-background it))
  (glaw:with-resources ((fnt "font"))
    (let ((current-line (* 4.0 (/ *height* 5.0)))
          (line-size (glaw::font-line-height fnt)))
      (glaw:set-color #(1.0 1.0 1.0 1.0))
      (glaw:render-wrapped-string 10 current-line *width* fnt
                                  "Game Infos" :justify :left)
      (decf current-line (* 2.0 line-size))
      (glaw:set-color (infos-screen-player-color it))
      (glaw:render-wrapped-string 10 current-line *width* fnt
                                  (format nil "Current player: ~A"
                                          (infos-screen-player it))
                                  :justify :left)
      (glaw:set-color #(1.0 1.0 1.0 1.0))
      (decf current-line line-size)
      (glaw:render-wrapped-string 10 current-line *width* fnt
                                  (format nil "Bonus terrain: ~A (+ ~A pts)"
                                          (infos-screen-bonus-terrain it)
                                          (infos-screen-bonus-points it))
                                  :justify :left)
      (decf current-line line-size)
      (glaw:render-wrapped-string 10 current-line *width* fnt
                                  (format nil "Malus terrain: ~A"
                                          (infos-screen-malus-terrain it))
                                  :justify :left))))

(defmethod glaw:update-screen ((it infos-screen) dt)
  (declare (ignore it dt)))

(glaw:key-handler (it infos-screen) (:space :release)
   ;; unstack ourselves
   (glaw:pop-screen *screens*))