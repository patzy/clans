;;;; title.lisp

(in-package #:clans)

;;; Title screen
(defclass title-screen (fading-screen)
  ((view :accessor title-screen-view :initarg :view)
   (nb-players :accessor title-screen-nb-players :initform 2)))

(defmethod glaw:init-screen ((it title-screen) &key)
  (setf (title-screen-view it) (glaw:create-2d-view 0 0 *width* *height*))
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (glaw:add-input-handler it))

(defmethod glaw:resume-screen ((it title-screen))
  (glaw:add-input-handler it))

(defmethod glaw:suspend-screen ((it title-screen))
  (glaw:remove-input-handler it))

(defmethod glaw:shutdown-screen ((it title-screen))
  (glaw:remove-input-handler it))

(defmethod glaw:render-screen ((it title-screen))
  (glaw:set-view-2d (title-screen-view it))
  (glaw:with-resources ((fnt "font"))
    (glaw:set-color/rgb 1.0 1.0 1.0 1.0)
    (glaw:render-wrapped-string 0 (/ *height* 2.0) *width* fnt
                                "CLANS" :justify :center)
    (glaw:render-wrapped-string 0 (/ *height* 3.0) *width* fnt
                                (format nil "Players: ~S" (title-screen-nb-players it))
                                :justify :center)
    (glaw:render-wrapped-string 0 (- (/ *height* 3.0) (glaw:font-line-height fnt))
                                *width* fnt
                                "Press SPACE to play" :justify :center)))

(defmethod glaw:update-screen ((it title-screen) dt)
  (declare (ignore it dt)))

(glaw:key-handler (it title-screen) (:space :press)
   (glaw:push-screen (make-loading-screen
                      :assets '("map-tex" "hut-tex" "map-picking"
                                "score-font" "hut-font" "player-font")
                      :next-screen (make-game-screen :nb-players (title-screen-nb-players it)))
                     *screens*))

(glaw:key-handler (it title-screen) (:left :press)
   (decf (title-screen-nb-players it))
   (when (< (title-screen-nb-players it) 2)
     (setf (title-screen-nb-players it) 2)))

(glaw:key-handler (it title-screen) (:right :press)
   (incf (title-screen-nb-players it))
   (when (> (title-screen-nb-players it) 5)
     (setf (title-screen-nb-players it) 5)))
