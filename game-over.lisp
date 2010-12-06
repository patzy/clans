(in-package #:clans)

(defclass game-over-screen (fading-screen)
  ((view :accessor game-over-screen-view :initarg :view)
   (scores :accessor game-over-screen-scores :initarg :scores)
   (winner :accessor game-over-screen-winner :initform 'unknown)
   (nb-players :accessor game-over-screen-nb-players :initarg :nb-players)))

(defmethod glaw:init-screen ((it game-over-screen) &key)
  (loop for player below 5
       when (>= player (game-over-screen-nb-players it))
       do (setf (nth player (game-over-screen-scores it)) 0))
  (setf (game-over-screen-view it) (glaw:create-2d-view 0 0 *width* *height*)
        (game-over-screen-winner it) (loop for score in (game-over-screen-scores it)
                                        for player from 1 upto 5
                                        with winner = 1 and best-score = 0
                                        when (> score best-score)
                                        do (setf winner player)
                                        finally (return winner)))
  (glaw:add-input-handler it))

(defmethod glaw:resume-screen ((it game-over-screen))
  (glaw:add-input-handler it))

(defmethod glaw:suspend-screen ((it game-over-screen))
  (glaw:remove-input-handler it))

(defmethod glaw:shutdown-screen ((it game-over-screen))
  (glaw:remove-input-handler it))

(defmethod glaw:render-screen ((it game-over-screen))
  (glaw:set-view-2d (game-over-screen-view it))
  (glaw:with-resources ((fnt "font"))
    (glaw:set-color/rgb 1.0 1.0 1.0 1.0)
    (glaw:render-wrapped-string 0 (/ *height* 2.0) *width* fnt
                                "Game Over" :justify :center)
    (glaw:render-wrapped-string 0 (/ *height* 3.0) *width* fnt
                                (format nil "Winner: player ~S" (game-over-screen-winner it))
                                :justify :center)
    (glaw:render-wrapped-string 0 (- (/ *height* 3.0) (glaw:font-line-height fnt))
                                *width* fnt
                                "Press ESC to get back to title screen" :justify :center)))


(defmethod glaw:update-screen ((it game-over-screen) dt)
  (declare (ignore it dt)))
