;;;; game.lisp

(in-package #:clans)

(defstruct hut
  color
  sprite)

(defun move-hut (hut x y)
  (glaw:move-sprite (hut-sprite hut) x y))

(defstruct territory
  index
  huts) ;; huts per slot in +colors+ order

(defun territory-nb-huts (ter)
  (loop for slot in (territory-huts ter)
       sum (length slot)))

(defun territory-slot-nb-huts (ter slot-index)
  (length (nth slot-index (territory-huts ter))))

(defun territory-add-hut (ter hut)
  (let* ((slot-index (position (hut-color hut) +colors+))
         (slot-pos (nth slot-index (nth (territory-index ter) +map-slots+))))
    (push hut (nth slot-index (territory-huts ter)))
    (move-hut hut (first slot-pos) (second slot-pos))))

(defun territory-remove-hut (ter hut)
  (let ((slot-index (position (hut-color hut) +colors+)))
    (setf (nth slot-index (territory-huts ter))
          (remove hut (nth slot-index (territory-huts ter))))))

(defun map-can-move (from-index to-index)
  (let ((neighbors (nth from-index +map-graph+)))
    (find to-index neighbors)))

(defun map-move-huts (from-ter to-ter)
  (loop for slot in (territory-huts from-ter) do
       (loop for hut in slot do
            (territory-add-hut to-ter hut)
            (territory-remove-hut from-ter hut))))

(defun game-move-possible-p (from-ter to-ter)
  (and (map-can-move (territory-index from-ter) (territory-index to-ter))
       (< (territory-nb-huts from-ter) 7)
       (not (zerop (territory-nb-huts from-ter)))))

(defun game-move-huts (from-ter to-ter)
  (if (game-move-possible-p from-ter to-ter)
      (prog1 t
        (glaw:with-resources ((snd "move-ok"))
          (glaw:play-sound snd))
        (map-move-huts from-ter to-ter))
      (prog1 nil
        (glaw:with-resources ((snd "move-error"))
          (glaw:play-sound snd))
        (format t "Move is *not* possible, try something else~%"))))

(defstruct score-board
  (alpha 0.0)
  scores
  (x 800) (y 700))

(defun create-score-board ()
  (let ((board (make-score-board)))
    (setf (score-board-scores board)
          (loop for i below 5 collect 0))
    board))

(defun score-board-add (board player score)
  (incf (nth player (score-board-scores board)) score))

(defmethod render ((it score-board))
  (glaw:with-resources ((font "score-font"))
    (let ((line-pos (score-board-y it)))
      (loop for i below 5 do
        (glaw:set-color (nth i +colors+))
        (glaw:render-wrapped-string (score-board-x it) line-pos 200 font
                                    (format nil "~A" (nth i (score-board-scores it)))
                                    :justify :right)
        (decf line-pos (glaw:font-line-height font))))))

(defstruct game-screen
  (last-fps 0.0)
  view map hut-sprite
  villages ;; created villages per player
  huts ;; all huts
  territories ;; all territories
  nb-players
  player    ;; current player
  player-colors
  show-player-color
  territory ;; selected territory
  scores)

(defun game-screen-nb-villages (scr)
  (loop for player-villages in (game-screen-villages scr)
       when player-villages
       sum (length player-villages)))

(defun game-screen-game-over-p (scr)
  (>= (game-screen-nb-villages scr) 12))

(defun game-screen-next-player (scr)
  (incf (game-screen-player scr))
  (when (> (game-screen-player scr) (game-screen-nb-players scr))
    (setf (game-screen-player scr) 1)))

(defun %pick-territory (territories img x y)
  (let ((res nil))
    (multiple-value-bind (r g b a)
        (glaw:image-get-pixel img (floor x) (floor y))
      (declare (ignore a))
      (let ((block-index (floor (/ r 10)))
            ;;(type-index (floor (/ g 50)))
            (territory-index (floor (/ b 50))))
        (unless (and (= r 255) (= g 0) (= b 255))
          (setf res (nth (1- (+ (* 5 block-index) territory-index)) territories)))))
    res))

(defun pick-territory/wrld (territories wrld-x wrld-y)
  (glaw:with-resources ((img "map-picking"))
    (glaw:with-2d-view-coords ((x wrld-x) (y wrld-y))
        (glaw:create-2d-view 0 0 *width* *height*)
        (glaw:create-2d-view 0 0 (glaw:image-width img) (glaw:image-height img))
      (%pick-territory territories img x (- (glaw:image-height img) y)))))

(defun pick-territory (territories scr-x scr-y)
  (glaw:with-resources ((img "map-picking"))
    (glaw:with-2d-view-coords ((x scr-x) (y scr-y))
        (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*)
        (glaw:create-2d-view 0 0 (glaw:image-width img) (glaw:image-height img))
      (%pick-territory territories img x y))))

(defun game-screen-place-huts (it)
  (loop for i below 60
     with colors = +colors+
     for color = (glaw:random-nth colors)
     for territory = (nth i (game-screen-territories it))
     for hut = (nth i (game-screen-huts it))
       do (progn (setf (hut-color hut) color)
                 (territory-add-hut territory hut)
                 (setf colors (remove color colors))
                 (unless colors
                   (setf colors +colors+)))))

(defmethod glaw:init-screen ((it game-screen) &key)
 (setf (game-screen-view it) (glaw:create-2d-view 0 0 *width* *height*)
       (game-screen-map it) (glaw:create-sprite 0 0 *width* *height* (glaw:use-resource "map-tex")))
 (setf (game-screen-territories it)
       (loop for i below 60 collect (make-territory :index i :huts (list nil nil nil nil nil)))
       (game-screen-huts it)
       (loop for i below 60 collect (make-hut :sprite (glaw:create-sprite 0 0 20 30
                                                             (glaw:use-resource "hut-tex"))))
       (game-screen-villages it)
       (loop for i below (game-screen-nb-players it) collect '()))
 (game-screen-place-huts it)
 (setf (game-screen-player it) (glaw:random-between 1 (game-screen-nb-players it))
       (game-screen-player-colors it) +colors+)
 (setf (game-screen-scores it) (create-score-board))
 (glaw:shuffle (game-screen-player-colors it))
 (glaw:with-resources ((snd "music"))
   (glaw:play-sound snd :loop t :volume 0.3))
 (glaw:add-input-handler it))

(defmethod glaw:shutdown-screen ((it game-screen))
  (glaw:with-resources ((snd "music"))
    (glaw:stop-sound snd))
  (glaw:dispose-asset "map-tex")
  (glaw:dispose-asset "hut-tex")
  (glaw:dispose-asset "map-picking")
  (glaw:dispose-asset "hut-font")
  (glaw:dispose-asset "player-font")
  (glaw:dispose-asset "score-font")
  (glaw:remove-input-handler it))

(defmethod glaw:update-screen ((it game-screen) dt)
  (declare (ignore dt))
  (unless (= (floor (game-screen-last-fps it)) (floor (glaw:current-fps)))
    ;;(format t "FPS: ~S~%" (glaw:current-fps))
    (setf (game-screen-last-fps it) (glaw:current-fps))))

(defmethod glaw:render-screen ((it game-screen))
  (glaw:set-view-2d (game-screen-view it))
  (glaw:set-color/rgb 1.0 1.0 1.0)
  (glaw:render-sprite (game-screen-map it))
  (loop for hut in (game-screen-huts it)
        for i below 60 do
       (glaw:set-color (hut-color hut))
       (glaw:render-sprite (hut-sprite hut)))
  (loop for ter in (game-screen-territories it) do
       (loop for slot in (territory-huts ter) do
            (unless (or (zerop (length slot)) (= 1 (length slot)))
              (glaw:with-resources ((fnt "hut-font"))
                (glaw:set-color #(1.0 1.0 1.0 1.0))
                (glaw:format-at (glaw::sprite-x (hut-sprite (first slot)))
                                (glaw::sprite-y (hut-sprite (first slot)))
                                fnt "~a" (length slot))))))
  (render (game-screen-scores it))
  (glaw:with-resources ((fnt "player-font"))
    (glaw:select-texture nil)
    (if (game-screen-show-player-color it)
        (glaw:set-color (nth (game-screen-player it) (game-screen-player-colors it)))
        (glaw:set-color #(1.0 1.0 1.0 1.0)))
    (glaw:format-at 10 10 fnt "Current player: ~S" (game-screen-player it))))


(defun game-village-p (territories ter)
  (every #'zerop (loop for ter-index in (nth (territory-index ter) +map-graph+)
                    collect (territory-nb-huts (nth ter-index territories)))))

(defun game-village-exists-p (scr ter)
  (some (lambda (player-villages)
         (member ter player-villages)) (game-screen-villages scr)))

(defun game-screen-check-villages (scr)
  "Returns a list of all newly created villages."
  (let ((res nil))
    (loop for ter in (game-screen-territories scr)
       do (when (and (not (game-village-exists-p scr ter))
                     (game-village-p (game-screen-territories scr) ter))
            (push ter res)
            (push ter (nth (1- (game-screen-player scr))
                           (game-screen-villages scr)))))
    res))

(defun village-remove-singles (ter)
  (loop for slot in (territory-huts ter)
       for index below 5
       when (= 1 (length slot))
       do (setf (nth index (territory-huts ter)) '())))

(defun village-score (ter)
  (territory-nb-huts ter))

(defun game-screen-update-scores (scr villages)
  (loop for v in villages do
       (let ((remove-singles (and (> (territory-nb-huts v) 5)
                                  (notany #'zerop (loop for slot in (territory-huts v)
                                                     collect (length slot))))))
         (when remove-singles
           (village-remove-singles v))
         (loop for index below 5 do
              (unless (null (nth index (territory-huts v)))
                (score-board-add (game-screen-scores scr)
                                 index
                                 (village-score v)))))))

(glaw:button-handler (it game-screen) :mouse (:left-button :press)
   (let ((territory (pick-territory (game-screen-territories it) glaw:*mouse-x* glaw:*mouse-y*)))
     (if (and territory (game-screen-territory it))
         (let ((moved (game-move-huts (game-screen-territory it) territory)))
           (setf (game-screen-territory it) nil)
           (when moved
             (let ((villages (game-screen-check-villages it)))
               (when villages
                 (game-screen-update-scores it villages)))
             (if (game-screen-game-over-p it)
                 (glaw:replace-screen *screens*
                                      (make-instance 'game-over-screen
                                                     :scores (score-board-scores
                                                              (game-screen-scores it))
                                                     :nb-players (game-screen-nb-players it)))
                 (game-screen-next-player it))))
         (when territory
           (glaw:with-resources ((snd "click"))
             (glaw:play-sound snd))
           (setf (game-screen-territory it) territory)))))

(glaw:key-handler (it game-screen) (:space :press)
   (setf (game-screen-show-player-color it) t))

(glaw:key-handler (it game-screen) (:space :release)
   (setf (game-screen-show-player-color it) nil))