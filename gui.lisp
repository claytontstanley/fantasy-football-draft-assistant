
(defparameter *path* (directory-namestring *load-truename*))
(defparameter *rank-csv* (format nil "~a~a" *path* "score.csv"))
(defparameter *pred-csv* (format nil "~a~a" *path* "predict.csv"))

(ql:quickload "cl-csv")

(defparameter *rank* (load-rank-csv *rank-csv*))
(defparameter *pred* (load-pred-csv *pred-csv*))

*rank*
*pred*

(defclass player ()
  ((picked :accessor picked :initarg :picked)))
  
(defclass rank-player (player)
  ((rank :accessor rank :initarg :rank)
   (type :accessor type :initarg :type)
   (score :accessor score :initarg :score)))

(defclass pred-player (player)
  ((name :accessor name :initarg :name)
   (type :accessor type :initarg :type)
   (rank :accessor rank :initarg :rank)))

(defun load-rank-csv (csv)
  (let ((csv (cl-csv:read-csv (file-string csv))))
    (assert (equalp (first csv) (list "name" "type" "rushYds" "rushTds" "recYds" "recTds" "passYds" "passTds" "kickFgs" "score" "rank")))
    (loop for (nil type nil nil nil nil nil nil nil score rank) in (rest csv)
          collect (make-instance 'rank-player
                                 :score score
                                 :type type
                                 :rank rank))))

(defun load-pred-csv (csv)
  (let ((csv (cl-csv:read-csv (file-string csv))))
    (assert (equalp (first csv) (list "name" "type" "rank")))
    (loop for (name type rank) in (rest csv)
          collect (make-instance 'pred-player
                                 :type type
                                 :rank rank
                                 :name name))))

(defparameter *num-teams* 15)
(defparameter *num-drafts* 12)

(defclass draft-window (window)
  ()
  (:default-initargs
    :view-size (make-point 1680 1050)
    :view-position (make-point 0 0)))

(defmethod initialize-instance :after ((win draft-window) &key)
  (add-draft-pick-view win))

(defclass draft-pick-view (menu-view)
  ())

(defmethod add-draft-pick-view ((win draft-window))
  (destructuring-bind (vx vy) (as-list (view-size win))
    (let ((dx (/ vx (+ 1 *num-drafts*)))
          (dy (/ vy *num-teams*)))
      (let ((views
              (loop for j from 0 below *num-teams*
                    for y = (* j dy)
                    append (loop for i from 0 below *num-drafts*
                                 for x = (* i dx)
                                 for y-off = (+ y 
                                                (* (mod i 2) (/ dy 3)))
                                 collect (make-instance
                                           'draft-pick-view
                                           :view-position (make-point x y-off)
                                           :default-item 1
                                           :menu-items (mapcar #'as-menu-item *pred*)
                                           :view-size (make-point (* dx 2) 50)
                                           )))))
        (apply #'add-subviews win views)))))


(defclass draft-pick-item (menu-item)
  ((pred-player :accessor pred-player :initarg :pred-player)))

(defmethod as-menu-item ((item pred-player))
  (make-instance
    'draft-pick-item
    :pred-player item
    :menu-item-title (name item)))

(defmethod get-draft-pick-views ((win draft-window))
  (remove-if-not
    (lambda (sv) (typep sv 'selected-view))
    (subviews win)))

(defmethod update-draft-pick-views ((win draft-window))
  (loop for draft-pick in (get-draft-pick-views win)
        do (easygui::add-menu-item draft-pick (list "foo"))))
  

#|
(get-draft-pick-views (front-window))
(update-draft-pick-views (front-window))

(make-instance 'menu-item)
(make-instance 'draft-window)

|#



