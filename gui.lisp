
(defparameter *path* (directory-namestring *load-truename*))
(defparameter *rank-csv* (format nil "~a~a" *path* "score.csv"))
(defparameter *pred-csv* (format nil "~a~a" *path* "predict.csv"))

(ql:quickload "cl-csv")
(ql:quickload "iterate")

(defparameter *rank* (load-rank-csv *rank-csv*))
(defparameter *pred* (load-pred-csv *pred-csv*))

(defclass player ()
  ((picked :accessor picked :initarg :picked)))
  
(defclass rank-player (player)
  ((rank :accessor rank :initarg :rank)
   (type :accessor type :initarg :type)
   (score :accessor score :initarg :score)))

(defclass pred-player (player)
  ((name :accessor name :initarg :name)
   (type :accessor type :initarg :type)
   (disabled-p :accessor disabled-p :initarg :disabled-p :initform nil)
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

(defmethod get-views-of ((win window) (class t))
  (remove-if-not
    (lambda (sv) (class-inherit-from-p
                   (class-of sv)
                   class))
    (subviews win)))

(defmethod get-draft-pick-views ((win draft-window))
  (get-views-of win (find-class 'draft-pick-view)))

(defclass choose-window (window)
  ()
  (:default-initargs
    :view-size (make-point 1024 768)
    :view-position (make-point 0 0)))

(defmethod initialize-instance :after ((win choose-window) &key)
  (add-subviews
    win
    (make-instance 'choose-sequence)
    (make-instance 'disable-sequence)
    (make-instance 'generate-button)
    (make-instance 'disable-button)
    (make-instance 'enable-button)))

(defmethod print-pred ((item pred-player) strm)
  (with-accessors ((name name) (type type) (rank rank)) item
  (format strm "~a, ~a, ~a" rank type name))) 

(defclass choose-sequence (sequence-dialog-item)
  ()
  (:default-initargs
    :view-size (make-point (/ 1024 3) 768)
    :table-sequence ()
    :table-print-function #'print-pred
    :view-nick-name :cs
    :view-position (make-point 0 0)))

(defclass disable-sequence (sequence-dialog-item)
  ()
  (:default-initargs
    :view-size (make-point (/ 1024 3) 768)
    :table-sequence ()
    :table-print-function #'print-pred
    :view-nick-name :ds
    :view-position (make-point (* 1024 2/3) 0)))

(defclass disable-button (button-dialog-item)
  ()
  (:default-initargs
    :view-position (make-point (* 1024 1/3) 40)
    :dialog-item-action #'disable-choices
    :dialog-item-text "Disable"))

(defclass enable-button (button-dialog-item)
  ()
  (:default-initargs
    :view-position (make-point (* 1024 1/3) 120)
    :dialog-item-action #'enable-choices
    :dialog-item-text "Enable"))

(defclass generate-button (button-dialog-item)
  ()
  (:default-initargs
    :view-position (make-point (* 1024 1/3) 480)
    :dialog-item-action #'display-choices
    :dialog-item-text "Generate"))

(defmethod generate-constraints ((draft-win draft-window) (drafting-starters-p (eql t)))
  (list
    (make-constraint 'qb (lambda () (= (get-my-drafted draft-win 'qb) 1))) ; one qb in the first 7 rounds
    (make-constraint 'rb (lambda () (= (get-my-drafted draft-win 'rb) 2))) ; two rbs in the first 7 rounds
    (make-constraint 'wr (lambda () (= (get-my-drafted draft-win 'wr) 3))) ; three wrs in the first 7 rounds
    (make-constraint 'k (lambda () (= (get-my-drafted draft-win 'k) 1)))   ; one k in the first 7 rounds
    ; if #teams-1 ks have been picked and a non-constrained pick avail, wait on k
    (make-constraint 'k (lambda () (and (eq (get-all-drafted draft-win 'k)
                                            (- *num-teams* 1))
                                        (free-pick-vail draft-win))))
    ; if #teams-1 qbs have been picked and a non-constrained pick avail, wait on qb
    (make-constraint 'qb (lambda () (and (eq (get-all-drafted draft-win 'qb)
                                             (- *num-teams* 1))
                                         (free-pick-vail draft-win))))
    ; if #teams*2-1 rbs have been picked and a non-constrained pick avail, wait on rb
    (make-constraint 'rb (lambda () (and (eq (* 2 (get-all-drafted draft-win 'rb))
                                             (- *num-teams* 1))
                                         (free-pick-vail draft-win))))
    ; if #teams*3-1 wrs have been picked and a non-constrained pick avail,  wait on wr
    (make-constraint 'wr (lambda () (and (eq (* 3 (get-all-drafted draft-win 'wr))
                                             (- *num-teams* 1))
                                         (free-pick-vail draft-win))))))

(defmethod generate-constraints ((draft-win draft-window) (drafting-starters-p (eql nil)))
    ; one qb in last 5 rounds
    ; two rbs in last 5 rounds
    ; two wrs in last 5 rounds
    )

(defmethod generate-choices ((draft-win draft-window))
  (let ((constraints (generate-constraints draft-win (drafting-starters-p draft-win))))
    (let ((choice-types (get-choice-types constraints)))
      (let ((filtered-rank (get-ranking *rank* choice-types)))
        (let ((choices (get-choices *pred* filtered-rank)))
          choices)))))

(defmethod display-choices ((obj generate-button))
  (display-choices (easygui::easygui-window-of obj)))

(defmethod display-choices ((win choose-window))
  (set-table-sequence (view-named :cs win) (generate-choices (get-draft-win))))

(defmethod get-ranking ((rank list) (choice-types list))
  (remove-if-not (lambda (rank-player)
                   (member (type rank-player) choice-types))
                 rank))

(defmethod selected-contents ((obj sequence-dialog-item))
  (mapcar (lambda (c) (cell-contents obj c))
          (selected-cells obj)))

(defmethod disable-choices ((obj disable-button))
  (let ((win (easygui::easygui-window-of obj)))
    (mapcar #'disable-choice (selected-contents (view-named :cs win)))
    (update-window win)))

(defmethod update-window ((win choose-window))
  (set-table-sequence
    (view-named :ds win)
    (get-disabled-preds win))
  (display-choices win))

(defmethod enable-choices ((obj enable-button))
  (let ((win (easygui::easygui-window-of obj)))
    (mapcar #'enable-choice (selected-contents (view-named :ds win)))
    (update-window win)))

(defmethod enable-choice ((obj pred-player))
  (setf (disabled-p obj) nil))

(defmethod disable-choice ((obj pred-player))
  (setf (disabled-p obj) t))

(defmethod get-disabled-preds ((obj window))
  (remove-if-not (lambda (pred-player) (disabled-p pred-player)) *pred*))

(defmethod get-enabled-preds ((obj window))
  (remove-if (lambda (pred-player) (disabled-p pred-player)) *pred*))

#|
(make-instance 'choose-window)

(make-instance 'draft-window)

(get-draft-pick-views (front-window))
|#



