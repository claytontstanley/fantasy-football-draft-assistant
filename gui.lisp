(defparameter *path* (directory-namestring *load-truename*))
(defparameter *rank-csv* (format nil "~a~a" *path* "score.csv"))
(defparameter *pred-csv* (format nil "~a~a" *path* "predict.csv"))
(defparameter *my-team-num* 1)
(defparameter *all-types* (list 'qb 'rb 'k 'wr))
(defparameter *num-starters* 7)

(ql:quickload "cl-csv")
(ql:quickload "iterate")

(defparameter *rank* (load-rank-csv *rank-csv*))
(defparameter *pred* (load-pred-csv *pred-csv*))

(setf *pred* (rank-pred *pred* *rank*))

(defmethod rank-pred ((pred list) (rank list))
  (loop for pred-player in pred
        for rank from 1
        do (setf (rank pred-player) rank)
        collect pred-player))

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

(defmethod print-pred ((item pred-player) strm)
  (with-accessors ((name name) (type type) (rank rank)) item
  (format strm "~a, ~a, ~a" rank type name))) 

(defun load-rank-csv (csv)
  (let ((csv (cl-csv:read-csv (file-string csv))))
    (assert (equalp (first csv) (list "name" "type" "rushYds" "rushTds" "recYds" "recTds" "passYds" "passTds" "kickFgs" "score" "rank")))
    (loop for (nil type nil nil nil nil nil nil nil score rank) in (rest csv)
          collect (make-instance 'rank-player
                                 :score score
                                 :type (intern (string-upcase type))
                                 :rank rank))))

(defun load-pred-csv (csv)
  (let ((csv (cl-csv:read-csv (file-string csv))))
    (assert (equalp (first csv) (list "name" "type" "rank")))
    (loop for (name type nil) in (rest csv)
          collect (make-instance 'pred-player
                                 :type (intern (string-upcase type))
                                 :name name))))

(defparameter *num-teams* 15)
(defparameter *num-drafts* 12)

(defclass draft-window (window)
  ()
  (:default-initargs
    :view-size (make-point 1680 1050)
    :view-position (make-point 0 0)))

(defun get-draft-win ()
  (front-window :class (find-class 'draft-window)))

(defmethod initialize-instance :after ((win draft-window) &key)
  (add-draft-pick-views win))

(defclass draft-pick-view (menu-view)
  ((team-num :accessor team-num :initarg :team-num)))

(defmethod add-draft-pick-views ((win draft-window))
  (destructuring-bind (vx vy) (as-list (view-size win))
    (let ((dx (/ vx (+ 1 *num-drafts*)))
          (dy (/ vy *num-teams*)))
      (let ((views
              (loop for j from 0 below *num-teams*
                    for team-num = (+ j 1)
                    for y = (* j dy)
                    append (loop for i from 0 below *num-drafts*
                                 for x = (* i dx)
                                 for y-off = (+ y 
                                                (* (mod i 2) (/ dy 2.5)))
                                 with nil-picks = (make-nil-picks)
                                 collect (make-instance
                                           'draft-pick-view
                                           :view-position (make-point x y-off)
                                           :default-item 1
                                           :team-num team-num
                                           :menu-items (mapcar #'as-menu-item (append nil-picks *pred*))
                                           :view-size (make-point (* dx 2) 50)
                                           )))))
        (apply #'add-subviews win views)))))

(defun make-nil-picks ()
  (list*
    (make-instance 'pred-player
                   :type nil
                   :name ""
                   :rank 0)
    (loop for type in *all-types* 
          collect (make-instance 'pred-player
                                 :type type
                                 :rank 0
                                 :name "not present"))))

(defclass draft-pick-item (menu-item)
  ((pred-player :accessor pred-player :initarg :pred-player)))

(defmethod as-menu-item ((item pred-player))
  (make-instance
    'draft-pick-item
    :pred-player item
    :menu-item-title (print-pred item nil)))

(defmethod get-draft-pick-views ((win draft-window))
  (get-views-of win (find-class 'draft-pick-view)))

(defmethod get-views-of ((win window) (class t))
  (remove-if-not
    (lambda (sv) (class-inherit-from-p
                   (class-of sv)
                   class))
    (subviews win)))

(defmethod get-draft-pick-view ((pred-player pred-player))
  (loop for draft-pick-view in (get-draft-pick-views (get-draft-win))
        when (eq (get-pred-player draft-pick-view) pred-player) return draft-pick-view))

(defmethod get-pred-player ((view draft-pick-view))
  (pred-player (easygui::menu-selection view)))
 
(defmethod get-all-drafted ((win draft-window) (type (eql 'all)))
  (remove-if #'nil-player-p
             (mapcar #'get-pred-player (get-draft-pick-views win))))

(defmethod nil-player-p ((pred-player pred-player)) 
  (null (type pred-player)))

(defmethod get-all-drafted ((win draft-window) type)
  (remove-if-not (lambda (x)
                   (eql type (type x)))
                 (get-all-drafted win 'all)))

(defmethod get-my-drafted ((win draft-window) type)
  (remove-if-not #'my-player-p (get-all-drafted win type)))

(defmethod my-player-p ((pred-player pred-player))
  (eq (team-num (get-draft-pick-view pred-player)) *my-team-num*))

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

(defmethod disable-choices ((obj disable-button))
  (let ((win (easygui::easygui-window-of obj)))
    (mapcar #'disable-choice (selected-contents (view-named :cs win)))
    (update-window win)))

(defmethod update-window ((win choose-window))
  (set-table-sequence
    (view-named :ds win)
    (get-disabled-preds win))
  (display-choices win))

(defmethod selected-contents ((obj sequence-dialog-item))
  (mapcar (lambda (c) (cell-contents obj c))
          (selected-cells obj)))

(defmethod get-disabled-preds ((obj window))
  (remove-if-not (lambda (pred-player) (disabled-p pred-player)) *pred*))

(defmethod disable-choice ((obj pred-player))
  (setf (disabled-p obj) t))

(defclass enable-button (button-dialog-item)
  ()
  (:default-initargs
    :view-position (make-point (* 1024 1/3) 120)
    :dialog-item-action #'enable-choices
    :dialog-item-text "Enable"))

(defmethod enable-choices ((obj enable-button))
  (let ((win (easygui::easygui-window-of obj)))
    (mapcar #'enable-choice (selected-contents (view-named :ds win)))
    (update-window win)))

(defmethod enable-choice ((obj pred-player))
  (setf (disabled-p obj) nil))

(defclass generate-button (button-dialog-item)
  ()
  (:default-initargs
    :view-position (make-point (* 1024 1/3) 480)
    :dialog-item-action #'display-choices
    :dialog-item-text "Generate"))

(defmethod display-choices ((obj generate-button))
  (display-choices (easygui::easygui-window-of obj)))

(defmethod display-choices ((win choose-window))
  (set-table-sequence (view-named :cs win) (generate-choices (get-draft-win))))

(defmethod generate-choices ((draft-win draft-window))
  (let ((constraints (generate-constraints draft-win (drafting-starters-p draft-win))))
    (let ((choice-types (get-choice-types constraints)))
      (let ((choices (get-ranking *pred* choice-types)))
          (remove-if #'disabled-p choices)))))

(defmethod generate-constraints ((draft-win draft-window) (drafting-starters-p (eql t)))
  (list
    (make-constraint 'qb (lambda () (= (length (get-my-drafted draft-win 'qb)) 1))) 
    (make-constraint 'rb (lambda () (= (length (get-my-drafted draft-win 'rb)) 2))) 
    (make-constraint 'wr (lambda () (= (length (get-my-drafted draft-win 'wr)) 3))) 
    (make-constraint 'k (lambda () (= (length (get-my-drafted draft-win 'k)) 1)))   
    (make-constraint 'k (lambda () (and (eq (length (get-all-drafted draft-win 'k))
                                            (- *num-teams* 1))
                                        (free-pick-vail draft-win))))
    (make-constraint 'qb (lambda () (and (eq (length (get-all-drafted draft-win 'qb))
                                             (- *num-teams* 1))
                                         (free-pick-vail draft-win))))
    (make-constraint 'rb (lambda () (and (eq (* 2 (length (get-all-drafted draft-win 'rb)))
                                             (- *num-teams* 1))
                                         (free-pick-vail draft-win))))
    (make-constraint 'wr (lambda () (and (eq (* 3 (length (get-all-drafted draft-win 'wr)))
                                             (- *num-teams* 1))
                                         (free-pick-vail draft-win))))))

(defmethod generate-constraints ((draft-win draft-window) (drafting-starters-p (eql nil)))
    ; one qb in last 5 rounds
    ; two rbs in last 5 rounds
    ; two wrs in last 5 rounds
    )
(defmethod drafting-starters-p ((win draft-window))
  (<= (get-draft-round win) *num-starters*))

(defmethod get-draft-round ((win draft-window))
  (ceiling (/ (+ (length (get-all-drafted win 'all)) 1)
              *num-teams*)))

(defmethod make-constraint ((type symbol) (constraint function))
  (lambda ()
    (when (funcall constraint)
      type)))

(defmethod get-choice-types ((constraints list))
  (let ((constraint-types
        (remove-duplicates
          (loop for constraint in constraints
                when (funcall constraint) collect it))))
    (set-difference *all-types* constraint-types)))

(defmethod get-ranking ((rank list) (choice-types list))
  (sort 
    (remove-if-not (lambda (rank-player)
                     (member (type rank-player) choice-types))
                   rank)
    (lambda (a b) (< (rank a) (rank b)))))

#|
(make-instance 'choose-window)

(make-instance 'draft-window)

|#



