(defmacro rebind (vars &body body)
  `(let ,(mapcar #'list vars vars)
     ,@body))

(require :cl-csv)
(require :parse-number)
(require :ccl-simple-view)
(require :experiment-window4)

; Don't edit these
(defparameter *path* (directory-namestring *load-truename*))
(defparameter *rank* nil)
(defparameter *pred* nil)
(defparameter *num-starters* nil) 
(defparameter *by-weeks* nil)

; Configure these
(defparameter *rank-csv* (format nil "~a~a" *path* "score-real.csv"))
(defparameter *pred-csv* (format nil "~a~a" *path* "pred-real.csv"))
(defparameter *my-team-num* 1)
(defparameter *all-types* (list 'qb 'rb 'k 'wr 'df))
(defparameter *num-teams* 16)
(defparameter *num-drafts* 14)
(defparameter *display-font* (list "Courier" 12))

(defun initialize-data ()
  (ccl::create-autorelease-pool) ; Ensures this runs on the executing thread, not some slime compile thread
  (setf *num-starters* (reduce #'+ (mapcar (lambda (type) (get-num-picks-per-team type t)) *all-types*)))
  (setf *pred* (load-pred-csv *pred-csv*))
  (setf *rank* (load-rank-csv *rank-csv*))
  (setf *pred* (rank-pred *pred* *rank*))
  (setf *by-weeks* (sort
                     (remove-duplicates (mapcar #'by-week *pred*))
                     #'<)))

(defun save-data ()
  (with-open-file (strm (format nil "~a~a" *path* "rank.lisp") :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-readable *rank* strm))
  (with-open-file (strm (format nil "~a~a" *path* "pred.lisp") :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-readable *pred* strm)))

(defun load-rank-csv (csv)
  (let ((csv (cl-csv:read-csv (file-string csv))))
    (assert (equalp (first csv) (list "name" "displayType" "rushYds" "rushTds" "recYds" "recTds" "passYds" "passTds" "fgs0.20" "fgs20.30" "fgs30.40" "fgs40.50" "fgs50.inf" "score" "rank")))
    (loop for (nil display-type nil nil nil nil nil nil nil nil nil nil nil score rank) in (rest csv)
          collect (make-instance 'rank-player
                                 :score (parse-number:parse-number score)
                                 :display-type (intern (string-upcase display-type))
                                 :rank rank))))

(defun load-pred-csv (csv)
  (let ((csv (cl-csv:read-csv (file-string csv))))
    (assert (equalp (first csv) (list "name" "displayType" "rank" "byWeek")))
    (guard ((equal
              (sort (copy-list *all-types*) #'string-lessp)
              (sort (remove-duplicates (mapcar #'type it1)) #'string-lessp)))
      (loop for (name display-type nil by-week) in (rest csv)
            collect (make-instance 'pred-player
                                   :display-type (intern (string-upcase display-type))
                                   :name name
                                   :by-week (parse-integer by-week))))))

(defmethod rank-pred ((pred list) (rank list))
  (let ((pred
          (loop with pred-hash = (make-pred-hash pred)
                for rank-player in rank
                for pred-player = (pop (gethash (display-type rank-player) pred-hash))
                when pred-player do (setf (score pred-player) (score rank-player))
                when pred-player collect it into result
                finally (assert (= (length result) (length pred)))
                finally (return result))))
    (loop for pred-player in pred
          for rank from 1
          do (setf (rank pred-player) rank)
          collect pred-player)))

(defmethod make-pred-hash ((pred list))
  (let ((pred-hash (make-hash-table :test #'eq)))
    (loop for pred-player in pred
          do (push-to-end pred-player (gethash (display-type pred-player) pred-hash)))
    pred-hash))

(defclass player ()
  ((type :accessor type)
   (score :accessor score :initarg :score)
   (rank :accessor rank :initarg :rank)
   (display-type :accessor display-type :initarg :display-type)))

(defmethod initialize-instance :after ((player player) &key)
  (assert (not (slot-boundp player 'type)))
  (setf (type player) (as-type (display-type player))))

(defmethod as-type ((type (eql 'te)))
  'wr)

(defmethod as-type ((type symbol))
  type)

(defclass pred-player (player)
  ((name :accessor name :initarg :name)
   (drafted-p :accessor drafted-p :initarg :drafted-p :initform nil)
   (disabled-p :accessor disabled-p :initarg :disabled-p :initform nil)
   (by-week :accessor by-week :initarg :by-week)))

(defmethod print-pred ((item pred-player) strm)
  (with-accessors ((name name) (display-type display-type) (rank rank) (score score) (by-week by-week)) item
    (format strm "~3a|~3a|~2a, ~2a, ~a" rank (floor score) by-week display-type name)))

(defclass rank-player (player)
  ())

(defclass unlisted-player (pred-player)
  ((by-week :initform 0)
   (rank :initform 0)
   (score :initform 0)))

(defclass np-player (unlisted-player)
  ((display-type :initform 'np)
   (name :initform "")))

(defclass draft-window (borderless-window)
  ()
  (:default-initargs
    :view-size (make-point 1680 1050)
    :view-position (make-point 0 0)))

(defun get-draft-win ()
  (front-window :class (find-class 'draft-window)))

(defmethod initialize-instance :after ((win draft-window) &key)
  (add-draft-pick-views win))

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
                                                (* (mod i 2) 30))
                                 with nil-picks = (make-nil-picks)
                                 collect (make-instance
                                           'draft-pick-view
                                           :view-position (make-point x y-off)
                                           :default-item 1
                                           :team-num team-num
                                           :menu-items (mapcar #'as-menu-item (append nil-picks (sort (copy-list *pred*) #'string-lessp :key #'name)))
                                           :view-size (make-point (* dx 2) 25)
                                           )))))
        (apply #'add-subviews win views)))))

(defun make-nil-picks ()
  (list*
    (make-instance 'np-player)
    (loop for type in *all-types* 
          collect (make-instance 'unlisted-player
                                 :display-type type
                                 :name "not present"))))

(defclass draft-pick-view (menu-view)
  ((team-num :accessor team-num :initarg :team-num))
  (:default-initargs :view-font *display-font*))

(defclass draft-pick-item (menu-item)
  ((pred-player :accessor pred-player :initarg :pred-player))
  (:default-initargs :view-font *display-font*))

(defmethod as-menu-item ((item pred-player))
  (make-instance
    'draft-pick-item
    :pred-player item
    :menu-item-title (print-pred item nil)
    :action (lambda () (update-window (get-draft-win)))))

(defmethod update-window ((win draft-window))
  (dolist (player *pred*)
    (setf (drafted-p player) nil))
  (loop for drafted-player in (get-all-drafted win 'all)
        do (setf (drafted-p drafted-player) t)))

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
  (remove-if #'np-player-p
             (mapcar #'get-pred-player (get-draft-pick-views win))))

(defmethod np-player-p ((pred-player np-player)) 
  t)

(defmethod np-player-p ((pred-player pred-player))
  nil)

(defmethod get-all-drafted ((win draft-window) type)
  (remove-if-not (lambda (x)
                   (eql type (type x)))
                 (get-all-drafted win 'all)))

(defmethod get-my-drafted ((win draft-window) type)
  (remove-if-not #'my-player-p (get-all-drafted win type)))

(defmethod get-their-drafted ((win draft-window) type)
  (remove-if-not #'their-player-p (get-all-drafted win type)))

(defmethod my-player-p ((pred-player pred-player))
  (eq (team-num (get-draft-pick-view pred-player)) *my-team-num*))

(defmethod their-player-p ((pred-player pred-player))
  (not (my-player-p pred-player)))

(defclass choose-window (window)
  ()
  (:default-initargs
    :view-size (make-point 1024 768)
    :view-position (list :right 0)))

(defmethod initialize-instance :after ((win choose-window) &key)
  (add-subviews
    win
    (make-instance 'choose-sequence)
    (make-instance 'disable-sequence)
    (make-instance 'generate-button)
    (make-instance 'disable-button)
    (make-instance 'enable-button)))

(defclass draft-sequence (sequence-dialog-item)
  ()
  (:default-initargs
    :view-font *display-font*))

(defclass choose-sequence (draft-sequence)
  ()
  (:default-initargs
    :view-size (make-point (/ 1024 3) 768)
    :table-print-function #'print-pred
    :view-nick-name :cs
    :view-position (make-point 0 0)))

(defclass disable-sequence (draft-sequence)
  ()
  (:default-initargs
    :view-size (make-point (/ 1024 3) 768)
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

(defmethod disable-choice ((obj pred-player))
  (setf (disabled-p obj) t))

(defmethod update-window ((win choose-window))
  (display-disabled win)
  (display-choices win))

(defmethod display-disabled ((win choose-window))
  (set-table-sequence
    (view-named :ds win)
    (get-disabled-preds win)))

(defmethod selected-contents ((obj sequence-dialog-item))
  (mapcar (lambda (c) (cell-contents obj c))
          (selected-cells obj)))

(defmethod get-disabled-preds ((obj window))
  (remove-if-not (lambda (pred-player) (disabled-p pred-player)) *pred*))

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
    (let ((choices
            (loop for constraint in constraints
                  collect (funcall constraint *pred*))))
      (let ((filtered-choices
              (remove-if #'drafted-p
                         (remove-if #'disabled-p
                                    (reduce #'intersection choices)))))
        (sort (copy-list filtered-choices) #'< :key #'rank)))))

(defmethod drafting-starters-p ((win draft-window))
  (<= (get-draft-round win) *num-starters*))

(defmethod get-draft-round ((win draft-window))
  (ceiling (/ (+ (length (get-all-drafted win 'all)) 1)
              *num-teams*)))

(defmethod generate-constraints ((draft-win draft-window) (drafting-starters-p (eql t)))
  (append
    (loop for type in *all-types*
          collect (rebind (type)
                    (lambda (players)
                      (if (all-mine-picked draft-win type)
                        (remove-if (lambda (player)
                                     (eq (type player) type)) players)
                        players))))
    (loop for type in *all-types*
          collect (rebind (type)
                    (lambda (players)
                      (if (and (all-but-mine-picked draft-win type)
                               (free-pick-vail draft-win))
                        (remove-if (lambda (player)
                                     (eq (type player) type)) players)
                        players))))))

(defmethod free-pick-vail ((win draft-window))
  (notevery (lambda (type)
              (all-but-mine-picked win type))
            (remove-if (lambda (type)
                         (all-mine-picked win type))
                       *all-types*)))

(defmethod all-mine-picked ((win draft-window) type)
  (= (length (get-my-drafted win type))
     (get-num-picks-per-team type (drafting-starters-p win))))

(defmethod all-but-mine-picked ((win draft-window) type)
  (let ((num-picks-per-team (get-num-picks-per-team type (drafting-starters-p win))))
    (eq (length (get-their-drafted win type))
        (* num-picks-per-team (- *num-teams* 1)))))

(defmethod get-num-picks-per-team ((type symbol) (drafting-starters-p (eql t)))
  (ecase type
    (qb 1)
    (rb 2)
    (wr 3)
    (df 1)
    (k 1)))

(defmethod get-num-picks-per-team ((type symbol) (drafting-starters-p (eql nil)))
  (ecase type
    (qb 2)
    (rb 4) 
    (wr 4) 
    (df 2)
    (k 2)))

(defmethod generate-constraints ((draft-win draft-window) (drafting-starters-p (eql nil)))
  (append
    (loop for type in *all-types*
          collect (rebind (type)
                    (lambda (players)
                      (if (all-mine-picked draft-win type)
                        (remove-if (lambda (player)
                                     (eq (type player) type)) players)
                        players))))
    (unless (all-by-weeks-filled draft-win)
      (loop for type in *all-types*
            collect (rebind (type)
                      (lambda (players)
                        (let ((unfilled-by-weeks 
                                (get-unfilled-by-weeks
                                  (get-my-drafted draft-win type))))
                          (remove-if (lambda (player)
                                       (and (eq (type player) type)
                                            (member (by-week player) unfilled-by-weeks)))
                                     players))))))))

(defmethod all-by-weeks-filled ((draft-win draft-window))
  (every (lambda (filled-by-weeks)
           (equal filled-by-weeks *by-weeks*))
         (loop for type in *all-types*
               collect (get-filled-by-weeks (get-my-drafted draft-win type)))))

(defmethod get-filled-by-weeks ((players null))
  ())

(defmethod get-filled-by-weeks ((players cons))
  (let ((type (type (first players))))
    (assert (every (lambda (player)
                     (eq (type player) type))
                   players))
    (let ((players (remove-if #'np-player-p players)))
      (loop for by-week in *by-weeks*
            for avail-players = (remove-if (lambda (player)
                                             (= (by-week player) by-week)) players)
            when (>= (length avail-players) (get-num-picks-per-team type t)) collect by-week))))

(defmethod get-unfilled-by-weeks ((players list))
  (set-difference *by-weeks* (get-filled-by-weeks players)))

(defun run-draft ()
  (initialize-data)
  (save-data)
  (hide-menubar)
  (make-instance 'choose-window)
  (make-instance 'draft-window))

#|
(run-draft)
(window-close (get-draft-win))
|#

