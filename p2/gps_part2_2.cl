;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File gps.lisp: Final version of GPS

(requires "gps1")

;;; ==============================

(defun executing-p (x)
  "Is x of the form: (executing ...) ?"
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
    (make-op :action action :preconds preconds
             :add-list add-list :del-list del-list)))

;;; ==============================

(mapc #'convert-op *school-ops*)

;;; ==============================

(defvar *ops* nil "A list of available operators.")
(defparameter *proc-goals* nil "A list of protected goals.")
(defparameter *holder* nil "goals done")

(defstruct op "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun GPS (state goals &optional (*ops* *ops*))
  (print "running task 2 version...")
  "General Problem Solver: from state, achieve goals using *ops*."
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil nil)))

;;; ==============================

(defun achieve-all (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack (set-difference goals g))))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun achieve (state goal goal-stack remaining-goals)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriate-p)))))

;;; ==============================

(defun member-equal (item list)
  (member item list :test #'equal))

;;; ==============================

(defun apply-op (state goal op goal-stack proc-goals)
  "Return a new, transformed state if op is applicable."
  (dbg-indent :gps-op (length goal-stack) "Apply-op: Consider => ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op) 
                             (cons goal goal-stack) proc-goals)))
    (unless (null state2)
      ;; Return an updated state
      (dbg-indent :action (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x) 
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member-equal goal (op-add-list op)))


;;; ==============================

(defun use (oplist)
  "Use oplist as the default list of operators."
  ;; Return something useful, but not too verbose: 
  ;; the number of operators.
  (length (setf *ops* oplist)))

;;; ==============================

(defparameter *banana-ops*
  (list
    (op 'climb-on-chair
        :preconds '(chair-at-middle-room at-middle-room on-floor)
        :add-list '(at-bananas on-chair)
        :del-list '(at-middle-room on-floor))
    (op 'push-chair-from-door-to-middle-room
        :preconds '(chair-at-door at-door)
        :add-list '(chair-at-middle-room at-middle-room)
        :del-list '(chair-at-door at-door))
    (op 'walk-from-door-to-middle-room
        :preconds '(at-door on-floor)
        :add-list '(at-middle-room)
        :del-list '(at-door))
    (op 'grasp-bananas
        :preconds '(at-bananas empty-handed)
        :add-list '(has-bananas)
        :del-list '(empty-handed))
    (op 'drop-ball
        :preconds '(has-ball)
        :add-list '(empty-handed)
        :del-list '(has-ball))
    (op 'eat-bananas
        :preconds '(has-bananas)
        :add-list '(empty-handed not-hungry)
        :del-list '(has-bananas hungry))))

;;; ==============================

(defun make-maze-ops (pair)
  "Make maze ops in both directions"
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))

(defun make-maze-op (here there)
  "Make an operator to move between two places"
  (op `(move from ,here to ,there)
      :preconds `((at ,here))
      :add-list `((at ,there))
      :del-list `((at ,here))))

(defparameter *maze-ops*
  (mappend #'make-maze-ops
     '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
       (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
       (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

;;; ==============================

(defun GPS (state goals &optional (*ops* *ops*))
  (print "running task 2 version...")
  "General Problem Solver: from state, achieve goals using *ops*."
  (format t "state:~a and goals:~a" state goals)
  (find-all-if #'action-p
               (progn (format t "gonna fuck your shit up~%") (achieve-all (cons '(start) state) goals nil nil))))

(defun action-p (x)
  "Is x something that is (start) or (executing ...)?"
  (or (equal x '(start)) (executing-p x)))

;;; ==============================

(defun find-path (start end)
  "Search a maze for a path from start to end."
  (let ((results (GPS `((at ,start)) `((at ,end)))))
    (unless (null results)
      (cons start (mapcar #'destination
                          (remove '(start) results
                                  :test #'equal))))))

(defun destination (action)
  "Find the Y in (executing (move from X to Y))"
  (fifth (second action)))

;;; ==============================

(defun make-block-ops (blocks)
  (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (equal a b)
          (dolist (c blocks)
            (unless (or (equal c a) (equal c b))
              (push (move-op a b c) ops)))
          (push (move-op a 'table b) ops)
          (push (move-op a b 'table) ops))))
    ops))

(defun move-op (a b c)
  "Make an operator to move A from B to C."
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on ,b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))


;;; ==============================

(defun achieve-all (state goals goal-stack proc-goals)
  "Achieve each goal, trying several orderings."
  (dbg-indent :proc-end (length proc-goals) "~&Proc-goals: ~a~%" proc-goals)
  (let ((tmp nil))
  (some #'(lambda (goals) 
            (progn (setf tmp (achieve-each state goals goal-stack nil))
              (first tmp)))
          (orderings goals))))

  |#(let ((tmp nil))
    (some #'(lambda (goals) (progn  
                              ;;(format t "beforeordering all goals: ~a s:~a~%" (first (rest tmp)) (first tmp))
                              ;;(format t "beforeordering all goalsv: ~a s:~a other:~a~%" *holder* (first tmp) tmp)
                              (setf tmp (achieve-each state goals goal-stack *holder*)))
              ;;(format t "change ordering all goals: ~a s:~a~%" (first (rest tmp)) (first tmp))
              (setf *holder* (first (rest tmp)))
              (first tmp)
              )
        (orderings goals))))
#|
(defun achieve-each (state goals goal-stack proc-goals)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state)
        (hold nil))
    (if (and (every #'(lambda (g)
                        (progn 
                          ;(setf hold (multiple-value-bind (current-state proc-goals)
                          ;(achieve current-state g goal-stack (set-difference goals g) proc-goals) (list current-state proc-goals)))
                          (setf hold (achieve current-state g goal-stack (set-difference goals g) proc-goals))
                          (setf current-state (first hold))
                          (setf proc-goals (first (rest hold))) 
                          ;;(format t "in each state: ~a goals: ~a ~%" current-state proc-goals)
                          current-state))
                    goals)
             (subsetp goals current-state :test #'equal))
        (list current-state proc-goals)
      (list nil proc-goals))))

(defun orderings (l) 
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))

;;; ==============================

(defun achieve (state goal goal-stack remaining-goals proc-goals)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  ;(dbg-indent :achieve (length goal) "Achieve: Goal => ~a" goal)
  ;(dbg-indent :gps (length state) "State: ~a" state)
  ;(dbg-indent :achieve (length state) "Achieve: State => ~a" state)
  ;(dbg-indent :proc-achieve (length proc-goals) "~&Proc-goals: ~a~%" proc-goals)
  
  ;if the goal is already in state, return state
  (let ((new-state (cond ((member-equal goal state) state) 
                         ((member-equal goal goal-stack) nil)
                         (t 1)))
        (tmp nil))
    
    (if (equal new-state 1)
        (progn (setf new-state nil) 
          (setf tmp (checkBeforeLeaping state goal goal-stack remaining-goals proc-goals))
          ;(setf tmp (multiple-value-bind (new-state proc-goals) (checkBeforeLeaping state goal goal-stack remaining-goals proc-goals) (list new-state proc-goals)))
          (setf proc-goals (first (rest tmp))) ; starts off with a nil
          (setf new-state (first tmp))
          ;;(format t "in achieve goals are ~a and state ~a~%" proc-goals new-state)
          ;(format t "in achieve the tmp is are ~a~%" tmp)
          (list new-state proc-goals))
      (list new-state proc-goals)))
  )


(defun checkBeforeLeaping (state goal goal-stack remaining-goals proc-goals)
  
  ;(dbg-indent :leap (length goal) "checkBeforeLeaping: Goal => ~a" goal)
  ;(dbg-indent :proc-check (length proc-goals) "checkProc-goals: ~a~%" proc-goals)
  ;find all approriate operations and try to apply them all to current goal

  (some #'(lambda (op) 
            (let* ((new-state (apply-op state goal op goal-stack proc-goals))
                  (pred (notany #'(lambda (del-item)
                                    (progn 
                                      (format t "comparing :~a to :~a result:~a action: ~a~%" del-item proc-goals (member-equal del-item proc-goals) (op-action op) );so its not this
                                      (format t "done so far: ~a~%" new-state)
                                      (member-equal del-item proc-goals)))
                                (op-del-list op))))
              (progn 
                 (if (not (null (equal '(MOVE B FROM A TO C) (op-action op))))
                      (break))
             
              (if (and (not (null new-state))
                       (not (null pred))
                       (achieve-all new-state remaining-goals goal-stack proc-goals))
                  (progn
                    (setf proc-goals (append proc-goals (rest (op-add-list op))))
                    ;;(format t "check proc-goals is ~a checkstate is ~a~%" proc-goals new-state)
                    ;(format t "check ok the value is ~a~%" (multiple-value-bind (firsty secondy) (values new-state proc-goals) (list firsty secondy)))
                    (list new-state proc-goals))
                    ;(values new-state proc-goals))
                nil))))
         (appropriate-ops goal state)))


;gives us a list of which operations 
(defun appropriate-ops (goal state)
  "Return a list of appropriate operators, 
  sorted by the number of unfulfilled preconditions."
  (sort (copy-list (find-all goal *ops* :test #'appropriate-p)) #'<
        :key #'(lambda (op) 
                 (count-if #'(lambda (precond)
                               (not (member-equal precond state)))
                           (op-preconds op)))))

;;; ==============================

(defun permutations (bag)
  "Return a list of all the permutations of the input."
  ;; If the input is nil, there is only one permutation:
  ;; nil itself
  (if (null bag)
      '(())
      ;; Otherwise, take an element, e, out of the bag.
      ;; Generate all permutations of the remaining elements,
      ;; And add e to the front of each of these.
      ;; Do this for all possible e to generate all permutations.
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permutations
                            (remove e bag :count 1 :test #'eq))))
              bag)))

;;; ==============================

