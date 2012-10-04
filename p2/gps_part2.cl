;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EECS543 Fall 2012
; Programming Assignment 2
;
; Student Name: Jason Varbedian, Dan Jonik
; uniqname: jpvarbed, djonik
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| Task 2 evaluation follows:

DISCUSSION:

To add goal protection functionality to our GPS program, we kept a list of goals
that had been achieved through calls to the achieve function (the list was maintained
in achieve-each). By maintaining the list in achieve-each, we made sure that different
search branches did not interfere with each other's protected goals list. Each list would
be used by a single search path, and would be erased as soon as its call to achieve-each
returned. The initial list of protected goals for all top level search paths was an empty
list. 

The actual protected goal violation check occurred in our helper function (essentially inside
of the achieve function), where every operation's delete list would be compared to goals on our
protected goals list. If there was any overlap, the operation would be instantly disregarded,
and our search would continue with the next op.


Below is an excerpt of our output when running the following:

(GPS '((SON-AT-HOME) (HAVE-MONEY)) '((SON-AT-SCHOOL) (HAVE-MONEY)) *SCHOOL-OPS*)

>> Goal: (HAVE-MONEY)
   Goal: (SON-AT-SCHOOL)
   Consider: (TAXI-SON-TO-SCHOOL)
   Terminating search tree.
   Consider: (DRIVE-SON-TO-SCHOOL)
        Goal: (SON-AT-HOME)
        ....

When we start by achieving (HAVE-MONEY), which we can do trivially, our program then attempts to
achieve the goal (SON-AT-SCHOOL). The first operation it considers is (TAXI-SOON-TO-SCHOOL), which 
will violate the protected goal (HAVE-MONEY) at this point. The program recognizes this, prints the
terminating search message, and goes to the next operation, (DRIVE-SON-TO-SCHOOL), to again attempt
to achieve (SOON-AT-SCHOOL).


The obvious strength of this method is that time is saved from avoiding destructive search paths. 
We did, however, discover that we were using up considerably more memory while running our task 2
code, in relation to task 1. We were unsure of the exact cause of this phenomenon. 

|#




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

(defstruct op "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun GPS (state goals &optional (*ops* *ops*))

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
  (dbg-indent :gps (length goal) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriate-p)))))

;;; ==============================

(defun member-equal (item list)
  (member item list :test #'equal))

;;; ==============================



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

  "General Problem Solver: from state, achieve goals using *ops*."
  (find-all-if #'action-p
               (achieve-all (cons '(start) state) goals nil nil)))

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
  (some #'(lambda (goals) (achieve-each state goals goal-stack proc-goals))
        (orderings goals)))


(defun achieve-each (state goals goal-stack proc-goals)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                          (achieve current-state g goal-stack (set-difference goals g) proc-goals))
                        
                        ;if achieve returned a non-null state, we have achieved a new goal, so let's 
                        ;protect it
                        (if (not (null current-state))
                            (setf proc-goals (cons g proc-goals)))
                        
                        current-state)
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))


(defun orderings (l) 
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))

;;; ==============================


(defun apply-op (state goal op goal-stack proc-goals)
  "Return a new, transformed state if op is applicable."
  ;(dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  ;(dbg-indent :gps (length proc-goals) "proc-goals: ~a" proc-goals)

  (let ((state2 (achieve-all state (op-preconds op) 
                             (cons goal goal-stack) (cons goal proc-goals))))

    (unless (null state2)
      ;; Return an updated state
      ;(dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x) 
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))


(defun achieve (state goal goal-stack remaining-goals proc-goals)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)

  (cond ((member-equal goal state) state) 
        ((member-equal goal goal-stack) nil)
        (t (checkBeforeLeaping state goal goal-stack remaining-goals proc-goals))))


(defun checkBeforeLeaping (state goal goal-stack remaining-goals proc-goals)
  
  ;find all approriate operations and try to apply them all to current goal
  (some #'(lambda (op) 
            
            (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
            
            ;make sure op will not clobber any of our protected goals
            (if (some #'(lambda (del-item) (member-equal del-item proc-goals))
                      (op-del-list op))
                (progn
                  (dbg-indent :gps (length goal-stack) "Terminating search tree." '())
                  nil)
              
              (let ((new-state (apply-op state goal op (cons goal goal-stack) proc-goals)))
                (if (and (not (null new-state))
                       (achieve-all new-state remaining-goals goal-stack (cons goal proc-goals)))
                    new-state
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


(defparameter *school-ops*
  (list
   (make-op :action '(taxi-son-to-school)
            :preconds '((son-at-home) (have-money))
            :add-list '((executing (taxi-son-to-school)) (son-at-school))
            :del-list '((son-at-home) (have-money)))
    (make-op :action '(drive-son-to-school)
         :preconds '((son-at-home) (car-works))
         :add-list '((executing (drive-son-to-school)) (son-at-school))
         :del-list '((son-at-home)))
    (make-op :action '(shop-installs-battery)
         :preconds '((car-needs-battery) (shop-knows-problem) (shop-has-money))
         :add-list '((executing (shop-installs-battery)) (car-works)))
    (make-op :action '(tell-shop-problem)
         :preconds '((in-communication-with-shop))
         :add-list '((executing (tell-shop-problem)) (shop-knows-problem)))
    (make-op :action '(telephone-shop)
         :preconds '((know-phone-number))
         :add-list '((executing (telephone-shop)) (in-communication-with-shop)))
    (make-op :action '(look-up-number)
         :preconds '((have-phone-book))
         :add-list '((executing (look-up-number)) (know-phone-number)))
    (make-op :action '(give-shop-money)
         :preconds '((have-money))
         :add-list '((executing (give-shop-money)) (shop-has-money))
             :del-list '((have-money)))))
