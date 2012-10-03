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

(defstruct op "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: from state, achieve goals using *ops*."
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))

;;; ==============================

(defun achieve-all (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun achieve (state goal goal-stack)
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

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op) 
                             (cons goal goal-stack))))
    (unless (null state2)
      ;; Return an updated state
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
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

(defparameter *block-rules*
  '(   
     ((move ?x from table to ?y)
      (OP 
       '(MOVE ?x FROM TABLE TO ?y)
       :PRECONDS '((SPACE ON ?x) (SPACE ON ?y) (?y ON TABLE))
       :ADD-LIST '((EXECUTING (MOVE ?x FROM TABLE TO ?y)) (?x ON ?y))
       :DEL-LIST '((?x ON TABLE) (SPACE ON ?y))))
   
     ((move ?x from ?y to table)
      (OP  
       '(MOVE ?x FROM ?y TO TABLE)
       :PRECONDS '((SPACE ON ?y) (SPACE ON TABLE) (?y ON ?x))
       :ADD-LIST '((EXECUTING (MOVE ?x FROM ?y TO TABLE)) (?x ON TABLE) (SPACE ON ?y))
       :DEL-LIST '((?y ON ?x))))
     ((move ?x from ?y to ?z);;has to be last or else table can be the z or y
      (OP 
       '(MOVE ?x FROM ?y TO ?z)
       :PRECONDS '((SPACE ON ?x) (SPACE ON ?z) (?y ON ?x))
       :ADD-LIST '((EXECUTING (MOVE ?x FROM ?y TO ?z)) (?x ON ?z) (SPACE ON ?y))
       :DEL-LIST '((?x ON ?y) (SPACE ON ?z))))
   )
  )
(defparameter *add-rules*
  '(      
     ((?x on table)
      (OP  
       '(MOVE ?x FROM ?y TO TABLE)
       :PRECONDS '((SPACE ON ?y) (SPACE ON TABLE) (?y ON ?x))
       :ADD-LIST '((EXECUTING (MOVE ?x FROM ?y TO TABLE)) (?x ON TABLE) (SPACE ON ?y))
       :DEL-LIST '((?y ON ?x))))
     ((space on ?x);;has to be last or else table can be the z or y
      (OP 
       '(MOVE ?x FROM ?y TO ?z)
       :PRECONDS '((SPACE ON ?x) (SPACE ON ?z) (?y ON ?x))
       :ADD-LIST '((EXECUTING (MOVE ?x FROM ?y TO ?z)) (?x ON ?z) (SPACE ON ?y))
       :DEL-LIST '((?x ON ?y) (SPACE ON ?z)))
      (OP  
       '(MOVE ?x FROM ?y TO TABLE)
       :PRECONDS '((SPACE ON ?y) (SPACE ON TABLE) (?y ON ?x))
       :ADD-LIST '((EXECUTING (MOVE ?x FROM ?y TO TABLE)) (?x ON TABLE) (SPACE ON ?y))
       :DEL-LIST '((?y ON ?x)))
      )
     ((?x on ?y)
      (op 
       '(MOVE ?x FROM TABLE TO ?y)
       :PRECONDS '((SPACE ON ?x) (SPACE ON ?y) (?y ON TABLE))
       :ADD-LIST '((EXECUTING (MOVE ?x FROM TABLE TO ?y)) (?x ON ?y))
       :DEL-LIST '((?x ON TABLE) (SPACE ON ?y)))
      (OP 
       '(MOVE ?x FROM ?y TO ?z)
       :PRECONDS '((SPACE ON ?x) (SPACE ON ?z) (?y ON ?x))
       :ADD-LIST '((EXECUTING (MOVE ?x FROM ?y TO ?z)) (?x ON ?z) (SPACE ON ?y))
       :DEL-LIST '((?x ON ?y) (SPACE ON ?z)))
       )
   )
  )
;; if goal is ?x on table then op is move ?x from ?y to table

;; if goal (space on ?y) either to table or to ?z

;;if goal (?x on ?y) either (move from ?x from table to ?y) or move ?x from ?y to ?z
(defparameter *goal-rules*
  '(
    ((space on ?x)
     ((?y on ?x) (?y on ?x)));;(look in state for ?y on ?x and then if space on ?y, move))
    ((?x on table)
     (space on ?x));;(space on x? move))
    ((?x on ?y)
     (space on ?y));;(look in state for (space on ?y) and then move))
    )
)

(defparameter *prec-rules*
  '(
    ((?y on ?x)
     (?y on b));;(look in state for ?y on ?x and then if space on ?y, move))
    )
)


(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))


;;; ==============================
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
               (achieve-all (cons '(start) state) goals nil)))

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

(defun achieve-all (state goals goal-stack)
  "Achieve each goal, trying several orderings."
  (some #'(lambda (goals) (achieve-each state goals goal-stack))
        (orderings goals)))

(defun achieve-each (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun orderings (l) 
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))

;;; ==============================

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (appropriate-ops goal state))))) ;***

(defun appropriate-ops (goal state)
  "Return a list of appropriate operators, 
  sorted by the number of unfulfilled preconditions."
  (let ((gops nil)
        (nops nil))
    (progn (setf gops (get-op goal state))
      (setf nops (sort (copy-list (find-all goal *ops* :test #'appropriate-p)) #'<
        :key #'(lambda (op) 
                 (count-if #'(lambda (precond)
                               (not (member-equal precond state)))
                           (op-preconds op)))))
      (format t "gops is: ~a~% nops is: ~a~%" gops nops)
      nops
      ))

  )
  #|(sort (copy-list (find-all goal *ops* :test #'appropriate-p)) #'<
        :key #'(lambda (op) 
                 (count-if #'(lambda (precond)
                               (not (member-equal precond state)))
                           (op-preconds op)))))
|#
;;; ==============================
;;do match, first of rest is stuff you make sure it isnt, second is what you are looking for
(defparameter *no-conf-rules*
  '(
    ((MOVE ?x from ?y to ?z)
    (SPACE ON ?z));;gotta say where z doesnt equal x or y somehow
    )
  )

(defun fix-op (goal state op)
  (setf *input* (op-action op))
  (let ((ans nil)
        (tmp nil)
        (possible-third nil)
    (progn (setf ans (mapcar #' (lambda (op) 
                                  (progn (setf tmp (rule-based-translator op *no-conf-rules*))
                                    ))))
      ))
  )
(defun get-op (goal state)
  (format t "goal is: ~a~% state is: ~a~%" goal state)
  ;;(format t "output is: ~a~%" (rule-based-translator goal *add-rules*))
  (let ((result (rule-based-translator goal *goal-rules*))
        (gres nil)
        (possible-ops (rule-based-translator goal *add-rules*))
        (tmp nil)
        (precond nil)
        (answer nil))
    (progn 
      (format t "result is:~a~%" result)
      (setf tmp (sort possible-ops #'< 
                      :key #'(lambda (g)
                          (progn (setf precond (op-preconds (eval g))) ;;get preconds
                            (format t "preconds is: ~a~%" precond)
                            ;; from state find one that matches!!!where we could bind wrong
                            (count-if #'(lambda (precond)
                                          (if (not (member-equal precond state))
                                              (rule-based-translator state (list precond precond))
                                              t)
                                               )
                           precond)
                            ))
                      ))
     ;; (format t "tmp is : ~a~%" tmp)
      (setf answer (mapcar #' (lambda (g) (eval g)) tmp))
      (format t "answer is : ~a~%~%" answer)
      answer
      )
    )
  )

#|
      (format t "result is:~a~%" result)
    (if (not (null result))
        (setf gres (member-equal (first result) state)) ;; first because its a list in a list
      nil
      )
     (format t "gres is: ~a~%" gres)
      (if (null gres)
          (setf result (some #'(lambda (g) 
                                 (progn (format t "trying: ~a with: ~a~%" g result)
                                   (rule-based-translator g result)))
                             state)))
         (format t "second result is: ~a~%~%" result)
|#
;;; ==============================
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