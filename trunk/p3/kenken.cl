;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EECS543 Fall 2012
; Programming Assignment 3
;
; Student Name: Jason Varbedian, Dan Jonik
; uniqname: jpvarbed, djonik
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct puzzle "A grid is a two dimensional array of cells." 
  (cells       nil :type array)
  (size        nil :type atom)
  (constraints nil :type list))

(defparameter *num-guesses* 0)

;for debugging internal puzzle states externally
(defparameter *ext-puzzle* nil)
(defparameter *ext-cell* nil)

(defun cell-at (puzzle x y)
  "Returns an array reference to the cell with coordinate (x,y)."
  (aref (puzzle-cells puzzle) (- x 1) (- y 1)))

(defun set-cell-at (puzzle x y val)
  "Assigns the cell at location (x,y) the value 'val'."
  (setf (aref (puzzle-cells puzzle) (- x 1) (- y 1)) val))


(defstruct cell
  (x          nil :type atom)
  (y          nil :type atom)
  (domain     nil :type list)   ;of possible cell values (1 - dimension)
  (constraint nil :type list) 
  (neighbors  nil :type list)) ;list of all cells considered neighbors

(defun print-cell (cell &optional (stream t))
  (format stream "Cell (~d,~d) Domain: ~a  Neighbors: ~a  Constraint: " (cell-x cell) (cell-y cell)
    (cell-domain cell) (cell-neighbors cell))
  (print-constraint (cell-constraint cell)))


(defstruct constraint
  (outcome      nil :type atom)   ;result for the constrain't operation
  (operation    nil :type atom)   ;the operation for the constraint
  (region-cells nil :type list))  ;a list of the cells in the constraint's subregion
  
(defun print-constraint (constraint &optional (stream t))
  (format stream "~d~s ~a~%" (constraint-outcome constraint) (constraint-operation constraint)
    (constraint-region-cells constraint)))  


(defun impossible-cell-p (cell)
  "A cell is impossible if its domain has no values."
  (null (cell-domain cell)))

(defun ambiguous-cell-p (cell)
  "A cell is ambiguous if its domain has more than 2 values."
  (> (length (cell-domain cell)) 1))

(defun impossible-puzzle-p (puzzle)
  "A puzzle is impossible if one or more of its cells have no
   possible solutions."
  (some #'impossible-cell-p (enumerate-cells puzzle)))


(defun enumerate-cells (puzzle)
  "Creates and returns a list from the 2D array of cells in puzzle."

  (let ((cell-list '()))
    (loop for x from 1 to (puzzle-size puzzle) do
          (loop for y from 1 to (puzzle-size puzzle) do
                (setf cell-list (cons (cell-at puzzle x y) cell-list))))
    (reverse cell-list)))


(defun create-puzzle (dimension &optional (tuples nil))
  "Creates a kenken puzzle with the specified dimensions and sets up subregions
   based on the specified tuples"
  
  (let ((puzzle (make-puzzle :cells (make-array `(,dimension ,dimension))
                             :size dimension
                             :constraints (mapcar #'(lambda (tuple)
                                                      (make-constraint :outcome (first tuple)
                                                                       :operation (first (rest tuple))
                                                                       :region-cells (first (rest (rest tuple)))))
                                                      tuples))))

    (loop for x from 1 to dimension do
          (loop for y from 1 to dimension do
                (set-cell-at puzzle x y (create-cell puzzle dimension x y))))
    puzzle))



(defun create-cell (puzzle dimension x y)
  "Initializes a cell by generating its initial domain of possible values, finds its 
   constraint from all possible constraints, and generates a list of neighbors."

  (let ((constraint (some #'(lambda (constraint)
                              (let ((coord-list (constraint-region-cells constraint)))
                                (if (member-equal `(,x ,y) coord-list)
                                    constraint)))
                          (puzzle-constraints puzzle))))

    (make-cell :x x
               :y y
               :domain (genList dimension)
               :constraint constraint
               :neighbors (genNeighborList x y dimension constraint))))


(defun genNeighborList (x y dimension constraint)
  "Builds a list of neighbors for a cell (x,y) by adding all cells from similar
   rows, as well as other cells listed in cell (x,y)'s constraint."

  (let ((neighbors (constraint-region-cells constraint)))
    (loop for x-it from 1 to dimension do
          (if (and (/= x-it x)
                   (not (member-equal `(,x-it ,y) neighbors)))
                (setf neighbors (cons `(,x-it ,y) neighbors))))
    (loop for y-it from 1 to dimension do
          (if (and (/= y-it y)
                   (not (member-equal `(,x ,y-it) neighbors)))
              (setf neighbors (cons `(,x ,y-it) neighbors))))
    
   (remove `(,x ,y) neighbors :test #'equal)))



(defun print-solutions (puzzle &key 
                               (search-heuristic #'first-ambiguous)
                               (extended-consistency nil))
  "Top level call to KenKen solving algorithm. Start by deducing all values possible,
   then begin guessing and searching for solutions. Will print a list of possible 
   solutions, if any."
  
  (show-constraints puzzle)
  (every #'(lambda (cell) (propogate-constraints cell puzzle extended-consistency)) 
         (enumerate-cells puzzle))
  
  (format t "~%After constraint propogation the puzzle is:~%")
  (show-puzzle puzzle)
  (show-domain-sizes puzzle)
  
  (format t "~%~%Searching...~%~%")
  (break)
  (setf *num-guesses* 0)

  (let* ((solutions (if (impossible-puzzle-p puzzle)
                        nil
                      (search-solutions puzzle search-heuristic extended-consistency)))
         (n (length solutions)))
    
    (if (= n 1)
        (progn
          (format t "~2&There is one solution:")
          (show-puzzle (first solutions)))

      (progn
        (format t "~2&There are ~r solution~:p:" n)
        (mapc #'show-puzzle solutions))))
  (format t "~%Guesses made => ~d~%~%" *num-guesses*)
  (values))



(defun search-solutions (puzzle search-heuristic extended-consistency)  
  "Start by guessing a value for an ambiguous cell, and propogate the value searching
   for a solution."
  
  (let ((c (funcall search-heuristic puzzle)))
    
    (if (null c)
        (if (impossible-puzzle-p puzzle)
            nil
          (list puzzle))
      
      (let ((cell-c (cell-at puzzle (first c) (first (rest c)))))
        (mapcan #'(lambda (possible-val)
                    (setf *num-guesses* (+ 1 *num-guesses*))
                    (let* ((puzzle2 (make-copy-puzzle puzzle))
                           (c2 (cell-at puzzle2 (cell-x cell-c) (cell-y cell-c))))
                      (progn
                        (setf (cell-domain c2) (list possible-val))
                        (if (propogate-constraints c2 puzzle2 extended-consistency t)
                            (search-solutions puzzle2 search-heuristic extended-consistency)
                          nil))))
          (cell-domain cell-c))))))


(defun propogate-constraints (cell puzzle extended-consistency &optional (override nil))
  "Look at a cell; if it is in a subregion where every other cell in the subregion
   is assigned, calculate its value. Also, remove from its domain all values that have
   been assigned to other cells in its row and column."
  
  (let ((region-neighbors (remove `(,(cell-x cell) ,(cell-y cell)) 
                                  (constraint-region-cells (cell-constraint cell)) :test #'equal))
        (dom-size (length (cell-domain cell)))
        (tmp nil))
    
    (if (and (equal dom-size 1)
             (null override))
        t
      (progn
        (format t "domain before: ~a~%" (cell-domain cell))
        (remove-inconsistent-vals cell puzzle)
        
        (format t "domain in-between: ~a~%~%" (cell-domain cell))
        (remove-neighbor-vals (cell-x cell) (cell-y cell) puzzle)
        (format t "domain after: ~a~%~%" (cell-domain cell))
        (if (all-neighbors-satisfied region-neighbors puzzle) 
            (progn (setf tmp (calculate-cell cell region-neighbors puzzle))
              (format t "return from calc is ~a~%" tmp)
              ;(break)
              (if (equal (length tmp) 1) (setf (cell-domain cell) (first tmp)))))
          (format t "domain doubleafter: ~a~%~%" (cell-domain cell))
        (if (impossible-puzzle-p puzzle)
            nil
          (if (or (< (length (cell-domain cell)) dom-size)
                  (not (null override)))
              (every #'(lambda (coords) 
                         (propogate-constraints (cell-at puzzle (first coords) (second coords)) 
                                                puzzle extended-consistency))
                       (cell-neighbors cell))
            t))))))


(defun remove-inconsistent-vals (cell puzzle)
  "Remove any values in cell's domain that can't possibly satisfy cell's constraint
   given the remaining possible values in cell's region neighbors."
  (format t "looking at cell ~a~%" cell)
  (let ((outcome (constraint-outcome (cell-constraint cell)))
        (operation (constraint-operation (cell-constraint cell)))
        (possible-vals (region-possible-vals cell puzzle)))
    (unless (null possible-vals)
    (dolist (val (cell-domain cell))
               (if (not (some #'(lambda (param-list) 
                            (test-param-set outcome (cons val (if (atom param-list) (list param-list) param-list)) operation))
                        possible-vals))
                   (remove-domain-val val cell))))))


(defun region-possible-vals (cell puzzle)
  "Enumerates a list of possible valiues from the domains of all neighbors in 
   cell's region."
  
  (let* ((region-neighbors (remove `(,(cell-x cell) ,(cell-y cell)) 
                                   (constraint-region-cells (cell-constraint cell)) :test #'equal))
         (tmp nil))
    ;;get first cell and put each val in there in front of every possibility, so slowly trim down neighbor list
    (format t "region neighbors ~a~%" region-neighbors)
    (unless (null region-neighbors)
      (setf tmp (region-possible-vals-helper region-neighbors puzzle)))
    (format t "answer is ~a~%~%" tmp )
    tmp

  ))
(defun region-possible-vals-helper(neighbors puzzle)
  (format t "neighbors is ~a~%" neighbors)
  (let* ((first-cell (cell-at puzzle (first (first neighbors)) (second (first neighbors))) ))
    (format t "domain is ~a~%" (cell-domain first-cell))
    (if (eql (length neighbors) 1) 
        (cell-domain first-cell)
    
    (mapcan #'(lambda (dval) 
                  (mapcar #'(lambda (rest-list) (cons dval (if (atom rest-list) (list rest-list) rest-list)))
                    (region-possible-vals-helper (rest neighbors) puzzle))) 
      (cell-domain first-cell)))
    )
  )
  
(defun test-param-set (outcome val-list operation)
  "Returns true if any permutation of the val-list values evaluate to outcome."
  ;(format t "outcome: ~a val-list ~a operation ~a~%" outcome val-list operation)
  (some #'(lambda (ordering)
            (progn
              ;(print (cons operation ordering))
              ;(print (eval (cons operation ordering)))
              ;(format t "~%")
              (equal (eval (cons operation ordering)) outcome)
            ))
       (permutations val-list))) 
  

(defun remove-neighbor-vals (x y puzzle)
  "If any of cell's column/row neighbors have been assigned a value, remove it from
   cell's domain."
  
  (let ((cell (cell-at puzzle x y)))
        
    (loop for xn from 1 to (puzzle-size puzzle) do
          (unless (equal xn x)
            (let* ((n-cell (cell-at puzzle xn y))
                   (n-dom (cell-domain n-cell)))
              (if (equal (length n-dom) 1) 
                  (setf (cell-domain cell) (remove (first n-dom) (cell-domain cell)))))))
    
    (loop for yn from 1 to (puzzle-size puzzle) do
          (unless (equal yn y)
            (let* ((n-cell (cell-at puzzle x yn))
                   (n-dom (cell-domain n-cell)))
              (if (equal (length n-dom) 1)
                  (setf (cell-domain cell) (remove (first n-dom) (cell-domain cell)))))))))  



(defun find-cell-value (cell puzzle)
  "Check if we can deduce a cell's value by checking that all other cells in its region
   have been assigned a value."
  
  (let* ((region-neighbors (remove `(,(cell-x cell) ,(cell-y cell)) 
                                   (constraint-region-cells (cell-constraint cell)) :test #'equal)))

    ;if there are no other cell's in the constraint's region, or if all other cells in the region
    ;have a definite value, or if there is only 1 value in the domain, we can determine this cell's value
    (if (or (equal (length (cell-domain cell)) 1)
            (null region-neighbors)
            (all-neighbors-satisfied region-neighbors puzzle))
          (calculate-cell cell region-neighbors puzzle)
      nil)))
      
 
(defun calculate-cell (cell n-list puzzle)
  "Called once we know we have enough info to calculate the value for cell.
   Use its constraint and its neighbors' values to calculate value.
   NOTE: n-list is a list of region neighbors, not neighbors from rows/cols."
  
  (let ((operation (cons (constraint-operation (cell-constraint cell)) '()))
        (outcome (constraint-outcome (cell-constraint cell)))

        ;loop through all neighbors and add their values to out op-list
        ;at this point, we know each neighbor has only a single domain value
        (op-list (mapcan #'(lambda (n-cell)
                             (copy-list (cell-domain (cell-at puzzle (first n-cell) (second n-cell)))))
                   n-list))
        (ans nil)
        (tmp nil))

    (setf tmp (mapcar #'(lambda (x) (let ((new-op-list (append op-list (cons x '()))))
                          ;now we have all of the parameter values, we need to try all permuations
                          (some #'(lambda (perm) (if (equal (eval (append operation perm)) outcome)
                                                     (list x)
                                                   nil))
                                (permutations new-op-list))))
                (cell-domain cell)))
    (format t "tmp is ~a~%" tmp)
    (dolist (x tmp) 
      (if (not (null x)) 
                    (setf ans 
                      (if (null ans) (list x) (cons x ans)))))
    ans
    ))



(defun all-neighbors-satisfied (neighbor-list puzzle)
  "Returns true if all cells in neighbor-list have a domain of size 1."
  (every #'(lambda (r-neighbor) 
             (let ((r-cell (cell-at puzzle (first r-neighbor) (second r-neighbor))))
               (equal (length (cell-domain r-cell)) 1)))
         neighbor-list))


(defun remove-domain-val (val cell)
  "Removes 'val' from the cell's domain if it exists in the domain."

  (setf (cell-domain cell) (remove val (cell-domain cell) :test #'equal))
  (format t "for cell (~a, ~a) domain is now ~a~%" (cell-x cell) (cell-y cell) (cell-domain cell) )
  t)



(defun show-puzzle (puzzle &optional (stream t))
  "Print the puzzle, showing the current known solution for each cell."
  (print-formatted-puzzle puzzle stream #'getCellValue "~%Puzzle:"))


(defun show-domain-sizes (puzzle &optional (stream t))
  "Print the puzzle, showing the number of possible values for each cell."
  (print-formatted-puzzle puzzle stream #'getCellDomainSize "~%Domain Sizes:"))


(defun show-constraints (puzzle &optional (stream t))
  "Print the puzzle, showing each cell's arithmetic constaints."
  (print-formatted-puzzle puzzle stream #'getCellConstraintLabel "~%Arithmetic Constraints:")
  (print-constraint-list puzzle stream))


(defun print-constraint-list (puzzle stream)
  "Prints out the list of constraints with the letter label."
  (let ((char-val 65))
    (every #'(lambda (constraint)
               (format stream "Constraint ")
               (write-char (code-char char-val) stream)
               (setf char-val (+ 1 char-val))
               (format stream ": ")
               (print-constraint constraint stream)
               t)
           (puzzle-constraints puzzle)))
  (format stream "~%")
  nil)


(defun getCellConstraintLabel (puzzle x y)
  "Returns a character corresponding to the arithmetic constraint that 
   cell (x,y) falls under."

  (let* ((constraint (getCellConstraint puzzle x y ))
         (idx (position constraint (puzzle-constraints puzzle) :test #'constraint-equal)))
    (if (not (null idx))
        ;65 is the ASCII value of 'A' 
        (code-char (+ 65 idx))
      #\?)))
         
  
(defun getCellConstraint (puzzle x y)
  "Called while constructing the puzzle; returns the constraint that references
   cell (x,y)."
  (let ((cell (cell-at puzzle x y)))
    (cell-constraint cell)))
  

(defun getCellDomainSize (puzzle x y)
  "Returns the number of possible values in the domain of a given
   (x,y) cell"
  (let ((cell (cell-at puzzle x y)))
    (length (cell-domain cell))))


(defun getCellValue (puzzle x y)
  "Returns the value for cell (x,y) if it is known, '.' is it is still uknown,
   or '?' if there is no possible value to satisfy all constraints for that cell"
  (let* ((cell (cell-at puzzle x y))
         (dom (cell-domain cell)))
    (cond ((eq (length dom) 1) (first dom))
          ((null dom) #\?)
          (t #\.))))

(defun constraint-equal (c1 c2)
  "Returns true if the two constraints are the same, false otherwise."
  
  (and (equal (constraint-outcome c1) (constraint-outcome c2))
       (equal (constraint-operation c1) (constraint-operation c2))
       (equal (constraint-region-cells c1) (constraint-region-cells c2))))



(defun print-formatted-puzzle (puzzle stream fname &optional(title "~2&Diagram:")) 
  "Generic function to print out a formatted puzzle, where fname is a function
   that takes two coordinates, x and y, and returns the value that should be
   printed at that cell's location."
  
  (format stream title)
  (format t "~%  ")
  (loop for ctr from 1 to (puzzle-size puzzle) do
        (format t "~D" ctr))
  (format stream "~% ")
  
  (loop for ctr from 1 to (+ 2 (puzzle-size puzzle)) do
        (format stream "-"))
  (format stream "~%")

  
  (loop for x from 1 to (puzzle-size puzzle) do
        (format stream "~D|" (+ x 1))
        (loop for y from 1 to (puzzle-size puzzle) do
              (let ((print-val (funcall fname puzzle x y)))
                (if (numberp print-val)
                    (format stream "~S" print-val)
                  (write-char print-val stream))))
        (format stream "|~%")) 
  
  (format stream " ")
  (loop for ctr from 1 to (+ 2 (puzzle-size puzzle)) do
        (format stream "-"))
  (format stream "~%"))


(defun check-puzzle (puzzle) 
  "TODO: do some error checking on a puzzle to make sure it is correctly formed."
  ;;check if each cell is in a constraint
  ;;check that no cell is in two constraints
  ;;true if checked out to be ok
  (let ((all-constraints (mapcan #'(lambda (c) (copy-list (constraint-region-cells c))) (puzzle-constraints puzzle)))
        (counter nil))
    (every #'(lambda (c)
               (setf counter (count (list (cell-x c) (cell-y c)) all-constraints :test #'equal))
               (equal counter 1)) (enumerate-cells puzzle))
   
  ))


(defun make-copy-puzzle (puzzle) 
  "Returns a copy of the puzzle"
  
  (let* ((new (make-puzzle 
                :cells (make-array `(,(puzzle-size puzzle) ,(puzzle-size puzzle)))
                :constraints nil
                :size (puzzle-size puzzle))))

    (copy-cell-array puzzle new)
    (copy-constraint-list puzzle new)
  new))


(defun copy-cell-array (src-puzzle dest-puzzle)
  "Make a deep copy of the cells from src-puzzle and stores them in 
   dest-puzzle. Assumes dest-puzzle already has an array created to hold
   cells."

  (loop for x from 1 to (puzzle-size src-puzzle) do
        (loop for y from 1 to (puzzle-size src-puzzle) do
              (set-cell-at dest-puzzle x y 
                           (make-copy-cell (cell-at src-puzzle x y))))))


(defun copy-constraint-list (src-puzzle dest-puzzle)
  
  (setf (puzzle-constraints dest-puzzle)
    (mapcar #'(lambda (constraint)
                (make-copy-constraint constraint))
      (puzzle-constraints src-puzzle))))
  
  
(defun make-copy-cell (cell)
  "Returns a deep copy of a cell object."
  
  (make-cell
   :x (cell-x cell)
   :y (cell-y cell)
   :domain (copy-list (cell-domain cell))
   :constraint (make-copy-constraint (cell-constraint cell))
   :neighbors (copy-list (cell-neighbors cell))))


(defun make-copy-constraint (constraint)
  "Returns a deep copy of a constraint object."
  
  (make-constraint 
   :outcome (constraint-outcome constraint)
   :operation (constraint-operation constraint)
   :region-cells (copy-list (constraint-region-cells constraint))))


(defun mrv-ambiguous (puzzle)
  "find the cell with the smallest domain"
  (let* ((cell-list (enumerate-cells puzzle))
         (domain-list (mapcar #'(lambda (c) 
                                  (if (ambiguous-cell-p c) (length (cell-domain c)) 10000)) cell-list))
         (minimum-domain (apply 'min domain-list))
         (tmp-cell nil))
    (progn 
      ;(format t "domain-list ~a~% minimum-domain ~a~%" domain-list minimum-domain)
      (setf tmp-cell (find minimum-domain cell-list :key #'(lambda (c)(length (cell-domain c))) :test #'equal))
      ;(format t "tmp-cell is ~a~%" tmp-cell)
      (if (null tmp-cell)
          nil
      (list (cell-x tmp-cell) (cell-y tmp-cell)))
    )
    ))

(defun first-ambiguous (puzzle)
  "Returns the first cell with an amibigous value."
  (let ((tmp-cell (find-if #'ambiguous-cell-p (enumerate-cells puzzle))))
    (if (null tmp-cell)
        nil
      (list (cell-x tmp-cell) (cell-y tmp-cell)))))


(defun genList (max)
  "Builds a list of the numbers 1 to max"
 
  (let ((l nil))
    (loop for n from 1 to max do
          (push n l))
    (nreverse l)))


(defun permutations (bag)
  
  (if (null bag)
      '(())
    (mapcan #'(lambda (e)
                (mapcar #'(lambda (p) (cons e p))
                  (permutations
                   (remove e bag :count 1 :test #'eq))))
      bag)))

