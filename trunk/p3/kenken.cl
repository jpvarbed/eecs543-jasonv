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
  (format stream "Cell (~d,~d) Domain: ~a~%Neighbors: ~a~%Constraint: " (cell-x cell) (cell-y cell)
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


;(defun getCell (puzzle x y)
;  "Returns the cells struct at coordinate (x,y)."
;  
;  (aref (puzzle-cells puzzle) (- x 1) (- y 1)))


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



(defun print-solutions (puzzle) 
  "Top level call to KenKen solving algorithm. Start by deducing all values possible,
   then begin guessing and searching for solutions. Will print a list of possible 
   solutions, if any."
  
  (show-constraints puzzle)
  (map nil #'(lambda (cell) (propogate-constraints cell puzzle)) 
         (enumerate-cells puzzle))
  
  (format t "~%~%After constraint propogation the puzzle is:~%")
  (show-puzzle puzzle)
  (show-domain-size puzzle)
  
  (format t "~%~%Searching...~%~%")
  
  )


(defun propogate-constraints (cell puzzle)
  "Reduce the values in each cell's domain by considering its neighbor cells.
   Return nil only when the constraints lead to an impossible puzzle."
  ;(format t "Propogate-contraints => Cell (~d,~d)~%" (cell-x cell) (cell-y cell))
  (let ((old-dom-size (length (cell-domain cell)))
        (val (find-cell-value cell puzzle)))
    
    (unless (null val)
      ;(format t "    Value of Cell (~d,~d): ~d~%" (cell-x cell) (cell-y cell) val)
      (setf (cell-domain cell) val)
      (map nil #'(lambda (n-cell) 
                   (removeDomainVal val (cell-at puzzle (first n-cell) (second n-cell)) puzzle))
        (cell-neighbors cell))
      )
  
  ))
  
  
(defun find-cell-value (cell puzzle)
  "Check if we can deduce a cell's value by checking that all other cells in its region
   have been assigned a value."
  
  ;(format t "find-cell-value => Cell (~d,~d)~%" (cell-x cell) (cell-y cell))
  
  (let* ((constraint (cell-constraint cell))
         (region-neighbors (remove `(,(cell-x cell) ,(cell-y cell)) 
                                   (constraint-region-cells (cell-constraint cell)) :test #'equal)))
    
    ;if there are no other cell's in the constraint's region, or if all other cells in the region
    ;have a definite value, we can determine this cell's value
    (if (or (null region-neighbors)
            (all-neighbors-satisfied region-neighbors puzzle))
        (progn
          (calculate-cell cell region-neighbors puzzle))
          ;(cons (constraint-outcome (cell-constraint cell)) '()))
      nil)))
      
 
(defun calculate-cell (cell n-list puzzle)
  "Called once we know we have enough info to calculate the value for cell.
   Use its constraint and its neighbors' values to calculate value."

  (let ((op-list (cons (constraint-operation (cell-constraint cell)) '()))
        (outcome (constraint-outcome (cell-constraint cell))))
    
    ;loop through all neighbors and add their values to out op-list
    ;at this point, we know each neighbor has only a single domain value
    (append op-list (mapcan #'(lambda (n-cell)
                                (cell-domain (cell-at puzzle (first n-cell) (second n-cell))))
                      n-list))
    
    ;append every value in cell's domain to the op-list and evaluate
    ;once we see a value that matches outcome, we have our final value (return it)
    (some #'(lambda (x) (if (equal (eval (append op-list (cons x '()))) outcome)
                               (list x)))
           (cell-domain cell))

  ))


(defun all-neighbors-satisfied (neighbor-list puzzle)
  "Returns true if all cells in neighbor-list have a domain of size 1."
  (every #'(lambda (r-neighbor) 
             (let ((r-cell (cell-at puzzle (first r-neighbor) (second r-neighbor))))
               (equal (length (cell-domain r-cell)) 1)))
         neighbor-list))


(defun removeDomainVal (val cell puzzle)
  "Removes 'val' from the cell's domain if it exists in the domain."
  ;(format t "    Cell: (~d,~d) Val: ~s Before: ~a" (cell-x cell) (cell-y cell) val (cell-domain cell))

  (setf (cell-domain cell) (remove (first val) (cell-domain cell) :test #'equal))
  ;(format t "   After: ~a~%" (cell-domain cell))
  )




(defun show-puzzle (puzzle &optional (stream t))
  "Print the puzzle, showing the current known solution for each cell."
  (print-formatted-puzzle puzzle stream #'getCellValue "~%Puzzle:"))


(defun show-domain-size (puzzle &optional (stream t))
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
  (format stream "~%~%")
  nil)


(defun getCellConstraintLabel (puzzle x y)
  "Returns a character corresponding to the arithmetic constraint that 
   cell (x,y) falls under."

  (let* ((constraint (getCellConstraint puzzle x y ))
         (idx (position constraint (puzzle-constraints puzzle) :test #'equal)))
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
  
  )

(defun make-copy-puzzle (puzzle) 
  "Returns a copy of the puzzle"
  ;;need to figure out constraints
   (let* ((new (make-diagram 
                :cells (mapcar #'copy-cells
                         (puzzle-cells puzzle))
                :constraints puzzle-constraints puzzle
                :size puzzle-size puzzle)))
     ;; Put in the neighbors for each cell
     (progn
       ;;set constraints in each cell
       ;;constraints overall
       ;;Neighbors is good
    (dolist (c (puzzle-cells new))
      (setf (cell-neighbors c)
            (mapcar #'(lambda (neighbor) 
                        (cell-at neighbor-x neighbor-y new))
                    (cell-neighbors c)))))
    new)
  )


(defun genList (max)
  "Builds a list of the numbers 1 to max"
  
  (let ((l nil))
    (loop for n from 1 to max do
          (push n l))
    (nreverse l)))

