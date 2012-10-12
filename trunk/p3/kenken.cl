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


;currently aren't using this...do we need to?
;right now cells are essentially represented by a list of possible values
(defstruct cell
  (domain   nil :type list)   ;of possible cell values (1 - dimension)
  (constraint nil :type list) 
  (neighbors nil :type list)) ;list of all cells considered neighbors


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
    (loop for x from 0 to (- (puzzle-size puzzle) 1) do
          (loop for y from 0 to (- (puzzle-size puzzle) 1) do
                (setf cell-list (cons (aref (puzzle-cells puzzle) x y) cell-list))))
    cell-list))


(defun create-puzzle (dimension &optional (tuples nil))
  "Creates a kenken puzzle with the specified dimensions and sets up subregions
   based on the specified tuples"
  
  (let ((puzzle (make-puzzle :cells (make-array `(,dimension ,dimension))
                             :size dimension
                             :constraints tuples)))

    (loop for x from 1 to dimension do
          (loop for y from 1 to dimension do
                (setf (aref (puzzle-cells puzzle) (- x 1) (- y 1))
                  (create-cell puzzle dimension x y))))
    puzzle))


;(defun getCell (puzzle x y)
;  "Returns the cells struct at coordinate (x,y)."
;  
;  (aref (puzzle-cells puzzle) (- x 1) (- y 1)))


(defun create-cell (puzzle dimension x y)
  "Initializes a cell by generating its initial domain of possible values, finds its 
   constraint from all possible constraints, and generates a list of neighbors."

  (let ((constraint (some #'(lambda (constraint)
                              (let ((coord-list (first (rest (rest constraint)))))
                                (if (member-equal `(,x ,y) coord-list)
                                    constraint)))
                          (puzzle-constraints puzzle))))
    
    (make-cell :domain (genList dimension)
               :constraint constraint
               :neighbors (genNeighborList x y dimension constraint))))


(defun genNeighborList (x y dimension constraint)
  "Builds a list of neighbors for a cell (x,y) by adding all cells from similar
   rows, as well as other cells listed in cell (x,y)'s constraint."
  
  (let ((neighbors (first (rest (rest constraint)))))
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
  
  
  )


;(defun print-labelings (diagram)
;  "Label the diagram by propagating constraints and then
;  searching for solutions if necessary.  Print results."
;  (show-diagram diagram "~&The initial diagram is:")
;  (every #'propagate-constraints (diagram-vertexes diagram))
;  (show-diagram diagram
;                "~2&After constraint propagation the diagram is:")
;  (let* ((solutions (if (impossible-diagram-p diagram)
;                        nil
;                        (search-solutions diagram)))
;         (n (length solutions)))
;    (unless (= n 1)
;      (format t "~2&There are ~r solution~:p:" n)
;      (mapc #'show-diagram solutions)))
;  (values))


(defun show-puzzle (puzzle &optional (stream t))
  "Print the puzzle, showing the current known solution for each cell."

  (print-formatted-puzzle puzzle stream #'getCellValue "~%Puzzle:")
)


(defun show-domain-size (puzzle &optional (stream t))
  "Print the puzzle, showing the number of possible values for each cell."
  
  (print-formatted-puzzle puzzle stream #'getCellDomainSize "~%Domain Sizes:"))


(defun show-constraints (puzzle &optional (stream t))
  "Print the puzzle, showing each cell's arithmetic constaints."
  
  (print-formatted-puzzle puzzle stream #'getCellConstraintLabel "~%Arithmetic Constraints:")
  (print-constraint-list puzzle stream))


(defun print-constraint-list (puzzle stream)
  
  (let ((char-val 65))
    (every #'(lambda (constraint)
               (format stream "Constraint ")
               (write-char (code-char char-val) stream)
               (setf char-val (+ 1 char-val))
               (format stream ": ~s~%" constraint)
               t)
           (puzzle-constraints puzzle)))
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
  
  (let ((cell (aref (puzzle-cells puzzle) (- x 1) (- y 1))))
    (cell-constraint cell)))
  

(defun getCellDomainSize (puzzle x y)
  "Returns the number of possible values in the domain of a given
   (x,y) cell"
  
  (let ((cell (aref (puzzle-cells puzzle) (- x 1) (- y 1))))
    (length (cell-domain cell))))


(defun getCellValue (puzzle x y)
  "Returns the value for cell (x,y) if it is known, '.' is it is still uknown,
   or '?' if there is no possible value to satisfy all constraints for that cell"
 
  (let* ((cell (aref (puzzle-cells puzzle) (- x 1) (- y 1)))
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
  "TODO: do some error checking on a puzzle to make sure it is correctly formed.")
  
  )


(defun genList (max)
  "Builds a list of the numbers 1 to max"
  
  (let ((l nil))
    (loop for n from 1 to max do
          (push n l))
    (nreverse l)))

