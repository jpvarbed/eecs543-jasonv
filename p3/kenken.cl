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
  (domain   nil :type list)) ; of possible cell values (1 - dimension)


(defun impossible-p () )


(defun ambiguous-p () )


(defun create-puzzle (dimension &optional (tuples nil))
  "Creates a kenken puzzle with the specified dimensions and sets up subregions
   based on the specified tuples"
  
  (make-puzzle :cells (make-array `(,dimension ,dimension) :initial-element (genList dimension))
               :size dimension
               :constraints tuples))


(defun print-solutions (puzzle) 
  "Top level call to KenKen solving algorithm. Start by deducing all values possible,
   then begin guessing and searching for solutions. Will print a list of possible 
   solutions, if any."
  
  (show-constraints puzzle)
  
  
  )







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

  (let* ((constraint (getCellConstraint puzzle x y))
         (idx (position constraint (puzzle-constraints puzzle) :test #'equal)))
    (if (not (null idx))
        ;65 is the ASCII value of 'A' 
        (code-char (+ 65 idx))
      #\?)))
         
  
(defun getCellConstraint (puzzle x y)
  "Returns the list representing cell (x,y)'s arithmetic constraint."
  
  ;increment x and y by once since we are using them to match constraints,
  ;not indexing into our cells array with them (which is 0 based)
  (setf x (+ 1 x))
  (setf y (+ 1 y))
  (some #'(lambda (constraint)
            (let ((coord-list (first (rest (rest constraint)))))
              (if (member-equal `(,x ,y) coord-list)
                  constraint)))
        (puzzle-constraints puzzle)))


(defun getCellDomainSize (puzzle x y)
  "Returns the number of possible values in the domain of a given
   (x,y) cell"
  
  (let ((dom (aref (puzzle-cells puzzle) x y)))
    (length dom)))


(defun getCellValue (puzzle x y)
  "Returns the value for cell (x,y) if it is known, '.' is it is still uknown,
   or '?' if there is no possible value to satisfy all constraints for that cell"
 
  (let ((dom (aref (puzzle-cells puzzle) x y)))
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

  
  (loop for x from 0 to (- (puzzle-size puzzle) 1) do
        (format stream "~D|" (+ x 1))
        (loop for y from 0 to (- (puzzle-size puzzle) 1) do
              (let ((print-val (funcall fname puzzle x y)))
                (if (numberp print-val)
                    (format stream "~S" print-val)
                  (write-char print-val stream))))
        (format stream "|~%")) 
  
  (format stream " ")
  (loop for ctr from 1 to (+ 2 (puzzle-size puzzle)) do
        (format stream "-"))
  (format stream "~%"))



(defun genList (max)
  "Builds a list of the numbers 1 to max"
  
  (let ((l nil))
    (loop for n from 1 to max do
          (push n l))
    (nreverse l)))

