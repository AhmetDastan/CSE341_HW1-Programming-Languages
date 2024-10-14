;; these defines created for strict usage of varaibles
(defvar curr-line nil)
(defvar curr-type nil)
(defvar curr-convert-foo nil)
(defvar converted-line nil)
(defvar converted-lines '()) 

;; This function takes converted-lines and print to target new file
(defun write-file (filename converted-lines)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (dolist (line converted-lines)
      (format stream "~a~%" line))))

;; this function take a line string as an argument and return the struct(kalip)
(defun line-type (line)
  (cond
    ((search  "if" line) 'if)
    ((search  "for" line) 'for)
    ((search  "while" line) 'while)
    (t 'unknown)))


;; this function get filename and their index as arguments. And return the spesific line as a arguments
(defun read-file (filename line-index)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          for index from 0
          when (= index line-index)
          return line)))

;;This function takes the line type as input and returns the appropriate conversion function for that type
(defun conversion-foo (line-type)
  (cond
    ((eq line-type 'if) 'convert-if)
    ((eq line-type 'for) 'convert-for)
    ((eq line-type 'while) 'convert-while)
    ((eq line-type 'statement) 'convert-statement)
    (t 'convert-unknown)))          

;; Convert if statements
(defun convert-if (line)
  (let ((start (search "if" line)))
    (if start
        (concatenate 'string 
                     (subseq line 0 start)        ; Before the substring
                     "when"                        ; Replacement string
                     (subseq line (+ start (length "if")))) ; After the substring
        line)))
;; Convert while loops
(defun convert-while (line)
  (replace-regexp-in-string "while" "loop while" line))
(defun convert-for (line)
  (replace-regexp-in-string "while" "loop while" line))
(defun convert-statement (line)
  (replace-regexp-in-string "while" "loop while" line))
(defun convert-unknown (line)
  (replace-regexp-in-string "while" "loop while" line))

;; convert function call spesific conver-foo
(defun convert (line con-foo)
  (funcall con-foo line))




(defun recursive-conversion (counter)
  (when (read-file "main.c" counter)
    (format t "Current counter: ~A~%" counter)
    (setq curr-line (read-file "main.c" counter))
    (setq curr-type (line-type curr-line))
    (setq curr-convert-foo (conversion-foo curr-type))
    (setq converted-line (convert curr-line curr-convert-foo))
    (push converted-line converted-lines)
    (recursive-conversion (+ 1 counter))
    ))


(print "ananas")
(recursive-conversion 0)
(write-file "new.lisp" (nreverse converted-lines)) 

#| 
(setq curr-line (read-file "main.c" 0))          ;; read a current-line
(format t "Raw line read: ~a~%" curr-line)

(setq curr-type (line-type curr-line))        ;; send current-line to get current-type
(format t "Raw curr type: ~a~%" curr-type)

(setq curr-convert-foo (conversion-foo curr-type))
(format t "Raw line convert foo: ~a~%" curr-convert-foo)
 
(setq converted-line (convert curr-line 'convert-if))  
(format t "Raw line converted line: ~a~%" converted-line)
|#