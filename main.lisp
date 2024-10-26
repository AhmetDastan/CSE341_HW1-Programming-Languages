;; these defines created for strict usage of varaibles
(defvar curr-line nil)
(defvar curr-type nil)
(defvar curr-convert-foo nil)
(defvar converted-line nil)
(defvar converted-lines '()) 

#| custom functions for parsing line |#

(defun split-string (line)
  ;; these function split the line according to spaces
  (let ((start 0)
        (tokens '()))
    (loop for i from 0 to (length line)
          when (or (= i (length line)) (char= (char line i) #\Space))
          do (progn
               (push (subseq line start i) tokens)
               (setf start (1+ i))))
    (reverse tokens)))

(defun remove-semicolons (line)
  ;; these functions remove semicolon from line
  (remove #\; line :count nil))

(defun contains-operator (line)
  ;; these function check given line, if found any operator return true
  (let ((operators '(#\+ #\- #\* #\/)))  ;; Operatör listesi
    (loop for op in operators
          when (search (string op) line)
          return t)))  ;; Herhangi bir operatör bulunursa TRUE döner

(defun add-spaces (line)
  ;; these function add spaces to line for before and after the special characters
  (let ((result ""))
    (loop for i from 0 to (1- (length line))
          for ch = (char line i)
          do (progn
               (cond
                 ;; check (-)-;-,
                 ((member ch '(#\( #\) #\; #\,))
                  ;; add space before the char
                  (unless (and (> i 0) (char= (char line (1- i)) #\Space))
                    (setf result (concatenate 'string result " ")))
                  ;; add the char
                  (setf result (concatenate 'string result (string ch)))
                  ;; add space after the char
                  (unless (and (< (1+ i) (length line))
                               (char= (char line (1+ i)) #\Space))
                    (setf result (concatenate 'string result " "))))
                 (t (setf result (concatenate 'string result (string ch)))))))
    ;; clear rest of line if just space
    (string-trim " " result)))
 

#| Read and Write file functions |#

(defun write-file (filename converted-lines)
  ;; This function takes converted-lines and print to target new file
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (dolist (line converted-lines)
      (format stream "~a~%" line))))
 
(defun read-file (filename line-index)
  ;; this function get filename and their index as arguments. And return the spesific line as a arguments  
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          for index from 0
          when (= index line-index)
          return line)))
 
(defun convert (line con-foo)
  ;; convert function call spesific conver-foo function
  (funcall con-foo line)) 
 
(defun line-type (line)
  ;; this function take a line string as an argument and return the struct
  (cond
    ((search  "if" line) 'if)
    ((search  "for" line) 'for)
    ((search  "while" line) 'while)
    ((search "printf" line) 'printf)
    ((search "=" line) 'assignment) 
    ((and (member (first (split-string line)) '("int" "void" "float" "double" "string") :test 'string=)
      (search "(" line) (search ")" line) (search ";" line) (not (search "=" line))) 'func-pro)
    ((and (search "(" line) (search ")" line) (search "{" line)) 'func)  ;; "( ) { " karakterini kontrol eder
    ((and (search "(" line) (search ")" line) (search ";" line)) 'func-call)  ;; "( ) ; " karakterini kontrol eder
    
    (t 'exception)))

 

(defun conversion-foo (line-type)
  ;;This function takes the line type as input and returns the appropriate conversion function for that type
  (cond
    ((eq line-type 'if) 'convert-if)
    ((eq line-type 'for) 'convert-for)
    ((eq line-type 'while) 'convert-while)
    ((eq line-type 'printf) 'convert-printf)
    ((eq line-type 'assignment) 'convert-assignment)
    ((eq line-type 'func-pro) 'convert-func-pro)
    ((eq line-type 'func) 'convert-func)
    ((eq line-type 'func-call) 'convert-func-call) 
    ((eq line-type 'exception) 'convert-exception)
    (t 'convert-unknown)))          
 

(defun convert-if (line)
  (let* ((start (position #\( line))  ;; find first '(' position
         (end (position #\) line))    ;; find last ')' position
         (condition (subseq line (+ start 1) end))  ;; get between paranthesize
         (parts (split-string condition)))  ;; split the line
    (let ((new-line (format nil "(if (~a ~a ~a)" (second parts) (first parts) (third parts))))
      ;; add '(' if there are '{' symbol
      new-line))) 


(defun convert-while (line)
  (setq line (add-spaces line))  ;; add pspace to line
  (let* ((start (position #\( line))  ;; find first '(' position
         (end (position #\) line))    ;; find last ')' position
         (condition (subseq line (+ start 1) end))  ;; get between paranthesize
         (parts (split-string condition)))  ;; split the line
    ;; create a 'while'
    (let ((new-line (format nil "(loop while (~a ~a ~a) do" (third parts) (second parts) (fourth parts))))
      ;; add '(' if there are '{' symbol
      new-line)))
  
(defun convert-for (line)
  ;; parse it for (int i = 0; i < 10; i++)
  (setq line (add-spaces line))  ;; add pspace to line
  (let* ((tokens (split-string line))  ;; split the line
         (var (nth 3 tokens))  ;; varaible token
         (start (nth 5 tokens))  ;; start token
         (step (nth 9 tokens)))  ;; step token
    (format nil "(loop for ~a from ~a below ~a do" var start step))
  )
  
(defun convert-printf (line)
  ;; Replace all "%" with "~" and then all "\n" with "~%"
  (let* ((line (substitute #\~ #\% line))                 ; Replace all '%' with '~'
         (line (loop for pos = (search "\\n" line)
                     while pos
                     do (setf line (concatenate 'string (subseq line 0 pos) "~%" (subseq line (+ pos 2))))
                     finally (return line)))
         (start (position #\" line))                      ; Find first '"' position
         (end (position #\" line :start (1+ start)))      ; Find next '"' position
         (content (subseq line (1+ start) end)))          ; Get content between double quotes
    (format nil "(format t \"~a\")" content)))



 

(defun convert-assignment (line)
  (setq line (remove-semicolons line))  ;; remove ';' symbole
  (setq line (add-spaces line))  ;; add space to line
  (cond
    ;; search '(' and ')' symbols
    ((and (search "(" line) (search ")" line))
     (let* ((tokens (split-string line)))  ;; name of varaibles
       (cond
      ;; if size of tokens length bigger than 11
      ((>= (length tokens) 11)
       (format nil "( ~a (~a ~a ~a ~a))" (nth 1 tokens) (nth 3 tokens) (nth 5 tokens) (nth 7 tokens) (nth 9 tokens)))
      ;;  if size of tokens length bigger than 9
      ((>= (length tokens) 9)
        (format nil "( ~a (~a ~a ~a))" (nth 1 tokens) (nth 3 tokens) (nth 5 tokens) (nth 7 tokens)))
      ;;  if size of tokens length bigger than 7
      ((>= (length tokens) 7)
        (format nil "( ~a (~a ~a))" (nth 1 tokens) (nth 3 tokens) (nth 5 tokens)))
      ;;  if size of tokens length bigger than 5
      ((>= (length tokens) 5)
        (format nil "(~a (~a))" (nth 1 tokens) (nth 3 tokens)))
      ;; Diğer durumlar
      (t (format nil "Invalid tokenise operation")))))  
    ;; if there is no '(' and ')' symbols
    (t
     (let* ((tokens (split-string line))  ;; splite line
            (var (first tokens))  
            (value (last tokens))) 
       (format nil "(setf (~a ~a))" var value)))))  


(defun convert-func-pro (line)
  (setq line (add-spaces line))  ;; add space to line
  (let* ((tokens (split-string line))
         ;; change token if it is integer
         (token-0 (if (string= (nth 0 tokens) "int") "integer" (nth 0 tokens)))
         (token-1 (if (string= (nth 1 tokens) "int") "integer" (nth 1 tokens)))
         (token-3 (if (string= (nth 3 tokens) "int") "integer" (nth 3 tokens)))
         (token-6 (if (string= (nth 6 tokens) "int") "integer" (nth 6 tokens)))
         (token-9 (if (string= (nth 9 tokens) "int") "integer" (nth 9 tokens))))
    (cond
      ;; if size of tokens length bigger than 10
      ((>= (length tokens) 10)
       (format nil "(declaim (ftype (function (~a ~a ~a) ~a) ~a))" 
               token-3 token-6 token-9 token-0 token-1))
      ;; if size of tokens length bigger than 7
      ((>= (length tokens) 7)
       (format nil "(declaim (ftype (function (~a ~a) ~a) ~a))" 
               token-3 token-6 token-0 token-1))
      ;; if size of tokens length bigger than 4
      ((>= (length tokens) 4)
       (format nil "(declaim (ftype (function (~a) ~a) ~a))" 
               token-3 token-0 token-1))
      ;; if size of tokens length bigger than 2
      ((>= (length tokens) 2)
       (format nil "(declaim (ftype (function () ~a) ~a))" 
               token-0 token-1))
      (t (format nil "Invalid or insufficient tokens in line")))))

  
(defun convert-func (line)
  (setq line (add-spaces line))  ;; add line to space
  (let* ((tokens (split-string line)))
    (cond
      ;; if size of tokens length bigger than 13
      ((>= (length tokens) 13)
       (format nil "(defun ~a (~a ~a ~a)" (nth 1 tokens) (nth 4 tokens) (nth 7 tokens) (nth 10 tokens) ))
      ;; if size of tokens length bigger than 10
      ((>= (length tokens) 10)
        (format nil "(defun ~a (~a ~a)" (nth 1 tokens) (nth 4 tokens) (nth 7 tokens)))
      ;; if size of tokens length bigger than 7
      ((>= (length tokens) 7)
        (format nil "(defun ~a (~a)" (nth 1 tokens) (nth 4 tokens)))
      ;; if size of tokens length bigger than 5
      ((>= (length tokens) 5)
        (format nil "(defun ~a ()" (nth 1 tokens)))
      (t (format nil "Invalid line")))))
 

(defun convert-func-call (line)
  (setq line (add-spaces line))  ;; add space to line
  (format t "line func-call: ~A~%" line)
  (let* ((tokens (split-string line)))  ;; splite the line
    (cond
      ;; if size of tokens length bigger than 9
      ((>= (length tokens) 9)
       (format nil "(~a (~a ~a ~a))" (nth 0 tokens) (nth 2 tokens) (nth 4 tokens) (nth 6 tokens)))
      ;; if size of tokens length bigger than 7
      ((>= (length tokens) 7)
       (format nil "(~a (~a ~a))" (nth 0 tokens) (nth 2 tokens) (nth 4 tokens)))
      ;; if size of tokens length bigger than 5
      ((>= (length tokens) 5)
       (format nil "(~a (~a))" (nth 0 tokens) (nth 2 tokens)))
      ;; if size of tokens length bigger than 4
      ((>= (length tokens) 4)
       (format nil "(~a))" (nth 0 tokens)))
      (t (format nil "Invalid line")))))

(defun convert-exception (line)
  (cond
    ((search "{" line) "(")
    ((search "}" line) ")")
    ((search "return" line)
      (setq line (add-spaces line))  ;; add space the line
      (setq line (remove-semicolons line))
      ;;(format t "line exception: ~A~%" line)
     (let* ((return-pos (+ (search "return" line) 6))  ;; find "return" position
            (after-return (string-right-trim " " (subseq line return-pos)))) ;; after the 'return' positino and clear
       (cond
         ;; are there nothing return empty
         ((string= after-return "") "()")
         ;; check aritmatic operation
         ((not (contains-operator after-return)) after-return)
         ;; adjust aritmatick opeartions
         (t
          (let* ((tokens (split-string after-return))  ;; split-string 
                 (operator (nth 2 tokens))  ;; Aritmetic operatör
                 (operand1 (nth 1 tokens))  ;; first operand
                 (operand2 (nth 3 tokens))) ;; second operand
            (format nil "(~a ~a ~a)" operator operand1 operand2))))))
    (t line)))


(defun recursive-conversion (counter)
  (when (read-file "main.c" counter)
    ;;(format t "Current counter: ~A~%" counter) 
    (setq curr-line (read-file "main.c" counter))  
    ;;(format t "Current line : ~A~%" curr-line)
    (setq curr-type (line-type curr-line))
    ;;(format t "Current line type: ~A~%" curr-type)
    (setq curr-convert-foo (conversion-foo curr-type))
    ;;(format t "curr-convert-foo2: ~A~%" curr-convert-foo)
    (setq converted-line (convert curr-line curr-convert-foo))
    ;;(format t "Converteed : ~A~%" converted-line)
    (if (not (null converted-line)) 
      (push converted-line converted-lines))
    (recursive-conversion (+ 1 counter))
    ))

 
 
(recursive-conversion 0)
(write-file "new.lisp" (nreverse converted-lines)) 

