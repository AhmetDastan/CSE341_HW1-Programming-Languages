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
    (t 'exception)))


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
    ((eq line-type 'exception) 'convert-exception)
    (t 'convert-unknown)))          
 

(defun convert-if (line)
  (let* ((start (position #\( line))  ;; İlk parantez pozisyonu
         (end (position #\) line))    ;; Son parantez pozisyonu
         (condition (subseq line (+ start 1) end))  ;; Parantezler arasındaki kısmı al
         (parts (split-string condition)))  ;; Şartı boşluklara göre ayır
    ;; 'if' yapısını oluşturuyoruz
    (let ((new-line (format nil "(if (~a ~a ~a)" (second parts) (first parts) (third parts))))
      ;; Eğer satırda '{' varsa, satırın sonuna '(' ekle
      (if (position #\{ line)
          (concatenate 'string new-line "(")
          new-line))))  ;; Yoksa olduğu gibi döndü

(defun split-string (line)
  ;; Satırı boşluklara göre ayıran basit bir fonksiyon
  (let ((start 0)
        (tokens '()))
    (loop for i from 0 to (length line)
          when (or (= i (length line)) (char= (char line i) #\Space))
          do (progn
               (push (subseq line start i) tokens)
               (setf start (1+ i))))
    (reverse tokens)))


  

(defun convert-while (line)
  )
(defun convert-for (line)
  )
(defun convert-statement (line)
  )

(defun convert-unknown (line) ;;(replace-regexp-in-string "while" "loop while" line)
  )

#|(defun convert-exception (line) 
   (cond
    ((search "{" line) "(")       
    ((search "}" line) ")")    
    ((search "printf" line) ")")    
    (t line)) )  |#

(defun convert-exception (line) 
  (cond
    ((search "printf(" line)  ; printf kontrolü yapıyoruz
     (let* ((start (position #\" line))            ; İlk çift tırnağın yerini bul
            (end (position #\\ line :start (1+ start)))  ; İkinci çift tırnağı bul
            (content (subseq line (1+ start) end))) ; Çift tırnaklar arasındaki kısmı al
       (format nil " (format t \"~a~a" content "~%\")"))) ; format t stringine dönüştür
    ((search "{" line) "(")   
    ((search "}" line) ")")   
    (t line)))                    ; Hiçbiri yoksa orijinal stringi döner


(defun recursive-conversion (counter)
  (when (read-file "main.c" counter)
    ;;(format t "Current counter: ~A~%" counter)
    (setq curr-line (read-file "main.c" counter))
    (setq curr-type (line-type curr-line))
    ;;(format t "Current type: ~A~%" curr-type)
    (setq curr-convert-foo (conversion-foo curr-type))
    ;;(format t "curr-convert-foo2: ~A~%" curr-convert-foo)
    (setq converted-line (convert curr-line curr-convert-foo))
    ;;(format t "Converteed : ~A~%" converted-line)
    (push converted-line converted-lines)
    
    (recursive-conversion (+ 1 counter))
    ))

;; convert function call spesific conver-foo
(defun convert (line con-foo)
  (funcall con-foo line))

(print "ananas")
(recursive-conversion 0)
(write-file "new.lisp" (nreverse converted-lines)) 
 
