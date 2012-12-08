(in-package #:parenscript)

(defun char-invert (char)
  (if (upper-case-p char)
      (char-downcase char)
      (char-upcase char)))

(defun char-respect-readtable-case (char)
  ;; (break)
  ;; (case (print (readtable-case *readtable*))
  ;;   (:upcase (char-upcase char))
  ;;   (:downcase (char-downcase char))
  ;;   (:preserve char)
  ;;   (:invert 
     (char-invert char))


(defun respect-readtable-case (str)
  (case (readtable-case *readtable*)
    (:upcase (string-upcase str))
    (:downcase (string-downcase str))
    (:preserve str)
    (:invert (string-invert str))))

(defun encode-js-identifier% (identifier stream)
  (cond ((some (lambda (c) (find c "-*+!?#@%/=:<>^")) identifier)
                     ;(print identifier)
                      (let ((lowercase t)
                           (save-case t)
                           (all-uppercase nil))
                       (when (and (not (string= identifier "[]")) ;; HACK
                                  (find-if (lambda (x) (find x '(#\. #\[ #\]))) identifier))
                         (warn "Symbol ~A contains one of '.[]' - this compound naming convention is no longer supported by Parenscript!"
                               identifier))
                       (acond ((nth-value 1 (cl-ppcre:scan-to-strings "[\\*|\\+](.+)[\\*|\\+](.*)" identifier :sharedp t))
                               (setf all-uppercase t
                                     identifier (concatenate 'string (aref it 0) (aref it 1))))
                              ((and (> (length identifier) 1)
                                    (or (eql (char identifier 0) #\+)
                                        (eql (char identifier 0) #\*)))
                               (setf lowercase nil
                                     identifier (subseq identifier 1))))
                         (loop for c across identifier
                            do (acond
                                 ;; ((eql c #\.)
                                 ;;  (setf save-case t)
                                 ;;  (write-char c acc))
                                 ((eql c #\-)
                               ;   (setf save-case nil)
                                  (setf lowercase (not lowercase)))
                                 ((position c "!?#@%+*/=:<>^")
                                  (write-sequence (aref #("bang" "what" "hash" "at" "percent"
                                                          "plus" "star" "slash" "equals" "colon"
                                                          "lessthan" "greaterthan" "caret")
                                                        it)
                                                  acc))
                                 (t (write-char (cond
                                               ;   (save-case (char-respect-readtable-case c))
                                                  ((and lowercase (not all-uppercase)) (char-downcase c))
                                                  (t (char-upcase c)))
                                                stream)
                                    (setf lowercase t))))))
        ;((every #'upper-case-p (remove-if-not #'alpha-char-p identifier)) (write-sequence (string-downcase identifier) stream))
        ;((every #'lower-case-p (remove-if-not #'alpha-char-p identifier)) (write-sequence (string-upcase identifier) stream))
        (t (write-sequence identifier stream))))

(let ((cache (make-hash-table :test 'equal)))
  (defun encode-js-identifier (identifier)
    "Given a string, produces to a valid JavaScript identifier by
following transformation heuristics case conversion. For example,
paren-script becomes parenScript, *some-global* becomes SOMEGLOBAL."
    (or (gethash identifier cache)
        (setf (gethash identifier cache)
              (with-output-to-string (stream)
                (let ((parts (cl-utilities:split-sequence #\. identifier)))
                  (encode-js-identifier% (car parts) stream)
                  (setq parts (cdr parts))
                  (if parts
                      (loop for part in parts do
                            (write-char #\. stream)
                            (encode-js-identifier% part stream)))))))))

(defun ordered-set-difference (list1 list2 &key (test #'eql)) ; because the CL set-difference may not preserve order
  (reduce (lambda (list el) (remove el list :test test))
          (cons list1 list2)))

(defmacro once-only ((&rest names) &body body) ;; the version from PCL
  (let ((gensyms (loop for nil in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))

(defun flatten (x &optional acc)
  (cond ((null x) acc)
        ((atom x) (cons x acc))
        (t (flatten (car x) (flatten (cdr x) acc)))))
