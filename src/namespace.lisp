(in-package #:parenscript)
(in-readtable :parenscript)

(defvar *obfuscated-packages* (make-hash-table))

(defun obfuscate-package (package-designator &optional
                          (symbol-map
                           (let ((symbol-table (make-hash-table)))
                             (lambda (symbol)
                               (or #1=(gethash symbol symbol-table)
                                   (setf #1# (ps-gensym "G")))))))
  (setf (gethash (find-package package-designator) *obfuscated-packages*) symbol-map))

(defun unobfuscate-package (package-designator)
  (remhash (find-package package-designator) *obfuscated-packages*))

(defun maybe-obfuscate-symbol (symbol)
  (if (aand (symbol-package symbol) (eq :external (nth-value 1 (find-symbol (symbol-name symbol) it))))
      symbol
      (aif (gethash (symbol-package symbol) *obfuscated-packages*)
           (funcall it symbol)
           symbol)))

(defvar *package-prefix-table* (make-hash-table))

(defmacro ps-package-prefix (package)
  `(gethash (find-package ,package) *package-prefix-table*))

(defun string-invert (str)
  (declare (optimize (speed 3) (compilation-speed 0) (debug 0) (safety 0))
    (simple-string str) (bit up) (bit down))
  (let ((up 0) (down 0))
    (block skip
      (loop for char of-type character across str do
               (if (upper-case-p char) (setf up 1))
               (if (lower-case-p char) (setf down 1))
               (if (= up down 1) (return-from skip str)))
      (if (= up 0) (string-upcase str) (string-downcase str)))))

(defun symbol-to-js-string (symbol &optional (mangle-symbol-name? t))
  (let* ((symbol-name (symbol-name (maybe-obfuscate-symbol symbol)))
         (identifier (if mangle-symbol-name?
                         (encode-js-identifier (if (and (keywordp symbol) 
                                                        (every #'upper-case-p (remove-if-not #'alpha-char-p symbol-name)))
                                                        (string-downcase symbol-name)
                                                        symbol-name))
                         symbol-name)))
    (aif (ps-package-prefix (symbol-package symbol))
         (concatenate 'string it identifier)
         identifier)))
