(require 'json)

(defvar currency-convert-rates nil)

(defun currency-convert--rates-file ()
  (concat (file-name-as-directory user-emacs-directory)
          "currency-convert-rates.json"))

(defun currency-convert-load-rates ()
  (with-temp-buffer
    (insert-file-contents (currency-convert--rates-file))
    (setq currency-convert-rates (json-read))))

(defun currency-convert-download-rates ()
  "Download the latest exchange launches from the internet."
  (let* ((url-show-status nil)
         (url-mime-accept-string "application/json"))
    (with-temp-buffer
      (url-insert-file-contents "https://api.exchangeratesapi.io/latest")
      (write-region nil nil (currency-convert--rates-file)))))

(defun currency-convert--currency-names ()
  (sort (cons (cdr (assoc 'base currency-convert-rates))
              (mapcar #'symbol-name
                      (mapcar #'car
                              (cdr (assoc 'rates currency-convert-rates)))))
        #'string<))

(defun currency-convert--currency-rate (currency)
  (if (equal currency (cdr (assoc 'base currency-convert-rates))) 1
    (cdr (or (assoc currency (cdr (assoc 'rates currency-convert-rates))
                    (lambda (a b) (equal (symbol-name a) b)))
             (error "No such currency: %s" currency)))))

(defun currency-convert (amount from-currency &optional to-currency)
  (interactive
   (let* ((amount (string-to-number (read-string "Amount: ")))
          (from-currency (completing-read
                          "Currency: " (currency-convert--currency-names)
                          nil t)))
     (list amount from-currency)))
  (let* ((from-rate (currency-convert--currency-rate from-currency))
         (base-amount (/ amount from-rate)))
    (with-current-buffer-window
     "*Currency*" nil nil
     (let ((inhibit-read-only t))
       (erase-buffer)
       (dolist (to-currency (currency-convert--currency-names))
         (let* ((to-rate (currency-convert--currency-rate to-currency))
                (to-amount (* base-amount to-rate)))
           (insert (format "%10.2f %s\n" to-amount to-currency))))))))

(provide 'currency-convert)
