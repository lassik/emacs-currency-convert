;;; currency-convert.el --- Currency converter -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: ISC
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-currency-convert
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; Package-Version: 0.1.0
;; Keywords: comm convenience i18n
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Convert amounts of money from one currency to another in Emacs.
;;
;; Exchange rates are downloaded from exchangeratesapi.io. They ought
;; to be accurate enough for everyday purposes. It goes without saying
;; that you should not rely on this package for investment or business
;; decisions.
;;
;;; Code:

(require 'json)
(require 'url)

(defvar currency-convert--rates nil
  "Exchange rates for all known currencies.")

(defun currency-convert--rates-file ()
  "Internal helper to get local exchange rates file name."
  (concat (file-name-as-directory user-emacs-directory)
          "currency-convert-rates.json"))

(defun currency-convert--load-rates ()
  "Internal helper to load exchange rates from local file."
  (condition-case _
      (with-temp-buffer
        (insert-file-contents (currency-convert--rates-file))
        (setq currency-convert--rates (json-read)))
    ((file-missing file-error end-of-file json-error)
     (error "Please do M-x currency-convert-update-rates"))))

(defun currency-convert--ensure-rates ()
  "Internal helper to ensure exchange rates are loaded."
  (unless currency-convert--rates
    (currency-convert--load-rates)))

(defun currency-convert-update-rates ()
  "Get the latest exchange rates from the internet."
  (interactive)
  (let* ((url-show-status nil) (url-mime-accept-string "application/json"))
    (with-temp-buffer
      (url-insert-file-contents "https://api.exchangeratesapi.io/latest")
      (write-region nil nil (currency-convert--rates-file))))
  (currency-convert--load-rates))

(defun currency-convert--currency-names ()
  "Internal helper to list all known currency names."
  (sort (cons (cdr (assoc 'base currency-convert--rates))
              (mapcar (lambda (pair) (symbol-name (car pair)))
                      (cdr (assoc 'rates currency-convert--rates))))
        #'string<))

(defun currency-convert--currency-rate (currency)
  "Internal helper to get the exchange rate for CURRENCY."
  (if (equal currency (cdr (assoc 'base currency-convert--rates))) 1
    (cdr (or (assoc currency (cdr (assoc 'rates currency-convert--rates))
                    (lambda (a b) (equal (symbol-name a) b)))
             (error "No such currency: %s" currency)))))

(defun currency-convert--display-alist (alist)
  "Internal helper to display ALIST of currency-amount pairs."
  (with-current-buffer-window
   "*Currency*" nil nil
   (let ((inhibit-read-only t))
     (erase-buffer)
     (dolist (pair alist (current-buffer))
       (let* ((currency (car pair)) (amount (cdr pair)))
         (insert (format "%10.2f %s\n" amount currency)))))))

(defun currency-convert (amount from-currency)
  "Convert AMOUNT from FROM-CURRENCY to TO-CURRENCY."
  (interactive
   (progn (currency-convert--ensure-rates)
          (let* ((amount (string-to-number (read-string "Amount: ")))
                 (from-currency
                  (completing-read
                   "Currency: " (currency-convert--currency-names) nil t)))
            (list amount from-currency))))
  (let* ((from-rate (currency-convert--currency-rate from-currency))
         (base-amount (/ amount from-rate))
         (alist
          (mapcar
           (lambda (to-currency)
             (let* ((to-rate (currency-convert--currency-rate to-currency))
                    (to-amount (* base-amount to-rate)))
               (cons to-currency to-amount)))
           (currency-convert--currency-names))))
    (when (called-interactively-p 'interactive)
      (currency-convert--display-alist alist))
    alist))

(provide 'currency-convert)

;;; currency-convert.el ends here
