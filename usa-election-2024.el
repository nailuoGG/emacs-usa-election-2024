;;; usa-election-2024.el --- Display 2024 USA election status in mode-line -*- lexical-binding: t -*-

;; Author: Your Name <your.email@example.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/yourusername/emacs-usa-election-2024

;;; Code:

(require 'url)
(require 'json)

(defgroup usa-election-2024 nil
  "Display USA Election 2024 status in mode-line."
  :group 'convenience
  :prefix "usa-election-2024-")

(defcustom usa-election-2024-update-interval 60
  "Interval in seconds between updates of election data."
  :type 'integer
  :group 'usa-election-2024)

(defcustom usa-election-2024-api-url "https://data.ddhq.io/electoral_college/2024"
  "URL for the election data API."
  :type 'string
  :group 'usa-election-2024)

;; Core variables
(defvar-local usa-election-2024-mode-line ""
  "Text displayed in the modeline for the election status.")

(defvar-local usa-election-2024-timer nil
  "Timer for updating the election status.")

(defface usa-election-2024-mode-line-face
  '((t :inherit mode-line-emphasis :weight bold))
  "Face for displaying the election status in the mode line."
  :group 'usa-election-2024)

;; Core functions
(defun usa-election-2024--format-message (harris trump)
  "Format detailed message for HARRIS and TRUMP electoral votes."
  (format "USA Election 2024 Update: Harris has %d electoral votes, Trump has %d electoral votes. (%s)"
          harris trump
          (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun usa-election-2024--update-display (harris trump)
  "Update the mode line with HARRIS and TRUMP electoral votes."
  (setq usa-election-2024-mode-line
        (propertize (format " 🗽 Harris: %d | Trump: %d " harris trump)
                    'face 'usa-election-2024-mode-line-face))
  (force-mode-line-update t)
  ;; Display message in echo area
  (message (usa-election-2024--format-message harris trump)))

(defun usa-election-2024--handle-response (status)
  "Handle the API response with STATUS."
  (unwind-protect
      (if (plist-get status :error)
          (message "Error fetching election data: %s (at %s)"
                  (plist-get status :error)
                  (format-time-string "%Y-%m-%d %H:%M:%S"))
        (goto-char (point-min))
        (re-search-forward "^$")
        (let* ((json-data (json-read))
               (candidates (alist-get 'candidates json-data))
               (harris (alist-get 'electoral_votes_total
                                (seq-find (lambda (x)
                                          (string-equal (alist-get 'last_name x) "Harris"))
                                        candidates)))
               (trump (alist-get 'electoral_votes_total
                               (seq-find (lambda (x)
                                         (string-equal (alist-get 'last_name x) "Trump"))
                                       candidates))))
          (usa-election-2024--update-display harris trump)))
    (kill-buffer)))

(defun usa-election-2024-update ()
  "Fetch and update election status."
  (url-retrieve usa-election-2024-api-url
                #'usa-election-2024--handle-response
                nil t t))

(defun usa-election-2024-start-polling ()
  "Start polling for election data."
  (unless usa-election-2024-timer
    (setq usa-election-2024-timer
          (run-with-timer 0 usa-election-2024-update-interval #'usa-election-2024-update))
    (unless (member '(:eval usa-election-2024-mode-line) mode-line-format)
      (setq-local mode-line-format
                  (append mode-line-format '((:eval usa-election-2024-mode-line)))))
    (usa-election-2024-update)))

(defun usa-election-2024-stop-polling ()
  "Stop polling for election data."
  (when usa-election-2024-timer
    (cancel-timer usa-election-2024-timer)
    (setq usa-election-2024-timer nil
          usa-election-2024-mode-line "")
    (setq-local mode-line-format
                (delete '(:eval usa-election-2024-mode-line) mode-line-format))
    (force-mode-line-update t)))

(define-minor-mode usa-election-2024-mode
  "Display 2024 USA election status in mode-line."
  :lighter nil
  :global nil
  (if usa-election-2024-mode
      (usa-election-2024-start-polling)
    (usa-election-2024-stop-polling)))

(provide 'usa-election-2024)
;;; usa-election-2024.el ends here
