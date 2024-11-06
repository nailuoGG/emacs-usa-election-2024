(defvar usa-election-2024-status ""
  "Holds the latest electoral status of Harris vs Trump.")

(defvar usa-election-2024-res nil
  "Accumulated response data.")

;; Define a global variable for the modeline
(defvar usa-election-2024-mode-line ""
  "Text displayed in the modeline for the election status.")

(defvar usa-election-2024-timer nil
  "Timer for updating the election status.")

(defvar-local usa-election-2024-mode-line-format
  '(:eval usa-election-2024-mode-line)
  "Format for displaying election status in the mode-line for the current buffer.")

;; Define a custom face to manage the modeline appearance
(defface usa-election-2024-mode-line-face
  '((t (:background "steel blue" :foreground "white")))
  "Face for displaying the election status in the mode line."
  :group 'usa-election)


(defun usa-election-2024-status ()
  "Get the latest status comparing Harris vs Trump."
  usa-election-2024-status)

(defun usa-election-2024-update-modeline ()
  "Update the mode line text with the current election status."
  (setq usa-election-2024-mode-line
        (propertize (if (string-empty-p usa-election-2024-status)
                        "Election Status: (Fetching...)"
                       usa-election-2024-status)
                    'face 'usa-election-2024-mode-line-face))
  ;; Force the mode line to refresh so that changes are visible
  (force-mode-line-update))

(defun usa-election-2024-update-callback (proc event)
  "Callback function to process job output and update the status.
PROC is the process object and EVENT is the event that occurred.
EVENT is either exit or signal."
  (when (memq (process-status proc) '(exit signal))
    (with-current-buffer (process-buffer proc)
      (let ((data (buffer-string)))
        (condition-case err
            (let* ((res (json-parse-string data :object-type 'alist))
                   (candidates (alist-get 'candidates res))
                   (harris (alist-get 'electoral_votes_total
                                      (seq-find (lambda (x) (string-equal (alist-get 'last_name x) "Harris"))
                                                candidates)))
                   (trump (alist-get 'electoral_votes_total
                                     (seq-find (lambda (x) (string-equal (alist-get 'last_name x) "Trump"))
                                               candidates))))
              (setq usa-election-2024-status (format "Harris(%d) vs Trump(%d)" harris trump))
              ;; Update modeline
              (usa-election-2024-update-modeline))
          (error (message "Error parsing JSON: %S" err))))
      (kill-buffer (process-buffer proc)))))

(defun usa-election-2024-fetch-data ()
  "Fetch the latest data from the election API and update the status."
  (let ((url "https://data.ddhq.io/electoral_college/2024")
        (buffer (get-buffer-create "*usa-election-output*")))
    (with-current-buffer buffer (erase-buffer))
    (make-process :name "usa-election-process"
                  :buffer buffer
                  :command (list "curl" "-s" url)
                  :sentinel #'usa-election-2024-update-callback)))

(defun usa-election-2024-start-polling ()
  "Start polling for election data every 60 seconds for this buffer."
  (unless usa-election-2024-timer
    (message "Starting polling for USA election 2024 data in buffer: %s" (buffer-name))
    (setq usa-election-2024-timer
          (run-at-time 0 60 #'usa-election-2024-fetch-data))
    ;; Add mode-line format specific to this buffer
    (unless (member usa-election-2024-mode-line-format mode-line-format)
      (setq mode-line-format (append mode-line-format (list usa-election-2024-mode-line-format))))
    ;; Update the mode-line immediately
    (usa-election-2024-update-modeline)))

(defun usa-election-2024-stop-polling ()
  "Stop polling for election data in the current buffer."
  (when (timerp usa-election-2024-timer)
    (message "Stopping polling for USA election 2024 data in buffer: %s" (buffer-name))
    (cancel-timer usa-election-2024-timer)
    (setq usa-election-2024-timer nil))
  ;; Clear and remove the mode-line indicator for the current buffer
  (setq usa-election-2024-mode-line "")
  (setq mode-line-format (delete usa-election-2024-mode-line-format mode-line-format))
  ;; Ensure this buffer gets its mode-line updated
  (force-mode-line-update))

;; Define the buffer-local minor mode to toggle polling in individual buffers
(define-minor-mode usa-election-2024-mode
  "Minor mode to poll and display the 2024 USA election status in the mode-line for individual buffers."
  :lighter " USA Election"
  :init-value nil
  :global nil
  (if usa-election-2024-mode
      (usa-election-2024-start-polling)
    (usa-election-2024-stop-polling)))

;; Initialize the mode-line variable in mode-line-format if necessary
(unless (member '(:eval usa-election-2024-mode-line) mode-line-format)
  (setq mode-line-format (append mode-line-format '((:eval usa-election-2024-mode-line)))))

;; Start polling immediately for testing. You can turn this on and off using `usa-election-2024-mode`.
(usa-election-2024-mode)
