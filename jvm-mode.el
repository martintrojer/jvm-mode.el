(require 'dash)
(require 'cl)

(defun async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the
  background. Invokes CALLBACK with the result string."
  (lexical-let ((output-buffer (generate-new-buffer " *temp*"))
                (callback-fun callback))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback-fun output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(defun get-jvm-pids (callback &optional pattern)
  "Invokes CALLBACK with a list of (matching) JVM pids"
  (interactive)
  (lexical-let ((pattern (if pattern pattern ""))
                (callback-fun callback))
    (async-shell-command-to-string
     "jps -l"
     (lambda (out)
       (funcall callback-fun
                (->> (split-string out "\n")
                  (-map (lambda (line) (split-string line " ")))
                  (-filter (lambda (pair) (not (string= (car pair) ""))))
                  (-filter (lambda (pair) (string-match pattern (cadr pair))))
                  (-map (lambda (pair) (car pair)))))))))

(defun kill-all-jvms (&optional pattern)
  "Forcefully kills all matching JVMs"
  (interactive)
  (get-jvm-pids
   (lambda (pids)
     (-each pids (lambda (pid) (shell-command-to-string (format "kill -9 %s" pid)))))
   pattern))

(defvar jvm-string "jvm[]")

(defun update-jvm-string (&optional pattern)
  (get-jvm-pids
   (lambda (pids) (setq jvm-string (format "jvm[%d]" (- (length pids) 1))))
   pattern))

(defvar timer-object nil)

(defun start-timer ()
  (setq timer-object (run-with-timer 0 10 'update-jvm-string)))

(defun stop-timer ()
  (when timer-object
    (cancel-timer timer-object)
    (setq timer-object nil)))

(defvar jvm-old-car-mode-line-position nil)

(define-minor-mode jvm-mode
    "Manage your JVMs"
    :global t
    :group 'jvm
    (if jvm-mode
        (progn
          (unless jvm-old-car-mode-line-position
            (setq jvm-old-car-mode-line-position (car mode-line-position)))
          (start-timer)
          (setcar mode-line-position '(:eval (list jvm-string))))
      (progn
        (setcar mode-line-position jvm-old-car-mode-line-position)
        (stop-timer))))

(provide 'jvm-mode)
