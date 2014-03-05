;;; jvm-mode.el --- Monitor and manage your JVMs -*- lexical-binding: t -*-

;; Copyright (C) 2014 Martin Trojer <martin.trojer@gmail.com>

;; Author: Martin Trojer <martin.trojer@gmail.com>
;; URL: https://github.com/martintrojer/jvm-mode.el
;; Version: 0.1
;; Package-Requires: ((dash "2.6.0") (emacs "24"))
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This global minor mode monitors running JVM on the local system
;; and provides the interactive function "kill-jvms" that
;; kill the jvm processes with match the provided regex.
;; Make sure the JDK command 'jps' in the path.

;; See the README for more info:
;; https://github.com/martintrojer/jvm-mode.el

;;; Code:

(require 'dash)

(defun async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the
  background. Invokes CALLBACK with the result string."
  (let ((output-buffer (generate-new-buffer " *temp*"))
        (callback-fun callback))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string (buffer-substring-no-properties
                                 (point-min)
                                 (point-max))))
             (funcall callback-fun output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(defun get-jvm-pids (callback &optional pattern)
  "Invokes CALLBACK with a list of (matching) JVM pids"
  (let ((pattern (if pattern pattern ""))
        (callback-fun callback))
    (async-shell-command-to-string
     "jps -l"
     (lambda (out)
       (funcall callback-fun
                (->> (split-string out "\n")
                     (--map (split-string it " "))
                     (--filter (not (string= (car it) "")))
                     (--filter (string-match pattern (cadr it)))
                     (--map (car it))))))))

(defun kill-jvms (&optional pattern)
  "Kills all matching JVMs"
  (interactive "sPattern: ")
  (get-jvm-pids
   (lambda (pids)
     (--each pids (shell-command-to-string (format "kill %s" it))))
   pattern))

(defvar jvm-string "jvm[]")

(defun update-jvm-string ()
  (get-jvm-pids (lambda (all-pids)
                  (setq jvm-string (format "jvm[%d]" (- (length all-pids) 1))))))

(defvar timer-object nil)

(defun start-timer ()
  (setq timer-object (run-with-timer 0 10 'update-jvm-string)))

(defun stop-timer ()
  (when timer-object
    (cancel-timer timer-object)
    (setq timer-object nil)))

(defvar jvm-old-car-mode-line-position nil)

;;;###autoload
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

;;; jvm-mode.el ends here
