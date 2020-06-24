(require 'helm)
(require 'q-mode)

(defgroup helm-q nil
  "Helm mode for managing kdb+/q instances"
  :group 'helm)

(defcustom helm-q-config-directory "~/.helm-q/"
  "The directory containing kdb connection files"
  :group 'helm-q
  :type 'string)

;; (defcustom helm-q-action-function qcon
;;   "Function that invokes the qcon program"
;;   :group 'helm-q
;;   :type 'function)

(defun helm-q-read-configs ()
  "Read all configuration files in the config directory"
  (with-temp-buffer
    (dolist (elt (directory-files helm-q-config-directory t directory-files-no-dot-files-regexp))
      (insert-file-contents
       (expand-file-name elt helm-q-config-directory))
      )
    (goto-char (point-min))
    (split-string (buffer-string) "\n")))

;;;###autoload
(defun helm-q ()
  (interactive)
  (helm :sources (helm-build-sync-source "q-instances"
                   :candidates (helm-q-read-configs)
                   :fuzzy-match t
                   :filtered-candidate-transformer '(helm-adaptive-sort)
                   :action (helm-make-actions
                            "Run q-qcon"
                            (lambda (line)
                              ;; (helm-q-action-function (car (split-string line "\t")))
                              (qcon (car (split-string line "\t")))
                              (message line))
                            )
                   )
        :buffer "*helmtest*"
        )
  )
