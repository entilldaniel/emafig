;;; -*- lexical-binding:t -*-
;; A private plugin for posting thoughts to figueroa.se
;; Minor mode created by following daviwil (systemcrafters tutorial)
;; https://systemcrafters.net/learning-emacs-lisp/creating-minor-modes/
;;
;;
;;

(define-minor-mode emafig-mode
  "Toggles the emafig mode"
  nil ; Initial value, nil for disabled
  :global nil
  :group 'markdown
  :lighter " emafig"
  :keymap
  (list (cons (kbd "C-c r") (lambda ()
                              (interactive)
                              (emafig-convert-and-send))))
  (if emafig-mode
      (message "EMAFIG mode enabled")
    (message "EMAFIG mode disabled")))

(require 'url)

(defvar emafig-token "CHANGEME"
  "The token that you created in the admin backend.")
(defvar emafig-host "http://localhost:4000"
  "The host for emafig")

(defun emafig-open-template-buffer (has_image)
  "Open a new buffer formatted for sending a thought."
  (interactive (list (y-or-n-p "Is this an image thought?")))
  (let ((buffer-name "*Figueroa - Create Thought*")
        (template-content "# \n#tags\n\nBody"))
    (get-buffer-create buffer-name)
    (switch-to-buffer buffer-name)
    (erase-buffer)
    (insert template-content)
    (markdown-mode)
    (emafig-mode)
    (with-current-buffer (current-buffer)
      (progn
        (setq is_image has_image)
        (make-local-variable 'is_image)))
    (goto-char (+ (point-min) 2))))

(defun emafig-convert-buffer-to-thought ()
  (let* ((raw-thought (buffer-substring-no-properties (point-min) (point-max)))
         (parts (string-split raw-thought "\n"))
         (title (substring (car parts) 2))
         (tags (emafig-convert-to-tags (cadr parts)))
         (body (string-join (cl-subseq parts 3 (length parts)) "\n")))
    `((title . ,title)
      (body . ,body)
      (is_image . ,is_image)
      (tags . ,tags)))) ;; json-serialize needs a vector and not a list.

(defun emafig-convert-and-send ()
  (interactive)
  (let* ((thought (emafig-convert-buffer-to-thought))
         (json (json-serialize thought :false-object nil))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " emafig-token))
            ("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string json 'utf-8)))
    (url-retrieve-synchronously (concat `,emafig-host "/api/thoughts"))
    (kill-buffer "*Figueroa - Create Thought*")))

(defun emafig-convert-to-tags (tags-line)
  (vconcat (mapcar (lambda (x)
                     (substring x 1)) (string-split tags-line " "))))

