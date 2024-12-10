;;; -*- lexical-binding:t -*-
;; A private plugin for posting thoughts to figueroa.se

(require 'url)
(setq token "hltc8L1x6NCusoHqkUJUmmhdHbN8Hwfkzu5XRTKWiEqQym5n")

(defun open-template-buffer ()
  "Open a new buffer with pre-filled template content."
  (interactive)
  (let ((buffer-name "*Figueroa Create Thought*")
        (template-content "# \n#tags\n\nBody")) ;; Replace with your template

    (get-buffer-create buffer-name)
    (switch-to-buffer buffer-name)
    (erase-buffer)
    (insert template-content)
    (markdown-mode) 
    (goto-char (+ (point-min) 2))))


(defun convert-to-tags (tags-line)
  (vconcat (mapcar (lambda (x)
                      (substring x 1)) (string-split tags-line " "))))

(defun convert-to-thought ()
  (interactive)
  (let* ((raw-thought (buffer-substring-no-properties (point-min) (point-max)))
         (parts (string-split raw-thought "\n"))
         (title (substring (car parts) 2))
         (tags (convert-to-tags (cadr parts)))
         (body (cadddr parts)))
    `((title . ,title) (body . ,body) (is_image . :false) (tags . ,tags))))

(defun convert-to-json (payload)
  (prin1 payload)
  (json-serialize payload))

(defun convert-and-send ()
  (interactive)
  (let* ((thought (convert-to-thought))
         (json (convert-to-json thought)))
    (url-request-method "POST")
    (url-request-extra-headers
     '(("Authorization" . (concat "Bearer " token))))
    (message "Sending json")
    (url-request-data (encode-coding-string json 'utf-8))
    (url-retrieve "http://localhost:4000/api/thoughts")
    ))


(defun split-contents ()
  (interactive)
  (message (cadddr (string-split (buffer-string) "\n"))))


(global-set-key (kbd "C-c r") 'convert-and-send)




