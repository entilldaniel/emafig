;;; -*- lexical-binding:t -*-
;; A private plugin for posting thoughts to figueroa.se
;; Minor mode created by following daviwil (systemcrafters tutorial)
;; https://systemcrafters.net/learning-emacs-lisp/creating-minor-modes/
;;
;;
;;

(require 'url)
(setq token "hltc8L1x6NCusoHqkUJUmmhdHbN8Hwfkzu5XRTKWiEqQym5n") ;; Local instance no point in trying.

(defvar emafig-mode-map (make-sparse-keymap)
  "The keymap for emafig minor mode.")

(define-key emafig-basic-mode-map (kbd "C-c r")
            (lambda ()
              (interactive)
              (emafig-convert-and-send)))

(add-to-list 'minor-mode-alist '(emafig-mode " emafig"))
(add-to-list 'minor-mode-map-alist (cons 'emafig-mode 'emafig-mode-map))

(defun emafig-open-template-buffer ()
  "Open a new buffer formatted for sending a thought."
  (interactive)
  (let ((buffer-name "*Figueroa - Create Thought*")
        (template-content "# \n#tags\n\nBody"))
    (get-buffer-create buffer-name)
    (switch-to-buffer buffer-name)
    (erase-buffer)
    (insert template-content)
    (markdown-mode)
    
    (goto-char (+ (point-min) 2))))

(defun emafig-convert-buffer-to-thought ()
  (interactive)
  (let* ((raw-thought (buffer-substring-no-properties (point-min) (point-max)))
         (parts (string-split raw-thought "\n"))
         (title (substring (car parts) 2))
         (tags (emafig-convert-to-tags (cadr parts)))
         (body (cadddr parts)))
    `((title . ,title)
      (body . ,body)
      (is_image . :false)
      (tags . ,tags)))) ;; json-serialize needs a vector and not a list.

(defun emafig-convert-and-send ()
  (interactive)
  (let* ((thought (emafig-convert-buffer-to-thought))
         (json (json-serialize thought))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Authorization" . (concat "Bearer " token))))
         (url-request-data json))
    (url-retrieve-synchronously "http://localhost:4000/api/thoughts")))

(defun emafig-convert-to-tags (tags-line)
  (vconcat (mapcar (lambda (x)
                      (substring x 1)) (string-split tags-line " "))))

