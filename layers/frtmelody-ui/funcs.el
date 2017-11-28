
;;; funcs.el --- frtmelody Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 frtmelody
;;
;; Author: frtmelody <frtmelody@gmail.com>
;; URL: https://github.com/frtmelody/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defun frtmelody/update-persp-name ()
  (when (bound-and-true-p persp-mode)
    ;; There are multiple implementations of
    ;; persp-mode with different APIs
    (progn
      (or (not (string= persp-nil-name (safe-persp-name (get-frame-persp))))
          "Default")
      (let ((name (safe-persp-name (get-frame-persp))))
        (propertize (concat "[" name "] ")
                    'face 'font-lock-preprocessor-face
                    'help-echo "Current Layout name.")))))

(defun spaceline--unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) "➊")
   ((string= "2" str) "➋")
   ((string= "3" str) "➌")
   ((string= "4" str) "➍")
   ((string= "5" str) "➎")
   ((string= "6" str) "➏")
   ((string= "7" str) "➐")
   ((string= "8" str) "➑")
   ((string= "9" str) "➒")
   ((string= "0" str) "➓")))

(defun window-number-mode-line ()
  "The current window number. Requires `winum-mode' to be enabled."
  (when (bound-and-true-p winum-mode)
    (let* ((num (winum-get-number))
           (str (when num (int-to-string num))))
      (spaceline--unicode-number str))))

(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to
                                (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))



(defun frtmelody/layout-format-name (name pos)
  "Format the layout name given by NAME for display in mode-line."
  (let* ((layout-name (if (file-directory-p name)
                          (file-name-nondirectory (directory-file-name name))
                        name))
         (string-name (format "%s" layout-name))
         (current (equal name (spacemacs//current-layout-name)))
         (caption (concat (number-to-string (if (eq 9 pos) 0 (1+ pos)))
                          ". " string-name)))
    (if current
        ;; (propertize (concat "❰❰ " caption " ❱❱") 'face 'warning)
        (propertize (concat "★ " caption) 'face 'warning)
      caption)))


(defun frtmelody/layouts-for-title-bar ()
  "Return a one liner string containing all the layout names."
  (let* ((persp-list (or (persp-names-current-frame-fast-ordered)
                         (list persp-nil-name)))
         (spaces (if (< (display-pixel-width) 1300)
                     "    "
                   "          "))
         (formatted-persp-list
          (concat " "
                  (mapconcat (lambda (persp)
                               (frtmelody/layout-format-name
                                persp (position persp persp-list)))
                             persp-list spaces)))
         (file (if (projectile-project-p)
                   (if (buffer-file-name)
                       (s-replace (projectile-project-root) (format "【%s】" (projectile-project-name)) (buffer-file-name))
                     (buffer-name))
                 (if (buffer-file-name)
                     (if (string-match (concat "^" (getenv "HOME")) (buffer-file-name))
                         (concat "~" (substring (buffer-file-name) (length (getenv "HOME"))))
                       (buffer-file-name)) (buffer-name)))))
    (concat file "     -     " formatted-persp-list)))

(defun frtmelody/default-title-bar ()
  (if (projectile-project-p)
      (concat
       (projectile-project-name)
       (if (buffer-file-name)
           (concat "  ✈  " (substring (buffer-file-name) (length (projectile-project-root))))
         (concat "  ✈  "(buffer-name))))
    (if (buffer-file-name)
        (if (string-match (concat "^" (getenv "HOME")) (buffer-file-name))
            (concat "~" (substring (buffer-file-name) (length (getenv "HOME"))))
          (buffer-file-name)) (buffer-name))))

(defun frtmelody/toggle-title-format()
  (interactive)
  (if (equal frame-title-format '(:eval (frtmelody/layouts-for-title-bar)))
      (setq frame-title-format '(:eval (frtmelody/default-title-bar)))
    (setq frame-title-format '(:eval (frtmelody/layouts-for-title-bar))))
  (redraw-frame))
