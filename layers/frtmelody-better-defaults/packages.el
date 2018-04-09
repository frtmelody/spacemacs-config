;;; packages.el --- frtmelody Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2016 frtmelody
;;
;; Author: frtmelody <frtmelody@gmail.com>
;; URL: https://github.com/frtmelody/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst frtmelody-better-defaults-packages
  '(
    ;; (dired-mode :location built-in)
    (dired :location built-in)
    (profiler :location built-in)
    (recentf :location built-in))
  )

(defun frtmelody-better-defaults/post-init-recentf ()
  (progn
    (setq recentf-exclude
          '("COMMIT_MSG"
            "COMMIT_EDITMSG"
            "github.*txt$"
            "/tmp/"
            "/ssh:"
            "/sudo:"
            "/TAGS$"
            "/GTAGS$"
            "/GRAGS$"
            "/GPATH$"
            "\\.mkv$"
            "\\.mp[34]$"
            "\\.avi$"
            "\\.pdf$"
            "\\.sub$"
            "\\.srt$"
            "\\.ass$"
            ".*png$"))
    (setq recentf-max-saved-items 2048)))



(defun frtmelody-better-defaults/post-init-dired ()
  (use-package dired
    :defer t
    :init
    (progn
      (require 'dired-x)
      (require 'dired-aux)
      (setq dired-listing-switches "-alh")
      (setq dired-guess-shell-alist-user
            '(("\\.pdf\\'" "open")
              ("\\.docx\\'" "open")
              ("\\.\\(?:djvu\\|eps\\)\\'" "open")
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
              ("\\.\\(?:xcf\\)\\'" "open")
              ("\\.csv\\'" "open")
              ("\\.tex\\'" "open")
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
               "open")
              ("\\.\\(?:mp3\\|flac\\)\\'" "open")
              ("\\.html?\\'" "open")
              ("\\.md\\'" "open")))

      (setq dired-omit-files
            (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|\\.js\\.meta$\\|\\.meta$"))

      ;; always delete and copy recursively
      (setq dired-recursive-deletes 'always)
      (setq dired-recursive-copies 'always)

      (defun frtmelody-ediff-files ()
        (interactive)
        (let ((files (dired-get-marked-files))
              (wnd (current-window-configuration)))
          (if (<= (length files) 2)
              (let ((file1 (car files))
                    (file2 (if (cdr files)
                               (cadr files)
                             (read-file-name
                              "file: "
                              (dired-dwim-target-directory)))))
                (if (file-newer-than-file-p file1 file2)
                    (ediff-files file2 file1)
                  (ediff-files file1 file2))
                (add-hook 'ediff-after-quit-hook-internal
                          (lambda ()
                            (setq ediff-after-quit-hook-internal nil)
                            (set-window-configuration wnd))))
            (error "no more than 2 files should be marked"))))


      (define-key dired-mode-map "e" 'frtmelody-ediff-files)
      (defvar dired-filelist-cmd
        '(("vlc" "-L")))

      (add-hook 'dired-mode-hook '(lambda ()
                                    (with-eval-after-load 'dired
                                      (evilified-state-evilify-map dired-mode-map
                                        :mode dired-mode
                                        :bindings
                                        (kbd "C-k") 'frtmelody/dired-up-directory
                                        (kbd "~")   '(lambda ()(interactive) (find-alternate-file "~/"))
                                        "0"         'dired-back-to-start-of-files
                                        "<RET>" 'dired-find-alternate-file
                                        "E" 'dired-toggle-read-only
                                        "C" 'dired-do-copy
                                        "<mouse-2>" 'my-dired-find-file
                                        "`" 'dired-open-term
                                        "p" 'peep-dired-prev-file
                                        "n" 'peep-dired-next-file
                                        "z" 'dired-get-size
                                        "c" 'dired-copy-file-here
                                        "J" 'counsel-find-file
                                        "J"         'dired-goto-file
                                        "f" 'frtmelody/open-file-with-projectile-or-counsel-git
                                        "gg"        'vinegar/back-to-top
                                        "G"         'vinegar/jump-to-bottom
                                        ")" 'dired-omit-mode)
                                      )))
      )))


(defun frtmelody-better-defaults/init-profiler ()
  (use-package profiler
    :init
    (evilified-state-evilify profiler-report-mode profiler-report-mode-map)))

;;; packages.el ends here
