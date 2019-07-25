;;; packages.el --- frtmelody Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2016 frtmelody
;;
;; Author: frtmelody <guanghui8827@gmail.com>
;; URL: https://github.com/frtmelody/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.

(setq frtmelody-programming-packages
      '(
        css-mode
        paredit
        cmake-font-lock
        cmake-mode
        flycheck
        (nodejs-repl-eval :location local)
        (ox-confluence-en :location local)
        js2-mode
        js2-refactor
        json-mode
        racket-mode
        yasnippet
        web-mode
        js-doc
        prettier-js
        go-mode
        lua-mode
        (cc-mode :location built-in)
        etags-select
        (emacs-lisp :location built-in)
        company
        (eldoc :location built-in)
        dumb-jump
        graphviz-dot-mode
        cider
        robe
        (python :location built-in)
        counsel-etags
        lispy))

;; configuration scheme
;; https://prettier.io/docs/en/configuration.html#configuration-schema
(defun frtmelody-programming/post-init-prettier-js ()
  (use-package prettier-js
    :defer t
    :init
    ;; prettier js
    (spacemacs/add-to-hooks 'prettier-js-mode '(js2-mode-hook
                                                typescript-mode-hook
                                                rjsx-mode-hook
                                                json-mode-hook
                                                css-mode-hook
                                                markdown-mode-hook
                                                gfm-mode-hook))
    :config
    (progn
      (setq prettier-js-show-errors (quote echo))

      (spacemacs|diminish prettier-js-mode " Ⓟ" " P")

      ;; bind key
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode "=" 'prettier-js)
      (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "=" 'prettier-js)
      (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "=" 'prettier-js)
      (spacemacs/set-leader-keys-for-major-mode 'json-mode "=" 'prettier-js)
      (spacemacs/set-leader-keys-for-major-mode 'css-mode "=" 'prettier-js)
      (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "=" 'prettier-js)
      (spacemacs/set-leader-keys-for-major-mode 'gfm-mode "=" 'prettier-js))))

(defun frtmelody-programming/post-init-robe ()
  (progn
    (add-hook 'inf-ruby-mode-hook 'spacemacs/toggle-auto-completion-on)
    (defun frtmelody/ruby-send-current-line (&optional print)
      "Send the current line to the inferior Ruby process."
      (interactive "P")
      (ruby-send-region
       (line-beginning-position)
       (line-end-position))
      (when print (ruby-print-result)))

    (defun frtmelody/ruby-send-current-line-and-go ()
      (interactive)
      (frtmelody/ruby-send-current-line)
      (ruby-switch-to-inf t))

    (defun frtmelody/start-inf-ruby-and-robe ()
      (interactive)
      (when (not (get-buffer "*ruby*"))
        (inf-ruby))
      (robe-start))

    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "sb" 'ruby-send-block
        "sB" 'ruby-send-buffer
        "sl" 'frtmelody/ruby-send-current-line
        "sL" 'frtmelody/ruby-send-current-line-and-go
        "sI" 'frtmelody/start-inf-ruby-and-robe))))

(defun frtmelody-programming/init-editorconfig ()
  (use-package editorconfig
    :init
    (progn
      (defun conditional-enable-editorconfig ()
        (if (and (frtmelody/git-project-root)
                 (locate-dominating-file default-directory ".editorconfig"))
            (editorconfig-apply)))
      (add-hook 'prog-mode-hook 'conditional-enable-editorconfig))))


(defun frtmelody-programming/post-init-cider ()
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

  (defun frtmelody/cider-figwheel-repl ()
    (interactive)
    (save-some-buffers)
    (with-current-buffer (cider-current-repl-buffer)
      (goto-char (point-max))
      (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
      (cider-repl-return)))

  (global-set-key (kbd "C-c C-f") #'frtmelody/cider-figwheel-repl))

(defun frtmelody-programming/post-init-graphviz-dot-mode ()
  (with-eval-after-load 'graphviz-dot-mode
    (require 'company-keywords)
    (push '(graphviz-dot-mode  "digraph" "node" "shape" "subgraph" "label" "edge" "bgcolor" "style" "record") company-keywords-alist)))

(defun frtmelody-programming/post-init-dumb-jump ()
  (setq dumb-jump-selector 'ivy)
  (defun my-dumb-jump ()
    (interactive)
    (evil-set-jump)
    (dumb-jump-go))
  (global-set-key (kbd "C-s-g") 'my-dumb-jump))

(defun frtmelody-programming/post-init-clojure-mode ()
  )

(defun frtmelody-programming/post-init-emacs-lisp ()
  (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode))


(defun frtmelody-programming/post-init-go-mode ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

(defun frtmelody-programming/post-init-js-doc ()
  (setq js-doc-mail-address "frtmelody@gmail.com"
        js-doc-author (format "Ning Xue <%s>" js-doc-mail-address)
        js-doc-url "http://www.frtmelody.tech"
        js-doc-license "MIT")

  )


(defun frtmelody-programming/init-ctags-update ()
  (use-package ctags-update
    :init
    :defer t
    :config
    (spacemacs|hide-lighter ctags-auto-update-mode)))


(defun frtmelody-programming/post-init-web-mode ()
  (with-eval-after-load "web-mode"
    (web-mode-toggle-current-element-highlight)
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
    (web-mode-dom-errors-show))
  (setq company-backends-web-mode '((company-dabbrev-code
                                     company-keywords
                                     company-etags)
                                    company-files company-dabbrev)))



(defun frtmelody-programming/post-init-yasnippet ()
  (progn
    (set-face-background 'secondary-selection "gray")
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
                                                                      org-mode-hook
                                                                      markdown-mode-hook))

    (spacemacs/add-to-hooks 'frtmelody/load-yasnippet '(prog-mode-hook
                                                        markdown-mode-hook
                                                        org-mode-hook))
    ))

(defun frtmelody-programming/post-init-racket-mode ()
  (progn
    (eval-after-load 'racket-repl-mode
      '(progn
         (define-key racket-repl-mode-map (kbd "]") nil)
         (define-key racket-repl-mode-map (kbd "[") nil)))

    (add-hook 'racket-mode-hook #'(lambda () (lispy-mode 1)))
    (add-hook 'racket-repl-mode-hook #'(lambda () (lispy-mode t)))
    ;; (add-hook 'racket-repl-mode-hook #'(lambda () (smartparens-mode t)))
    ))

(defun frtmelody-programming/post-init-json-mode ()
  (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.fire\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.fire.meta\\'" . json-mode))
  (spacemacs/set-leader-keys-for-major-mode 'json-mode
    "ti" 'my-toggle-web-indent))



(defun frtmelody-programming/init-flycheck-package ()
  (use-package flycheck-package)
  (add-hook 'java-mode-hook 'flycheck-mode)
  (add-hook 'kotlin-mode-hook 'flycheck-mode)

  )

(defun frtmelody-programming/init-lispy ()
  (use-package lispy
    :defer t
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'ielm-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'inferior-emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      ;; (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))
      )
    :config
    (progn
      (push '(cider-repl-mode . ("[`'~@]+" "#" "#\\?@?")) lispy-parens-preceding-syntax-alist)

      (spacemacs|hide-lighter lispy-mode)
      (define-key lispy-mode-map (kbd "s-j") 'lispy-splice)
      (define-key lispy-mode-map (kbd "s-k") 'paredit-splice-sexp-killing-backward)

      (with-eval-after-load 'cider-repl
        (define-key cider-repl-mode-map (kbd "C-s-j") 'cider-repl-newline-and-indent))

      (add-hook
       'minibuffer-setup-hook
       'conditionally-enable-lispy)
      (define-key lispy-mode-map (kbd "s-m") 'lispy-mark-symbol)
      (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
      (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline))))


(defun frtmelody-programming/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))

(defun frtmelody-programming/init-google-c-style ()
  (use-package google-c-style
    :init (add-hook 'c-mode-common-hook 'google-set-c-style)))

(defun frtmelody-programming/post-init-cmake-mode ()
  (progn
    (spacemacs/declare-prefix-for-mode 'cmake-mode
      "mh" "docs")
    (spacemacs/set-leader-keys-for-major-mode 'cmake-mode
      "hd" 'cmake-help)
    (add-hook 'cmake-mode-hook (function cmake-rename-buffer))))


(defun frtmelody-programming/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (progn
      ;; (setq flycheck-display-errors-delay 0.9)
      ;; (setq flycheck-idle-change-delay 2.0)
      ;; disable jshint since we prefer eslint checking
      ;; disable json-jsonlist checking for json files
      (setq-default flycheck-disabled-checkers
                    (append flycheck-disabled-checkers
                            '(javascript-jshint
                              json-jsonlist)))
      )))

(defun frtmelody-programming/post-init-eldoc ()
  (setq eldoc-idle-delay 0.4))


(defun frtmelody-programming/post-init-js2-refactor ()
  (progn
    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "r>" 'js2r-forward-slurp
      "r<" 'js2r-forward-barf)))

(defun frtmelody-programming/post-init-js2-mode ()
  ;; js default variables
  ;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-javascript.el
  (setq-default js2-strict-inconsistent-return-warning nil ; return <=> return null
                js2-skip-preprocessor-directives t
                js2-bounce-indent-p t
                ;; Let flycheck handle parse errors
                js2-strict-trailing-comma-warning nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil
                js2-highlight-external-variables t)
  (evilified-state-evilify js2-error-buffer-mode js2-error-buffer-mode-map)
  )

(defun frtmelody-programming/post-init-css-mode ()
  (progn
    (dolist (hook '(css-mode-hook sass-mode-hook less-mode-hook))
      (add-hook hook 'rainbow-mode))

    (defun css-imenu-make-index ()
      (save-excursion
        (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

    (add-hook 'css-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'css-imenu-make-index)))))

(defun frtmelody-programming/post-init-tagedit ()
  (add-hook 'web-mode-hook (lambda () (tagedit-mode 1))))

;; For each extension, define a function frtmelody/init-<extension-name>
;;
(defun frtmelody-programming/init-doxymacs ()
  "Initialize doxymacs"
  (use-package doxymacs
    :init
    (add-hook 'c-mode-common-hook 'doxymacs-mode)
    :config
    (progn
      (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
      (spacemacs|hide-lighter doxymacs-mode))))

;; https://atlanis.net/blog/posts/nodejs-repl-eval.html
(defun frtmelody-programming/init-nodejs-repl-eval ()
  (use-package nodejs-repl-eval
    :commands (nodejs-repl-eval-buffer nodejs-repl-eval-dwim nodejs-repl-eval-function)
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'js2-mode
        "ms" "REPL")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "sb" 'nodejs-repl-eval-buffer
        "sf" 'nodejs-repl-eval-function
        "sd" 'nodejs-repl-eval-dwim))
    :defer t
    ))

(defun frtmelody-programming/post-init-lua-mode ()
  (progn
    (add-hook 'lua-mode-hook 'evil-matchit-mode)
    ;; (add-hook 'lua-mode-hook 'smartparens-mode)
    (setq lua-indent-level 2)

    ;; add lua language, basic, string and table keywords.
    ;; (with-eval-after-load 'lua-mode
    ;;   (require 'company-keywords)
    ;;   (push '(lua-mode  "setmetatable" "local" "function" "and" "break" "do" "else" "elseif" "self" "resume" "yield"
    ;;                     "end" "false" "for" "function" "goto" "if" "nil" "not" "or" "repeat" "return" "then" "true"
    ;;                     "until" "while" "__index" "dofile" "getmetatable" "ipairs" "pairs" "print" "rawget" "status"
    ;;                     "rawset" "select" "_G" "assert" "collectgarbage" "error" "pcall" "coroutine"
    ;;                     "rawequal" "require" "load" "tostring" "tonumber" "xpcall" "gmatch" "gsub"
    ;;                     "rep" "reverse" "sub" "upper" "concat" "pack" "insert" "remove" "unpack" "sort"
    ;;                     "lower") company-keywords-alist))

    ))

(defun frtmelody-programming/post-init-cc-mode ()
  (progn
    ;; http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
    (defadvice c-lineup-arglist (around my activate)
      "Improve indentation of continued C++11 lambda function opened as argument."
      (setq ad-return-value
            (if (and (equal major-mode 'c++-mode)
                     (ignore-errors
                       (save-excursion
                         (goto-char (c-langelem-pos langelem))
                         ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                         ;;   and with unclosed brace.
                         (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
                0                       ; no additional indent
              ad-do-it)))               ; default behavior


    (setq c-default-style "linux") ;; set style to "linux"
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)
    )
  )

(defun frtmelody-programming/init-flycheck-clojure ()
  (use-package flycheck-clojure
    :defer t
    :init
    (eval-after-load 'flycheck '(flycheck-clojure-setup))))


;; when many project has the need to use tags, I will give etags-table and etags-update a try
(defun frtmelody-programming/init-etags-select ()
  (use-package etags-select
    :init
    (progn
      (define-key evil-normal-state-map (kbd "gf")
        (lambda () (interactive) (find-tag (find-tag-default-as-regexp))))

      (define-key evil-normal-state-map (kbd "gb") 'pop-tag-mark)

      (define-key evil-normal-state-map (kbd "gn")
        (lambda () (interactive) (find-tag last-tag t)))

      (evilified-state-evilify etags-select-mode etags-select-mode-map)
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "gd" 'etags-select-find-tag-at-point))))

(defun frtmelody-programming/init-gulpjs ()
  (use-package gulpjs
    :init
    (progn
      (defun frtmelody/build-engine ()
        (interactive)
        (gulpjs-start-task-with-file-name "~/Github/fireball/app.js"))

      (spacemacs/set-leader-keys "ags" 'gulpjs-start-task)
      (spacemacs/set-leader-keys "agS" 'frtmelody/build-engine)
      (spacemacs/set-leader-keys "agr" 'gulpjs-restart-task))))


(defun frtmelody-programming/init-paredit ()
  (use-package paredit
    :commands (paredit-wrap-round
               paredit-wrap-square
               paredit-wrap-curly
               paredit-splice-sexp-killing-backward)
    :init
    (progn

      (bind-key* "s-(" #'paredit-wrap-round)
      (bind-key* "s-[" #'paredit-wrap-square)
      (bind-key* "s-{" #'paredit-wrap-curly)
      )))

(defun frtmelody-programming/post-init-company ()
  (progn
    (setq company-minimum-prefix-length 1
          company-echo-delay 0
          company-tooltip-limit 20
          company-idle-delay 0.08
          company-lsp-cache-candidates t
          company-begin-commands '(self-insert-command)
          )

    (when (configuration-layer/package-usedp 'company)
      (spacemacs|add-company-backends :modes shell-script-mode makefile-bsdmake-mode sh-mode lua-mode nxml-mode conf-unix-mode json-mode graphviz-dot-mode go-mode python-mode toml-mode))
    )
    ;; define company-mode keybindings
    (with-eval-after-load 'company
      (progn
        (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
        (define-key company-active-map (kbd "s-w") 'company-show-location)
        (define-key company-active-map (kbd "M-n") nil)
        (define-key company-active-map (kbd "M-p") nil)
        (define-key company-active-map (kbd "C-n") #'company-select-next)
        (define-key company-active-map (kbd "C-p") #'company-select-previous)))
  )

(defun frtmelody-programming/post-init-python ()
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  ;; if you use pyton3, then you could comment the following line
  (setq python-shell-interpreter "python")
  (add-hook 'python-mode-hook 'ycmd-mode)
  (spacemacs|add-company-backends :backends company-ycmd :modes python-mode)

  ;; (setq company-backends-web-mode '(( company-ycmd
  ;;                                     company-dabbrev-code
  ;;                                     company-keywords
  ;;                                     company-files
  ;;                                     company-dabbrev)))
  (add-to-list 'spacemacs-jump-handlers-python-mode 'ycmd-goto-definition)
  )


(defun frtmelody-programming/init-counsel-etags ()
  (use-package counsel-etags
    :defer t
    :config
    ;; Don't ask before rereading the TAGS files if they have changed
    (setq tags-revert-without-query t)
    ;; Don't warn when TAGS files are large
    (setq large-file-warning-threshold nil)
    ;; Setup auto update now
    (add-hook 'prog-mode-hook
              (lambda ()
                (add-hook 'after-save-hook
                          'counsel-etags-virtual-update-tags 'append 'local)))))



(defun frtmelody-programming/init-lispy ()
  (use-package lispy
    :defer t
    :init
    (spacemacs/add-to-hooks (lambda () (lispy-mode)) '(emacs-lisp-mode-hook
                                                  ielm-mode-hook
                                                  inferior-emacs-lisp-mode-hook
                                                  clojure-mode-hook
                                                  scheme-mode-hook
                                                  cider-repl-mode-hook))
    :config
    (progn
      (define-key lispy-mode-map (kbd "C-a") 'mwim-beginning-of-code-or-line)

      (push '(cider-repl-mode . ("[`'~@]+" "#" "#\\?@?")) lispy-parens-preceding-syntax-alist)

      (spacemacs|hide-lighter lispy-mode)
      (define-key lispy-mode-map (kbd "s-j") 'lispy-splice)
      (define-key lispy-mode-map (kbd "s-k") 'paredit-splice-sexp-killing-backward)

      (with-eval-after-load 'cider-repl
        (define-key cider-repl-mode-map (kbd "C-s-j") 'cider-repl-newline-and-indent))

      (add-hook
       'minibuffer-setup-hook
       'conditionally-enable-lispy))))

(defun frtmelody-programming/init-ox-confluence-en ()
  (use-package ox-confluence-en)
  )

(defun frtmelody-programming/post-init-lsp-intellij ()
  (spacemacs|define-jump-handlers java-mode)
  (spacemacs//setup-lsp-jump-handler 'java-mode))
