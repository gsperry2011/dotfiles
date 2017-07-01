;;;;;;;;;;;;;;;;;;;;
;; Greg's init.el ;;
;;;;;;;;;;;;;;;;;;;;
;(Normal-erase-is-backspace-mode)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-user-dir (locate-user-emacs-file "lisp/elpa"))
(package-initialize)

;;;;;;;;;;;;;;;;;
;; use-package ;;
;;;;;;;;;;;;;;;;;
(require 'use-package)


;;;;;;;;;;;;;;;;;;
;; company mode ;;
;;;;;;;;;;;;;;;;;;
;; http://company-mode.github.io/
;;
;; TAB to start completion
;; M-n  = pext
;; M-p  = previous
;; <return> = choose selected
;; <tab>    = to complete the common part.
;; C-s  = Search forward through completions
;; C-r  = Search backward through completions
;; M-(digit) =  quickly complete with one of the first 10 candidates.
(use-package company
  :ensure t
  :demand t
  :bind (("<C-tab>" . company-complete))
  :init (progn
          ;; start company mode now and run it in all modes
          (add-hook 'after-init-hook 'global-company-mode)

          ;; use TAB to indent or start completion
          ;; (global-set-key (kbd "TAB") 'company-indent-or-complete-common)
          ;; don't force a match so we can type some other stuff and not have
          ;; company block my typing
          (setq company-require-match nil)
          ;; show completion number
          (setq company-show-numbers t)
          ;; look in comments and strings
          (setq company-dabbrev-code-everywhere t)

          ;; Treat all characters strung together without a space as a
          ;; single completion candidate
          ;;
          ;; https://www.gnu.org/software/grep/manual/html_node/Character-Classes-and-Bracket-Expressions.html
          ;; https://www.emacswiki.org/emacs/RegularExpression#regexp
          ;;
          ;; [:alnum:] = all alpha (upper and lower case) characters and all numbers
          ;;             same as ‘[0-9A-Za-z]’
          ;; [:punct:] = all punctuation characters
          ;;             ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~.
          ;; \sw = match all "words" (add-to-list = add to list in text mode)
          ;; \s_ = match all symbols (add-to-list = add-to-list in lisp mode)
          ;;
          ;; @note default value for both of these is "\\sw\\|\\s_"
          (setq nmaludy/alpha-numeric-punc-regexp "\\([[:alnum:]\\|[:punct:]]+\\)\\|\\sw\\|\\s_")
          (setq company-dabbrev-char-regexp nmaludy/alpha-numeric-punc-regexp)
          (setq dabbrev-abbrev-char-regexp nmaludy/alpha-numeric-punc-regexp))
  :config (progn
            ;; complete things in my current buffer using company-dabbrev
            ;; http://emacs.stackexchange.com/questions/15246/how-add-company-dabbrev-to-the-company-completion-popup
            (add-to-list 'company-backends '(company-capf :with company-dabbrev))
            (add-to-list 'company-backends '(company-capf :with company-dabbrev-code))
            ;; Sort the company suggestions by preferring things within the
            ;; current buffer before stuff outside of this buffer
            (setq company-transformers (quote (company-sort-by-occurrence)))))

;;;;;;;;;;;
;; tramp ;;
;;;;;;;;;;;
;; description:
;;   Edit files using SSH on local or remote hosts
;;
;; usage: 
;;   C-x C-f /remotehost:filename  RET (or /method:user@remotehost:filename)
;;
;; examples:
;;   C-x C-f /sudo::/etc/hosts
;;   C-x C-f /rhino@192.168.1.102:/etc/hosts
;;
;; If tramp doesn't work, it's probably the Last Login message to disable this
;; ssh into the box:
;;   ssh r@r102
;;   touch .hushlogin
(use-package tramp
  :preface (progn
             ;; Cleanup all of tramps buffers and connections
             (defun nmaludy/tramp-cleanup ()
               (interactive)
               (tramp-cleanup-all-buffers)
               (tramp-cleanup-all-connections)
               ))
  :config (progn
            (setq tramp-default-method "ssh")
            (setq shell-prompt-pattern "^[^#$%>
]*[#$%>] *")
            ;; added support for brackets
            (setq tramp-shell-prompt-pattern "\\(?:^\\|
\\)[^]#$%>
]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*"))
            ;; tramp-cache.el
            (setq tramp-persistency-file-name (locate-user-emacs-file "data/tramp")))
