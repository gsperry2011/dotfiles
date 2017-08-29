;;;;;;;;;;;;;;;;;;;;
;; Greg's init.el ;;
;;;;;;;;;;;;;;;;;;;;

(require 'package)

; enable melpa (with stable archive)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
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
		     ;; start company mode and enable in all buffers
		     (add-hook 'after-init-hook 'global-company-mode)))

                     (setq company-require-match nil)
                     ;; show completion number
                     (setq company-show-numbers t)
                     ;; look in comments and strings
                     (setq company-dabbrev-code-everywhere t)

            :config (progn
                     ;; complete things in my current buffer using company-dabbrev
                     ;; http://emacs.stackexchange.com/questions/15246/how-add-company-dabbrev-to-the-company-completion-popup
            (add-to-list 'company-backends '(company-capf :with company-dabbrev))
            (add-to-list 'company-backends '(company-capf :with company-dabbrev-code))
            ;; Sort the company suggestions by preferring things within the
            ;; current buffer before stuff outside of this buffer
            (setq company-transformers (quote (company-sort-by-occurrence))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;   line num mode     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

; default turn on line numbers
(global-linum-mode t)

; adding space between line numbers and lines
(setq linum-format "%d ")

;;;;;;;;;;;;;;;;;;;;;;;;;
;;  minimilist  mode   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;disable these bars of junk
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;   auto-fill mode    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable word wrap in all modes
; (setq-default auto-fill-function 'do-auto-fill)

;; adjust autofill column column size from 80
(setq-default fill-column 100)

;; set default browser for org mode links to "chrome" if chrome is the default xdg browser
(setq browse-url-browser-function 'browse-url-xdg-open)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (org python w3 helm use-package company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;
;; transparency ;;
;;;;;;;;;;;;;;;;;;

 ;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
 ;;(set-frame-parameter (selected-frame) 'alpha <both>)
 (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
 (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;   org-export        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ox-confluence)
