; enable melpa (with stable archive)

(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

; default turn on line numbers
(global-linum-mode t)

; adding space between line numbers and lines
(setq linum-format "%d ")

;disable these bars of junk
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

; set default browser for org mode links to "chrome" if chrome is the default xdg browser
(setq browse-url-browser-function 'browse-url-xdg-open)

; default company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)
