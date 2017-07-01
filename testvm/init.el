
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

