;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; to use put in .emacs file:
;; (require 'arowindow)
;; (add-hook 'window-setup-hook 'arowindow-mode)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "A-<up>") 'arowindow-up)
(define-key global-map (kbd "A-<down>") 'arowindow-down)
(define-key global-map (kbd "A-<left>") 'arowindow-left)
(define-key global-map (kbd "A-<right>") 'arowindow-right)

(defun arowindow-up ()
  (interactive)
  (let ((aro-win (window-in-direction 'above (selected-window) t nil t t)))
    (if (window-live-p aro-win)
	(select-window aro-win))))

(defun arowindow-down ()
  (interactive)
  (let ((aro-win (window-in-direction 'below (selected-window) t nil t t)))
    (if (window-live-p aro-win)
	(select-window aro-win))))

(defun arowindow-left ()
  (interactive)
  (let ((aro-win (window-in-direction 'left (selected-window) t nil t t)))
    (if (window-live-p aro-win)
	(select-window aro-win))))

(defun arowindow-right ()
  (interactive)
  (let ((aro-win (window-in-direction 'right (selected-window) t nil t t)))
    (if (window-live-p aro-win)
	(select-window aro-win))))

(define-minor-mode arowindow-mode
  "a minor mode to help one move between windows"
)

(provide 'arowindow)
