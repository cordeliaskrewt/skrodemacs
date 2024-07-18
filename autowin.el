;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; to use, add these lines to .emacs file:
;; (require 'autowin)
;; (add-hook 'window-setup-hook 'autowin-mode)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; so that C-w can be used as a prefix key by this minor mode
(global-unset-key (kbd "C-w"))
(define-key global-map (kbd "C-w k") 'autowin-kill)

;; setting a user option used by delete-window
;; so when the current window is deleted by autowin-kill
;; the new selected window is the one that now
;; occupies the physical space where point had been located
(setq delete-window-choose-selected 'pos)

;; max characters allowed per line - for a skrode/text file
(defvar autowin-default-width 45)

;; a set of four helper functions to make me less confused
;; (window-edges nil) is the same as calling (window-edges (selected-window))
;; (window-edges) returns (LEFT TOP RIGHT BOTTOM)
(defun wnedge-left (&optional window)
  (nth 0 (window-edges window)))

(defun wnedge-right (&optional window)
  (nth 2 (window-edges window)))

(defun wnedge-top (&optional window)
  (nth 1 (window-edges window)))

(defun wnedge-bottom (&optional window)
  (nth 3 (window-edges window)))

;; helper function for autowin-which-horizon
;; (code below, as yet unnamed, possibly to be renamed)
(defun autowin-same-cdr-as-first (autowin-list)
  (let ((autowin-compar-val (cdar autowin-list)))
    (seq-take-while
     (lambda (elt) (eq (cdr elt) autowin-compar-val))
     autowin-list)))

;; autowin-which-horizon is gonna be called by autowin-horizontal-window
;; which will in turn be called by autowin-new

;; returns a list of windows which are the most eligible
;; for splitting horizontally to create a new window
(defun autowin-which-horizion ()
  ;; autowin-x-overlaps sorted max overlap to min
  (let ((autowin-x-overlaps-orted
	 (sort (autowin-x-overlaps (autowin-tallest-windows))
	       (lambda (one two) (> (cdr one) (cdr two))))))
    ;; if any tallest window has same x-coords as current window
    ;; return current window to be split
    (if (= (cdar autowin-x-overlaps-orted)
	   (window-total-width (selected-window)))
	(list (selected-window))
      ;; otherwise.. return the window(s) with most x-overlap, if any
      (if (> 0 (cdar autowin-x-overlaps-orted))
	  ;; but actually if there's multiple elts with the same cdr
	  ;; a list of all said elts' cars
	  (mapcar #'car
		  (autowin-same-cdr-as-first autowin-x-overlaps-orted))
	;; if there's no windows with x-coord overlap
	;; i need to make a list of (win . left-wnedge)
	;; to choose which window(s) to split instead
	;; sorted by smallest left-wnedge value to largest
	(let* ((autowin-swith-left-wnedge)
	       (mapcar
		(lambda (win)
		  (cons win (wnedge-left win)))
		;; rather than calling autowin-tallest-windows twice
		;; i should cache the value in a local variable
		;; actually it'll be passed into the function as a parameter ig
		(autowin-tallest-windows))
	       (autowin-ltr-wrap
		(sort autowin-swith-left-wnedge
		      (lambda (one two)
			(< (cdr one) (cdr two))))))
	  ;; filter autowin-ltr-wrap to elts where
	  ;; left-wnedge that-win >= right-wnedge this-win
	  (let ((autowin-right-of-cur
		 (seq-filter
		  (lambda (elt) (>= (cdr elt) (wnedge-right (selected-window))))
		  autowin-ltr-wrap)))
	    ;; if any, return (autowin-same-cdr-as-first filtered-list)
	    (if autowin-right-of-cur
		(mapcar #'car
			(autowin-same-cdr-as-first autowin-right-of-cur)))
	    ;; otherwise, return (autowin-same-cdr-as-first autowin-ltr-wrap)
	    (mapcar #'car
		    (autowin-same-cdr-as-first autowin-ltr-wrap))))))))

;; uses a list of windows and returns a list of the subset
;; which overlap with the current window on the x-coordinate axis
;; ACTUALLY, no
;; takes a list of windows (or uses as default the list of all windows)
;; and returns an assoc list containing said windows
;; helper function for autowin-which-horizon
(defun autowin-x-overlaps (&optional autowin-dow-list)
  ;; set up variables for the rest of the function to use
  (if (not autowin-dow-list) (setq autowin-dow-list (window-list)))
  (let ((autowin-left nil) (autowin-right nil) (autowin-results nil)
	(autowin-this (selected-window)))
    ;; compare every window to the current window in turn
    (dolist (autowin-that autowin-dow-list)
      ;; between current window & window being compared
      ;; figure out which one starts in a more left position
      (if (< (wnedge-left autowin-this) (wnedge-left autowin-that))
	  (progn (setq autowin-left autowin-this)
		 (setq autowin-right autowin-that))
	(progn (setq autowin-left autowin-that)
	       (setq autowin-right autowin-this)))
      ;; compute and save the x overlap between the windows being compared
      (push (cons autowin-that
		  (- (wnedge-right autowin-left)
		     (wnedge-left autowin-right)))
	    autowin-results))
    ;; return a list of value pairs (win . x-overlap)
    autowin-results))

;; returns a list of the tallest windows in the current frame
;; helper function for autowin-which-horizon
(defun autowin-tallest-windows ()
  ;; setting up variables for the function
  (let ((autowin-max-height 0) (autowin-tallest-wins nil))
    (dolist (autowin-win (window-list))
      ;; compare the height of each window to the tallest yet
      (let ((autowin-height (window-total-height autowin-win)))
	;; if current window is as tall as tallest, add it to list
	(if (= autowin-height autowin-max-height)
	    (push autowin-win autowin-tallest-wins)
	  ;; if current window is taller than the tallest
	  ;; reset the comparison value and make it the whole list
	  (when (> autowin-height autowin-max-height)
	    (setq autowin-max-height autowin-height)
	    (setq autowin-tallest-wins (list autowin-win))))))
    ;; return result explicitly rather than as third param from dolist
    ;; because i can't add elts to an empty list (with value nil)
    autowin-tallest-wins))

;; kills a window and the buffer displayed in it.
;; unless the buffer is displayed in (an)other window(s)
;; or is modified, and the user refuses to kill it,
;; in which case the buffer is left alive.
(defun autowin-kill (&optional autowin-window)
  (interactive)
  ;; can kill any window, but the current window is the default
  (if (not autowin-window) (setq autowin-window (selected-window)))
  ;; save the buffer so it can be killed after the window's been killed
  (let ((autowin-buf (window-buffer autowin-window)))
    (delete-window autowin-window)
    ;; kill the buffer only if it's not still open in other window(s)
    (if (not (get-buffer-window-list autowin-buf))
	;; and if the user lets you
	;; otherwise make a new window to keep displaying it in
	(if (not (kill-buffer autowin-buf))
	    (set-window-buffer (autowin-new) autowin-buf))))
  (balance-windows))

;; list of windows which overlap (selected-window) in y coords
;; helper function for autowin-new via autowin-width-reqd
;; and via autowin-wins-at-y (needed for autowin-width-at-y)
(defun autowin-yoverlap-wins ()
  (seq-filter (lambda (win)
		(and (> (wnedge-bottom) (wnedge-top win))
		     (> (wnedge-bottom win) (wnedge-top))))
	      (window-list)))

;; helper function for autowin-new via autowin-width-at-y
(defun autowin-wins-at-y (y)
  (seq-filter
   (lambda (win) (and (<= (wnedge-top win) y) (< y (wnedge-bottom win))))
   (autowin-yoverlap-wins)))

;; helper function for autowin-new via autowin-width-reqd
(defun autowin-width-at-y (y)
  (apply '+ (mapcar (lambda (win) autowin-default-width)
		    (autowin-wins-at-y y))))

;; helper function for autowin-new
(defun autowin-width-reqd ()
  (apply 'max (mapcar (lambda (win) (autowin-width-at-y (wnedge-top win)))
		      (autowin-yoverlap-wins))))

;; figures out the best place to create a new window in the current frame
;; then creates and returns said window
(defun autowin-new ()
  (let ((autowin-win nil))
    ;; if there's enough horizontal space to make a new window
    ;; to the right of the current window
    (if (<= (+ (autowin-width-reqd) autowin-default-width) (frame-width))
	;; do so
	(setq autowin-win (split-window nil nil 'right))
      ;; otherwise, make new window under current window
      (setq autowin-win (split-window)))
    (balance-windows)
    ;; return new window from the function
    autowin-win))

(define-minor-mode autowin-mode
  "a minor mode for automatic window management")

(provide 'autowin)
