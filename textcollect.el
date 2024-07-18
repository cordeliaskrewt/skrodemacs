;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; to use, make sure your .emacs file includes
;; (transient-mark-mode 1)
;; (require 'textcollect)
;; (add-hook 'text-mode-hook 'textcollect-mode)
;;
;; variables in textcollect mode:
;; textcollect-map, textcollect-face, textcollect mode variable
;;
;; functions in textcollect mode, listed in order:
;;
;; helper functions first:
;; in-pair-p - helper function for textcollect-add-selection-to-collection
;;   and textcollect-remove-selection-from-collection
;; overlap-pairs-p - helper function for textcollect-add-selection-to-collection
;;   and textcollect-remove-selection-from-collection
;; merge-two-pairs - helper function for textcollect-add-selection-to-collection
;; pair-excluding-pair - helper function for textcollect-remove-selection-from-
;;   collection
;;
;; textcollect-start-collection - called by textcollect-toggle-collecting
;; textcollect-add-selection-to-collection - called textcollect-end-collection
;; textcollect-highlight-selection - called by textcollect-end-collection
;; textcollect-end-collection - called by textcollect-toggle-collecting.
;;  also by textcollect-remove-from-collection and by textcollect-get-string,
;;    if textcollect-active variable is non-nil.
;;
;; textcollect-toggle-collecting - called by user (f1)
;; textcollect-remove-selection-from-collection - guts of textcollect-remove-
;;   from-collection
;; textcollect-remove-from-collection - called by user (alt-f1)
;; textcollect-get-string - called by user (by name)
;; textcollect-clear-collection - called by user (cmd-f1)
;; textcollect-delete-collection - called by user (f2)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar textcollect-map nil)
(progn (setq textcollect-map (make-sparse-keymap))
       (set-keymap-parent textcollect-map text-mode-map)
       ;; turn collection mode on or off with f1
       (define-key textcollect-map (kbd "<f1>")
	 'textcollect-toggle-collecting)
       ;; remove region from collection with option-f1
       (define-key textcollect-map (kbd "A-<f1>")
	 'textcollect-remove-from-collection)
       ;; empty collection in current buffer with cMd-f1
       (define-key textcollect-map (kbd "M-<f1>")
	 'textcollect-clear-collection)
       ;; delete all text in the collection in current buffer
       ;; with Ctl-f1. also calls clear-collection at the end.
       (define-key textcollect-map (kbd "<f2>")
	 'textcollect-delete-collection))

(defface textcollect-face '((t :background "#f8df9c"))
  "face used by textcollect mode to highlight selected regions.
in textcollect-start-collection and textcollect-end-collection.")

(defun in-pair-p (n pair)
  "takes a numeric value n and a cons cell pair with two numeric values where
the car >= the cdr. returns t if n is in between the pair of values
and nil otherwise. used by overlap-pairs-p."
  (and (>= n (car pair))
       (<= n (cdr pair))))

(defun overlap-pairs-p (pair qair)
  "takes two cons cells each containing a pair of numeric values, car >= cdr.
returns t if the two pairs overlap in value, and nil otherwise.
used by textcollect-add-selection-to-collection."
  (or (in-pair-p (car pair) qair)
      (in-pair-p (car qair) pair)))

(defun merge-two-pairs (pair qair)
  "takes two cons cells each containing a pair of markers, car >= cdr, which
overlap in some way.  retuns cons cell whose pair of markers describe the
entire overlapping range. used by textcollect-add-selection-to-collection."
  (cons (copy-marker (min (car pair) (car qair)))
	(copy-marker (max (cdr pair) (cdr qair)))))

(defun pair-excluding-pair (old-region region-to-remove)
  "helper function called by textcollect-remove-selection-from-collection.
takes two pairs of markers, and returns a list describing the range(s)
included in the first but not in the second."
  ;; takes a pair of cons cells each consisting of two markers
  ;; returns a list that may include one cons cell, two, or none.
  ;; if the start of old-region is before start of region-to-remove, keep it
  (if (< (car old-region) (car region-to-remove))
      ;; if end of old-region is also after end of region-to-remove
      ;; split old-region into two and return list of both parts
      ;; in reverse order because calling function will reverse the entire
      ;; list it is constructing before it returns it
      (if (> (cdr old-region) (cdr region-to-remove))
	  (list (cons (cdr region-to-remove) (cdr old-region))
		(cons (car old-region) (car region-to-remove)))
	;; if we're keeping only the start of old-region, create that range
	(list (cons (car old-region) (car region-to-remove))))
    ;; not keeping the start of old-region. but maybe the end?
    (if (> (cdr old-region) (cdr region-to-remove))
	(list (cons (cdr region-to-remove) (cdr old-region)))
      ;; or maybe not, in which case return the empty list
      nil)))

(defun textcollect-start-collection ()
  "initiates collection of a region of a buffer.
called by textcollect-toggle-collecting."
  ;; push-mark makes mark active and sets it to point
  ;; second argument displays 'mark set' in minibuffer
  ;; third argument activates transient mark mode
  ;; which continually highlights the region between mark and point
  (push-mark (point) nil t)
  ;; change the highlight color, for this buffer only
  (setq textcollect-undo-face-remapping
	(face-remap-add-relative 'region 'textcollect-face))
  ;; mark collection mode as active before leaving this function
  (setq textcollect-active t))

(defun textcollect-add-selection-to-collection (new-pair old-list)
  "called by textcollect-end-collection to add selected region to collection."
  ;; saves markers in list rather than saving positions as integers
  ;; because otherwise positions may change as buffer is modified
  (let ((new-list nil) (added-new nil))
    ;; goes through existing list of selected regions one at a time
    (dolist (pair old-list new-list)
      ;; after new-pair has been added to new-list
      ;; simply add each remaining element of the old (existing) list in turn
      (if added-new
	  (setq new-list (cons pair new-list))
	;; if new-pair hasn't been added to new-list yet, check some things
	;; if the new selection ends before the current selection begins
	(if (< (cdr new-pair) (car pair))
	    ;; add new-pair to the list now, and set flag saying so
	    (progn (setq new-list (cons new-pair new-list))
		   (setq added-new t)))
	;; if new-pair overlaps with the currently examined existing pair
	;; merge the two into a new value for new-pair
	(if (overlap-pairs-p pair new-pair)
	    (setq new-pair (merge-two-pairs pair new-pair))
	  ;; if currently examined pair doesn't overlap with new-pair
	  ;; add it to new-list now
	  (setq new-list (cons pair new-list)))))
    ;; if dolist finished without finding a place to add new-pair
    ;; add it to the start (aka end-to-be) of new-list, where it belongs
    (if (not added-new)
	(setq new-list (cons new-pair new-list)))
    ;; new-list was constructed in reverse order
    ;; with each new elemented consed onto the front instead of appending
    ;; to the rear. so return a reversed version of the list.
    (reverse new-list)))

(defun textcollect-highlight-selection (txcl-beg txcl-end)
  "makes an overlay to continue displaying highlighting of selected region.
called by textcollect-end-collection."
  ;; using an overlay rather than a text property because one can't
  ;; remove face properties indvidiually by name.
  (let ((txcl-new-overlay (make-overlay txcl-beg txcl-end)))
    	(overlay-put txcl-new-overlay 'face 'textcollect-face)
	;; give the overlay a label
	;; so it can be removed when collection is cleared
	(overlay-put txcl-new-overlay 'txcl t)))

(defun textcollect-end-collection ()
  "finishes collecting a region and resets the mode to collection off.
called by textcollect-toggle-collecting."
  ;; if a region is selected, add it to the collection
  (if (use-region-p)
      (let ((txcl-beg (copy-marker (region-beginning)))
	     (txcl-end (copy-marker (region-end))))
	(setq textcollect-list
	      (textcollect-add-selection-to-collection
	       (cons txcl-beg txcl-end) textcollect-list))
	;; and highlight it for as long as it's part of the collection
	(textcollect-highlight-selection txcl-beg txcl-end)))
  ;; reset some variables to turn off collection mode
  ;; when done with the current selection, remove and deactivate the mark
  (pop-mark)  
  ;; set highlight color in the buffer back to normal
  (if textcollect-undo-face-remapping
      (face-remap-remove-relative textcollect-undo-face-remapping))
  ;; mark collection as inactive before leaving this function
  (setq textcollect-active nil))

(defun textcollect-toggle-collecting ()
  "toggles collecting for textcollect mode" (interactive)
  ;; if collection mode isn't active, start collecting region from point
  (if (not textcollect-active)
      (textcollect-start-collection)
    ;; if collection mode is active, end this particular bit of collection
    (textcollect-end-collection)))

(defun textcollect-remove-selection-from-collection
    (txcl-pair-to-remove txcl-list)
  "internal guts of textcollect-remove-from-collection. removes all ranges or
parts of ranges from txcl-list that overlap with txcl-pair-to-remove."
  (let ((new-list nil))
    (dolist (txcl-old-pair txcl-list)
      (if (overlap-pairs-p txcl-pair-to-remove txcl-old-pair)
	  ;; if old-pair overlaps pair-to-remove, removing the necessary range
	  ;; could create zero, one, or two marker pairs to add to the list
	  ;; that's why append is used here rather than cons
	    (setq new-list
		  (append
		   (pair-excluding-pair txcl-old-pair txcl-pair-to-remove)
		  new-list))
	(cons txcl-old-pair new-list)))
    (reverse new-list)))

(defun textcollect-remove-from-collection (txcl-beg txcl-end)
  "remove selected region from collection, called with option-f1"
  (interactive "r")
  (if textcollect-active (textcollect-end-collection))
  (if (and txcl-beg txcl-end)
      (progn
	(setq textcollect-list
	      (textcollect-remove-selection-from-collection
	       (cons (copy-marker txcl-beg) (copy-marker txcl-end))
	       textcollect-list))
	;; remove textcollect's highlighting from the region that has been
	;; removed from textcollect's collection
	(remove-overlays txcl-beg txcl-end 'txcl t))))

(defun textcollect-get-string ()
  "collect all the selected regions from current buffer into a string.
created to be called by functions external to this mode."
  (if textcollect-active (textcollect-end-collection))
  (let ((textcollect-string
	 ;; if current buffer has no collection but a region is selected
	 ;; return the selection as a string
	 (if (not textcollect-list)
	     (if (use-region-p)
		 (buffer-substring (region-beginning) (region-end))
	       ;; if buffer has neither collection nor selection, return nil
	       nil)
	   ;; if buffer has collection, initialize collecting string to empty
	   "")))
    ;; access each element of textcollect-list in turn as textcollect-elt
    ;; and return the value of textcollect-string
    (dolist (textcollect-elt textcollect-list textcollect-string)
      (setq textcollect-string
	    (concat textcollect-string "\n\n"
		    (buffer-substring (car textcollect-elt)
				      (cdr textcollect-elt)))))))

(defun textcollect-clear-collection ()
  "empties collection in current buffer" (interactive)
  (remove-overlays nil nil 'txcl t)
  (if textcollect-undo-face-remapping
      (face-remap-remove-relative textcollect-undo-face-remapping))
  (setq textcollect-list nil)
  (setq textcollect-active nil))

(defun textcollect-delete-collection ()
  "deletes all the regions described in textcollect-list" (interactive)
  ;; maybe call this with Ctl-f1.. it's f2 right now
  ;; it seems that i can't do C-f combos actually
  ;; but i will probably rewrite all my shortcuts in a lil while...
  ;; if there's no collection but there is a selected region, delete that
  (if (and (not textcollect-list) (use-region-p))
      (delete-region (region-beginning) (region-end))
    ;; if there is a collection, delete all the selections in it one at a time
    (dolist (txcl-elt textcollect-list)
      (delete-region (car txcl-elt) (cdr txcl-elt)))
    (textcollect-clear-collection)))

(define-minor-mode textcollect-mode
  "a minor mode for selecting multiple non-contiguous regions in buffers"
  :init-value nil :lighter " textcollect" :keymap textcollect-map
  ;; tracks whether collection is on or off
  (defvar-local textcollect-active nil)
  ;; holds pairs of markers, each pair delimiting a selected region in buffer
  ;; the car of each element is the start and the cdr is the end of the region
  (defvar-local textcollect-list nil)
  ;; holds cookie returned by face-remap-add-relative
  ;; needed by face-remap-remove-relative when collecting is turned off
  (defvar-local textcollect-undo-face-remapping nil))

(provide 'textcollect)
