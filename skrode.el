;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to use skrode mode, put these lines into your .emacs file
;;
;; (require 'skrode)
;; (setq auto-mode-alist (cons '("\\.skrd\\'" . skrode-mode) auto-mode-alist))
;; (setq backup-directory-alist
;;   (cons '("~/skrode/.*" . "~/skrode-backups") backup-directory-alist))
;;
;; if your emacs setup makes auto-backups for skrode files, they need to be in
;; a different directory from the files that they're backing up
;; the default directories are ~/skrode and ~/skrode-backups
;; but they can be changed in the init file and the defconst skrode-directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst skrode-directory "~/skrode/" "where we keep the skrode files")
(defconst skrode-extension ".skrd" "skrode nodes end with this extension")
(defconst skrode-header-divider "\n------------------\n\n"
  "a skrode node consists of a title, this divider, and a body")
(defconst skrode-left-delimiter "[[")
(defconst skrode-right-delimiter "]]")
(defconst skrode-left-delimiter-broken "[-[")
(defconst skrode-right-delimiter-broken "]-]")
(defconst skrode-orphans-node "skrode orphans"
  "a node to collect links to nodes with no remaining non-broken links")

(defface skrode-name '((t :foreground "#fdf6e3" :background "#d33682"
			  :weight bold :box "#d33682"))
  "face to display the name of a skrode node")

;; the mode's keymap must have this name
;; so define-derived-mode will set it as the local map
;; defines shortcuts for functions i want to invoke directly as a user
(defvar skrode-mode-map nil)
(progn (setq skrode-mode-map (make-sparse-keymap))
       (set-keymap-parent skrode-mode-map text-mode-map)
       (define-key skrode-mode-map (kbd "<tab>")
	 'skrf-forward-button)
       (define-key skrode-mode-map (kbd "A-<tab>")
	 'skrf-backward-button)
       (define-key skrode-mode-map (kbd "C-z c")
	 'skrf-create-node-from-selection)
       (define-key skrode-mode-map (kbd "C-z t")
	 'skrf-throw-into-node)
       (define-key skrode-mode-map (kbd "C-z d")
	 'skrf-dump-from-node))

(defvar skrode-button-map nil)
(progn (setq skrode-button-map (make-sparse-keymap))
       (set-keymap-parent skrode-button-map button-map)
       (define-key skrode-button-map (kbd "M-RET")
	 'skrf-open-node-in-new-window-from-link)
       ;; key sequences that include mouse clicks must be [vectors]
       (define-key skrode-button-map [M-down-mouse-1]
	 'skrf-open-node-in-new-window-from-click)
       (define-key skrode-button-map [M-down-mouse-2]
	 'skrf-open-node-in-new-window-from-click)
       ;; M-drag-mouse gets triggered when mouse moves a bit while M-clicking
       ;; it's some sort of built in selection function that i don't want
       (define-key skrode-button-map [M-drag-mouse-1] 'ignore)
       (define-key skrode-button-map [M-drag-mouse-2] 'ignore))

(defvar skrode-title-line-keymap nil)
(progn (setq skrode-title-line-keymap (make-sparse-keymap))
       (set-keymap-parent skrode-title-line-keymap skrode-mode-map)
       ;; instead of 'enter' inserting a new line in the title line
       ;; it should leave the title line for the start of the node body
       (define-key skrode-title-line-keymap (kbd "RET")
	 'forward-line))

(defmacro with-inhibit-modification-hooks (&rest body)
  (append (list 'progn)
	  (list '(setq inhibit-modification-hooks t))
	  body
	  (list '(setq inhibit-modification-hooks nil))))

;; creating a function to shadow forward-button
;; so display-message (default t) will not show help-echo in minibuffer
(defun skrf-forward-button () (interactive)
       (forward-button 1 t nil t))

;; creating a function to shadow backward-button
;; so display-message (default t) will not show help-echo in minibuffer
(defun skrf-backward-button () (interactive)
  (backward-button 1 t nil t))

;; because the node's first line is the title
;; a function to return the position at the end of the title is useful
(defun skrf-first-newline ()
  (save-mark-and-excursion
    (goto-char (point-min))
    ;; third argument means move to limit of search if not found
    (let ((nl (search-forward "\n" nil 1)))
      (if (not nl)
	  (progn (newline) (point))
	nl))))

(defun skrf-text-to-link (skrv-node-name)
  (concat skrode-left-delimiter skrv-node-name skrode-right-delimiter))

(defun skrf-link-to-text (skrv-link)
  (if (>= (length skrv-link) (+ (length skrode-left-delimiter)
				(length skrode-right-delimiter)))
  (substring skrv-link (length skrode-left-delimiter)
	     (- (length skrode-right-delimiter)))))

;; returns list of positions of links in buffer
;; as (car.cdr) pairs of markers denoting (start.end) of links
(defun skrf-link-positions-in-buffer ()
  (save-mark-and-excursion
    (skrf-goto-body)
    (let ((link-positions nil)
	  (start (search-forward skrode-left-delimiter nil t)))
      (while (and start
		  (setq end (search-forward skrode-right-delimiter nil t)))
	(goto-char start)
	(setq second-start (search-forward skrode-left-delimiter nil t))
	(if (or (not second-start) (< end second-start))
	    (progn
	      (push (cons (- start (length skrode-left-delimiter))
			  end) ;; we want the start of the start brackets
		    ;; but the end of the end brackets
		    ;; to be saved to link-positions
		    link-positions)
	    (goto-char end))
	  (setq start second-start)))
      link-positions)))

;; returns list of link names from point in buffer to end
;; (end of accessible portion of buffer if relevant)
;; calls skrf-link-positions-in-buffer
;; because getting the positions from the links would be *far* more difficult
;; and there's no reason to code the same work more than once
(defun skrf-links-in-buffer ()
  (let ((link-positions (skrf-link-positions-in-buffer))
	(link-names nil))
    (dolist (position-pair link-positions link-names)
      (push
       ;; link *positions* correctly contain the delimiters
       ;; as well as the link text
       ;; the list of link names shouldn't
       (skrf-link-to-text
	(buffer-substring-no-properties (car position-pair)
					(cdr position-pair)))
       link-names))))

;; takes a string that may contain any number or placement of link delimiters
;; and returns the string with all link delimiters removed
(defun skrf-remove-link-delims (skrv-string)
  (string-replace skrode-left-delimiter ""
		  (string-replace skrode-right-delimiter "" skrv-string)))

(defun skrf-node-name ()
  (skrf-remove-link-delims
   ;; the function skrf-first-newline is needed to delimit the title field
   ;; but the node name should *not* contain a newline at the end!!
   (buffer-substring-no-properties (point-min) (- (skrf-first-newline) 1))))

(defun skrf-goto-body ()
  (goto-char (point-min))
  (end-of-line))
  
(defun skrf-new-window ()
  ;; if autowin mode is enabled, return new window from autowin-new
  (if (boundp 'autowin-mode) (autowin-new)
    ;; if autowin mode is not enabled
    ;; split current window in half to create and return a new window
    ;; note that height & width are measured here in chars rather than pixels
    ;; so not comparable one-to-one
    ;; but the bias to split into columns over rows might be about right
    (if (> (window-total-height) (window-total-width))
	(split-window)
      ;; if window is wider than tall, return right half of current window
      (split-window nil nil t))))

;; wraparound function to call skrf-open-node-in-new-window
;; using mouse position to find link
(defun skrf-open-node-in-new-window-from-click ()
  (interactive) ;; so user can invoke via key sequence
  (save-mark-and-excursion
    ;; sets point to mouse position using builtin emacs variable
    (mouse-set-point last-input-event)
    (skrf-open-node-in-new-window-from-link)))

;; wraparound function to call skrf-open-node-in-new-window
;; using link at point to choose file
(defun skrf-open-node-in-new-window-from-link ()
  (interactive) ;; so user can invoke via key sequence
    (let ((skrv-button (button-at (point))))
      (if skrv-button
	  (skrf-open-node-in-new-window
	   (button-get skrv-button 'link-target)))))

;; opens skrv-filename in a newly created window
(defun skrf-open-node-in-new-window (skrv-filename)
  (let ((skrv-buf (current-buffer)))
    (set-window-buffer (skrf-new-window) (find-file skrv-filename))
  ;; find-file opens target in both current window & split window
  ;; so skrv-buf sets current window back to the state it was in
    (switch-to-buffer skrv-buf)))

(defun skrf-dump-from-node (skrv-pt)
  "converts a link into a broken link and writes the contents of its node
under it into the current buffer.  also changes all links to the linked node
into links to the current buffer's node, and deletes the linked node
from the skrode."
  (interactive "d") ;; get position of cursor as function argument
  ;; check if point is on a link
  (let ((skrv-target-filename (get-text-property skrv-pt 'link-target)))
    (when skrv-target-filename
      (let* ((skrv-target-node-name (get-text-property skrv-pt 'link-text))
	     (skrv-string-to-insert "")
	     ;; storing to variable here to use in with-temp-buffer
	     (skrv-change-links-to (skrf-node-name)))
	(with-temp-buffer
	  (insert-file-contents (skrode-filename skrv-target-node-name))
	  ;; change backlinks to node-to-be-dumped to point to current buffer
	  (rename-this-node-throughout-skrode skrv-target-node-name
					      skrv-change-links-to)
	  ;; get body of node to be dumped as string
	  (skrf-goto-body)
	  (setq skrv-string-to-insert (buffer-substring (point) (point-max))))
	;; remove link and replace it in situ with broken link
	;; after generating string-to-insert so dumped node is never orphaned
	(delete-region (button-start skrv-pt) (button-end skrv-pt))
	(insert (skrf-text-to-broken-link skrv-target-node-name))
	;; newline to separate node-to-be-dumped's title from its body
	(insert "\n")
	;; insert body of node-to-be-dumped into current node at point
	(insert skrv-string-to-insert)
	(skrf-give-links-properties)
	(skrf-give-links-backlinks)
	;; finally, delete the dumped node's file
	(delete-file skrv-target-filename)
	;; and if a buffer is visiting the node-to-be-dumped, kill the buffer
	(when (get-file-buffer skrv-target-filename)
	  (with-current-buffer (get-file-buffer skrv-target-filename)
	    (restore-buffer-modified-p nil)
	    (kill-buffer)))))))

(defun skrf-cellect-string ()
  (if (boundp 'textcollect-mode)
      (textcollect-get-string)
    (if use-region-p
	(buffer-substring (region-beginning) (region-end))
      "")))

(defun skrf-delete-cellection ()
  (if (boundp 'textcollect-mode)
      (textcollect-delete-collection)
    (if (use-region-p)
	(delete-region (region-beginning) (region-end)))))

(defun skrf-throw-into-node (skrv-pt)
  "writes selected region from current buffer into the node pointed to by the
link the cursor is on, if any.  creates and removes backlinks as needed."
  (interactive "d") ;; get position of cursor as function argument
  ;; check if point is on a link
  (let ((skrv-target-filename (get-text-property skrv-pt 'link-target)))
    (if skrv-target-filename
	(let ((skrv-throw-string (skrf-cellect-string)))
	  (if skrv-throw-string
	    (let ((skrv-target-buf (get-file-buffer skrv-target-filename)))
	      (if skrv-target-buf
		  (with-current-buffer skrv-target-buf
		    (goto-char (point-max))
		    (insert skrv-throw-string)
		    (skrf-give-links-properties)
		    (skrf-give-links-backlinks))
		(with-temp-buffer
		  (write-region skrv-throw-string nil skrv-target-filename t)
		  (insert-file-contents skrv-target-filename)
		  (skrf-give-links-backlinks)))
	      (skrf-delete-cellection)))))))

(defun skrf-create-node-from-selection (skrv-new-node-name)
  "Creates a new node from a text selection in an existing skrode node.
or a collection if one exists."
  ;; interactive allows a function to be called by a key sequence
  ;; the code character s in its string argument
  ;; captures and passes a value to this function's argument
  (interactive
   "senter new node's name (empty string to cancel node creation) "
   skrode-mode)
  ;; if no region is selected, exit function with an error message
  (let ((skrv-new-node-contents (skrf-cellect-string)))
    (if (not skrv-new-node-contents)
	(message "cannot make node from selection when there is no selection")
  ;; first set up the variables with which to make the new node
      (let ((skrv-new-node-filename (skrode-filename skrv-new-node-name)))
	(if (not skrv-new-node-name) (message "node creation cancelled")
	  (if (file-exists-p skrv-new-node-filename)
	      (message (concat "node with title" skrv-new-node-name
			       "already exists"))
	    ;; if the user entered a usable node title, create the new node
	    (skrf-make-file skrv-new-node-name)
	    ;; append selected region of current buffer to the new node
	    (write-region skrv-new-node-contents nil
			  skrv-new-node-filename t)
	    ;; at end of new node, add link back to the current buffer
	    (write-region (concat "\n\n"
				  (skrf-text-to-link (skrf-node-name)))
			  nil skrv-new-node-filename t)
	    ;; remove selected text from the current (source) node
	    (skrf-delete-cellection)
	    ;; and put a link to the new node in its place
	    (insert (skrf-text-to-link skrv-new-node-name))
	    ;; re-skrode-ify the current buffer so that any links
	    ;; removed from it are broken on their other ends
	    ;; .... i don't think that would work
	    ;; BUT i do wanna give the new link properties etc. i guess
	    (skrf-give-links-properties)
	    ;; and create buffer from new node just to skrode-ify-it so that
	    ;; the the other ends of any links it may contain are created
	    ;; pointing to the newly created node
	    (with-temp-buffer
	      (insert-file-contents skrv-new-node-filename)
	      ;; optional param means we're just making backlinks
	      ;; not making text properties in the temp buffer itself
	      (skrf-give-links-backlinks))))))))

(defun skrode-filename-safe (node-name)
  "removes characters /,~,.,$ and extra whitespace from node names
 to make safe file names"
  ;; should i be using string-replace instead for escape sequences???
  (remove ?/ (remove ?~ (remove ?. (remove ?$
					   (string-clean-whitespace
					    node-name))))))

(defun skrode-filename (link-text)
  "gives the corresponding file name for a skrode node -
full absolute file path"
  (concat skrode-directory (skrode-filename-safe link-text)
	  skrode-extension))

(defun check-skrode-title ()
  "prints warning if node's title and filename don't match"
  (setq skrode-node-name (skrf-node-name))
  (if (not (string= buffer-file-name
		    (expand-file-name (skrode-filename (skrf-node-name)))))
      (display-warning 'skrode
		       (concat "skrode file " buffer-file-name
			       " has non-matching title " (skrf-node-name))
		       :error)))

(defun rename-this-node-throughout-skrode (old-node-name new-title)
  "change link to current node in all the nodes it's linked to"
  (let ((skrv-old-name (skrf-text-to-link old-node-name))
	(skrv-new-name (skrf-text-to-link new-title)))
    (save-mark-and-excursion
      (skrf-goto-body)
      ;; first find each link to change
      (let ((skrv-links-in-node-being-renamed (skrf-links-in-buffer)))
	(dolist (node-with-backlink-to-change
		 skrv-links-in-node-being-renamed)
	  (let ((linked-to-buffer
		 (get-file-buffer (skrode-filename
				   node-with-backlink-to-change))))
	    ;; if link is open in a buffer, change the other node there
	    (if linked-to-buffer
		(with-current-buffer linked-to-buffer
		  ;; so that search-and-replace
		  ;; does not trigger link breaking etc.
		  (with-inhibit-modification-hooks
		   (save-mark-and-excursion
		     (goto-char (point-min))
		     (while (search-forward skrv-old-name nil t)
		       (replace-match skrv-new-name t t)))
		   (skrf-give-links-properties)))
	      ;; otherwise, change link straight in the file
	      (progn
		(skrf-make-file node-with-backlink-to-change)
		(with-temp-file (skrode-filename node-with-backlink-to-change)
		  (insert-file-contents (skrode-filename
					 node-with-backlink-to-change))
		  (while (search-forward skrv-old-name nil t)
		    (replace-match skrv-new-name t t)))))))))))

(defun skrf-rename-node (skrv-new-name skrv-old-name)
  "makes all the changes necessary to rename a skrode node"
  ;; rename the file... and the buffer, so that it's visiting the new file
  (rename-file buffer-file-name (skrode-filename skrv-new-name))
  ;; WARNING, set-visited-file-name appears to clobber buffer-local variables
  (set-visited-file-name (skrode-filename skrv-new-name) nil t)
  (if (file-exists-p (skrode-filename skrv-old-name))
      (delete-file (skrode-filename skrv-old-name)))
  ;; change the link in all the other nodes this node is linked to
  (rename-this-node-throughout-skrode skrv-old-name skrv-new-name)
  (setq skrode-node-name (skrf-node-name))
  ;; save-buffer has to come after the backlinks have been changed
  ;; or it will simply duplicate every backlink
  (save-buffer))

(defun skrf-rewrite-name (skrv-name)
  (setq inhibit-read-only t)
  (save-mark-and-excursion
    (delete-region
     (point-min)
     (next-single-property-change (point-min) 'skrode-name-boundary))
    (goto-char (point-min))
    (insert skrv-name)
    ;; needed because deleting the region deletes its properties too
    (skrf-propertize-title))
  (setq inhibit-read-only nil))

(defun skrf-clean-name (skrv-name)
  (let ((corregida (list)) (err-str nil)
	(possible-problems
	 '(("[\n\r\f]" . "newlines")
	   ;; all ascii chars not between SPC (32) and TILDE (126)
	   ;; are unprintable - except tab and the three newline chars above
	   ("[^\t -~]" . "unprintable characters")
	   ;; $ matches newline or end of string
	   ;; this works bc newlines were deleted first
	   ("[\s\t\v]*$" . "trailing whitespace characters")
	   ("\\[\\[\\|]]" . "link delimiters"))))
    (dolist (prob possible-problems)
      (let ((cleaner-name (replace-regexp-in-string (car prob) "" skrv-name)))
	(when (not (string= cleaner-name skrv-name))
	  (push (cdr prob) corregida)
	  (setq skrv-name cleaner-name))))
    (when corregida (setq err-str (pop corregida))
	  (when corregida
	    (setq err-str (concat (pop corregida) " and " err-str))
	    (dolist (corr corregida)
	      (setq err-str (concat corr ", " err-str)))))
    (cons skrv-name err-str)))

(defun skrf-rename-resume-or-revert
    (skrv-err-str skrv-proposed-name skrv-old-name skrv-prev-pos)
  (let* ((read-answer-short t) (use-dialog-box t)
	 (ans (read-answer
	       (concat skrv-err-str "rename node to " skrv-proposed-name
		       " throughout skrode? ")
	       '(("rename" ?y "rename node throughout skrode")
		 ("keep editing" ?n "resume editing node name")
		 ("cancel" ?q "let node revert to previous name")))))
    (cond
     ((string= ans "rename")
      (if (not (string= "" skrv-err-str))
	  (skrf-rewrite-name skrv-proposed-name))
      (skrf-rename-node skrv-proposed-name skrv-old-name))
     ((string= ans "keep editing") (goto-char skrv-prev-pos))
     ((string= ans "cancel") (skrf-rewrite-name skrv-old-name)))))

(defun skrf-reject-name (skrv-err-str skrv-old-name skrv-prev-pos)
  (if (y-or-n-p (concat skrv-err-str "keep editing name? \
if not, node will revert to previous name."))
      (goto-char skrv-prev-pos)
    (skrf-rewrite-name skrv-old-name)))

;; the value of the special text property 'cursor-special-functions
;; has to have these three parameters. skrv-win is not used.
;; entered-or-left can have two values: 'entered or 'left
(defun skrf-new-node-name (skrv-win skrv-pos entered-or-left)
  (setq cursor-sensor-inhibit t)
  (when (eq entered-or-left 'left)
    (let* ((skrv-displayed-name
	    (buffer-substring-no-properties
	     (point-min)
	     (next-single-property-change (point-min) 'skrode-name-boundary)))
	   (clean-name-and-message (skrf-clean-name skrv-displayed-name))
	   (skrv-corrected-name (car clean-name-and-message)))
      (cond
       ((string= skrv-displayed-name skrode-node-name))
       ((string= skrv-corrected-name skrode-node-name)
	(skrf-rewrite-name skrv-corrected-name))
       ((file-exists-p (skrode-filename skrv-corrected-name))
	(skrf-reject-name (concat "node with name " skrv-corrected-name
				  " already exists. ")
			  skrode-node-name skrv-pos))
       ((string= skrv-corrected-name "")
	(skrf-reject-name (concat "node must have name. ")
			  skrode-node-name skrv-pos))
       ((cdr clean-name-and-message)
	(skrf-rename-resume-or-revert
	 (concat (cdr clean-name-and-message)
		 " are not allowed in node names. ")
	 skrv-corrected-name skrode-node-name skrv-pos))
       (t (skrf-rename-resume-or-revert
	   "" skrv-corrected-name skrode-node-name skrv-pos)))))
  (setq cursor-sensor-inhibit nil))

(defun skrf-propertize-title ()
  (setq inhibit-read-only t)
  (let ((buffer-modified-flag (buffer-modified-p)))
    (add-text-properties
     (point-min)
     (skrf-first-newline)
     (list 'cursor-sensor-functions '(list skrf-new-node-name)
	   ;; so pressing enter triggers leaving the title line
	   ;; rather than creating a newline
	   'keymap
	   (let ((map (make-sparse-keymap)))
	     (set-keymap-parent map (get-text-property (point-min) 'keymap))
	     (define-key map (kbd "RET") 'forward-line) map)
	   'face 'skrode-name
	   'front-sticky
	   (append (get-text-property (point-min) 'front-sticky)
		   '(cursor-sensor-functions keymap face))
	   'rear-nonsticky
	   (append (get-text-property (point-min) 'rear-nonsticky)
		   '(cursor-sensor-functions keymap face))))
    (if (> (skrf-first-newline) 1)
	(add-text-properties
	 (- (skrf-first-newline) 1) (skrf-first-newline)
	 (list
	  'read-only "cannot delete or overwrite boundary of title line"
	  ;; so one cannot accidentally right-arrow out of the title line
	  'keymap
	  (let ((map (make-sparse-keymap)))
	    (set-keymap-parent map
	     (get-text-property (- (skrf-first-newline) 1) 'keymap))
	    (define-key map (kbd "<right>") 'ignore) map)
	  'skrode-name-boundary t
	  'rear-nonsticky
	  (append
	   (get-text-property (- (skrf-first-newline) 1) 'rear-nonsticky)
	   '(read-only keymap skrode-name-boundary)))))
        (restore-buffer-modified-p buffer-modified-flag))
  (setq inhibit-read-only nil))

(defun find-start-of-broken-skrode-link-s (start-from-hook end-from-hook)
  "find the position from which to start breaking link(s) because of edit"
  ;; if the character before insertion/deletion is part of a link
  ;; we're fucking about *inside* a link. so return start of link.
  (if (get-text-property (- start-from-hook 1) 'skrode-link)
      (button-start (- start-from-hook 1))
    ;; otherwise use the start of insertion/deletion
    start-from-hook))

(defun find-end-of-broken-skrode-link-s (start-from-hook end-from-hook)
  "find the position at which to end breaking link(s) because of edit"
  ;; if the character after insertion/deletion is part of a link
  ;; we're fucking about *inside* a link. so return end of link.
  (if (get-text-property end-from-hook 'skrode-link)
      (button-end end-from-hook)
    ;; otherwise use end of insertion/deletion
    end-from-hook))

;; returns t if current node has no non-broken skrode links, nil otherwise
(defun skrf-node-orphan-p ()
  ;; advance past header
  (save-mark-and-excursion
    (skrf-goto-body)
    ;; using skrf-link-positions-in-buffer rather than skrf-link-in-buffer
    ;; because skrf-links-in-buffer calls skrf-link-positions-in-buffer
    (not (skrf-link-positions-in-buffer))))

(defun dealing-with-broken-skrode-link-target (this-node-name)
  "replace links to ~this~ node with broken links, and
say if node should be deleted"
  ;; function is called when target node is already the current buffer
  (while (search-forward (skrf-text-to-link this-node-name) nil t)
    ;; replace-match uses the last match found, in this case by search-forward
    ;; fresh new string has no button properties
    (replace-match (concat "[-[" this-node-name "]-]") nil t))
  (when (skrf-node-orphan-p)
    (goto-char (point-max))
    (insert (concat " " (skrf-text-to-link skrode-orphans-node)))
    (skrf-give-links-backlinks)))

(defun break-other-side-of-skrode-link (link-target)
  "when a link is being broken, go to linked node and
 break link(s) back to this node"
  (let ((target-node-buffer (get-file-buffer link-target))
	(this-node-name (skrf-node-name)))
    ;; if linked node is being visited by a buffer, break link in buffer
    (if target-node-buffer
	(with-current-buffer target-node-buffer
	  (save-mark-and-excursion
	    (goto-char (point-min))
	    (dealing-with-broken-skrode-link-target this-node-name)))
      ;; if linked node is not being visited, break link directly in file
      ;; check if file exists first to avoid errors if it doesn't exist
      (if (file-exists-p link-target)
	  (with-temp-file
	      link-target (insert-file-contents link-target)
	    (dealing-with-broken-skrode-link-target this-node-name))))))

(defun break-individual-skrode-link
    (start end start-modification-region end-modification-region)
  "do the work to break a single link"
  ;; gotta set this so remove doesn't call break-skrode-link
  (with-inhibit-modification-hooks
   (let ((link-target (get-text-property start 'link-target))
	 (linked-node-name (get-text-property start 'link-text)))
     ;; remove all the text properties associated with a skrode link
     (remove-list-of-text-properties
      start end
      '(insert-behind-hooks insert-in-front-hooks modification-hooks
			    button category skrode-link link-text link-target
			    keymap action help-echo))
     ;; so search doesn't move point
     (save-mark-and-excursion
       (goto-char (point-min))
       ;; iff this is the only link to target, break other side of link
       (if (not (or (search-forward (concat "[[" linked-node-name "]]")
				    start-modification-region t)
		    (progn (goto-char end-modification-region)
			   (search-forward (concat "[[" linked-node-name "]]")
					   nil t))))
	   (break-other-side-of-skrode-link link-target))))))

;; called when user modifies a skrode link, breaking it
;; start and end define the part of the buffer that was modified
(defun break-skrode-link (start end)
  "turn edited links into plain text. and break other ~ends~ of these links,
in other nodes."
  ;; check if the edit occurred in the middle of a single link
  ;; if so, only break that one.
  (if (and (get-text-property (- start 1) 'skrode-link)
	   (get-text-property end 'skrode-link)
	   (string-equal (get-text-property (- start 1) 'link-target)
			 (get-text-property end 'link-target)))
      (break-individual-skrode-link (button-start (- start 1))
				    (button-end end)
				    (button-start (- start 1))
				    (button-end end))
  ;; if not, find  the part of the buffer i want to break links in
  (let ((start-stretch (find-start-of-broken-skrode-link-s start end))
	(end-stretch (find-end-of-broken-skrode-link-s start end)))
    ;; and go through affected stretch of buffer looking for links to break
      (save-mark-and-excursion
	(goto-char start-stretch)
	(narrow-to-region start-stretch end-stretch)
	(let ((positions-of-links-affected (skrf-link-positions-in-buffer)))
	  (widen)
	  (dolist (positions-of-individual-link positions-of-links-affected)
	    (break-individual-skrode-link (car positions-of-individual-link)
					  (cdr positions-of-individual-link)
					  start-stretch end-stretch)))))))

(defun make-skrode-link-break-on-edit-attempt (start end)
  "set up hooks so attempt to edit link will break both it
and its reciprocal other end"
  (with-inhibit-modification-hooks
   (add-text-properties start end
			'(modification-hooks (list break-skrode-link)))
   (add-text-properties (+ start 1) end
			'(insert-in-front-hooks (list break-skrode-link)))
   (add-text-properties start (- end 1)
			'(insert-behind-hooks (list break-skrode-link)))))

;; written to be set as the 'action property of 'skrode-link buttons
(defun skrf-open-node-in-same-window (skrv-button)
  (let ((skrv-old-buf (current-buffer))
	(skrv-buffer-window-list (get-buffer-window-list (current-buffer))))
    (find-file (button-get skrv-button 'link-target))
    ;; kill the buffer you followed the link from
    ;; iff it is not displayed in any other windows
    (if (not (cdr skrv-buffer-window-list))
	;; if you can't kill it eg because the user says no
	;; then move it into a new window instead
	(if (not (kill-buffer skrv-old-buf))
	    (set-window-buffer (skrf-new-window) skrv-old-buf)))))

(defun skrf-preview-node (skrv-win skrv-buf skrv-pos)
  ;; first get the 'link-target property of skrv-buf at skrv-pos
  (let ((skrv-target (get-text-property skrv-pos 'link-target)))
  ;; if none, return nil
    ;; if some, return file *after* header divider as string
    (if skrv-target
	(with-temp-buffer
	  (insert-file-contents skrv-target)
	  (skrf-goto-body)
	  (buffer-substring-no-properties (point) (point-max))))))

;; (buffer-substring start end) is the whole link
;; including both right and left link delimiters
(defun make-skrode-link (start end)
  "turns [[text in double square brackets]] into a link to a skrode node
of that name"
  (let ((skrv-link (skrf-link-to-text (buffer-substring start end))))
    (make-text-button
     start end
     'skrode-link t
     'link-text skrv-link
     ;; had to assign this statically, not as a function
     ;; so link could be broken after it's been modified
     'link-target (skrode-filename skrv-link)
     'keymap skrode-button-map
     'action 'skrf-open-node-in-same-window
     'help-echo 'skrf-preview-node))
  (make-skrode-link-break-on-edit-attempt start end))

(defun skrf-make-file (linked-node-name)
  "if linked-to node does not exist, create it with appropriate title.
no other contents."
  (let ((linked-node-filename (skrode-filename linked-node-name)))
    (if (not (file-exists-p linked-node-filename))
	(write-region
	 ;; to use write-region w string instead of buffer, 2nd param is nil
	 (concat linked-node-name "\n") nil linked-node-filename))))

(defun skrf-text-to-broken-link (skrv-link-text)
  "turns this string into [-[that string]-]"
  (concat skrode-left-delimiter-broken skrv-link-text
	  skrode-right-delimiter-broken))

;; if current buffer contains a link to the skrode orphans node
;; break both that link
;; and any link in the skrode orphans node back to the current node
(defun skrf-unorphan-node ()
  (save-mark-and-excursion
    (goto-char (point-min))
    (let ((orphan-p nil))
      (while (search-forward (skrf-text-to-link skrode-orphans-node) nil t)
	(with-inhibit-modification-hooks
	 (replace-match (skrf-text-to-broken-link skrode-orphans-node)))
	(setq orphan-p t))
      (if orphan-p
	  (break-other-side-of-skrode-link (skrode-filename
					    skrode-orphans-node))))))

(defun put-skrode-backlink-in-distant-node (this-node-name)
  "a helper function for make-skrode-backlink, to be called
whether node's ~open~ or not"
  ;; called from context where distant node is already current buffer
  ;; also, point is at the start of the node
  (let ((found-broken-link-s nil))
    ;; if there are broken links to this node, then un-break them
    (while (search-forward (skrf-text-to-broken-link this-node-name) nil t)
      (replace-match (skrf-text-to-link this-node-name))
      (setq found-broken-link-s t))
    ;; point moves forward iff at least one match is found
    ;; otherwise, if there are no working links either, add one at the end
    (if (not found-broken-link-s)
	;; again, point moves forward iff a match is found
	(when (not (search-forward (skrf-text-to-link this-node-name) nil t))
	  (goto-char (point-max))
	  (insert (concat " " (skrf-text-to-link this-node-name))))))
  (skrf-unorphan-node))

(defun make-skrode-backlink (linked-node-filename)
  "create link back to current node in ~linked-to~ node,
if one does not exist already"
  (let ((linked-to-buffer (get-file-buffer linked-node-filename))
	(this-node-name (skrf-node-name)))
    (if linked-to-buffer
	;; if the node is being visited in a buffer
	;;search & add link to buffer
	(with-current-buffer linked-to-buffer
	    (save-mark-and-excursion
	      (goto-char (point-min))
	      (put-skrode-backlink-in-distant-node this-node-name)
	      ;; so link is clickable immediately
	      (skrf-give-links-properties)))
      ;; if node is not being visited in buffer - search & add link to file
      (with-temp-file linked-node-filename
	(insert-file-contents linked-node-filename)
	(put-skrode-backlink-in-distant-node this-node-name)))))

(defun skrf-format-title ()
  "checks that displayed title matches filename,
and sets properties of displayed node title"
      (check-skrode-title)
      (skrf-propertize-title))

;; for all links in the current buffer
;; that don't already have the skrode-link property
(defun skrf-give-links-properties ()
  ;; because we're only changing properties, not text
  (with-silent-modifications
    (dolist (link-start-and-end (skrf-link-positions-in-buffer))
      (if (text-property-not-all (car link-start-and-end)
				 (cdr link-start-and-end)
				 'skrode-link t)
	  (make-skrode-link (car link-start-and-end)
			    (cdr link-start-and-end))))))

;; for all links in the current buffer
(defun skrf-give-links-backlinks ()
  ;; because we're only changing properties, not text
  (with-silent-modifications
    (dolist (link-name (skrf-links-in-buffer))
      (skrf-make-file link-name)
      (make-skrode-backlink (skrode-filename link-name)))))

(defun skrf-open-node ()
  (button-mode) ;; manual call because hooks can run in any order
  (cursor-sensor-mode) ;; so that cursor-sensor-functions will work
  (defvar-local skrode-node-name ""
    "variable to hold old node name during renaming process")
  (skrf-format-title)
  (skrf-eval-links))

(defun skrf-eval-links ()
  ;; add link properties to any link that doesn't already have them
  (skrf-give-links-properties)
  ;; add a backlink to any link that doesn't have one
  (skrf-give-links-backlinks))

(define-derived-mode skrode-mode text-mode "Skrode"
  "a programmable personal knowledge base system" :interactive nil
  (add-hook 'skrode-mode-hook 'skrf-open-node 0 t) ;; hooks are buffer-local
  (add-hook 'before-save-hook 'skrf-eval-links 0 t)
  )

(provide 'skrode)
