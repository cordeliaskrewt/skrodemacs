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
(defconst skrode-history-node "skrode recents")
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
       (define-key skrode-mode-map (kbd "C-z n")
	 'skrf-jump-to-rename)
       (define-key skrode-mode-map (kbd "C-z c")
	 'skrf-create-node-from-selection)
       (define-key skrode-mode-map (kbd "C-z t")
	 'skrf-throw-into-node)
       (define-key skrode-mode-map (kbd "C-z d")
	 'skrf-dump-from-node)
       (define-key skrode-mode-map (kbd "C-z h")
	 'skrf-open-history))

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

;; Node names cache for autocomplete
(defvar skrode-node-names-cache nil)

(defun skrf-open-history () (interactive)
  (skrf-open-node-in-new-window (skrf-filename skrode-history-node)))

(defun init-skrode-node-names-cache ()
  "Initialize the skrode node names cache, if not already initialized."
  (if (not skrode-node-names-cache)
      (setq skrode-node-names-cache
            (mapcar #'file-name-sans-extension
                    (directory-files
                     skrode-directory nil
                     (concat
                      "\\`[^.]+" (regexp-quote skrode-extension) "\\'"))))))

(defun intern-to-skrode-names-cache (node-name)
  "Intern a new node name into the skrode node names cache."
  (add-to-list 'skrode-node-names-cache node-name))

(defun evict-from-skrode-names-cache (node-name)
  "Remove a node name from the skrode node names cache, if it exists there."
  (setq skrode-node-names-cache
        (delete node-name skrode-node-names-cache)))

(defun skrf-is-special-node (node-name)
  (string= node-name skrode-history-node))

(defmacro with-inhibit-modification-hooks (&rest body)
  `(let ((inhibit-modification-hooks t))
     ,@body))

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

;; returns list of positions of links in buffer
;; as (car.cdr) pairs of markers denoting (start.end) of links
(defun skrf-link-positions-in-buffer ()
  (save-mark-and-excursion
    (goto-char (point-min))
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
       (skrf-remove-link-delims
	(buffer-substring-no-properties (car position-pair)
					(cdr position-pair)))
       link-names))))

;; takes a string that may contain any number or placement of link delimiters
;; and returns the string with all link delimiters removed
(defun skrf-remove-link-delims (skrv-string)
  (string-replace skrode-left-delimiter ""
		  (string-replace skrode-right-delimiter "" skrv-string)))

(defun skrf-node-name ()
   ;; the function skrf-first-newline is needed to delimit the title field
   ;; but the node name should *not* contain a newline at the end!!
   (buffer-substring-no-properties (point-min) (- (skrf-first-newline) 1)))

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

(defun skrf-new-window ()
   ;; use autowin mode to make the new window if it's present
   (if (boundp 'autowin-mode) (autowin-new)
     ;; if not - if current window is more chars tall than wide
     ;; the new window is the bottom half of the current window
     ;; otherwise it's the right half
     (if (> (window-total-height) (window-total-width))
	 (split-window)
       (split-window nil nil t))))

(defun skrf-open-node-in-new-window (skrv-filename)
  "opens skrode file SKRV-FILENAME in a newly created window."
  (set-window-buffer (skrf-new-window) (find-file-noselect skrv-filename)))

(defun skrf-dump-from-node (skrv-pt)
  "replaces a link at point with the contents of the linked-to node.
deletes linked-to node, replaces all links to it with links to
the current node."
  (interactive "d") ;; get position of cursor as function argument
  ;; if point is on a link, that's the target node name and file
  (let* ((target-node-name (get-text-property skrv-pt 'link-text))
	 (target-filename (skrf-filename target-node-name))
	 (target-as-string "")
	 ;; storing the value here to use in with-temp-buffer
	 (current-node-name (skrf-node-name)))
    (when target-node-name
      (with-temp-buffer
	(insert-file-contents target-filename)
	;; change backlinks to node-to-be-dumped to point to current buffer
	(rename-this-node-throughout-skrode target-node-name
					    current-node-name)
	;; get body of node to be dumped as string
	(setq target-as-string (buffer-substring (point) (point-max))))
      ;; remove link after saving its body as a string so that string
      ;; doesn't have temporary link orphans node
      (delete-region (button-start skrv-pt) (button-end skrv-pt))
      ;; insert body of node-to-be-dumped into current node at point
      (insert target-as-string)
      (skrf-eval-links)
      ;; finally, delete the dumped node's file
      (delete-file target-filename)
      ;; and if a buffer is visiting the node-to-be-dumped, kill the buffer
      (evict-from-skrode-names-cache target-node-name)
      (if (get-file-buffer target-filename)
	  (with-current-buffer (get-file-buffer target-filename)
	    ;; so that kill-buffer will not ask user for confirmation
	    (restore-buffer-modified-p nil)
	    (kill-buffer))))))

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
  "removes selected region from current node and appends it to the node
linked at point. creates and removes backlinks as needed."
  (interactive "d") ;; get position of cursor as function argument
  ;; check if point is on a link  
  (let ((target-filename
	 (skrf-filename (get-text-property skrv-pt 'link-text)))
	(throw-string (skrf-cellect-string)))
    (if (and target-filename throw-string)
	(let ((target-buf (get-file-buffer target-filename)))
	  (if target-buf
	      (with-current-buffer target-buf
		(goto-char (point-max))
		(insert throw-string)
		(skrf-give-links-properties)
		(skrf-give-links-backlinks))
	    (with-temp-buffer
	      (write-region throw-string nil target-filename t)
	      (insert-file-contents target-filename)
	      (skrf-give-links-backlinks)))
	  (skrf-delete-cellection)))))

(defun skrf-create-node-from-selection (new-node-name)
  "creates a new node from selection."
  ;; the code character s in interactive's string argument
  ;; captures and passes a string to this function's argument
  (interactive "senter new node name (empty string to cancel node creation) ")
  (let ((new-node-body (skrf-cellect-string))
	(new-node-filename
	 (if new-node-name (skrf-filename new-node-name) "")))
    (cond
     ((not new-node-name) (message "node creation cancelled"))
     ((not new-node-body)
      (message "cannot make node from selection when there is no selection"))
     ((file-exists-p new-node-filename)
      (message (concat "node " new-node-filename " already exists")))
     (t (skrf-make-file new-node-name)
	;; if usable node title and selection exist, create the new node
	;; including a link back to the current node at the end
	(write-region
	 (concat new-node-body "\n\n" (skrf-text-to-link skrode-node-name))
	 nil new-node-filename)
	;; use temp buffer to create any backlinks to new node
	(with-temp-buffer (insert-file-contents new-node-filename)
			  (skrf-give-links-backlinks))
	;; remove selected text from the current node
	(skrf-delete-cellection)
	;; and put a link to the new node in its place
	(insert (skrf-text-to-link new-node-name))
	(skrf-give-links-properties)))))

(defun skrf-filename (skrv-name)
  "gives the corresponding full absolute file path for a skrode node name"
  (let ((node-name
	 (string-clean-whitespace
	  (seq-take skrv-name (- 255 (length skrode-extension))))))
    (dolist (chr '(?/?~?.?$?:?*?\\?>?<?\|?\"??))
      (setq node-name (remove chr node-name)))
    (concat skrode-directory node-name skrode-extension)))

;; what arguments function gets depends on what hook calls function
(defun skrf-display-header (&optional skrv-win skrv-display-start)
  "displays node title in header line if first line of file is not visible.
also makes header line clickable to edit node title."
  (if (not skrv-win) (setq skrv-win (selected-window)))
  (with-current-buffer (window-buffer skrv-win)
    (when (eq major-mode 'skrode-mode)
      (if (not skrv-display-start) (setq skrv-display-start (window-start)))
      ;; if start of file is not displayed in window
      (if (> skrv-display-start 1)
	  ;; if node name is not currently in header line, add it
	  (if (not (member skrode-node-name header-line-format))
	      (push
	       (propertize skrode-node-name
			   'keymap
			   (let ((map (make-sparse-keymap)))
			     (define-key map [header-line mouse-1]
			       'skrf-jump-to-rename)
			     map))
	       header-line-format))
	;; if start of file is displayed, remove name from header line
	(setq header-line-format
	      (delete skrode-node-name header-line-format))))))

(defun rename-this-node-throughout-skrode (old-node-name new-title)
  "change link to current node in all the nodes it's linked to"
  (let ((skrv-old-name (skrf-text-to-link old-node-name))
	(skrv-new-name (skrf-text-to-link new-title)))
    (save-mark-and-excursion
      ;; first find each link to change
      (let ((skrv-links-in-node-being-renamed (skrf-links-in-buffer)))
	(dolist (node-with-backlink-to-change
		 skrv-links-in-node-being-renamed)
	  (let ((linked-to-buffer
		 (get-file-buffer (skrf-filename
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
		(with-temp-file (skrf-filename node-with-backlink-to-change)
		  (insert-file-contents (skrf-filename
					 node-with-backlink-to-change))
		  (while (search-forward skrv-old-name nil t)
		    (replace-match skrv-new-name t t)))))))))))

(defun skrf-rename-node (skrv-new-name skrv-old-name)
  "makes all the changes necessary to rename a skrode node"
  ;; rename the file... and the buffer, so that it's visiting the new file
  (rename-file buffer-file-name (skrf-filename skrv-new-name))
  ;; WARNING, set-visited-file-name appears to clobber buffer-local variables
  (set-visited-file-name (skrf-filename skrv-new-name) nil t)
  (if (file-exists-p (skrf-filename skrv-old-name))
      (delete-file (skrf-filename skrv-old-name)))
  ;; change the link in all the other nodes this node is linked to
  (rename-this-node-throughout-skrode skrv-old-name skrv-new-name)
  (setq skrode-node-name (skrf-node-name))
  ;; update the cache
  (evict-from-skrode-names-cache skrv-old-name)
  (intern-to-skrode-names-cache skrode-node-name)
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
	   ;; would be nice to change this to use skrf-remove-link-delimiters
	   ;; also skrf-remove-broken-link-delimiters
	   ;; even though it couldn't use the replace-regexp
	   ;; in the dolist that way
	   ;; in the interests of removing literals with variables
	   ;; 100% of the time
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
    (skrv-err-str skrv-proposed-name skrv-old-name)
  (let* ((read-answer-short t) (use-dialog-box t)
	 (ans (read-answer
	       (concat skrv-err-str "rename node to " skrv-proposed-name
		       " throughout skrode? ")
	       '(("rename" ?y "rename node throughout skrode")
		 ("keep editing" ?n "resume editing node name")
		 ("cancel" ?q "let node revert to previous name")))))
    (cond
     ((string= ans "rename")
      (skrf-rewrite-name skrv-proposed-name)
      (skrf-rename-node skrv-proposed-name skrv-old-name))
     ((string= ans "keep editing")
      (skrf-keep-editing-name (skrf-node-name)))
     ((string= ans "cancel") (skrf-rewrite-name skrv-old-name)))))

(defun skrf-reject-name (skrv-err-str skrv-old-name)
  (if (y-or-n-p (concat skrv-err-str "keep editing name? \
if not, node will revert to previous name."))
      (skrf-keep-editing-name (skrf-node-name))
    (skrf-rewrite-name skrv-old-name)))

(defun skrf-keep-editing-name (skrv-proposed-name)
  (let ((new-name
	 (read-string "rename node to (empty string to cancel renaming): "
		      skrv-proposed-name)))
    (if (string= new-name "")
	(skrf-rewrite-name skrode-node-name)
      ;; to keep displayed name and proposed name always in sync
      (skrf-rewrite-name new-name)
      (skrf-check-before-rename new-name))))

(defun skrf-jump-to-rename ()
  (interactive)
  (if (and (listp last-input-event) (eq (car last-input-event) 'mouse-1))
    (select-window (caadr last-input-event)))
  (setq skrode-rename-jump-restore
	(list (point) (pop-mark) (window-start)))
  (add-hook 'skrf-rename-jump-hook 'skrf-jump-back-from-rename 0 t)
  (goto-char (point-min)))

(defun skrf-jump-back-from-rename ()
  (goto-char (car skrode-rename-jump-restore))
  (push-mark (cadr skrode-rename-jump-restore))
  (set-window-start nil (caddr skrode-rename-jump-restore) nil)
  (setq skrode-rename-jump-restore nil)
  (remove-hook 'skrf-rename-jump-hook 'skrf-jump-back-from-rename t))

(defun skrf-check-before-rename (skrv-proposed-name)
  (let* ((clean-name-and-message (skrf-clean-name skrv-proposed-name))
	 (skrv-corrected-name (car clean-name-and-message)))
    (cond
     ((string= skrv-proposed-name skrode-node-name))
     ((string= skrv-corrected-name skrode-node-name)
      (skrf-rewrite-name skrv-corrected-name))
     ((file-exists-p (skrf-filename skrv-corrected-name))
      (skrf-reject-name (concat "node with name " skrv-corrected-name
				" already exists. ")
			skrode-node-name))
     ((string= skrv-corrected-name "")
      (skrf-reject-name (concat "node must have name. ")
			skrode-node-name))
     ((cdr clean-name-and-message)
      (skrf-rename-resume-or-revert
       (concat (cdr clean-name-and-message)
	       " are not allowed in node names. ")
       skrv-corrected-name skrode-node-name))
     (t (skrf-rename-node skrv-proposed-name skrode-node-name)))))

;; the value of the special text property 'cursor-special-functions
;; has to have these three parameters. skrv-win and skrv-pos are not used.
;; entered-or-left can have two values: 'entered or 'left
(defun skrf-attempt-rename (skrv-win skrv-pos entered-or-left)
  (setq cursor-sensor-inhibit t)
  (when (eq entered-or-left 'left)
    (let* ((skrv-displayed-name
	    (buffer-substring-no-properties
	     (point-min)
       	     (next-single-property-change (point-min)
					  'skrode-name-boundary))))
      (skrf-check-before-rename skrv-displayed-name))
    (run-hooks 'skrf-rename-jump-hook))
  (setq cursor-sensor-inhibit nil))

(defun skrf-propertize-title ()
  (setq inhibit-read-only t)
  (let ((buffer-modified-flag (buffer-modified-p)))
    ;; visible properties should end with name text, not after it
    (add-text-properties
     (point-min)
     (if (> (skrf-first-newline) 1) (- (skrf-first-newline) 1) 1)
     '(face skrode-name))
    (add-text-properties
     (point-min)
     (skrf-first-newline)
     (list 'skrode-category 'name
	   'cursor-sensor-functions '(list skrf-attempt-rename)
	   ;; so pressing enter triggers leaving the title line
	   ;; rather than creating a newline
	   'keymap
	   (let ((map (make-sparse-keymap)))
	     (set-keymap-parent map (get-text-property (point-min) 'keymap))
	     (define-key map (kbd "RET") 'forward-line) map)
	   'front-sticky
	   (append (get-text-property (point-min) 'front-sticky)
		   '(cursor-sensor-functions keymap face))
	   ;; face property does not extend past name text
	   ;; so it needs to be rear-sticky to apply
	   ;; to typing after end of name
	   'rear-nonsticky
	   (append (get-text-property (point-min) 'rear-nonsticky)
		   '(cursor-sensor-functions keymap))))
    (add-text-properties
     (if (> (skrf-first-newline) 1) (- (skrf-first-newline) 1) 1)
     (skrf-first-newline)
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
	   '(read-only keymap skrode-name-boundary))))
        (restore-buffer-modified-p buffer-modified-flag))
  (setq inhibit-read-only nil))

(defun find-start-of-broken-skrode-link-s (start-from-hook end-from-hook)
  "find the position from which to start breaking link(s) because of edit"
  ;; if the character before insertion/deletion is part of a link
  ;; we're fucking about *inside* a link. so return start of link.
  (if (eq (get-text-property (- start-from-hook 1) 'skrode-category) 'link)
      (button-start (- start-from-hook 1))
    ;; otherwise use the start of insertion/deletion
    start-from-hook))

(defun find-end-of-broken-skrode-link-s (start-from-hook end-from-hook)
  "find the position at which to end breaking link(s) because of edit"
  ;; if the character after insertion/deletion is part of a link
  ;; we're fucking about *inside* a link. so return end of link.
  (if (eq (get-text-property end-from-hook 'skrode-category) 'link)
      (button-end end-from-hook)
    ;; otherwise use end of insertion/deletion
    end-from-hook))

;; returns t if current node has no non-broken skrode links, nil otherwise
(defun skrf-node-orphan-p ()
  (save-mark-and-excursion
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
			    button category skrode-category link-text
			    link-target
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
  ;; if hook is called while buffer's not in skrode mode, do nothing
  (if (eq major-mode 'skrode-mode)
      ;; check if the edit occurred in the middle of a single link
      ;; if so, only break that one.
      (if (and (eq (get-text-property (- start 1) 'skrode-category) 'link)
	       (eq (get-text-property end 'skrode-category) 'link)
	       (string-equal (get-text-property (- start 1) 'link-target)
			     (get-text-property end 'link-target)))
	  (break-individual-skrode-link (button-start (- start 1))
					(button-end end)
					(button-start (- start 1))
					(button-end end))
	;; if not, find  the part of the buffer i want to break links in
	(let ((start-stretch (find-start-of-broken-skrode-link-s start end))
	      (end-stretch (find-end-of-broken-skrode-link-s start end)))
	  ;; and go through affected stretch of buffer
	  ;; looking for links to break
	  (save-mark-and-excursion
	    (goto-char start-stretch)
	    (narrow-to-region start-stretch end-stretch)
	    (let ((positions-of-links-affected
		   (skrf-link-positions-in-buffer)))
	      (widen)
	      (dolist (positions-of-individual-link
		       positions-of-links-affected)
		(break-individual-skrode-link
		 (car positions-of-individual-link)
		 (cdr positions-of-individual-link)
		 start-stretch end-stretch))))))))

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
	  (buffer-substring-no-properties (point-min) (point-max))))))

;; (buffer-substring start end) is the whole link
;; including both right and left link delimiters
(defun make-skrode-link (start end)
  "turns [[text in double square brackets]] into a link to a skrode node
of that name"
  (let ((skrv-link (skrf-remove-link-delims (buffer-substring start end))))
    (make-text-button
     start end
     'skrode-category 'link
     'link-text skrv-link
     ;; had to assign this statically, not as a function
     ;; so link could be broken after it's been modified
     'link-target (skrf-filename skrv-link)
     'keymap skrode-button-map
     'action 'skrf-open-node-in-same-window
     'help-echo 'skrf-preview-node))
  (make-skrode-link-break-on-edit-attempt start end))

(defun skrf-make-file (linked-node-name)
  "if linked-to node does not exist, create it with appropriate title.
no other contents."
  (let ((linked-node-filename (skrf-filename linked-node-name)))
    (if (not (file-exists-p linked-node-filename))
        (progn
	  (write-region
	   ;; to use write-region w string instead of buffer, 2nd param is nil
	   (concat linked-node-name "\n") nil linked-node-filename)
          (intern-to-skrode-names-cache linked-node-name)))))

(defun skrf-text-to-broken-link (skrv-link-text)
  "turns this string into [-[that string]-]"
  (concat skrode-left-delimiter-broken skrv-link-text
	  skrode-right-delimiter-broken))

;; if current buffer contains a link to the skrode orphans node
;; remove that link
;; and any link in the skrode orphans node back to the current node
(defun skrf-unorphan-node ()
  (if (replace-string-in-region (skrf-text-to-link skrode-orphans-node)
				"" (point-min) (point-max))
      (let ((orphans-node-buffer
	     (get-file-buffer (skrf-filename skrode-orphans-node))))
	(if orphans-node-buffer
	    (with-current-buffer orphans-node-buffer
	      (replace-string-in-region
	       (skrf-text-to-link (skrf-node-name))
	       "" (point-min) (point-max)))
	  (if (file-exists-p (skrf-filename skrode-orphans-node))
	      (with-temp-file (skrf-filename skrode-orphans-node)
		(replace-string-in-region (skrf-text-to-link (skrf-node-name))
					  "" (point-min) (point-max))))))))

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

(defun skrf-check-name ()
  "checks that node name matches filename"
  (setq skrode-node-name (skrf-node-name))
  ;; remove any forbidden chars and sequences from the node name
  (let ((clean-name (car (skrf-clean-name skrode-node-name))))
    (when (not (string= skrode-node-name clean-name))
      (skrf-rewrite-name clean-name)
      (setq skrode-node-name (skrf-node-name))))
  ;; buffer-file-name and expand-file-name return full absolute path
  (when (not (string= buffer-file-name
		      (expand-file-name (skrf-filename skrode-node-name))))
    (display-warning
     'skrode
     (concat "skrode file " buffer-file-name
	     " has non-matching name " skrode-node-name)
     :error)))

;; for all links in the current buffer
;; that don't already have the skrode-link property
(defun skrf-give-links-properties ()
  ;; because we're only changing properties, not text
  (with-silent-modifications
    (dolist (link-start-and-end (skrf-link-positions-in-buffer))
      (if (text-property-not-all (car link-start-and-end)
				 (cdr link-start-and-end)
				 'skrode-category 'link)
	  (make-skrode-link (car link-start-and-end)
			    (cdr link-start-and-end))))))

;; for all links in the current buffer
(defun skrf-give-links-backlinks ()
  ;; because we're only changing properties, not text
  (with-silent-modifications
    (dolist (link-name (skrf-links-in-buffer))
      (skrf-make-file link-name)
      (make-skrode-backlink (skrf-filename link-name)))))

(defun skrf-yank-handler (skrv-category skrv-start skrv-end)
  (if (or (eq skrv-category 'link) (eq skrv-category 'name))
      (remove-text-properties
       skrv-start skrv-end
       '(skrode-category nil
			 keymap nil
			 link-text nil link-target nil
			 action nil help-echo nil
			 modification-hooks nil
			 insert-in-front-hooks nil
			 insert-behind-hooks nil
			 cursor-sensor-functions nil
			 front-sticky nil
			 rear-nonsticky nil
			 skrode-name-boundary nil)))
  ;; also remove 'skrode-name face if present, but not all faces
  (face-remap-remove-relative skrode-name-face-unmap))

(defun skrf-record-visit (node-name)
  "Log a visit to a skrode node in the history special node."
  (if (not (skrf-is-special-node node-name))
      (let* ((log (skrf-filename skrode-history-node))
             (entry (concat skrode-left-delimiter node-name skrode-right-delimiter "\n"))
             (header (concat skrode-history-node "\n"))
         (header-skip-length (+ 1 (length header))))
    (with-temp-buffer
      (if (not (file-exists-p log))
          (insert header)
        (insert-file-contents log))
      (goto-char header-skip-length)
      (insert entry)
      (write-region (point-min) (point-max) log)))))

(defun skrf-open-node ()
  (init-skrode-node-names-cache)
  (button-mode) ;; manual call because hooks can run in any order
  (cursor-sensor-mode) ;; so that cursor-sensor-functions will work
  (defvar-local skrode-node-name ""
    "variable to hold old node name during renaming process. \
accessed to get current node name at other times.")
  ;; variables needed to edit node name from anywhere in file
  ;; and return automatically to previous point when done editing name
  (defvar-local skrf-rename-jump-hook nil)
  (defvar-local skrode-rename-jump-restore nil)
  (if (not (member '(skrode-category . skrf-yank-handler)
		   yank-handled-properties))
      (push '(skrode-category . skrf-yank-handler) yank-handled-properties))
  ;; default depth of 0, t makes it a buffer local hook
  (add-hook 'window-scroll-functions 'skrf-display-header 0 t)
  (add-hook 'window-buffer-change-functions 'skrf-display-header 0 t)
  ;; return value of face-remap-add-relative must be stored in a variable
  ;; for if one wishes to undo the remapping
  (let ((skrookie (face-remap-add-relative 'header-line 'skrode-name)))
    (defvar-local skrode-name-face-unmap skrookie))  
  (skrf-check-name)
  (skrf-propertize-title)
  (skrf-eval-links)
  (skrf-record-visit skrode-node-name))

(defun skrf-eval-links ()
  ;; if displayed node name and saved node name are different
  (if (not (string=
	    (skrf-node-name)
	    (let ((filename (buffer-file-name)))
	      (with-temp-buffer
		(insert-file-contents filename)
		(skrf-node-name)))))
      ;; check and possibly change node name first
      ;; only proceed with saving if displayed node name is a legal node name
      (skrf-attempt-rename (selected-window) (point) 'left))
  ;; add link properties to any link that doesn't already have them
  (skrf-give-links-properties)
  ;; add a backlink to any link that doesn't have one
  (if (not (skrf-is-special-node skrode-node-name))
      (skrf-give-links-backlinks)))

(define-derived-mode skrode-mode text-mode "Skrode"
  "a programmable personal knowledge base system" :interactive nil
  (add-hook 'skrode-mode-hook 'skrf-open-node 0 t) ;; hooks are buffer-local
  (add-hook 'before-save-hook 'skrf-eval-links 0 t)
  )

(provide 'skrode)
