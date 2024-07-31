;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst skrode-directory "~/skrode/" "where we keep the skrode files")
(defconst skrode-extension ".skrd" "skrode nodes end with this extension")
(defconst skrode-deleted-nodes-directory "~/skrode/deleted/"
  "where e.g. orphan nodes go")
(defconst skrode-header-divider "\n------------------\n\n"
  "a skrode node consists of a title, this divider, and a body")
(defconst skrode-left-delimiter "[[")
(defconst skrode-right-delimiter "]]")
(defconst skrode-left-delimiter-broken "[-[")
(defconst skrode-right-delimiter-broken "]-]")


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
       (define-key skrode-mode-map (kbd "C-z n")
	 'get-new-skrode-node-name)
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

;; next feature that i need to add
;; function skrf-link-positions-in-buffer
;; takes a buffer containing a skrode file
;; returns list of start & end pairs of markers into buffer
;; each pair describing a link...
;; also function skrf-links-in-buffer
;; returns a list of link-text values
;; rename-this-node-throughout-skrode wants skrf-links-in-buffer
;; break-skrode-link wants skrf-link-positions-in-buffer
;; skrode-ify-buffer currently wants to call both
;; although the two calls will be split out into two separate functions later.

;; FIRST: write and test skrf-link-positions-in-buffer, & document
;; SECOND: write and test srkf-links-in-buffer, & document
;; THIRD: rewrite rename-this-node-throughout-skrode to use skrf-links-in-buffer, & test, & document
;; FOURTH: rewrite break-skrode-link to use skrf-link-positions-in-buffer, & test, & document
;; (possibly switch the orders of THIRD and FOURTH, and of FIFTH and SIXTH)
;; FIFTH: rewrite skrode-ify-buffer to use skrf-links-in-buffer, & test & document
;; SIXTH: rewrite skrode-ify-buffer to use skrf-link-positions-in-buffer, & test & document

;; creating a function to shadow forward-button
;; so display-message (default t) will not show help-echo in minibuffer
(defun skrf-forward-button () (interactive)
  (forward-button 1 t nil t))

;; creating a function to shadow backward-button
;; so display-message (default t) will not show help-echo in minibuffer
(defun skrf-backward-button () (interactive)
  (backward-button 1 t nil t))

(defun skrf-text-to-link (skrv-node-name)
  (concat skrode-left-delimiter skrv-node-name skrode-right-delimiter))

(defun skrf-link-to-text (skrv-link)
  (substring skrv-link (length skrode-left-delimiter)
	     (- (length skrode-right-delimiter))))

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
	  (skrf-open-node-in-new-window (button-get skrv-button 'link-target)))))

;; opens skrv-filename in a newly created window
(defun skrf-open-node-in-new-window (skrv-filename)
  (let ((skrv-buf (current-buffer)))
    ;;    (set-window-buffer (split-window) (find-file skrv-filename))
    (set-window-buffer (autowin-new) (find-file skrv-filename))
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
      (let* ((skrv-link-start (button-start skrv-pt))
	     (skrv-link-end (button-end skrv-pt))
	     (skrv-target-node-name (get-text-property skrv-pt 'link-text))
	     ;; saving the buffer-local variable to use in with-temp-buffer
	     (skrv-current-node-name skrode-node-name)
	     (skrv-string-to-insert ""))
	;; remove link and replace it in situ with broken link
	(delete-region skrv-link-start skrv-link-end)
	(insert (skrf-text-to-broken-link skrv-target-node-name))
	;; newline to separate node-to-be-dumped's title from its body
	(insert "\n")
	(with-temp-buffer
	  (insert-file-contents (skrode-filename skrv-target-node-name))
	  ;; change backlinks to node-to-be-dumped to point to current buffer
	  (rename-this-node-throughout-skrode
	   skrv-target-node-name skrv-current-node-name)
	  ;; get body of node to be dumped as string
	  (goto-char (point-min))
	  (search-forward skrode-header-divider)
	  (setq skrv-string-to-insert (buffer-substring (point) (point-max))))
	;; insert body of node-to-be-dumped into current node at point
	(insert skrv-string-to-insert)
	(skrode-ify-buffer)
	;; finally, delete the dumped node's file
	(delete-file skrv-target-filename)
	;; and if a buffer is visiting the node-to-be-dumped, kill the buffer
	(when (get-file-buffer skrv-target-filename)
	  (with-current-buffer (get-file-buffer skrv-target-filename)
	    (restore-buffer-modified-p nil)
	    (kill-buffer)))))))

(defun skrf-throw-into-node (skrv-pt)
  "writes selected region from current buffer into the node pointed to by the
link the cursor is on, if any.  creates and removes backlinks as needed."
  (interactive "d") ;; get position of cursor as function argument
  ;; check if point is on a link
  (let ((skrv-target-filename (get-text-property skrv-pt 'link-target)))
    (if skrv-target-filename
	(let ((skrv-throw-string (textcollect-get-string)))
	  (if skrv-throw-string
	    (let ((skrv-target-buf (get-file-buffer skrv-target-filename)))
	      (if skrv-target-buf
		  (with-current-buffer skrv-target-buf
		    (goto-char (point-max))
		    (insert skrv-throw-string)
		    (skrode-ify-buffer t))
		(with-temp-buffer
		  (write-region skrv-throw-string nil skrv-target-filename t)
		  (insert-file-contents skrv-target-filename)
		  (skrode-ify-buffer t)))
	      (textcollect-delete-collection)))))))

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
  (let ((skrv-new-node-contents (textcollect-get-string)))
    (if (not skrv-new-node-contents)
	(message "cannot make node from selection when there is no selection")
  ;; first set up the variables with which to make the new node
      (let ((skrv-new-node-filename (skrode-filename skrv-new-node-name)))
	(if (not skrv-new-node-name) (message "node creation cancelled")
	  (if (file-exists-p skrv-new-node-filename)
	      (message (concat "node with title" skrv-new-node-name
			       "already exists"))
	    ;; if the user entered a usable node title, create the new node
	    (make-skrode-file skrv-new-node-name)
	    ;; append selected region of current buffer to the new node
	    (write-region skrv-new-node-contents nil
			  skrv-new-node-filename t)
	    ;; at end of new node, add link back to the current buffer
	    (write-region (concat "\n\n" (skrf-text-to-link skrode-node-name)) nil
			  skrv-new-node-filename t)
	    ;; remove selected text from the current (source) node
	    (textcollect-delete-collection)
	    ;; and put a link to the new node in its place
	    (insert (skrf-text-to-link skrv-new-node-name))
	    ;; re-skrode-ify the current buffer so that any links
	    ;; removed from it are broken on their other ends
	    (skrode-ify-buffer)
	    ;; and create buffer from new node just to skrode-ify-it so that
	    ;; the the other ends of any links it may contain are created
	    ;; pointing to the newly created node
	    (with-temp-buffer
	      (insert-file-contents skrv-new-node-filename)
	      ;; optional param means we're just making backlinks
	      ;; not making text properties in the temp buffer itself
			      (skrode-ify-buffer t))))))))

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
  (if (not (string= buffer-file-name
		    (expand-file-name (skrode-filename skrode-node-name))))
      (display-warning 'skrode
		       (concat "skrode file " buffer-file-name
			       " has non-matching title " skrode-node-name)
		       :error)))

(defun rename-this-node-throughout-skrode (old-node-name new-title)
  "change link to current node in all the nodes it's linked to"
  (let ((skrv-old-name (skrf-text-to-link old-node-name))
	(skrv-new-name (skrf-text-to-link new-title)))
    ;; first find each link to change
    (save-excursion
      (goto-char (point-min))
      (search-forward skrode-header-divider)
      (while (search-forward "[[" nil t)
	(let* ((other-node-name
		(buffer-substring (point) (- (search-forward "]]" nil t) 2)))
	       (other-node (skrode-filename other-node-name)))
	  (let ((linked-to-buffer (get-file-buffer other-node)))
	    ;; if link is open in a buffer, change the other node there
	    (if linked-to-buffer
		(with-current-buffer linked-to-buffer
		  ;; so that search-and-replace
		  ;; does not trigger link breaking etc.
		  (setq inhibit-modification-hooks t)
		  (save-excursion
		    (goto-char (point-min))
		    (while (search-forward skrv-old-name nil t)
		  		      (replace-match skrv-new-name t t)))
		  (skrode-ify-buffer)
		  (setq inhibit-modification-hooks nil))
	      ;; otherwise, change link straight in the file
	      (progn
		(make-skrode-file other-node-name)
		(with-temp-file other-node (insert-file-contents other-node)
				(while (search-forward skrv-old-name nil t)
				  (replace-match skrv-new-name t t)
				  ))))))))))

(defun rename-this-skrode-node (new-title)
  "makes all the changes necessary to rename a skrode node"
  ;; rewriting the title lines and giving them the necessary properties
  ;; to make sure search-forward starts from the right place
  (goto-char (point-min))
  (setq inhibit-modification-hooks t) ;; so we don't trigger rename dialog
  (delete-region (point-min)
		 (- (search-forward skrode-header-divider nil t) 21))
  (setq inhibit-read-only t)
  (goto-char (point-min))
  (insert (skrf-text-to-link new-title))
  ;; setting these variables back to their default state
  (setq inhibit-modification-hooks nil)
  (setq inhibit-read-only nil)
  (make-skrode-title-trigger-rename-dialog (point-min) (point))
  ;; rename the file... and the buffer, so that it's visiting the new file
  (rename-file buffer-file-name (skrode-filename new-title))
  ;; WARNING, set-visited-file-name appears to clobber buffer-local variables
  (set-visited-file-name (skrode-filename new-title) nil t)
  (if (file-exists-p (skrode-filename skrode-node-name))
      (delete-file (skrode-filename skrode-node-name)))
  ;; change the link in all the other nodes this node is linked to
  (rename-this-node-throughout-skrode skrode-node-name new-title)
  ;; and finally reset the internal name variable
  (setq skrode-node-name new-title))

;; parameters begin and end are not used, but when function is invoked as an action from a button
;; they are sent automatically.  interactive invocation of the function (via key sequence) does not
;; send the parameters, hence they are made optional.
(defun get-new-skrode-node-name (&optional beg end)
  "ask user for new node name, and rename node if given acceptable answer"
  (interactive)
  (let ((new-title (read-string "node name: " skrode-node-name)))
    (if (or (string= new-title "") (string= new-title skrode-node-name))
	(message "rename attempt cancelled")
      (if (file-exists-p (skrode-filename new-title))
	  (message (concat "node with title " new-title " already exists."))
	(rename-this-skrode-node new-title)))))

(defun make-skrode-title-trigger-rename-dialog (start-pos end-pos)
  "set up hooks so attempts to edit title will trigger node renaming dialogue"
  ;; so that adding properties doesn't trigger rename dialog!
  (setq inhibit-modification-hooks t)
  (setq inhibit-read-only t) ;; or 'text is read-only' error
  (add-text-properties
   start-pos end-pos
   '(modification-hooks (list get-new-skrode-node-name)
			insert-in-front-hooks (list get-new-skrode-node-name)
  			insert-behind-hooks (list get-new-skrode-node-name)))
  ;; this makes clicking also a hook
  (make-text-button
   start-pos end-pos 'skrode-title t
   'help-echo nil
   'action (function (lambda (button)
		       (get-new-skrode-node-name (button-start button)
						 (button-end button)))))
  ;; make header divider read-only
  (add-text-properties (+ end-pos 1) (+ end-pos 21)
		       '(read-only t rear-nonsticky t))
  ;; reset variables back to their default state
  (setq inhibit-modification-hooks nil)
  (setq inhbit-read-only nil))

(defun make-skrode-title (start-pos end-pos)
  "sets skrode-node-name variable, and properties of displayed node title.
 checks against filename."
  (check-skrode-title)
  (make-skrode-title-trigger-rename-dialog start-pos end-pos))

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

(defun is-skrode-node-orphan ()
  "looks at current buffer to see if node's an orphan and should be deleted"
  ;; i'm either in a save-excursion already or in a temp buffer
  (goto-char (point-min))
  ;; backslash needs to be escaped so it appears literally in regexp
  ;; . matches every character except newline
  ;;to match every character, need this workaround
  (when (looking-at (concat "\\[\\[[[:ascii:][:nonascii:]]*?]]"
			    skrode-header-divider))
    ;; if there's nothing abnormal about the header, move past it & keep looking
    (goto-char (match-end 0))
    ;; \s is space. \r is carriage return.
    ;; regexp's built-in whitespace class [:space:] didn't seem to work.
    (while (looking-at "[\s\n\t\r]*?\\[-\\[[[:ascii:][:nonascii:]]*?]-]")
      (goto-char (match-end 0)))
    ;; while loop ends at end of buffer, or when text after point
    ;; is anything other than broken link optionally preceded by whitespace
    (skip-chars-forward "\s\n\t\r")
    ;; if there's nothing besides whitespace left in the buffer
    ;; *then* node's an orphan
    (if (equal (point) (point-max)) t)))

(defun dealing-with-broken-skrode-link-target (this-node-name)
  "replace links to ~this~ node with broken links, and
say if node should be deleted"
  ;; function is called when target node is already the current buffer
  (while (search-forward (skrf-text-to-link this-node-name) nil t)
    ;; replace-match uses the last match found, in this case by search-forward
    ;; fresh new string has no button properties
    (replace-match (concat "[-[" this-node-name "]-]") nil t))
  (if (is-skrode-node-orphan)
      (progn
	;; write-region avoids calling any save-buffer hooks
	(write-region nil nil
		      (concat skrode-deleted-nodes-directory
			      (substring link-target (length skrode-directory)
					 nil)))
	t) ;; returns t if target needs to be deleted, and nil otherwise
    nil))

(defun break-other-side-of-skrode-link (link-target linked-node-name)
  "when a link is being broken, go to linked node and
 break link(s) back to this node"
  (let ((target-node-buffer (get-file-buffer link-target))
	(target-is-orphan nil)
	(this-node-name skrode-node-name))
    ;; if linked node is being visited by a buffer, break link in buffer
    (if target-node-buffer
	(progn
	  (with-current-buffer target-node-buffer
	    (save-excursion
	      (goto-char (point-min))
	      ;; this line does most of the work - whether with buffer or file
	      (setq target-is-orphan
		    (dealing-with-broken-skrode-link-target this-node-name))
	      ;; so killing buffer doesn't ask user for permission
	      (if target-is-orphan (set-buffer-modified-p nil))))
	  ;; wait for buffer to no longer be current before we kill it
	  (if target-is-orphan
	      (kill-buffer target-node-buffer)))
      ;; if linked node is not being visited, break link directly in file
      (progn
	(make-skrode-file linked-node-name)
	;; to avoid errors if it doesn't exist
	(with-temp-file
	    link-target (insert-file-contents link-target)
	    (setq target-is-orphan
		  (dealing-with-broken-skrode-link-target this-node-name)))))
    (if target-is-orphan (delete-file link-target))))

(defun break-individual-skrode-link
    (start end start-modification-region end-modification-region)
  "do the work to break a single link"
  ;; gotta set this so remove doesn't call break-skrode-link
  (setq inhibit-modification-hooks t)
  (let ((link-target (get-text-property start 'link-target))
	(linked-node-name (buffer-substring-no-properties (+ start 2)
							  (- end 2))))
    ;; remove all the text properties associated with a skrode link
    (remove-list-of-text-properties
     start end
     '(insert-behind-hooks insert-in-front-hooks modification-hooks
			   action link-target skrode-link category button))
    ;; so search doesn't move point
    (save-excursion
	(goto-char (point-min))
	;; iff this is the only link to target, break other side of link
	(if (not (or (search-forward (concat "[[" linked-node-name "]]")
				     start-modification-region t)
		     (progn (goto-char end-modification-region)
			    (search-forward (concat "[[" linked-node-name "]]")
					    nil t))))
	    (break-other-side-of-skrode-link link-target linked-node-name))))
  (setq inhibit-modification-hooks nil))

(defun break-skrode-link (start end)
  "turn edited links into plain text. and break other ~ends~ of these links,
in other nodes."
  ;; check if the edit occurred in the middle of a single link
  ;; if so, only break that one.
  (if (and (get-text-property (- start 1) 'skrode-link)
	   (get-text-property end 'skrode-link)
	   (string-equal (get-text-property (- start 1) 'link-target)
			 (get-text-property end 'link-target)))
      (break-individual-skrode-link (button-start (- start 1)) (button-end end)
				    (button-start (- start 1)) (button-end end))
  ;; if not, find  the part of the buffer i want to break links in
  (let ((start-stretch (find-start-of-broken-skrode-link-s start end))
	(end-stretch (find-end-of-broken-skrode-link-s start end))
	(links-affected nil))
    ;; and go through affected stretch of buffer looking for links to break
      (save-excursion
	(goto-char start-stretch)
	(narrow-to-region start-stretch end-stretch)
	(while (re-search-forward "\\[\\[[[:ascii:][:nonascii:]]*?]]" nil t)
	  (push (cons (match-beginning 0) (match-end 0)) links-affected))
	(widen)
	(while links-affected
	  (let ((individual-link (pop links-affected)))
	    (break-individual-skrode-link (car individual-link)
					  (cdr individual-link)
					  start-stretch end-stretch)))))))

(defun make-skrode-link-break-on-edit-attempt (start end)
  "set up hooks so attempt to edit link will break both it
and its reciprocal other end"
  (setq inhibit-modification-hooks t)
  (add-text-properties start end '(modification-hooks (list break-skrode-link)))
  (add-text-properties (+ start 1) end
		       '(insert-in-front-hooks (list break-skrode-link)))
  (add-text-properties start (- end 1)
		       '(insert-behind-hooks (list break-skrode-link)))
  (setq inhibit-modification-hooks nil))

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
	    (set-window-buffer (split-window) skrv-old-buf)))))

(defun skrf-preview-node (skrv-win skrv-buf skrv-pos)
  ;; first get the 'link-target property of skrv-buf at skrv-pos
  (let ((skrv-target (get-text-property skrv-pos 'link-target)))
  ;; if none, return nil
    ;; if some, return file *after* header divider as string
    (if skrv-target
	(with-temp-buffer
	  (insert-file-contents skrv-target)
	  (goto-char (point-min))
	  (search-forward skrode-header-divider)
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

(defun make-skrode-file (linked-node-name)
  "if linked-to node does not exist, create it with appropriate title.
no other contents."
  (let ((linked-node-filename (skrode-filename linked-node-name)))
    (if (not (file-exists-p linked-node-filename))
	(write-region
	 ;; to use write-region w string instead of buffer, 2nd param is nil
	 (concat (skrf-text-to-link linked-node-name) skrode-header-divider)
	 nil linked-node-filename))))

(defun skrf-text-to-broken-link (skrv-link-text)
  "turns this string into [-[that string]-]"
  (concat skrode-left-delimiter-broken skrv-link-text skrode-right-delimiter-broken))

(defun put-skrode-backlink-in-distant-node (this-node-name)
  "a helper function for make-skrode-backlink, to be called
whether node's ~open~ or not"
  ;; called from context where distant node is already current buffer
  (let ((found-broken-link-s nil))
    ;; if there are broken links to this node, then un-break them
    (while (search-forward (skrf-text-to-broken-link this-node-name) nil t)
      (replace-match (skrf-text-to-link this-node-name))
      (setq found-broken-link-s t))
    ;; otherwise, if there are no working links either, add one at the end
    (if (not found-broken-link-s)
	(when (not (search-forward (skrf-text-to-link this-node-name) nil t))
	  (goto-char (point-max))
	  (insert (concat " " (skrf-text-to-link this-node-name)))))))

(defun make-skrode-backlink (linked-node-filename this-node-name)
  "create link back to ~this~ node in ~linked-to~ node,
if one does not exist already"
  (let ((linked-to-buffer (get-file-buffer linked-node-filename))
	(this-node-name skrode-node-name))
    (if linked-to-buffer
	;; if the node is being visited in a buffer
	;;search & add link to buffer
	(with-current-buffer linked-to-buffer
	    (save-excursion
	      (goto-char (point-min))
	      (put-skrode-backlink-in-distant-node this-node-name)
	      (skrode-ify-buffer))) ;; so link is clickable immediately
      ;; if node is not being visited in buffer - search & add link to file
      (with-temp-file linked-node-filename
	(insert-file-contents linked-node-filename)
	(put-skrode-backlink-in-distant-node this-node-name)))))

;; in skrode mode, call this function whenever node is opened or saved
(defun skrode-ify-buffer (&optional in-temp-buffer)
  (if (not in-temp-buffer)
      ;; don't rely on making it a hook, bc hooks can run in any order
      (button-mode))
  (with-silent-modifications ;; bc we're only changing properties, not text
    (save-excursion
      (goto-char (point-min))

      ;; shouldn't have to re-make name every time we re-skrode-ify, but..
      ;; we'll figure that out later i hope
      (when (not skrode-node-name)
	(re-search-forward "\\[\\[[[:ascii:][:nonascii:]]*?]]" nil t)
	(setq skrode-node-name
	      (skrf-link-to-text (match-string-no-properties 0)))
	(if (not in-temp-buffer)
	    (make-skrode-title
	     (match-beginning 0) (match-end 0))))

      (search-forward skrode-header-divider)
    
      ;; find each instance of  [[text like this]]
      ;; search till end of buffer & if not found return nil
      (while (search-forward "[[" nil t)
	(let* ((button-start (- (point) 2))
	       (button-end (search-forward "]]" nil t))
	       (linked-node-name (skrf-link-to-text
				  (buffer-substring-no-properties
				   button-start button-end)))
      	       (linked-node-filename (skrode-filename linked-node-name)))
	  ;; if [[text like this]] isn't a link, make it into one
	  (when (not (get-text-property (+ button-start 1) 'skrode-link))
	    (if (not in-temp-buffer)
	      (make-skrode-link button-start button-end))
	      ;; and add title and backlink to linked node
	    ;; if they don't already exist
	    (make-skrode-file linked-node-name)
	    (make-skrode-backlink linked-node-filename
				  skrode-node-name)))))))


(define-derived-mode skrode-mode text-mode "Skrode"
  "a programmable personal knowledge base system" :interactive nil
  (defvar-local skrode-node-name nil)
;;  (wc-mode t) - commented out since wc-mode doesn't work currently
  (add-hook 'skrode-mode-hook 'skrode-ify-buffer 0 t) ;; hooks are buffer-local
  (add-hook 'before-save-hook 'skrode-ify-buffer 0 t)
  )

(provide 'skrode)
