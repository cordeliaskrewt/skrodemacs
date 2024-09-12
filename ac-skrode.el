;;;; After skrode init in .emacs:
;; (require 'ac-skrode)

(require 'auto-complete)

(defun ac-skrode-candidates ()
  skrode-node-names-cache)

(defun ac-skrode-action ()
  (insert "]]"))

(defvar ac-skrode-nodes
  '((candidates
     . (lambda ()
	 (ac-skrode-candidates)))
    (prefix . "^\\[\\[\\(.*\\)")
    (action . ac-skrode-action)))

(add-hook 'skrode-mode-hook
	  (lambda () "Makes autocomplete work in skrode mode"
            (message "skrode autocomp enabled!")
	    (make-local-variable 'ac-sources)
	    (setq ac-sources '(ac-skrode-nodes))
            (setq ac-auto-start t)
	    (auto-complete-mode 1)))

(provide 'ac-skrode)
