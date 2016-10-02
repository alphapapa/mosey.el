(require 'cl)

(cl-defun mosey (position-funcs &key (backward nil backward-set))
  ;; position-funcs should MOVE THE POINT, not just return the position
  (interactive)
  (let* ((backward backward-set)
         (current-pos (point))

         ;; Make list of positions on current line, one per position-func
         (positions (mapcar (lambda (func) (save-excursion
                                             (funcall func)
                                             (point)))
                            position-funcs))
         ;; Reverse list if :backward is set (is there a more elegant way to do this?)
         (positions (if backward
                        (nreverse positions)
                      positions))
         ;; Determine next target position
         (target (cl-loop for p in positions
                          if (funcall (if backward '< '>) p current-pos)
                          return p
                          finally return (first positions))))
    ;; Goto the target position
    (goto-char target)))

(defmacro defmosey (&rest position-funcs)
  `(progn (defun mosey-forward ()
            (interactive)
            (mosey ',position-funcs)
            )
          (defun mosey-backward ()
            (interactive)
            (mosey ',position-funcs :backward)
            )))

(defmosey
  beginning-of-line
  back-to-indentation
  mosey/goto-beginning-of-comment-text
  end-of-line)

(defun mosey/goto-beginning-of-comment-text ()
  (let (target)
    (save-excursion
      (end-of-line)
      (when (re-search-backward (rx (syntax comment-start) (1+ space)) (line-beginning-position) t)
        (setq target (match-end 0))))
    (when target
      (goto-char target))))
