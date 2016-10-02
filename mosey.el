(require 'cl)

(cl-defun mosey (position-funcs &key (backward nil backward-set) (cycle nil cycle-set))
  ;; position-funcs should MOVE THE POINT, not just return the position
  (interactive)
  (let* ((backward backward-set)
         (cycle cycle-set)
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
                          finally return (if cycle
                                             (first positions)
                                           current-pos))))
    ;; Goto the target position
    (goto-char target)))

(cl-defmacro defmosey (&rest position-funcs)
  `(progn (defun mosey-forward ()
            (interactive)
            (mosey ',position-funcs)
            )
          (defun mosey-backward ()
            (interactive)
            (mosey ',position-funcs :backward)
            )
          (defun mosey-forward-cycle ()
            (interactive)
            (mosey ',position-funcs :cycle)
            )
          (defun mosey-backward-cycle ()
            (interactive)
            (mosey ',position-funcs :backward :cycle))))

(defmosey
  beginning-of-line
  back-to-indentation
  mosey/goto-beginning-of-comment-text
  end-of-line)

(defun mosey/goto-beginning-of-comment-text ()
  (let (target)
    (save-excursion
      (end-of-line)
      (when (re-search-backward (rx (syntax comment-start) (* space)) (line-beginning-position) t)
        (setq target (match-end 0))))
    (when target
      (goto-char target))))
