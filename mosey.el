;;; mosey.el --- Mosey around your buffers


;;; Commentary:


;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Require

(require 'cl)

;;;; Mosey function and macro

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

;;;; Helper functions

(defun mosey/goto-beginning-of-comment-text ()
  (let (target)
    (save-excursion
      (end-of-line)
      (when (re-search-backward (rx (syntax comment-start) (* space)) (line-beginning-position) t)
        (setq target (match-end 0))))
    (when target
      (goto-char target))))

(defun mosey/goto-end-of-code ()
  (let (target)
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward (rx (1+ space) (syntax comment-start)) (line-end-position) t)
        (setq target (match-beginning 0))))
    (when target
      (goto-char target))))

;;;; Default mosey

(defmosey
  beginning-of-line
  back-to-indentation
  mosey/goto-end-of-code
  mosey/goto-beginning-of-comment-text
  end-of-line)

(provide 'mosey)

;;; mosey.el ends here
