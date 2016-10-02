;;; mosey.el --- Mosey around your buffers

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/mosey.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "24.4")
;; Keywords: convenience

;;; Commentary:

;; Howdy.  Why don't ya mosey on in here.  Have a sit-down.

;; `mosey' makes it easy to mosey back and forth in your buffers.
;; Just pass `mosey' a list of functions that move the point to
;; certain places, and it'll mosey the point between those places.
;; Tell it `:backward' if you want to mosey on back, otherwise it'll
;; mosey on ahead.  Tell it to `:cycle' if you want it to loop around
;; when it gets to one end or the other.

;; To make it easier for ya, just pass a list of point-moving
;; functions to `defmosey', and it'll cook up four functions:
;; `mosey/forward', `mosey/backward', `mosey/forward-cycle', and
;; `mosey/backward-cycle'.

;;; Installation

;; There's even a default set of those commands, so all ya need to do is:

;; (require 'mosey)

;; ...and then you can start moseying around.  You might even want to
;; rebind your keys to them, maybe like this:

;; (global-set-key (kbd "C-a") 'mosey/backward)
;; (global-set-key (kbd "C-e") 'mosey/forward)

;; ...but that'd be even easier with `use-package' and its handy-dandy
;; `:bind*' form.

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
  "Move the point according to the list of POSITION-FUNCS.

If BACKWARD is set, move backwards.

If CYCLE is set, cycle around when the beginning/end of line is
hit.  Otherwise, stop at beginning/end of line."
  ;; position-funcs should MOVE THE POINT, not just return the position
  (interactive)
  (let* ((backward backward-set)
         (cycle cycle-set)
         (current-pos (point))

         ;; Make list of positions on current line, one per position-func
         (positions (sort (delete-dups (mapcar (lambda (func) (save-excursion
                                                                (funcall func)
                                                                (point)))
                                               position-funcs))
                          '<))
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

(cl-defmacro defmosey (position-funcs &key prefix)
  "Define `mosey/forward' and `mosey/backward' functions, with
`-cycle' variants.

POSITION-FUNCS is a list of functions that move the point.

PREFIX, if set, appends a prefix to the function names, like
`mosey/prefix-forward', useful for defining different sets of
moseys for different modes."
  (when prefix
    (setq prefix (concat prefix "-")))
  `(progn (defun ,(intern (concat "mosey/" prefix "forward")) ()
            (interactive)
            (mosey ,position-funcs)
            )
          (defun ,(intern (concat "mosey/" prefix "backward")) ()
            (interactive)
            (mosey ,position-funcs :backward)
            )
          (defun ,(intern (concat "mosey/" prefix "forward-cycle")) ()
            (interactive)
            (mosey ,position-funcs :cycle)
            )
          (defun ,(intern (concat "mosey/" prefix "backward-cycle")) ()
            (interactive)
            (mosey ,position-funcs :backward :cycle))))

;;;; Helper functions

(defun mosey/goto-beginning-of-comment-text ()
  "Move point to beginning of comment text on current line."
  (let (target)
    (save-excursion
      (end-of-line)
      (when (re-search-backward (rx (syntax comment-start) (* space)) (line-beginning-position) t)
        (setq target (match-end 0))))
    (when target
      (goto-char target))))

(defun mosey/goto-end-of-code ()
  "Move point to end of code on current line."
  (let (target)
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward (rx (or (and (1+ space) (syntax comment-start))
                                       (and (* space) eol)))
                               (line-end-position)
                               t)
        (setq target (match-beginning 0))))
    (when target
      (goto-char target))))

(defun mosey/goto-org-table-next-field ()
  "Move point to next Org table field."
  (when (equal major-mode 'org-mode)
    (let (target)
      (save-excursion
        (when (org-at-table-p)
          (when (looking-at-p (rx (* space) "|" (* space)))
            ;; Skip current column
            (re-search-forward (rx (* space) "|" (* space)) (line-end-position) t))
          (re-search-forward (rx (* space) "|" (* space)) (line-end-position) t)
          (setq target (match-end 0))))
      (when target
        (goto-char target)))))

(defun mosey/goto-org-table-prev-field ()
  "Move point to previous Org table field."
  (when (equal major-mode 'org-mode)
    (let (target)
      (save-excursion
        (when (org-at-table-p)
          (when (looking-back (rx (* space) "|" (* space)))
            ;; Skip current column
            (re-search-backward (rx (* space) "|" (* space)) (line-beginning-position) t))
          (re-search-backward (rx (* space) "|" (* space)) (line-beginning-position) t)
          (setq target (match-end 0))))
      (when target
        (goto-char target)))))

;;;; Default mosey

(defmosey '(beginning-of-line
            back-to-indentation
            mosey/goto-org-table-prev-field
            mosey/goto-org-table-next-field
            mosey/goto-end-of-code
            mosey/goto-beginning-of-comment-text
            end-of-line))

(provide 'mosey)

;;; mosey.el ends here
