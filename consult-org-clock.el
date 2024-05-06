;;; consult-org-clock.el --- A consult-based interface to the org clock -*- lexical-binding: t -*-

;; Copyright (C) 2023 overideal

;; Author: overideal
;; Maintainer: overideal
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (org "9.3") (consult "0.10"))

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Consult-org-clock provides a consult-based interface to org clock.
;; This allows the user to quickly clock into previous tasks. The main command
;; of this package is `consult-org-clock' which can be seen as a "do what I
;; mean" replacement of the built-in `org-clock-in' command (when called with a
;; prefix argument). The new `consult-org-clock' command clocks out when
;; selecting a heading that is currently clocked in, which removes the need for
;; the `org-clock-out' command.
;; However, the main improvement of this command is that `consult-org-clock'
;; makes use of the minibuffer to get the user's input. By leveraging `consult'
;; and the `completing-read' interface, the selection process should be more
;; convenient and informative (e.g. live previews are provided) while staying
;; fast.
;; In the same vein, this package also provides the command
;; `consult-org-clock-goto', replacing `org-clock-goto'.


;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-clock)
(require 'consult)
(require 'consult-org)


;;;; Customization

(defgroup consult-org-clock nil
  "A consult-based interface to org-clock."
  :group 'convenience
  :group 'minibuffer
  :prefix "consult-org-clock-")

(defcustom consult-org-clock-add-heading-clocked-out t
  "Whether `consult-org-clock' shows the org heading at point when clocked out.
If this variable is-non-nil, add the current org heading to the front of the
list of candidates. As a special case, if this variable is \\='second, the
current org heading is added as the second entry.
If there is no org heading at point or the user is clocked in, this option has
no effect."
  :type '(choice
		  (const :tag "Don't add current org heading" nil)
		  (const :tag "Add current org heading to front" t)
		  (const :tag "Add current org heading as second entry" second)))

(defcustom consult-org-clock-add-heading-clocked-in 'second
  "Whether `consult-org-clock' shows the org heading at point when clocked in.
If this variable is-non-nil, add the current org heading to the front of the
list of candidates. As a special case, if this variable is \\='second, the
current org heading is added as the second entry.
If there is no org heading at point or the user is clocked out, this option has
no effect."
  :type '(choice
		  (const :tag "Don't add current org heading" nil)
		  (const :tag "Add current org heading to front" t)
		  (const :tag "Add current org heading as second entry" second)))

(defcustom consult-org-clock-add-heading-predicate nil
  "Predicate function deciding whether the org heading at point is shown.
This affects the commands `consult-org-clock' and `consult-org-clock-goto'.
The function is called without any argument and should infer from the position
of point whether to include the corresponding org heading or not. If it returns
non-nil, the heading is included; otherwise it is not.
This variable may also be nil, in which case the current org heading is
included.
Note that the variable only takes effect if there is an org heading and the
variables `consult-org-clock-add-heading-clocked-out' and
`consult-org-clock-add-heading-clocked-in' are set appropriately."
  :type '(choice function (const nil)))

(defcustom consult-org-clock-predicate nil
  "A predicate function used by `consult-org-clock' to filter the history.
This variable only has an effect if `consult-org-clock' is called without a
prefix argument.
The input of the function is a marker (as in `org-clock-history'). It should
return a non-nil value if and only the corresponding entry should be shown by
`consult-org-clock'. When the function is called, point is at the marker
position.
This variable may also be nil, in which case no filtering takes place."
  :type '(choice function (const nil)))

(defcustom consult-org-clock-predicate-secondary nil
  "A predicate function used by `consult-org-clock' to filter the history.
This variable only has an effect if `consult-org-clock' is called with a prefix
argument.
The input of the function is a marker (as in `org-clock-history'). It should
return a non-nil value if and only the corresponding entry should be shown by
`consult-org-clock'. When the function is called, point is at the marker
position.
This variable may also be nil, in which case no filtering takes place."
  :type '(choice function (const nil)))

(defcustom consult-org-clock-show-name t
  "When non-nil, all headings will be prefixed by their buffer name.
If set to group, the headings will be grouped per file (which destroys their
order by recency)
This influences the commands `consult-org-clock' and `consult-org-clock-goto'."
  :type '(choice
		  (const :tag "Don't show buffer names" nil)
		  (const :tag "Group the headings by their files" group)
		  (const :tag "Add buffer name as prefix" t)))

(defcustom consult-org-clock-show-tags t
  "When non-nil, `consult-org-clock' and `consult-org-clock-goto' will show tags."
  :type '(choice
		  (const :tag "Do not show tags" nil)
		  (const :tag "Show tags" t)))

(defcustom consult-org-clock-formatter #'consult-org-clock-formatter
  "A function to format the entries for consult org clock.
This influences the commands `consult-org-clock' and `consult-org-clock-goto'.
The function is called without any arguments but with point at the current org
heading and should return a string to represent this candidate."
  :type 'function)



;;;; Internal Variables

(defvar consult-org-clock--history nil
  "History variable for `consult-org-clock-select'.")


;;;; Commands

;;;###autoload
(defun consult-org-clock (&optional arg)
  "Display the org clock history in order to clock in or out.
When currently clocked into a task, selecting that task will stop the clock and
selecting another task will stop the clock and start a clock for the new task.
ARG (the prefix argument) controls whether the candidates are filtered by
`consult-org-clock-predicate' or `consult-org-clock-predicate-secondary'.
The behavior of this command can be further altered by
customizing `consult-org-clock-add-heading-clocked-out' and
`consult-org-clock-add-heading-clocked-in'."
  (interactive "P")
  (let ((marker (consult-org-clock-select "Clock in or out: "
										  consult-org-clock-show-name
										  nil
										  (if arg
											  consult-org-clock-predicate-secondary
											consult-org-clock-predicate))))
	(when-let ((buffer (marker-buffer marker))
			   (pos (marker-position marker)))
	  (with-current-buffer buffer
		(org-with-wide-buffer
		 (goto-char pos)
		 (if (and
			  (org-clocking-p)
			  (eq buffer (marker-buffer org-clock-marker))
			  (progn
				(goto-char org-clock-marker)
				(org-back-to-heading t)
				(= pos (point))))
			 (org-clock-out)
		   (org-clock-in)))))))

;;;###autoload
(defun consult-org-clock-goto (&optional select)
  "Go to a recently clocked in item.
When SELECT is nil (without a prefix argument), call `org-clock-goto'.
Otherwise, present the user with the list of recent clocks and jump to the
heading of the selected one.
If `consult-org-clock-show-name' is non-nil, each candidate is prefixed
by the corresponding buffer name."
  (interactive "P")
  (if select
	  (consult--jump
	   (consult-org-clock-select "Goto entry: " consult-org-clock-show-name))
    (org-clock-goto)))



;;;; Functions

(defun consult-org-clock--history (&optional history predicate prefix)
  "Return a collection of recently used clocks.
HISTORY should be of the same form as `org-clock-history' and if nil, it
defaults to that variable.
PREDICATE may either be nil or be a predicate function used to filter HISTORY.
Depending on the values of `consult-org-clock-add-heading-clocked-out' and
`consult-org-clock-add-heading-clocked-in', add the current org entry (if there
is one) to the top of the list of candidates. That entry is never filtered by
PREDICATE.
If PREFIX is non-nil, prefix the candidates with the buffer name."
  (let ((list (consult-org-clock--format-history history predicate prefix))
        (entry-at-point (consult-org-clock--get-heading prefix)))
	;; `org-clock-history' may contain different markers whose corresponding
	;; headings are the same. Therefore, we need to remove duplicates.
	;; Here it is important that always the first occurance of duplicate items
	;; is kept.
	(seq-uniq
     (if entry-at-point
		 (if (and list
				  (or (and (org-clocking-p)
						   (eq consult-org-clock-add-heading-clocked-in 'second))
					  (and (not (org-clocking-p))
						   (eq consult-org-clock-add-heading-clocked-out 'second))))
			 (cons (car list) (cons entry-at-point (cdr list)))
		   (cons entry-at-point list))
       list)
	 #'string-equal)))

(defun consult-org-clock--format-history (&optional history predicate prefix)
  "Turn HISTORY into a collection that can be used with `consult--read'.
HISTORY should be of the same form as `org-clock-history' and if nil, it
defaults to that variable.
PREDICATE may either be nil or be a predicate function used to filter HISTORY.
It is called with the history element as its only argument and with point at the
heading in question.
A list of pairs of headline strings and markers is returned.
If PREFIX is non-nil, prefix the candidates with the buffer name."
  (let (result)
	(cl-dolist (entry (or history org-clock-history))
	  ;; Ensure that the buffer still exists.
	  (when-let ((buf (marker-buffer entry)))
		(with-current-buffer (org-base-buffer buf)
		  (org-with-wide-buffer
		   (goto-char (marker-position entry))
		   (unless (org-before-first-heading-p)
			 (org-back-to-heading)
			 (when (or (not predicate)
					   (funcall predicate entry))
			   (push (consult-org-clock--format-entry prefix) result)))))))
	(nreverse result)))

(defun consult-org-clock--get-heading (prefix)
  "Return the org heading at point (if any) as a candidate.
If PREFIX is non-nil, prefix the candidates with the buffer name.
If we are not in an org buffer or the variables
`consult-org-clock-add-heading-clocked-out' and
`consult-org-clock-add-heading-clocked-in' forbid it, return nil."
  (save-excursion
	(when (and
		   (or (and (org-clocking-p) consult-org-clock-add-heading-clocked-in)
			   (and (not (org-clocking-p)) consult-org-clock-add-heading-clocked-out)))
	  (pcase major-mode
		('org-mode
		 (when (and (not (org-before-first-heading-p))
					(or (not consult-org-clock-add-heading-predicate)
						(funcall consult-org-clock-add-heading-predicate)))
		   (org-back-to-heading t)
		   (consult-org-clock--format-entry prefix)))
		('org-agenda-mode
		 ;; In org agenda: first follow link to org file.
		 ;; From `org-mru-clock--collect-entry-at-point'.
		 (when-let ((m (org-get-at-bol 'org-hd-marker)))
		   (with-current-buffer (org-base-buffer (marker-buffer m))
			 (org-with-wide-buffer
			  (goto-char (marker-position m))
			  ;; Making sure we do not get an infinite recursion.
			  (when (eq major-mode 'org-mode)
				(consult-org-clock--get-heading prefix))))))))))

;; Adapted from `consult-org--headings'.
(defun consult-org-clock--format-entry (prefix)
  "Translate the current org heading to a candidate.
If PREFIX is non-nil, prefix the candidates with the buffer name."
  (pcase-let ((`(_ ,level ,todo ,prio ,_hl ,tags) (org-heading-components))
			  (cand (funcall consult-org-clock-formatter)))
	(setq tags
		  (when consult-org-clock-show-tags
			(if org-use-tag-inheritance
                (when-let ((tags (org-get-tags)))
                  (concat ":" (string-join tags ":") ":"))
              tags)))
	(when tags
      (put-text-property 0 (length tags) 'face 'org-tag tags))
	(setq cand (if prefix
				   (concat (buffer-name) " " cand (and tags " ")
                           tags (consult--tofu-encode (point)))
				 (concat cand (and tags " ")
                         tags (consult--tofu-encode (point)))))
	(add-text-properties 0 1
						 `(org-marker ,(point-marker)
											  consult-org--heading (,level ,todo . ,prio))
						 cand)
	cand))

(defun consult-org-clock-formatter ()
  "Default function to format an org entry at point using its path."
  (org-format-outline-path
   (org-get-outline-path 'with-self)
   most-positive-fixnum))



;;;; The `consult' interface

;; Code adapted from `consult-org-heading'.
(defun consult-org-clock-select (prompt prefix &optional history predicate)
  "Let the user choose an org entry from history HISTORY.
PROMPT is interpreted as in `completing-read'. HISTORY is a history variable
like `org-clock-history', which also serves as the default value in case HISTORY
is nil. PREDICATE may either be nil or be a predicate function taking an entry
of HISTORY as input and returning a non-nil value if and only if that entry
should be a candidate.
PREFIX is interpreted as the value of `consult-org-clock-show-name', which see."
  (consult--read
   (consult--slow-operation "Collecting headings..."
	 (or (consult-org-clock--history history predicate prefix)
		 (user-error "The clock history is empty")))
   :prompt prompt
   :category 'consult-org-clock
   :sort nil
   :require-match t
   :history 'consult-org-clock--history
   :narrow (consult-org--narrow)
   :state (consult--state-with-return (consult--jump-preview) #'ignore)
   :annotate #'consult-org--annotate
   :group
   (when (eq prefix 'group)
     (lambda (cand transform)
       (let ((name (buffer-name
                    (marker-buffer
                     (get-text-property 0 'org-marker cand)))))
         (if transform (substring cand (1+ (length name))) name))))
   :lookup (apply-partially #'consult--lookup-prop 'org-marker)))

(provide 'consult-org-clock)
;;; consult-org-clock.el ends here
