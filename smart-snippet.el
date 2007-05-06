;;; smart-snippet.el --- snippet with conditional expansion

;; Copyright 2007 pluskid
;;
;; Author: pluskid.zju@gmail.com
;; Version: $Id: smart-snippet.el,v 0.0 2007/05/05 23:06:37 kid Exp $
;; Keywords: snippet smart condition
;; X-URL: http://code.google.com/p/smart-snippet/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Do you feel that the emace abbrev expansion is not smart enough?
;; Expansion to code snippet should not happen in comment. And
;; sometimes the same abbrev have different meaning. e.g. in ruby,
;; <--------------------
;; if
;; -------------------->
;; should expand into
;; <--------------------
;; if cond
;;   
;; end
;; -------------------->
;; but the "if" at the follow statement
;; <--------------------
;; puts "foo" if predicate
;; -------------------->
;; should not expand to anything.
;;
;; So I write this code trying to solve this problem. It require the
;; wonderful snippet.el.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'smart-snippet)
;; On how to define a conditional snippet, refer to the document of
;; `smart-snippet-abbrev' and `smart-snippet-with-abbrev-table'

;;; Implementation Notes:

;; `snippet-insert' from snippet.el is used to insert snippet.  In
;; order to allow multiple meaning of abbrev(i.e. the same abbrev
;; might expand to different thing under different condition), I
;; maintain a list of (condition . template) in a distinct variable
;; for distincting mode(e.g. smart-snippet-ruby-mode-snippets) , those
;; added later will come to the first of the list. When an abbrev is
;; triggered, the list is searched, the first snippet whose condition
;; is satisfied is then expanded. If no snippet is satisfied, expand
;; nothing(just insert a space).

;;; Code:

(provide 'smart-snippet)
(eval-when-compile
  (require 'cl))

(require 'snippet)

(defun smart-snippet-expand (abbrev &optional abbrev-table)
  "Search the snippets related to ABBREV in TABLE(if supplied)
or the major-mode's default smart-snippet table. Expand the first
snippet whose condition is satisfied. Expand to one space if no
snippet's condition can be satisfied."
  (let* ((table (or abbrev-table
		    (smart-snippet-abbrev-table
		     (format "%s-abbrev-table"
			     major-mode))))
	 (snippet-list (gethash abbrev table)))
    (if (not snippet-list)		; no abbrev found
	(progn (insert abbrev)
	       nil)			; let abbrev insert extra space
      (while (and snippet-list
		  (not (apply
			'smart-snippet-try-expand
			abbrev
			(car snippet-list))))
	(setq snippet-list (cdr snippet-list)))
      (if (not snippet-list)
	  (progn (insert abbrev)
		 nil)                   ; let abbrev insert extra space
	t))))

(defun smart-snippet-try-expand (abbrev condition template)
  "Test CONDITION, if it satisfied, expand ABBREV with TEMPLATE
using `snippet-insert'. Returning t to indicate that this expansion
didn't take place, should try the successive one."
  (let ((abbrev-name abbrev)
	(in-comment? (smart-snippet-inside-comment-p))
	(bol? (looking-back "^[[:blank:]]*")))
    (cond ((and (functionp condition) (funcall condition))
	   (snippet-insert template)
	   t)
	  ((and (or (symbolp condition) (listp condition))
		(eval condition))
	   (snippet-insert template)
	   t)
	  (t nil))))

;; code from http://www.mathcs.emory.edu/~mic/ftp/emacs/lisp-mic.el
(defun smart-snippet-inside-comment-p (&optional on)
  "Is the point inside a comment?
Optional ON means to also count being on a comment start."
  ;; Note: this only handles single-character commenting, as in lisp.
  (or (and on (looking-at "\\s<"))
      (save-excursion
	(skip-syntax-backward "^><")
	(and (not (bobp))
	     (eq (char-syntax (preceding-char)) ?\<)))))

(defun smart-snippet-make-abbrev-expansion-hook
  (abbrev-table abbrev-name template condition)
  "Define a function with the `no-self-insert' property set non-nil.
The function name is composed of \"smart-snippet-abbrev-\", the
abbrev table name, and the name of the abbrev.  If the abbrev
table name ends in \"-abbrev-table\", it is stripped."
  (let ((abbrev-expansion (intern
			   (concat "smart-snippet-abbrev-"
				   (snippet-strip-abbrev-table-suffix
                                    (symbol-name abbrev-table))
                                   "-"
                                   abbrev-name))))
        (fset abbrev-expansion 
          `(lambda ()
	     ,(format (concat "Abbrev expansion hook for \"%s\".\n"
			      "Expand to the following snippet:\n\n%s"
			      "\n\nif the following condition satisfied\n\n%s")
		      abbrev-name
		      template
		      condition)
	     (smart-snippet-expand ,abbrev-name ,table)))
	(put abbrev-expansion 'no-self-insert t)
	abbrev-expansion))
  
(defun smart-snippet-abbrev-table (abbrev-table-name)
  (let ((table-symbol (intern
		      (concat (snippet-strip-abbrev-table-suffix
			       abbrev-table-name)
			      "-smart-snippet-abbrev-table"))))
    (unless (and (boundp table-symbol)
		 (hash-table-p (symbol-value table-symbol)))
      (set table-symbol (make-hash-table :test 'equal)))
    (symbol-value table-symbol)))

(defun smart-snippet-abbrev (abbrev-table abbrev-name template condition)
  "Establish an abbrev for a snippet template.
Set up an abbreviation called ABBREV-NAME in the
ABBREV-TABLE(must be quoted) that expands into a snippet using
the specified TEMPLATE string when CONDITION is satisfied.

if CONDITION is a function, it must accept no argument. It is
called to determine whether to expand the current abbrev.

if CONDITION is a list, it is then evaluated to determine
whether to expand the current abbrev.

if CONDITION is a symbol, it is then evaluated to determine
whether to expand the current abbrev.

All evaluation will be done under a circumstance where those
variables are available:

 abbrev-name : the current abbrev-name being expanded
 inside-comment? : t if currently position is inside comment
 bol? : beginning of line(whitespaces are ignored)
"
  ;; first store the snippet template
  (let* ((table (smart-snippet-abbrev-table (symbol-name abbrev-table)))
	 (snippet-list (gethash abbrev-name table)))
    (puthash abbrev-name
	     (cons (list condition template)
		   snippet-list)
	     table)
  
    ;; then setup the abbrev-table hook
    (define-abbrev (symbol-value abbrev-table) abbrev-name ""
      (smart-snippet-make-abbrev-expansion-hook
       abbrev-table abbrev-name template condition))))

(defmacro smart-snippet-with-abbrev-table
  (abbrev-table &rest snippet-list)
    "Establish a set of abbrevs for snippet templates.
Set up a series of snippet abbreviations in the ABBREV-TABLE (note
that ABBREV-TABLE must be quoted.  The abbrevs are specified in
SNIPPET-LIST.  On how to write each abbrev item, please refer to
`smart-snippet-abbrev-table'."
    (let ((table (gensym)))
      `(let ((,table ,abbrev-table))
	 (progn
	   ,@(loop for (name template condition) in snippet-list
		   collect (list 'smart-snippet-abbrev
				 table
				 name
				 template
				 condition))))))


;;; some snippet.el function rewrite

;; Reason:

;; (setq snippet-exit-identifier "$;")
;; 
;; This is a triky. The default identifier is "$."
;; When you write a snippet(for c/c++) like this:
;;
;;  if ($${condition})
;;  {$>
;;  $.$>
;;  }$>
;;
;; The last "}" won't indent correctly. since there is a "$." at the
;; previous line which is not a complete sentence. So I use "$;" which
;; has a ";" character at the end. This is exactly the character for
;; terminating a sentence in c/c++. Thus the "}" can indent correctly.
;; But this is only a solution for c/c++. If other languages have
;; similar problems, it won't be easy to fix it. So a better way is
;; to rewrite the `snippet-insert' function in snippet.el so that it
;; removes the "$." character before it begins to do indentation (or
;; it just never insert it).

;; Solution:

(defun snippet-split-regexp ()
  "Return a regexp to split the template into component parts."
  (concat (regexp-quote snippet-line-terminator)
          "\\|"
          (regexp-quote snippet-indent)
	  "\\|"
	  (regexp-quote snippet-exit-identifier)))

(defun snippet-insert (template)
  "Insert a snippet into the current buffer at point.  
TEMPLATE is a string that may optionally contain fields which are
specified by `snippet-field-identifier'.  Fields may optionally also
include default values delimited by `snippet-field-default-beg-char'
and `snippet-field-default-end-char'.

For example, the following template specifies two fields which have
the default values of \"element\" and \"sequence\":

  \"for $${element} in $${sequence}:\"

In the next example, only one field is specified and no default has
been provided:

  \"import $$\"

This function may be called interactively, in which case, the TEMPLATE
is prompted for.  However, users do not typically invoke this function
interactively, rather it is most often called as part of an abbrev
expansion.  See `snippet-abbrev' and `snippet-with-abbrev-table' for
more information."
  (interactive "sSnippet template: ")

  ;; Step 1: Ensure only one snippet exists at a time
  (snippet-cleanup)

  ;; Step 2: Create a new snippet and add the overlay to bound the
  ;; template body.  It should be noted that the bounded overlay is
  ;; sized to be one character larger than the template body text.
  ;; This enables our keymap to be active when a field happens to be
  ;; the last item in a template.  We disable abbrev mode to prevent
  ;; our template from triggering another abbrev expansion (I do not
  ;; know if the use of `insert' will actually trigger abbrevs).
  (let ((abbrev-mode nil))
    (setq snippet (make-snippet :bound (snippet-make-bound-overlay)))
    (let ((start (point))
          (count 0))
      (dolist (line (snippet-split-string template (snippet-split-regexp) t))
        (cond ((string-equal snippet-line-terminator line)
               (insert "\n"))
              ((string-equal snippet-indent line)
               (indent-according-to-mode))
	      ((string-equal snippet-exit-identifier line)
	       (setf (snippet-exit-marker snippet) (point-marker)))
              (t
               (insert line))))
      (move-overlay (snippet-bound snippet) start (1+ (point))))


    ;; Step 3: Insert the exit marker so we know where to move point
    ;; to when user is done with snippet.  If they did not specify
    ;; where point should land, set the exit marker to the end of the
    ;; snippet. 
    (goto-char (overlay-start (snippet-bound snippet)))
    
    (unless (snippet-exit-marker snippet)
      (let ((end (overlay-end (snippet-bound snippet))))
        (goto-char (if (= end (point-max)) end (1- end))))
      (setf (snippet-exit-marker snippet) (point-marker)))
  
    (set-marker-insertion-type (snippet-exit-marker snippet) t)

    ;; Step 4: Create field overlays for each field and insert any
    ;; default values for the field.
    (goto-char (overlay-start (snippet-bound snippet)))
    (while (re-search-forward (snippet-field-regexp)
                              (overlay-end (snippet-bound snippet)) 
                              t)
      (let ((field (snippet-make-field-overlay (match-string 2)))
            (start (match-beginning 0)))
        (push field (snippet-fields snippet))
        (replace-match (if (match-beginning 2) "\\2" ""))
        (move-overlay field start (point))))
    
    ;; These are reversed so they are in order of how they appeared in
    ;; the template as we index into this list when cycling field to
    ;; field. 
    (setf (snippet-fields snippet) (reverse (snippet-fields snippet))))

  ;; Step 5: Position the point at the first field or the end of the
  ;; template body if no fields are present.  We need to take into
  ;; consideration the special case where the first field is at the
  ;; start of the snippet (otherwise the call to snippet-next-field
  ;; will go past it).
  (let ((bound (snippet-bound snippet))
        (first (car (snippet-fields snippet))))
    (if (and first (= (overlay-start bound) (overlay-start first)))
        (goto-char (overlay-start first))
      (goto-char (overlay-start (snippet-bound snippet)))
      (snippet-next-field))))


;;; smart-snippet.el ends here
