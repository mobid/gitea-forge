;;; gitea-review.el --- Gitea pull request review board -*- lexical-binding: t -*-

;; Author: Matija Obid <matija.obid@posteo.net>
;; Version: 1.0.0
;; URL: https://github.com/mobid/gitea-forge
;; Package-Requires: ((emacs "29.1") (f>orge "20231213") (ghub "20220621"))
;; Keywords: git tools vc

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Gitea review board based on `diff-mode'.  Review can be started with
;; `gitea-review' function on pullrequest and finished with C-c C-c.
;;
;; On top of file are, in ugly way, shown all reviws. Below are diff,
;; where comment can be placed with "m", etc, ... check help mode.
;;
;; If you use `embark-vc' package, this package add "g" key to
;; `embark-vc-pull-request-map' map.
;;
;; This package is WIP and most probably will be published separately.
;;
;; Api doc is available on https://docs.gitea.com/api/1.20/
;;

;;; Code:

(require 'gitea-forge)
(require 'deferred)

;; TODO - If comment is placed over other comment, it take bottom line for reference.
;; TODO - Maybe, narrow comments to review.

(quote ;; XXX
 (progn
   ;; Required:
   (use-package deferred
     :demand t)
   ;; Optional:
   (use-package embark-vc
     :demand t
     :ensure t))
 )

(defface gitea-review-comment-sign
  '((t :foreground "black"
       :background "black"))
  "Face used for diff comment sign.")

(defvar-local gitea-review-comments nil
  "List of all gitea comments.

Unsubmitted comments are at end of list.")

(defun gitea-review-comments ()
  "Get comments waiting to be submitted."
  (seq-remove (lambda (c)
                (not (gitea-comment-new c)))
              gitea-review-comments))

(cl-defstruct gitea-comment
  author created updated body file old-line new-line
  new pos)

(defvar-local gitea-review-pr nil)

;;; Api calls:

(defun gitea-review--diff (pullreq)
  "Load PULLREQ diff file (string).

This function is deferred."
  (let ((d (deferred:new #'identity)))
    (forge--gtea-get pullreq (format "repos/:owner/:repo/pulls/%s.diff" (oref pullreq number)) nil
      :callback (lambda (diff &rest _args)
                  (deferred:callback-post d (alist-get 'message diff))))
    d))

(cl-defmethod gitea-review--reviews ((pullreq forge-pullreq))
  "Load PULLREQ reviews."
  (let ((d (deferred:new #'identity)))
    (forge--gtea-get pullreq "repos/:owner/:repo/pulls/:number/reviews" nil
      :callback (lambda (value &rest _args)
                  (deferred:callback-post d value)))
    d))

(cl-defmethod gitea-review--notes ((pullreq forge-pullreq) review)
  "Load PULLREQ REVIEW notes."
  (let ((d (deferred:new #'identity)))
    (let-alist review
      (if (string= .state "REQUEST_REVIEW") ; Review requests does not have a comments.
          (deferred:callback-post d '())
        (forge--gtea-get pullreq (format "repos/:owner/:repo/pulls/:number/reviews/%s/comments" .id)
          nil
          :callback (lambda (value &rest _args)
                      (deferred:callback-post d value)))))
    d))

(defun gitea-review--data (pullreq)
  "Get plist of all PULLREQ required data."
  (deferred:$
   (deferred:parallel
    (gitea-review--diff pullreq)
    (deferred:$ (gitea-review--reviews pullreq)
                (deferred:nextc it
                                (lambda (reviews)
                                  (apply #'deferred:parallel
                                         (cons (deferred:succeed reviews)
                                               (mapcar (lambda (review)
                                                         (gitea-review--notes pullreq review))
                                                       reviews)))))
                (deferred:nextc it
                                (lambda (x)
                                  (cons (car x)
                                        (seq-remove
                                         (lambda (c)
                                           (and
                                            (not (gitea-comment-old-line c))
                                            (not (gitea-comment-new-line c))))
                                         (mapcar #'gitea-review-note->comment (apply #'append (cdr x)))))))))
   (deferred:nextc it
                   (lambda (obj)
                     (list :diff (cl-first obj)
                           :reviews (car (cl-second obj))
                           :comments (cdr (cl-second obj)))))))

(defun gitea-review-note->comment (note)
  "Convert NOTE (from api) into comment."
  (let-alist note
    (make-gitea-comment :author .user.full_name
                        :created .created_at
                        :updated .updated_at
                        :body .body
                        :file .path
                        :old-line (unless (zerop .original_position) .original_position)
                        :new-line (unless (zerop .position) .position))))

;;; Embark Support:

(with-eval-after-load 'embark-vc
  (define-key embark-vc-pull-request-map (kbd "g")
              (lambda (num)
                "Start gitea review"
                (gitea-review num))))

;;; Major mode:

(defvar-keymap gitea-review-mode-map
  :doc "Keymap for `gitea-review-mode'."
  "n" #'gitea-review-comment-next
  "p" #'gitea-review-comment-prev
  "N" #'gitea-review-comment-next-new
  "P" #'gitea-review-comment-prev-new
  "M-p" #'diff-hunk-prev
  "M-n" #'diff-hunk-next
  "C-M-p" #'diff-file-prev
  "C-M-n" #'diff-file-next
  "m" #'gitea-review-new-comment
  "e" #'gitea-review-edit-comment
  "k" #'gitea-review-delete-comment
  "C-c C-c" #'gitea-review-submit
  "RET" #'gitea-review-find-file)

(define-derived-mode gitea-review-mode diff-mode "Code Review"
  "Major mode for code review."
  (setq tab-width 4)
  (setq-local revert-buffer-function (lambda (&rest _args)
                                       (emacs-lock-mode -1)
                                       (gitea-review-update-header)
                                       (gitea-review (oref gitea-review-pr number))))
  (font-lock-add-keywords nil '(("^# " 0'gitea-review-comment-sign t)))

  ;; Unset some keys:
  (make-local-variable 'diff-mode-shared-map)
  (define-key diff-mode-shared-map (kbd "p") nil)
  (define-key diff-mode-shared-map (kbd "n") nil)
  (define-key diff-mode-shared-map (kbd "P") nil)
  (define-key diff-mode-shared-map (kbd "N") nil)
  (define-key diff-mode-shared-map (kbd "k") nil)
  (define-key diff-mode-shared-map (kbd "RET") nil))

(defun gitea-review (number)
  "Start reviewing pullrequest with given NUMBER.

After you make first comment, `emacs-lock-mode' will be enabled,
until review is submitted.

This function is deferred."
  (interactive (list (forge-read-topic "View topic")))
  (let* ((pullreq (forge-get-topic (if (numberp number) number (string-to-number number)))))
    (deferred:$ (gitea-review--data pullreq)
                (deferred:nextc it
                                (lambda (obj)
                                  (gitea-review- pullreq
                                                 (plist-get obj :diff)
                                                 (plist-get obj :reviews)
                                                 (plist-get obj :comments)))))))


(defun gitea-review- (pullreq diff reviews comments)
  "Start review with all data supplied.

PULLREQ - forge pull request
DIFF - string with diff
REVIEWS - alist of reviews fetched from api
COMMENTS - `gitea-comment' list"
  (with-current-buffer (get-buffer-create (format "review: #%s - %s" (oref pullreq number) (oref pullreq title)))
    (gitea-review-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (review reviews (insert "======================================\n"))
        (let-alist review
          (insert (format "%s - %s %s\n" .user.full_name .state
                          (magit--age
                           (float-time
                            (date-to-time .submitted_at)))))))
      (insert diff)
      (newline)
      (setq gitea-review-pr pullreq)
      (gitea-review--display comments))
    (read-only-mode +1)
    (gitea-review-update-header)
    (switch-to-buffer (current-buffer))))

;;; Comments:

(defun gitea-review--comment< (c1 c2)
  "Compare comments C1 to C2 by line and date.

This function ensure, that, if we placing comment in this order,
proper comment threads will be created (in chronological order)."
  (let ((l1 (or (gitea-comment-old-line c1)
                (gitea-comment-new-line c1)))
        (l2 (or (gitea-comment-old-line c2)
                (gitea-comment-new-line c2))))
    (if (not (= l1 l2))
        (< l1 l2)
      (let ((d1 (gitea-comment-created c1))
            (d2 (gitea-comment-created c2)))
        (compare-strings d1 nil nil d2 nil nil)))))

(defun gitea-review--display (comments)
  "Display COMMENTS with placing them to proper position in diff."
  (let* ((comments (sort comments #'gitea-review--comment<)))
    (dolist (c comments)
      (gitea-review--place-comment c)
      (add-to-list 'gitea-review-comments c))
    (gitea-review--sort-comments)))

(defun gitea-review--hunk-parse ()
  "Parse hunk description at point.

Result is cons (old line, new line)"
  (when (looking-at "@@ -\\([0-9]+\\),[0-9]+ \\+\\([0-9]+\\),[0-9]+ @@")
    (cons (string-to-number (match-string 1))
          (string-to-number (match-string 2)))))

(defun gitea-review--goto-change (file old-line new-line)
  "Place cursor in diff to FILE and line.

OLD-LINE take precedence over NEW-LINE, only one should be given."
  (let* ((old? old-line)
         (rx (if old? "^[- \t]" "^[+ \t]"))
         (line (if old? old-line new-line)))
    (goto-char (point-min))
    (re-search-forward (concat "\\(---\\|\\+\\+\\+\\) \\(a\\|b\\)/" file))
    (let ((bound (save-excursion
                   (or
                    (ignore-errors
                      (diff-file-next)
                      (point))
                    (point-max)))))
      (let ((base-pos nil)
            (base-hunk nil)
            (doit t))
        (while doit
          (diff-hunk-next)
          (if (or (eobp)
                  (<= bound (point)))
              (setq doit nil)
            (let ((hunk (gitea-review--hunk-parse)))
              (if (and hunk (<= (if old? (car hunk) (cdr hunk)) line))
                  (setq base-pos (point)
                        base-hunk hunk)
                (setq doit nil)))))
        (unless base-pos
          (error "Hunk not found"))
        (let* ((base-line (if old? (car base-hunk) (cdr base-hunk)))
               (forw (- line base-line)))
          (goto-char base-pos)
          (forward-line 1)
          (while (not (zerop forw))
            (unless (zerop (forward-line 1))
              (error "Hunk eob %s" forw))
            (when (looking-at rx)
              (cl-decf forw))))))))

(defun gitea-review--place-comment (comment)
  "Place COMMENT into diff."
  (let ((inhibit-read-only t))
    (gitea-review--goto-change (gitea-comment-file comment)
                               (gitea-comment-old-line comment)
                               (gitea-comment-new-line comment))

    (forward-line 1)
    (while (looking-at "# ")
      (forward-line))
    (open-line 1)
    (let ((ago (apply #'format "%s %s ago"
                      (magit--age
                       (float-time
                        (date-to-time (gitea-comment-updated comment))))))
          (point (point-marker)))
      (insert (propertize (format "# %s %s%s\n" (gitea-comment-author comment) ago
                                  (if (gitea-comment-new comment) " ⭐" ""))
                          'font-lock-face 'magit-diff-hunk-heading-highlight
                          'type 'comment
                          'comment comment))
      (insert (propertize (concat (propertize "# " 'font-lock-face '((nil (:foreground "cyan"))))
                                  (string-join (string-lines (forge--fontify-markdown (concat (gitea-comment-body comment) "\n")))
                                               "\n# ")
                                  "\n")
                          'font-lock-face 'magit-section-highlight
                          'type 'comment
                          'comment comment))
      (when (looking-at "^$")
        (delete-char 1))
      (put-text-property point (point) 'comment-range (cons point (point-marker)))
      (setf (oref comment pos) point))))

;;; Navigation over comments:

(defun gitea-review-comment-next ()
  "Goto next comment."
  (interactive)
  (goto-char (catch 'pos
               (dolist (c gitea-review-comments (error "No next comment"))
                 (when (< (point) (gitea-comment-pos c))
                   (throw 'pos (gitea-comment-pos c)))))))

(defun gitea-review-comment-prev ()
  "Goto previous comment."
  (interactive)
  (goto-char (catch 'pos
               (dolist (c (reverse gitea-review-comments) (error "No prev comment"))
                 (when (< (gitea-comment-pos c) (point))
                   (throw 'pos (gitea-comment-pos c)))))))

(defun gitea-review-comment-next-new ()
  "Goto next unsubmitted comment."
  (interactive)
  (goto-char (catch 'pos
               (dolist (c (gitea-review-comments) (error "No next comment"))
                 (when (< (point) (gitea-comment-pos c))
                   (throw 'pos (gitea-comment-pos c)))))))

(defun gitea-review-comment-prev-new ()
  "Goto previous unsubmitted comment."
  (interactive)
  (goto-char (catch 'pos
               (dolist (c (reverse (gitea-review-comments)) (error "No prev comment"))
                 (when (< (gitea-comment-pos c) (point))
                   (throw 'pos (gitea-comment-pos c)))))))

;;; Hunks:

(defun gitea-review--hunk-old? ()
  "Return t if line at point is deleted."
  (save-excursion
    (beginning-of-line)
    (looking-at "-")))

(defun gitea-review--hunk-file ()
  "Return filename of line at point in current hunk."
  (save-excursion
    (let ((use-old (gitea-review--hunk-old?)))
      (diff-beginning-of-file)
      (unless use-old
        (forward-line 1))
      (re-search-forward "^\\(?:---\\|\\+\\+\\+\\) \\(?:a/\\|b/\\)\\(.*\\)$")
      (substring-no-properties (match-string 1)))))

(defun gitea-review--hunk-line ()
  "Return line in file at point in current hunk.

If line has prefix \"-\" it is old position otherwise it is new position.

This code is similar to `diff-split-hunk'."
  (save-excursion
    (let ((use-old (gitea-review--hunk-old?))
          (pos (point)))
      (diff-beginning-of-hunk)
      (unless (looking-at diff-hunk-header-re-unified)
        (error "Diff is not in unified format"))
      (forward-line 1)
      (let* ((start1 (string-to-number (match-string 1)))
	     (start2 (string-to-number (match-string 3)))
	     (newstart1 (+ start1 (diff-count-matches "^[- \t]" (point) pos)))
	     (newstart2 (+ start2 (diff-count-matches "^[+ \t]" (point) pos))))
        (if use-old newstart1 newstart2)))))

;;; Comments add / edit / remove

(defun gitea-review-new-comment ()
  "Place new comment in current position."
  (interactive)
  (save-excursion
    (re-search-backward "^\\(-\\|\\+\\| \\)"))
  (let ((old? (gitea-review--hunk-old?))
        (file (gitea-review--hunk-file))
        (line (gitea-review--hunk-line))
        (body (gitea-review-read "Comment: "))
        (date (format-time-string "%FT%T%z")))
    (let ((comment (make-gitea-comment :author user-full-name
                                       :created date
                                       :updated date
                                       :body body
                                       :file file
                                       :old-line (and old? line)
                                       :new-line (and (not old?) line)
                                       :new t)))
      (emacs-lock-mode +1)
      (gitea-review--place-comment comment)
      (setcdr (last gitea-review-comments) (list comment))
      (gitea-review-update-header))))

(defun gitea-review-delete-comment ()
  "Delete (submitted) comment at point."
  (interactive)
  (let ((comment (get-text-property (point) 'comment))
        (range (get-text-property (point) 'comment-range)))
    (unless comment
      (error "No comment at point"))
    (unless (cl-find comment (gitea-review-comments))
      (error "No pending comment"))
    (let ((inhibit-read-only t))
      (setq gitea-review-comments (remove comment gitea-review-comments))
      (delete-region (car range) (cdr range)))))

(defun gitea-review-clean-comments ()
  "Clear all comments from diff."
  (save-excursion
    (let ((inhibit-read-only t))
      (dolist (c gitea-review-comments)
        (when (gitea-comment-pos c)
          (goto-char (gitea-comment-pos c))
          (let ((range (get-text-property (point) 'comment-range)))
            (delete-region (car range) (cdr range))))))))

(defun gitea-review-edit-comment ()
  "Edit comment at point."
  (interactive)
  (let ((c (get-text-property (point) 'comment)))
    (unless (and c (gitea-comment-new c))
      (error "Only unsaved comments can be edited"))
    (setf (gitea-comment-body c) (gitea-review-read "Edit:" (gitea-comment-body c)))
    (gitea-review-delete-comment)
    (gitea-review--place-comment c)
    (setcdr (last gitea-review-comments) (list c))
    (gitea-review--sort-comments)))

;;; Submit pull request:

(defun gitea-review-submit (action)
  "Submit pull request with ACTION (approve, comment, reject)."
  (interactive (list (let ((str (completing-read "Action: " '("Approved" "Comment" "Request Changes"))))
                       (string-replace " " "_" (upcase str)))))
  (let ((pr gitea-review-pr)
        (body (gitea-review-read "Final comment:"))
        (buffer (current-buffer)))
    (let ((comments (mapcar (lambda (c)
                              `((body . ,(gitea-comment-body c))
                                (old_position . ,(or (gitea-comment-old-line c) 0))
                                (new_position . ,(or (gitea-comment-new-line c) 0))
                                (path . ,(gitea-comment-file c))))
                            (gitea-review-comments))))
      (forge--gtea-post pr "repos/:owner/:repo/pulls/:number/reviews"
        `((body . ,body)
          (event . ,action)
          (commit_id . ,(oref pr head-rev))
          ,@(when comments `((comments . ,comments))))
        :callback (lambda (&rest _args)
                    (with-current-buffer buffer
                      (revert-buffer)))))))

;;; Aux:

(defun gitea-review-read (prompt &optional string)
  "Read STRING from minibuffer."
  (string-edit
   prompt
   (or string "")
   (lambda (edited)
     (setq string edited)
     (exit-recursive-edit))
   :abort-callback (lambda () (error "Aborted edit")))
  (recursive-edit)
  (substring-no-properties string))

(defun gitea-review-find-file ()
  "Find file at point."
  (interactive)
  (let ((old? (gitea-review--hunk-old?))
        (file (gitea-review--hunk-file))
        (line (1- (gitea-review--hunk-line)))
        (col (1- (current-column))))
    (magit-find-file (if old?
                         (oref gitea-review-pr base-rev)
                       (oref gitea-review-pr head-rev))
                     file)
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char col)))

(defun gitea-review-update-header ()
  "Update `header-line'."
  (let ((pr gitea-review-pr))
    (setq header-line-format
          (concat (format "#%s %s" (oref pr number) (oref pr title))
                  (when-let ((cs (gitea-review-comments)))
                    (format " (%s pending comments)" (length cs)))))))




(defun gitea-review--sort-comments ()
  "Sort comments to ensure correct display.

Submitted comments are sorted before persisted ones.  Comments
are sorted by position in buffer."
  (setq gitea-review-comments
        (sort gitea-review-comments
              (lambda (a b)
                (let ((x (gitea-comment-new a))
                      (y (gitea-comment-new b)))
                  (if (eq x y)
                      (< (gitea-comment-pos a) (gitea-comment-pos b))
                    y))))))

(provide 'gitea-review)

;;; gitea-review.el ends here
