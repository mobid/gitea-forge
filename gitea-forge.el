;;; gitea-forge.el --- Gitea support for forge package -*- lexical-binding: t -*-

;; Author: Matija Obid <matija.obid@posteo.net>
;; Version: 1.0.0
;; URL: https://github.com/mobid/gitea-forge
;; Package-Requires: ((forge "20231213") (ghub "20220621"))
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

;; Forge extensions which add gitea support.

;;; Code:

(require 'gtea)
(require 'forge)
(require 'forge-gitea)
(require 'forge-issue)
(require 'forge-pullreq)
(require 'forge-commands)

;;; Pull

(defvar forge--gtea-batch-size 240
  "Number of pullreqs/issues to be fetched in one page.")

;;;; Repository

(cl-defmethod forge--pull ((repo forge-gitea-repository) since &optional callback)
  (cl-assert (not (and since (forge-get-repository repo :tracked?))))
  (setq since (or since (oref repo updated)))
  (let ((cb (let ((buf (current-buffer))
                  (val nil)
                  (initial-pull (not (oref repo pullreqs-until))))
              (lambda (cb &optional v)
                (when v (if val (push v val) (setq val v)))
                (let-alist val
                  (cond
                   ((not val)
                    (forge--fetch-repository repo cb))
                   ((not (assq 'assignees val))
                    (forge--fetch-assignees repo cb))
                   ((not (assq 'labels val))
                    (forge--fetch-labels repo cb))
                   ((not (assq 'milestones val))
                    (forge--fetch-milestones repo cb))
                   ((and .has_issues
                         (not (assq 'issues val)))
                    (forge--fetch-issues repo cb since))
                   ((and .has_pull_requests
                         (not (assq 'pullreqs val)))
                    (forge--fetch-pullreqs repo cb since))
                   (t
                    (forge--msg repo t t   "Pulling REPO")
                    (forge--msg repo t nil "Storing REPO")
                    (closql-with-transaction (forge-db)
                      (forge--update-repository repo val)
                      (forge--update-assignees  repo .assignees)
                      (forge--update-labels     repo .labels)
                      (forge--update-milestones repo .milestones)
                      (dolist (v .issues)   (forge--update-issue repo v 'bump initial-pull))
                      (dolist (v .pullreqs) (forge--update-pullreq repo v 'bump initial-pull))
                      (oset repo condition :tracked))
                    (forge--msg repo t t "Storing REPO")
                    (cond
                     ((oref repo selective-p))
                     (callback (funcall callback))
                     ((forge--maybe-git-fetch repo buf))))))))))
    (funcall cb cb)))

(cl-defmethod forge--fetch-repository ((repo forge-gitea-repository) callback)
  (forge--gtea-get repo "repos/:owner/:repo" nil
    :callback (lambda (value _headers _status _req)
                (when (oref repo selective-p)
                  (setq value (append '((assignees) (labels)
                                        (issues) (pullreqs))
                                      value)))
                (funcall callback callback value))))

(cl-defmethod forge--update-repository ((repo forge-gitea-repository) data)
  (let-alist data
    (oset repo created        .created_at)
    (oset repo updated        .updated_at)
    (oset repo pushed         nil)
    (oset repo parent         .parent.full_name)
    (oset repo description    .description)
    (oset repo homepage       .html_url)
    (oset repo default-branch .default_branch)
    (oset repo archived-p     .archived)
    (oset repo fork-p         .fork)
    (oset repo locked-p       nil)
    (oset repo mirror-p       .mirror)
    (oset repo private-p      .private)
    (oset repo issues-p       .has_issues)
    (oset repo wiki-p         .has_wiki)
    (oset repo stars          .stars_count)
    (oset repo watchers       .watchers_count)))

(cl-defmethod forge--fetch-assignees ((repo forge-gitea-repository) callback)
  (forge--gtea-get repo "repos/:owner/:repo/assignees" nil
    :callback (lambda (value _headers _status _req)
                (funcall callback callback (cons 'assignees value)))))

(cl-defmethod forge--update-assignees ((repo forge-gitea-repository) data)
  (oset repo assignees
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      ;; For other forges we don't need to store `id'
                      ;; but here we do because that's what has to be
                      ;; used when assigning issues.
                      (list (forge--object-id id .id)
                            .login
                            .full_name
                            .id)))
                  data))))

(cl-defmethod forge--fetch-milestones ((repo forge-gitea-repository) callback)
  (forge--gtea-get repo "repos/:owner/:repo/milestones" nil
    :callback (lambda (value _headers _status _req)
                (funcall callback callback (cons 'milestones value)))))

(cl-defmethod forge--update-milestones ((repo forge-gitea-repository) data)
  (oset repo milestones
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .id)
                            .id
                            .title
                            .created_at
                            .updated_at
                            .due_on
                            .closed_at
                            .description)))
                  (delete-dups data)))))

(cl-defmethod forge--fetch-labels ((repo forge-gitea-repository) callback)
  (forge--gtea-get repo "repos/:owner/:repo/labels" nil
    :callback (lambda (value _headers _status _req)
                (funcall callback callback (cons 'labels value)))))

(cl-defmethod forge--update-labels ((repo forge-gitea-repository) data)
  (oset repo labels
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .id)
                            .name
                            (concat "#" (downcase .color))
                            .description)))
                  data))))


;;;; Issues

(defun forge--gtea-fetch-topics-cb (field repo callback)
  (let ((typ (cl-ecase field
               (issues "issues")
               (pullreqs "PRs")))
        i cnt)
    (lambda (cb topics &optional val)
      (unless cnt
        (setq i 0
              cnt (length topics)))
      (cl-incf i)
      (if topics
          (progn
            (forge--msg nil nil nil "Pulling %s %s/%s" typ i cnt)
            (forge--fetch-issue-posts
             repo topics
             (lambda (cbb)
               (if (or (eq field 'issues)
                       (not cbb))
                   (funcall cb cb (cdr topics) (cons (car topics) val))
                 ;; Load reviews for pull requests:
                 (forge--fetch-issue-reviews repo topics
                                             (lambda (_)
                                               (funcall cbb nil))
                                             t)))))
        (forge--msg nil nil t "Pulling %s %s/%s" typ i cnt)
        (funcall callback callback (cons field (nreverse val)))))))

(cl-defmethod forge--fetch-issues ((repo forge-gitea-repository) callback since)
  (let ((cb (forge--gtea-fetch-topics-cb 'issues repo callback)))
    (forge--msg repo t nil "Pulling REPO issues")
    (forge--gtea-get repo "repos/:owner/:repo/issues"
      `((limit . ,forge--gtea-batch-size)
        (type . "issues")
        (state . "all")
        ,@(and-let* ((after (or since (oref repo issues-until))))
            `((updated_after . ,after))))
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (if since
                      (funcall cb cb value)
                    (funcall callback callback (cons 'issues value)))))))

(cl-defmethod forge--fetch-issue-posts ((repo forge-gitea-repository) cur cb)
  (let-alist (car cur)
    (forge--gtea-get repo (format "repos/:owner/:repo/issues/%s/comments" .number) nil
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (setf (alist-get 'notes (car cur)) value)
                  (funcall cb cb)))))

(cl-defmethod forge--update-issue ((repo forge-gitea-repository) data &optional bump initial-pull)
  (closql-with-transaction (forge-db)
    (let-alist data
      (let* ((issue-id (forge--object-id 'forge-issue repo .number))
             (issue (or (forge-get-issue repo .number)
                        (closql-insert
                         (forge-db)
                         (forge-issue :id           issue-id
                                      :repository   (oref repo id)
                                      :number       .number)))))
        (oset issue slug         (format "#%s" .number))
        (oset issue their-id     .id)
        (oset issue number       .number)
        (oset issue repository   (oref repo id))
        (oset issue state        (pcase-exhaustive .state
                                   ("closed" 'completed)
                                   ("open" 'open)))
        (oset issue author       .user.username)
        (oset issue title        .title)
        (oset issue created      .created_at)
        (oset issue updated      .updated_at)
        (oset issue closed       (or .closed_at (and (equal .state "closed") 1)))
        (oset issue locked-p     .is_locked)
        (oset issue milestone    .milestone.title)
        (oset issue labels       (mapcar (lambda (label)
                                           (forge--object-id (oref repo id) (cdr (assoc 'id label))))
                                         .labels))
        (oset issue body         (forge--sanitize-string .body))

        (unless (magit-get-boolean "forge.omitExpensive")
          (forge--set-id-slot repo issue 'assignees .assignees)
          (forge--set-id-slot repo issue 'labels .labels))
        (dolist (comment .notes)
          (let-alist comment
            (closql-insert
             (forge-db)
             (forge-issue-post
              :id      (forge--object-id issue-id .id)
              :issue    issue-id
              :number  .id
              :author  .user.login
              :created .created_at
              :updated .updated_at
              :body (let ((body (forge--sanitize-string .body))
                          (hunk .diff_hunk))
                      (if (not hunk)
                          body
                        (concat "```diff\n"
                                (string-trim hunk)
                                "\n```\n--\n"
                                body))))
             t)))
        (forge--update-status repo issue data bump initial-pull)
        issue))))

(cl-defmethod forge--pull-topic ((repo forge-gitea-repository)
                                 (topic forge-topic)
                                 &key callback _errorback)
  (condition-case _
      (let ((data (forge--gtea-get topic "repos/:owner/:repo/pulls/:number"))
            (cb (forge--gtea-fetch-topics-cb 'pullreqs repo
                                             (lambda (_ data)
                                               (forge--update-pullreq repo (cadr data))
                                               (when callback
                                                 (funcall callback))))))
        (funcall cb cb (list data)))
    (error
     (let ((data (forge--gtea-get topic "repos/:owner/:repo/issues/:number"))
           (cb (forge--gtea-fetch-topics-cb 'issues repo
                                            (lambda (_ data)
                                              (forge--update-issue repo (cadr data))
                                              (when callback
                                                 (funcall callback))))))
       (funcall cb cb (list data))))))

;;;; Pull requests

(cl-defmethod forge--fetch-pullreqs ((repo forge-gitea-repository) callback since)
  (let ((cb (forge--gtea-fetch-topics-cb 'pullreqs repo callback))
        (since (and since (date-to-time since))))
    (forge--msg repo t nil "Pulling REPO PRs")
    (forge--gtea-get* repo "repos/:owner/:repo/pulls"
      `((limit . ,forge--gtea-batch-size)
        (sort . "recentupdate"))
      :callback (lambda (value _headers _status _req)
                  (if since
                      (funcall cb cb value)
                    (funcall callback callback (cons 'pullreqs value))))
      :while (lambda (res)
               (let-alist res
                 (or (not since) (time-less-p since (date-to-time .updated_at))))))))

(cl-defmethod forge--fetch-pullreq-posts ((repo forge-gitea-repository) cur cb)
  (let-alist (car cur)
    (forge--gtea-get repo (format "repos/:owner/:repo/pulls/%s/comments" .number) nil
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (setf (alist-get 'notes (car cur)) value)
                  (funcall cb cb)))))

(cl-defmethod forge--update-pullreq ((repo forge-gitea-repository) data &optional bump initial-pull)
  (closql-with-transaction (forge-db)
    (let-alist data
      (let* ((pullreq-id (forge--object-id 'forge-pullreq repo .number))
             (pullreq (or (forge-get-pullreq repo .number)
                          (closql-insert
                           (forge-db)
                           (forge-pullreq :id           pullreq-id
                                          :repository   (oref repo id)
                                          :number       .number)))))
        (oset pullreq their-id     .id)
        (oset pullreq slug         (format "#%s" .number))
        (oset pullreq author       .user.login)
        (oset pullreq title        .title)
        (oset pullreq created      .created_at)
        (oset pullreq updated      .updated_at)
        (oset pullreq merged       .merged_at)
        (oset pullreq closed       .closed_at)
        (oset pullreq body         (forge--sanitize-string .body))
        (oset pullreq state        (if .merged
                                       'merged
                                     (pcase-exhaustive .state
                                       ("closed" 'rejected)
                                       ("open"   'open))))
        (oset pullreq base-repo    "unknown")
        (oset pullreq head-repo    "unknown")
        (oset pullreq draft-p      (gitea-forge-is-draft? .title))
        (oset pullreq cross-repo-p nil)
        (oset pullreq base-ref     .base.ref)
        (oset pullreq base-rev     .merge_base)
        (oset pullreq head-ref     .head.ref)
        (oset pullreq head-rev     .head.sha)
        (oset pullreq milestone    (and .milestone.id
                                        (forge--object-id (oref repo id)
                                                          .milestone.id)))
        (oset pullreq labels       (mapcar (lambda (label)
                                             (forge--object-id (oref repo id) (cdr (assoc 'id label))))
                                           .labels))
        (forge--set-id-slot repo pullreq 'assignees .assignees)

        (forge--set-id-slot repo pullreq 'review-requests
                            (mapcan (lambda (review)
                                      (when (string= "REQUEST_REVIEW" (alist-get 'state review))
                                        (list (alist-get 'user review))))
                                    .reviews))
        (let ((reviews (mapcan (lambda (review)
                                 (unless (string-empty-p (alist-get 'body review))
                                   (list review)))
                               .reviews))
              (review-notes (mapcan (lambda (review)
                                      (alist-get 'notes review))
                                    .reviews)))
          (dolist (comment (append .notes reviews review-notes))
            (let-alist comment
              (closql-insert
               (forge-db)
               (forge-pullreq-post
                :id      (forge--object-id pullreq-id .id)
                :pullreq pullreq-id
                :number  .id
                :author  .user.login
                :created (or .created_at .submitted_at)
                :updated (or .updated_at .submitted_at)
                :body (let ((body (forge--sanitize-string .body))
                            (hunk .diff_hunk))
                        (if (not hunk)
                            body
                          (concat "```diff\n"
                                  (string-trim hunk)
                                  "\n```\n--\n"
                                  body))))
               t))))
        (forge--update-status repo pullreq data bump initial-pull)
        pullreq))))

;;;; Reviews:
(cl-defmethod forge--fetch-issue-reviews ((repo forge-gitea-repository) cur callback &optional load-notes)
  (let-alist (car cur)
    (let ((cb (lambda (cb reviews)
                ;; TODO Add main comment of review.
                (if reviews
                    (forge--fetch-issue-review-notes repo .number reviews
                                                     (lambda (_)
                                                       (funcall cb cb (cdr reviews))))
                  (funcall callback callback)))))
      (forge--gtea-get repo (format "repos/:owner/:repo/pulls/%s/reviews" .number) nil
        :callback (lambda (value &rest _args)
                    (setf (alist-get 'reviews (car cur)) value)
                    (if load-notes
                        (funcall cb cb value)
                      (funcall callback callback)))))))

(cl-defmethod forge--fetch-issue-review-notes ((repo forge-gitea-repository) number cur callback)
  (let-alist (car cur)
    (if (string= .state "REQUEST_REVIEW") ; Review requests does not have a comments.
        (funcall callback callback)
      (forge--gtea-get repo (format "repos/:owner/:repo/pulls/%s/reviews/%s/comments" number .id)
        nil
        :callback (lambda (value &rest _args)
                    (setf (alist-get 'notes (car cur)) value)
                    (funcall callback callback))))))


;;;; Notifications:

(defun forge--gtea-massage-notification (data _forge githost callback)
  (let-alist data
    (let* ((number (and (string-match "[0-9]*\\'" .subject.url)
                        (string-to-number (match-string 0 .subject.url))))
           (type (pcase-exhaustive .subject.type
                                     ("Pull" 'pullreq)
                                     ("Issue" 'issue)))
           (url-part (if (eq type 'pullreq) "pulls" "issues"))
           (repo   (forge-get-repository
                    (list githost
                          .repository.owner.login
                          .repository.name)
                    nil :insert!))
           (repoid (oref repo id))
           (id     (forge--object-id repoid .id))
           (cb (lambda (_ pullreq)
                 (funcall
                  callback
                  (list
                   repo
                   pullreq
                   (forge-notification
                    :id           id
                    :thread-id    .id
                    :repository   repoid
                    :reason       (intern (downcase .subject.state))
                    :last-read    .last_read_at
                    :updated      .updated_at
                    :title        (concat "[" .subject.state "] - " .subject.title)
                    :type         type
                    :topic        (forge--object-id 'forge-pullreq repo (cdr (assoc 'number pullreq)))
                    :url          .subject.url))))))
      (forge--gtea-get nil (format "repos/%s/%s/%s" .repository.full_name url-part number)
        nil
        :callback (lambda (pull &rest _args)
                    (funcall cb cb pull))))))

(setq last-notifications nil)

(cl-defmethod forge--pull-notifications ((repo (subclass forge-gitea-repository)) githost &optional callback)
  (cl-flet ((update-topic (repo data)
              (let* ((url (alist-get 'url data))
                     (field (if (string-match "/pulls/[0-9]+$" url) 'pullreqs 'topics))
                     (cb (lambda (_ data)
                           (if (eq field 'pullreqs)
                               (forge--update-pullreq repo (cadr data))
                             (forge--update-issue repo (cadr data)))))
                     (cb (forge--gtea-fetch-topics-cb 'pullreqs repo cb)))
                (funcall cb cb (list data)))))
    (let ((spec (assoc githost forge-alist)))
      (unless spec
        (error "No entry for %S in forge-alist" githost))
      (forge--msg nil t nil "Pulling notifications")
      (let ((cb (lambda (cb data &optional n)
                  (if data
                      (forge--gtea-massage-notification
                       (car data) repo githost
                       (lambda (noti)
                         (pcase-let ((`(,repo ,topic ,obj) noti))
                           (update-topic repo topic)
                           (closql-with-transaction (forge-db)
                             (closql-insert (forge-db) obj t))
                           (funcall cb cb (cdr data) (or n (length data))))))
                    (forge--msg nil t t "Pulled %s notifications" (or n 0))
                    (ignore-errors
                      (forge--gtea-put nil "notifications")) ; mark as read
                    (when callback
                      (funcall callback))))))
        (funcall cb cb (forge--gtea-get nil "notifications"))))))

;;; Mutations:

(cl-defmethod forge--submit-create-pullreq ((_ forge-gitea-repository) repo)
  (let-alist (forge--topic-parse-buffer)
    (when (and .yaml (local-variable-p 'forge-buffer-draft-p))
      (user-error "Cannot use yaml frontmatter and set `%s' at the same time"
                  'forge-buffer-draft-p))
    (pcase-let* ((`(,base-remote . ,base-branch)
                  (magit-split-branch-name forge--buffer-base-branch))
                 (`(,head-remote . ,head-branch)
                  (magit-split-branch-name forge--buffer-head-branch))
                 (head-repo (forge-get-repository :stub head-remote))
                 (url-mime-accept-string
                  ;; Support draft pull-requests.
                  "application/vnd.github.shadow-cat-preview+json"))
      (forge--gtea-post repo "repos/:owner/:repo/pulls"
        `((title . ,.title)
          (body . ,.body)
          (base . ,base-branch)
          (head  . ,(if (equal head-remote base-remote)
                        head-branch
                      (concat (oref head-repo owner) ":"
                              head-branch))))
        :callback  (forge--post-submit-gitea-callback)
        :errorback (forge--post-submit-errorback)))))

(cl-defmethod forge--submit-create-issue ((_ forge-gitea-repository) repo)
  (let-alist (forge--topic-parse-buffer)
    (forge--gtea-post repo "repos/:owner/:repo/issues"
      `((title       . , .title)
        (description . , .body))
      :callback  (forge--post-submit-gitea-callback)
      :errorback (forge--post-submit-errorback))))

(cl-defmethod forge--submit-create-post ((_ forge-gitea-repository) topic)
  (forge--gtea-post topic "repos/:owner/:repo/issues/:number/comments"
    `((body . ,(string-trim (buffer-string))))
    :callback  (forge--post-submit-gitea-callback)
    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--submit-edit-post ((_ forge-gitea-repository) post)
  (forge--gtea-patch
    nil
    (cl-typecase post
      (forge-pullreq (forge--format-resource post "repos/:owner/:repo/pulls/:number"))
      (forge-issue   (forge--format-resource post "repos/:owner/:repo/issues/:number"))
      (forge-post    (forge--format-resource post "repos/:owner/:repo/issues/comments/:number")))
    (if (cl-typep post 'forge-topic)
        (let-alist (forge--topic-parse-buffer)
          `((title . , .title)
            (body  . , .body)))
      `((body . ,(string-trim (buffer-string)))))
    :callback  (forge--post-submit-gitea-callback)
    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--delete-comment ((_repo forge-gitea-repository) post)
  (forge--gtea-delete post "repos/:owner/:repo/issues/comments/:number")
  (closql-delete post)
  (magit-refresh))

(cl-defmethod forge--set-topic-labels ((repo forge-gitea-repository) topic labels)
  (let ((cb (forge--set-field-callback topic)))
    (forge--fetch-labels
     repo (lambda (_cb data)
            (let ((ids (mapcan (lambda (label)
                                 (let-alist label
                                   (when (member .name labels)
                                     (list .id))))
                               (cdr data))))
              (forge--gtea-put topic "repos/:owner/:repo/issues/:number/labels"
                `((labels . ,(or ids [])))
                :callback cb))))))

(cl-defmethod forge--set-topic-field
  ((_repo forge-gitea-repository) topic field value)
  (forge--gtea-patch topic
    (cl-typecase topic
      (forge-pullreq "repos/:owner/:repo/pulls/:number")
      (forge-issue   "repos/:owner/:repo/issues/:number"))
    `((,field . ,value))
    :callback (forge--set-field-callback topic)))

(cl-defmethod forge--set-topic-milestone ((repo forge-gitea-repository) topic milestone)
  (forge--set-topic-field repo topic 'milestone (or (caar (forge-sql [:select [number]
                                                                              :from milestone
                                                                              :where (and (= repository $s1)
                                                                                          (= title $s2))]
                                                                     (oref repo id)
                                                                     milestone))
                                                    -1)))

(cl-defmethod forge--set-topic-title ((repo forge-gitea-repository) topic title)
  (forge--set-topic-field repo topic 'title title))

(cl-defmethod forge--set-topic-state ((repo forge-gitea-repository) topic value)
  (forge--set-topic-field repo topic 'state (cl-ecase value
                                              (rejected "closed")
                                              (completed "closed")
                                              (open   "open"))))

(cl-defmethod forge--set-topic-review-requests ((_repo forge-gitea-repository) topic reviewers)
  (let ((value (mapcar #'car (closql--iref topic 'review-requests))))
    (when-let ((add (cl-set-difference reviewers value :test #'equal)))
      (forge--gtea-post topic "repos/:owner/:repo/pulls/:number/requested_reviewers"
        `((reviewers . ,add))))
    (when-let ((remove (cl-set-difference value reviewers :test #'equal)))
      (forge--gtea-delete topic "repos/:owner/:repo/pulls/:number/requested_reviewers"
        `((reviewers . ,remove)))))
  (forge-pull))

(cl-defmethod forge--set-topic-assignees ((repo forge-gitea-repository) topic assignees)
  (forge--set-topic-field repo topic 'assignees (or assignees [])))

(defun forge--gitea-map-logins (logins)
  (mapcan (lambda (user)
            (let ((id (cl-first user))
                  (login (cl-second user)))
              (when (cl-member login logins :test 'string=)
                (list id))))
          (oref (forge-get-repository nil) assignees)))
;;;; Drafts:
;; Drafts are implemented as WIP word in title.

(defun gitea-forge-is-draft? (title)
  "Topic is draft if TITLE first word include \"WIP\"."
  (string-match-p "WIP" (car (string-split title))))

(cl-defmethod forge--set-topic-draft
  ((repo forge-gitea-repository) topic draft-p)
  (let ((title (oref topic title)))
    (cond
     ((eq draft-p (gitea-forge-is-draft? title)) 'pass)
     (draft-p (forge--set-topic-title
               repo topic
               (concat "[WIP] " title)))
     (t (forge--set-topic-title
         repo topic
         (string-trim
          (string-join (cdr (seq-drop-while #'string-empty-p (string-split title " "))) " ")))))))

(cl-defmethod forge--update-status ((repo forge-gitea-repository)
                                    topic data bump initial-pull)
  (let-alist data
    (let ((updated (or .updated_at .created_at))
          (current-status (oref topic status)))
      (cond (initial-pull
             (oset topic status 'done))
            ((null current-status)
             (oset topic status 'unread))
            ((string> updated (oref topic updated))
             (oset topic status 'pending)))
      (oset topic updated updated)
      (when bump
        (let* ((slot (if (forge-issue-p topic) 'issues-until 'pullreqs-until))
               (until (eieio-oref repo slot)))
          (when (or (not until) (string> updated until))
            (eieio-oset repo slot updated)))))))

;;;; Templates:

(cl-defmethod forge--topic-template-files ((repo forge-gitea-repository)
                                           (_ (subclass forge-issue)))
  (and-let* ((files (magit-revision-files (oref repo default-branch))))
    (let ((case-fold-search t))
      (if-let ((file (--first (string-match-p "\
\\`\\(\\|docs/\\|\\.github/\\)issue_template\\(\\.[a-zA-Z0-9]+\\)?\\'" it)
                              files)))
          (list file)
        (setq files
              (--filter (string-match-p "\\`\\.github/ISSUE_TEMPLATE/[^/]*" it)
                        files))
        (if-let ((conf (cl-find-if
                        (lambda (f)
                          (equal (file-name-nondirectory f) "config.yml"))
                        files)))
            (nconc (delete conf files)
                   (list conf))
          files)))))

(cl-defmethod forge--topic-template-files ((repo forge-gitea-repository)
                                           (_ (subclass forge-pullreq)))
  (and-let* ((files (magit-revision-files (oref repo default-branch))))
    (let ((case-fold-search t))
      (if-let ((file (--first (string-match-p "\
\\`\\(\\|docs/\\|\\.github/\\)pull_request_template\\(\\.[a-zA-Z0-9]+\\)?\\'" it)
                              files)))
          (list file)
        ;; Unlike for issues, the web interface does not support
        ;; multiple pull-request templates.  The API does though,
        ;; but due to this limitation I doubt many people use them,
        ;; so Forge doesn't support them either.
        ))))

;;;; Merge

(defun forge--pullreq-commit-msg (hash)
  (let* ((msg (magit-git-output "log" "--format=%B" "-n" "1" hash))
         (lines (string-lines msg))
         (txt (string-trim (string-join (cddr lines) "\n"))))
    txt))

(cl-defmethod forge--merge-pullreq ((_repo forge-gitea-repository)
                                    topic hash method)
  (forge--gtea-post topic "repos/:owner/:repo/pulls/:number/merge"
    `((delete_branch_after_merge . t)
      (MergeMessageField . ,msg)
      (do . ,(symbol-name method))
      ,@(and hash `((head_commit_id . ,hash))))))

;;;; Approve

(defun forge-gitea-approve (pullreq)
  "Approve pullrequest.  Currently done only for gitea hosting."
  (interactive
   (list (forge-read-pullreq "Approve pull-request" t)))
  (let* ((pullreq (forge-get-pullreq pullreq))
         (hash (magit-rev-hash
                (concat "origin/"
                        (forge--pullreq-branch-internal pullreq))))
         (buf (forge--prepare-post-buffer
               "merge-message")))
    (unless hash
      (error "Branch is missing"))
    (when buf
      (with-current-buffer buf
        (erase-buffer)
        (insert (format "# %s" (oref pullreq title)))
        (setq forge--buffer-post-object pullreq)
        (setq forge--submit-post-function
              (lambda (repo topic)
                (let-alist (forge--topic-parse-buffer)
                  (print  `((body . ,.body)
                            ;; (comments . ,nil)
                            (event . "APPROVED")
                            (commit_id . ,hash)))
                  (forge--gtea-post pullreq "repos/:owner/:repo/pulls/:number/reviews"
                    `((body . ,.body)
                      ;; (comments . ,nil)
                      (event . "APPROVED")
                      (commit_id . ,hash))
                    :callback (forge--post-submit-gitea-callback)
                    :errorback (forge--post-submit-errorback))))))
      (forge--display-post-buffer buf))))

;;; Wrappers

(cl-defun forge--gtea-get* (obj resource
                                &optional params
                                &key callback while)
  "Unpaginate list of resources until WHILE condition or end of list
is met."
  (declare (indent defun))
  (forge--msg nil nil nil "Fetch page %s" 1)
  (let* ((result '())
         (url (if obj (forge--format-resource obj resource) resource))
         (cb (lambda (cb)
               (forge--gtea-get nil url params
                 :callback
                 (lambda (value headers status req)
                   (while (and value
                               (or (null while)
                                   (funcall while (car value))))
                     (setq result (cons (car value) result)
                           value (cdr value)))
                   (if-let* ((no-next-page (not value))
                             ;; Get next page:
                             (rels (cdr (assoc "Link" headers)))
                             (ok (string-match "<\\([^>]+\\)>; rel=\"next\"" rels))
                             (next-page (match-string 1 rels))
                             (ok (string-match "&page=\\([0-9]+\\)" next-page))
                             (page (match-string 1 next-page)))
                       (progn
                         ;; Set next page to params:
                         (setq params
	                       (if-let ((pair (assoc 'page params)))
	                           (progn (setcdr pair page) params)
	                         (cons `(page . ,page) params)))
                         (forge--msg nil nil nil "Fetch page %s" page)
                         (funcall cb cb))
                     (forge--msg nil nil t "Fetch pages")
                     (funcall callback result headers status req)))))))
    (funcall cb cb)))

(cl-defun forge--gtea-get (obj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               host
                               callback errorback)
  (declare (indent defun))
  (gtea-get (if obj (forge--format-resource obj resource) resource)
            params
            :host (or host (oref (forge-get-repository obj) apihost))
            :auth 'forge
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :callback callback :errorback errorback))

(cl-defun forge--gtea-put (obj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               host
                               callback errorback)
  (declare (indent defun))
  (gtea-put (if obj (forge--format-resource obj resource) resource)
            params
            :host (or host (oref (forge-get-repository obj) apihost))
            :auth 'forge
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :callback callback :errorback errorback))

(cl-defun forge--gtea-post (obj resource
                                &optional params
                                &key query payload headers
                                silent unpaginate noerror reader
                                host callback errorback)
  (declare (indent defun))
  (gtea-post (forge--format-resource obj resource)
             params
             :host (or host (oref (forge-get-repository obj) apihost))
             :auth 'forge
             :query query :payload payload :headers headers
             :silent silent :unpaginate unpaginate
             :noerror noerror :reader reader
             :callback callback :errorback errorback))

(cl-defun forge--gtea-patch (obj resource
                                 &optional params
                                 &key query payload headers
                                 silent unpaginate noerror reader
                                 host callback errorback)
  (declare (indent defun))
  (gtea-patch (if obj (forge--format-resource obj resource) resource)
              params
              :host (or host (oref (forge-get-repository obj) apihost))
              :auth 'forge
              :query query :payload payload :headers headers
              :silent silent :unpaginate unpaginate
              :noerror noerror :reader reader
              :callback callback :errorback errorback))

(cl-defun forge--gtea-delete (obj resource
                                  &optional params
                                  &key query payload headers
                                  silent unpaginate noerror reader
                                  host callback errorback)
  (declare (indent defun))
  (gtea-delete (forge--format-resource obj resource)
               params
               :host (or host (oref (forge-get-repository obj) apihost))
               :auth 'forge
               :query query :payload payload :headers headers
               :silent silent :unpaginate unpaginate
               :noerror noerror :reader reader
               :callback callback :errorback errorback))

;;; Forks:
(cl-defmethod forge--fork-repository ((repo forge-gitea-repository) fork)
  (with-slots (owner name) repo
    (forge--gtea-post repo
      (format "/repos/%s/%s/forks" owner name)
      (and (not (equal fork (ghub--username (ghub--host nil))))
           `((organization . ,fork))))))

;;; Forge hacks:

(add-function :before-until (symbol-function 'forge-pull-notifications)
              (defun forge--pull-notifications--gitea ()
                (if-let ((repo (forge-get-repository :stub?)))
                    (let ((class (eieio-object-class repo)))
                      (when (eq class 'forge-gitea-repository)
                        (forge--pull-notifications class (oref repo githost))
                        t)))))

(defun forge--post-submit-gitea-callback ()
  (let* ((file    buffer-file-name)
         (editbuf (current-buffer))
         (prevbuf forge--pre-post-buffer)
         (topic   (ignore-errors (forge-get-topic forge--buffer-post-object)))
         (repo    (forge-get-repository topic)))
    (lambda (value headers status req)
      (run-hook-with-args 'forge-post-submit-callback-hook
                          value headers status req)
      (delete-file file t)
      (let ((dir (file-name-directory file)))
        (unless (cddr (directory-files dir nil nil t))
          (delete-directory dir nil t)))
      (when (buffer-live-p editbuf)
        (with-current-buffer editbuf
          (magit-mode-bury-buffer 'kill)))
      (with-current-buffer
          (if (buffer-live-p prevbuf) prevbuf (current-buffer))
        (if (and topic
                 (or (forge--childp repo 'forge-github-repository)
                     (forge--childp repo 'forge-gitea-repository))
                 (or (and (fboundp 'forge-pullreq-p)
                          (forge-pullreq-p topic))
                     (oref repo selective-p)))
            (forge--pull-topic repo topic)
          (forge-pull))))))

(defun forge-topic-toggle-draft/gitea-fix ()
  "Trigger `forge--set-topic-draft' event, when change draft state."
  (when-let ((pullreq (forge-current-pullreq)))
    (forge--set-topic-draft (forge-get-repository :tracked?) pullreq (oref pullreq draft-p))))
(add-function :after (symbol-function 'forge-topic-toggle-draft) 'forge-topic-toggle-draft/gitea-fix)

(provide 'gitea-forge)

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:

;;; gitea-forge.el ends here
