;;; reddit.el --- Support for the Reddit time sink

;; Copyright (C) 2008 DEATH
;; Copyright (C) 2015 massix

;; Authors: DEATH, massix
;; Keywords: games
;; Website: http://github.com/death http://github.com/massix

;; This file is not part of GNU Emacs.

;;; Commentary:

;;;  _ __ ___  __| | __| (_) |_      _ __ ___   ___   __| | ___
;;; | '__/ _ \/ _` |/ _` | | __|____| '_ ` _ \ / _ \ / _` |/ _ \
;;; | | |  __/ (_| | (_| | | ||_____| | | | | | (_) | (_| |  __/
;;; |_|  \___|\__,_|\__,_|_|\__|    |_| |_| |_|\___/ \__,_|\___|
;;;
;;; A lot of stuff is missing:
;;;
;;; - logout
;;; - markdown
;;; - comments: edit/delete
;;; - submitting urls (incl. delete)
;;; - voting urls/comments
;;; - saving links
;;; - other pages (e.g. what's new/browse/saved/recommended/stats/blog)
;;; - user pages
;;; - user preferences
;;; - customization
;;; - documentation
;;; - menus
;;; - error checking
;;; - handle <more comments>
;;; - much more...
;;;

;;; Code:

(require 'cl)
(require 'thingatpt)
(require 'json)
(require 'url)
(require 'url-http)
(require 'tree-mode)
(require 'markdown-mode)


;;;; Variables

(defvar reddit-root "http://www.reddit.com")
(defvar reddit-api-root "http://www.reddit.com/api")
(defvar reddit-site '(main))

(defvar reddit-entry-format "%N. %[%T%] (%D, %C comments, %U upvotes)\n")

(defvar reddit-user nil)
(defvar reddit-password nil)

(defvar reddit-threads-limit "30")
(defvar reddit-entry-id nil)
(defvar reddit-parent-id nil)

(defvar reddit-kind-listing "Listing")
(defvar reddit-kind-comment "t1")
(defvar reddit-kind-entry "t3")


;;;; Utilities

(defmacro reddit-alet (vars alist &rest forms)
  (let ((alist-var (make-symbol "alist")))
    `(let* ((,alist-var ,alist)
            ,@(loop for var in vars
                    collecting `(,var (assoc-default ',var ,alist-var))))
       ,@forms)))

(put 'reddit-alet 'lisp-indent-function 2)

(defun reddit-kill ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (kill-buffer buffer)))

(defun reddit-chomp (s)
  (if (string-match "\\(.*\\)\n" s)
      (match-string 1 s)
    s))

(defun reddit-check-status (status)
  (when (eq (car status) :error)
    (destructuring-bind (sym . data) (cadr status)
      (signal sym data))))

(defun reddit-format-request-data (alist)
  (with-temp-buffer
    (loop for delim = "" then "&"
          for (key . value) in alist
          do (insert delim key "=" value))
    (buffer-string)))


;;;; Reddit-specific utilities

(defun reddit-parse ()
  (goto-char (point-min))
  (re-search-forward "^$")
  (json-read))

(defun reddit-api (op data)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data
          (reddit-format-request-data
           (append data
                   `(("_" . "")))))
         (buffer (url-retrieve-synchronously (concat reddit-api-root "/" op))))
    (when (null buffer)
      (error "Reddit API call for '%s' failed" op))
    (with-current-buffer buffer
      (save-excursion
        (let ((code (url-http-parse-response)))
          (case code
            (200)
            (t
             (url-mark-buffer-as-dead buffer)
             (error "Reddit API call for '%s' failed with code %d" op code))))))
    buffer))

(defun reddit-login (&optional user password)
  (interactive)
  (when (null user)
    (setq user (read-string "User: " nil nil reddit-user)))
  (when (null password)
    (setq password (read-passwd "Password: " nil reddit-password)))
  (with-current-buffer
      (reddit-api "login"
                  `(("op" . "login-main")
                    ("user" . ,user)
                    ("passwd" . ,password)))
    (url-mark-buffer-as-dead (current-buffer))
    (let* ((data (reddit-parse))
           (error (assoc-default 'error data)))
      (if error
          (error "Problem with login: %s"
                 (assoc-default 'message error nil "<no message>"))
        (message "Login successful")))))

(defun reddit-site-json (&optional after-param before-param)
  (if (and after-param before-param)
      (error "Only one param should be provided")
    (let ((reddit-base (concat reddit-root "/.json?limit=" reddit-threads-limit))
          (reddit-subreddit-base (concat reddit-root "/r/" (second reddit-site) "/.json?limit=" reddit-threads-limit)))
      (ecase (first reddit-site)
        (main
         (cond (after-param (concat reddit-base "&after=" after-param))
               (before-param (concat reddit-base "&before=" before-param))
               (t reddit-base)))
        (subreddit
         (cond (after-param (concat reddit-subreddit-base "&after=" after-param))
               (before-param (concat reddit-subreddit-base "&before=" before-param))
               (t reddit-subreddit-base)))
        (search (destructuring-bind (query &optional subreddit)
                    (rest reddit-site)
                  (setq query (url-hexify-string query))
                  (if subreddit
                      (concat reddit-root "/r/" subreddit "/search.json?q=" query)
                    (concat reddit-root "/search.json?q=" query))))))))

(defun reddit-comments-site-root (entry-id)
  (concat reddit-root "/info/" entry-id "/comments"))

(defun reddit-modhash (entry-id)
  ;; Ugly, ugly hack
  (let ((modhash (with-current-buffer
                     (url-retrieve-synchronously (reddit-comments-site-root entry-id))
                   (url-mark-buffer-as-dead (current-buffer))
                   (goto-char (point-min))
                   (re-search-forward "modhash = '\\([^']*\\)")
                   (match-string 1))))
    (if (equal modhash "")
        (error "Can't find modhash; not logged in?")
      modhash)))


;;;; Reddit mode

(defun reddit ()
  "Switch to Reddit buffer, creating it if necessary."
  (interactive)
  (when (and reddit-user
             (not (get-buffer (reddit-buffer-name reddit-site))))
    (reddit-login reddit-user reddit-password))
  (reddit-new-buffer reddit-site))

(defvar reddit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "q" 'quit-window)
    (define-key map "g" 'reddit-refresh)
    (define-key map "c" 'reddit-comments)
    (define-key map "L" 'reddit-login)
    (define-key map "S" 'reddit-search)
    (define-key map "n" 'reddit-next)
    (define-key map "p" 'reddit-prev)
    map))

(define-derived-mode reddit-mode nil "Reddit"
  "Major mode for using Reddit."
  (widen)
  (setq buffer-read-only t)
  (auto-save-mode 0))

(defun reddit-new-buffer (site)
  (with-current-buffer (get-buffer-create (reddit-buffer-name site))
    (reddit-mode)
    (switch-to-buffer (current-buffer))
    (set (make-local-variable 'reddit-site) site)
    (reddit-refresh)))

(defun reddit-buffer-name (site)
  (format "*Reddit %S*" site))

(defun reddit-search (&optional query subreddit)
  (interactive "MSearch:
MSubreddit: ")
  (when (string= query "") (setq query nil))
  (when (string= subreddit "") (setq subreddit nil))
  (reddit-new-buffer
   (cond ((and query subreddit) (list 'search query subreddit))
         (query (list 'search query))
         (subreddit (list 'subreddit subreddit))
         (t '(main)))))

(defun reddit-refresh ()
  (interactive)
  (url-retrieve (reddit-site-json)
                'reddit-refresh-cb
                (list (current-buffer))))

(defun reddit-refresh-cb (status buffer)
  (url-mark-buffer-as-dead (current-buffer))
  (reddit-check-status status)
  (reddit-render (reddit-parse) buffer))

(defun reddit-render (data buffer)
  (let ((inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (let ((kind (assoc-default 'kind data)))
        (assert (equal reddit-kind-listing kind))
        (let ((children (assoc-default 'children (assoc-default 'data data))))
          (loop for n from 0
                for child across children
                do (widget-create (reddit-make-entry child n)))))
      (widget-setup)
      (goto-char (point-min))))
  (message "Got entries"))

(define-widget 'reddit-entry 'url-link
  "A widget representing a Reddit entry."
  :format-handler 'reddit-entry-format)

(defun reddit-make-entry (data n)
  (let ((kind (assoc-default 'kind data)))
    (assert (equal reddit-kind-entry kind))
    (reddit-alet (ups saved hidden id likes score created title downs
                      num_comments url author name clicked domain
                      subreddit)
        (assoc-default 'data data)
      (list 'reddit-entry
            :format reddit-entry-format
            :value url
            :help-echo url
            :tab-order n
            :reddit-upvotes ups
            :reddit-title title
            :reddit-entry-id id
            :reddit-n n
            :reddit-domain domain
            :reddit-score score
            :reddit-author author
            :reddit-num-comments num_comments
            :reddit-subreddit subreddit))))

(defun reddit-entry-format (widget char)
  (case char
    (?N (insert (format "%3d" (1+ (widget-get widget :reddit-n)))))
    (?D (insert (widget-get widget :reddit-domain)))
    (?T (insert (truncate-string-to-width (widget-get widget :reddit-title) 80 nil nil t)))
    (?S (insert (format "%d" (widget-get widget :reddit-score))))
    (?A (insert (widget-get widget :reddit-author)))
    (?C (insert (format "%d" (widget-get widget :reddit-num-comments))))
    (?R (insert (widget-get widget :reddit-subreddit)))
    (?U (insert (format "%d" (widget-get widget :reddit-upvotes))))
    (t (widget-default-format-handler widget char))))

(defun reddit-next ()
  "Get next page of threads"
  (interactive)
  (let ((widget (widget-at)))
    (when widget
      (url-retrieve (reddit-site-json (concat reddit-kind-entry "_" (widget-get widget :reddit-entry-id)) nil)
                    'reddit-refresh-cb
                    (list (current-buffer))))))

(defun reddit-prev ()
  "Get previous page of threads"
  (interactive)
  (let ((widget (widget-at)))
    (when widget
      (url-retrieve (reddit-site-json nil (concat reddit-kind-entry "_" (widget-get widget :reddit-entry-id)))
                    'reddit-refresh-cb
                    (list (current-buffer))))))

;;;; Reddit Comments mode

(defun reddit-comments ()
  (interactive)
  (let ((widget (widget-at)))
    (when widget
      (reddit-comments-new-buffer (widget-get widget :reddit-entry-id)))))

(define-derived-mode reddit-comments-mode tree-mode "Reddit Comments"
  (widen)
  (setq buffer-read-only t)
  (auto-save-mode 0))

(define-key reddit-comments-mode-map "q" 'reddit-kill)
(define-key reddit-comments-mode-map "g" 'reddit-comments-refresh)
(define-key reddit-comments-mode-map "a" 'reddit-comments-post)
(define-key reddit-comments-mode-map "F" 'reddit-comments-followup)

(defun reddit-comments-new-buffer (id)
  (with-current-buffer (get-buffer-create (format "*Reddit Comments %s*" id))
    (reddit-comments-mode)
    (switch-to-buffer (current-buffer))
    (set (make-local-variable 'reddit-entry-id) id)
    (reddit-comments-refresh)))

(defun reddit-comments-refresh ()
  (interactive)
  (url-retrieve (concat (reddit-comments-site-root reddit-entry-id) "/.json")
                'reddit-comments-refresh-cb
                (list (current-buffer))))

(defun reddit-comments-refresh-cb (status buffer)
  (url-mark-buffer-as-dead (current-buffer))
  (reddit-check-status status)
  (reddit-comments-render (reddit-parse) buffer))

(defun reddit-comments-render (data buffer)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; The first element of data contains the reddit entry.  The
      ;; second element of data contains all the comments.
      (if (or (not (arrayp data))
              (< (length data) 2))
          (error "Weird data: %s" data)
        (let* ((entry (assoc-default 'data (aref data 0)))
               (data (assoc-default 'data (aref data 1)))
               (children (assoc-default 'children data))
               (trees (reddit-comments-trees children)))
          (dolist (tree trees)
            (tree-mode-expand-level-1 (tree-mode-insert tree) -1))
          (goto-char (point-min))
          (let ((text (assoc-default 'selftext (assoc-default 'data (aref (assoc-default 'children entry) 0)))))
            (insert text "\n\n")
            (fill-region (point-min) (point)))
          (message "Got comments"))))))

(define-widget 'reddit-comment-widget 'tree-widget
  "A widget representing a Reddit comment.")

(define-widget 'reddit-comment-line-widget 'text
  "A widget representing a Reddit comment's line.")

(defun reddit-comments-trees (data)
  (if (arrayp data)
      (loop for x across data
            appending (reddit-comments-trees x))
    (let ((kind (assoc-default 'kind data)))
      (cond ((equal reddit-kind-listing kind)
             (reddit-comments-trees (assoc-default 'children (assoc-default 'data data) nil [])))
            ((equal reddit-kind-comment kind)
             (reddit-alet (ups replies likes id author downs created name body)
                 (assoc-default 'data data)
               `((reddit-comment-widget
                  :reddit-comment-id ,id
                  :reddit-author ,author
                  :reddit-upvotes ,ups
                  :node (push-button :tag ,author
                                     :format ,(format "%%[%%t%%] (%d point(s))\n" ups))
                  ,@(reddit-comments-body-widgets body)
                  ,@(when replies
                      (reddit-comments-trees replies))))))
            (t (warn "reddit-comments-trees: unknown kind: %s" kind)
               nil)))))

(defun reddit-comments-body-widgets (body)
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match "\n" nil t))
    (fill-region (point-min) (point-max))
    (goto-char (point-min))
    (let ((widgets '())
          (blank nil))
      (while (not (eobp))
        (let ((line (reddit-chomp (thing-at-point 'line))))
          (cond ((equal "" line)
                 (setq blank t))
                (t
                 (when blank
                   (push `(reddit-comment-line-widget "") widgets)
                   (setq blank nil))
                 (push `(reddit-comment-line-widget ,line) widgets))))
        (forward-line))
      (nreverse widgets))))

(defun reddit-comments-current-comment ()
  (labels ((lookup (widget)
             (cond ((null widget)
                    nil)
                   ((not (eq 'reddit-comment-widget (widget-type widget)))
                    (lookup (widget-get widget :parent)))
                   (t
                    widget))))
    (lookup (tree-mode-icon-current-line))))

(defun reddit-comments-post ()
  (interactive)
  (reddit-post-new-buffer `(entry ,reddit-entry-id) reddit-entry-id))

(defun reddit-comments-followup ()
  (interactive)
  (let ((comment (reddit-comments-current-comment)))
    (if (null comment)
        (error "No comment for followup")
      (reddit-post-new-buffer
       `(comment ,(widget-get comment :reddit-comment-id)
                 ,(widget-get comment :reddit-author))
       reddit-entry-id))))


;;;; Reddit Post mode

(defun reddit-post-new-buffer (parent-id entry-id)
  (with-current-buffer (get-buffer-create (format "*Reddit Post %s*" parent-id))
    (reddit-post-mode)
    (set (make-local-variable 'reddit-parent-id) parent-id)
    (set (make-local-variable 'reddit-entry-id) entry-id)
    (set-window-buffer (split-window) (current-buffer))
    (select-window (next-window))))

(define-derived-mode reddit-post-mode markdown-mode "Reddit-Post"
  (widen)
  (auto-save-mode 0))

(define-key reddit-post-mode-map (kbd "C-c C-q") 'reddit-kill)
(define-key reddit-post-mode-map (kbd "C-c C-c") 'reddit-post-save)

(defun reddit-post-save ()
  (interactive)
  (destructuring-bind (type parent-id author) reddit-parent-id
    (let ((modhash (reddit-modhash entry-id))
          (id-string (concat (ecase type
                               (comment reddit-kind-comment)
                               (entry reddit-kind-entry))
                             "_"
                             parent-id))
          (comment (url-hexify-string (buffer-string))))
      (url-mark-buffer-as-dead
       (reddit-api "comment"
                   `(("uh" . ,modhash)
                     ("id" . ,id-string)
                     ("comment" . ,comment)
                     ,@(when (eq type 'entry)
                         `(("isroot" . "1"))))))
      (reddit-kill)
      (message "Posted followup to comment by %s" author))))


;;;; Finally...

(provide 'reddit)

;;; reddit.el ends here
