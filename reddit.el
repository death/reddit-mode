;;;; Reddit mode
;;;
;;; DEATH, 2008
;;;               _     _ _ _                            _
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
;;; - subreddits
;;; - search
;;; - customization
;;; - documentation
;;; - menus
;;; - error checking
;;; - much more...
;;;

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
(defvar reddit-site "programming")

(defvar reddit-entry-format "%N. %[%T%] (%D, %C comments)\n")

(defvar reddit-user nil)
(defvar reddit-password nil)

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
         (url-request-data
          (reddit-format-request-data
           (append data
                   `(("r" . ,(or reddit-site "%20reddit.com"))
                     ("_" . "")))))
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
                  `(("uh" . "")
                    ("op" . "login-main")
                    ("user_login" . ,user)
                    ("passwd_login" . ,password)))
    (url-mark-buffer-as-dead (current-buffer))
    (let* ((data (reddit-parse))
           (error (assoc-default 'error data)))
      (if error
          (error "Problem with login: %s"
                 (assoc-default 'message error nil "<no message>"))
        (message "Login successful")))))

(defun reddit-site-root ()
  (if reddit-site
      (concat reddit-root "/r/" reddit-site)
    reddit-root))

(defun reddit-comments-site-root (entry-id)
  (concat (reddit-site-root) "/info/" entry-id "/comments"))

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
  (cond ((get-buffer "*Reddit*")
         (switch-to-buffer "*Reddit*"))
        (t
         (switch-to-buffer "*Reddit*")
         (reddit-mode)
         (when reddit-user
           (reddit-login reddit-user reddit-password))
         (reddit-refresh))))

(defvar reddit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "q" 'bury-buffer)
    (define-key map "g" 'reddit-refresh)
    (define-key map "c" 'reddit-comments)
    (define-key map "L" 'reddit-login)
    map))

(define-derived-mode reddit-mode nil "Reddit"
  "Major mode for using Reddit."
  (widen)
  (setq buffer-read-only t)
  (auto-save-mode 0))

(defun reddit-refresh ()
  (interactive)
  (url-retrieve (concat (reddit-site-root) "/.json")
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
        (let ((children (assoc-default 'children (car data))))
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
                      num_comments url author name clicked domain)
        (assoc-default 'data data)
      (list 'reddit-entry
            :format reddit-entry-format
            :value url
            :help-echo url
            :tab-order n
            :reddit-title title
            :reddit-entry-id id
            :reddit-n n
            :reddit-domain domain
            :reddit-score score
            :reddit-author author
            :reddit-num-comments num_comments))))

(defun reddit-entry-format (widget char)
  (case char
    (?N (insert (format "%2d" (1+ (widget-get widget :reddit-n)))))
    (?D (insert (widget-get widget :reddit-domain)))
    (?T (insert (truncate-string-to-width (widget-get widget :reddit-title) 80 nil nil t)))
    (?S (insert (format "%d" (widget-get widget :reddit-score))))
    (?A (insert (widget-get widget :reddit-author)))
    (?C (insert (format "%d" (widget-get widget :reddit-num-comments))))
    (t (widget-default-format-handler widget char))))


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
        (let* ((data (assoc-default 'data (aref data 1)))
               (children (assoc-default 'children data))
               (trees (reddit-comments-trees children)))
          (dolist (tree trees)
            (tree-mode-expand-level-1 (tree-mode-insert tree) -1))
          (goto-char (point-min))
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
                  :node (push-button :tag ,author :format "%t\n")
                  ,@(reddit-comments-body-widgets body)
                  ,@(when replies
                      (reddit-comments-trees replies))))))
            (t (error "reddit-comments-trees: unknown kind: %s" kind))))))

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
  (let ((widget (tree-mode-parent-current-line)))
    (cond ((null widget) nil)
          ((eq 'reddit-comment-line-widget (widget-type widget))
           (widget-get widget :parent))
          (t widget))))

(defun reddit-comments-post ()
  (interactive)
  (reddit-post-new-buffer `(entry ,reddit-entry-id) reddit-entry-id))

(defun reddit-comments-followup ()
  (interactive)
  (let ((comment (reddit-comments-current-comment)))
    (if (null comment)
        (error "No comment for followup")
      (reddit-post-new-buffer
       `(comment ,(widget-get comment :reddit-comment-id))
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
  (destructuring-bind (type parent-id) reddit-parent-id
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
      (message "Posted"))))


;;;; Finally...

(provide 'reddit)
