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
;;; - posting comments (incl. edit/delete)
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
(require 'tree-mode)


;;;; Variables

(defvar reddit-root "http://www.beta.reddit.com")
(defvar reddit-api-root "http://reddit.com/api")
(defvar reddit-site "programming")

(defvar reddit-entry-format "%N. %[%T%] (%D, %C comments)\n")

(defvar reddit-user nil)
(defvar reddit-password nil)

(defvar reddit-entry-id nil)


;;;; Utilities

(defmacro reddit-alet (vars alist &rest forms)
  (let ((alist-var (make-symbol "alist")))
    `(let* ((,alist-var ,alist)
            ,@(loop for var in vars
                    collecting `(,var (assoc-default ',var ,alist-var))))
       ,@forms)))

(put 'reddit-alet 'lisp-indent-function 2)

(defun reddit-kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

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
                    ("op" . "login_main")
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
        (assert (equal "Listing" kind))
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
    (assert (equal "t3" kind))
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

(define-key reddit-comments-mode-map "q" 'reddit-kill-current-buffer)
(define-key reddit-comments-mode-map "g" 'reddit-comments-refresh)

(defun reddit-comments-new-buffer (id)
  (with-current-buffer (get-buffer-create (format "*Reddit Comments %s*" id))
    (reddit-comments-mode)
    (switch-to-buffer (current-buffer))
    (set (make-local-variable 'reddit-entry-id) id)
    (reddit-comments-refresh)))

(defun reddit-comments-refresh ()
  (interactive)
  (url-retrieve (concat (reddit-site-root) "/info/" reddit-entry-id "/comments/.json")
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
      (cond ((equal "Listing" kind)
             (reddit-comments-trees (assoc-default 'children (assoc-default 'data data) nil [])))
            ((equal "t1" kind)
             (reddit-alet (ups replies likes id author downs created name body)
                 (assoc-default 'data data)
               `((reddit-comment-widget
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


;;;; Finally...

(provide 'reddit)
