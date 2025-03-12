(require 'forge)
(require 'llm)

;;;###autoload
(defun forge-llm-hello ()
  "Display pull request branch information and show git diff in a Forge PR buffer."
  (interactive)
  (if (not (derived-mode-p 'forge-post-mode))
      (message "Not in a Forge pull request buffer")
    (let ((head (and (boundp 'forge--buffer-head-branch) forge--buffer-head-branch))
          (base (and (boundp 'forge--buffer-base-branch) forge--buffer-base-branch)))
      (if (and head base)
          (let* ((default-directory (file-name-directory
                                     (directory-file-name
                                      (file-name-directory
                                       (or buffer-file-name default-directory)))))
                 (pr-desc (format "Pull Request: %s → %s" head base))
                 (repo-root (locate-dominating-file default-directory ".git")))
            ;; First show the PR branches in the message area
            (message "%s" pr-desc)

            ;; Now create a buffer with the git diff
            (when repo-root
              (let ((diff-command (format "git diff %s..%s" base head))
                    (buffer (get-buffer-create "*forge-llm-diff*")))
                (with-current-buffer buffer
                  (setq buffer-read-only nil)
                  (erase-buffer)
                  (insert (format "Diff for %s\n\n" pr-desc))
                  (let ((default-directory repo-root))
                    (call-process-shell-command diff-command nil buffer))
                  (diff-mode)
                  (setq buffer-read-only t)
                  (goto-char (point-min)))
                (display-buffer buffer))))
        (message "Branch information not available")))))

;;;###autoload
(defun forge-llm-debug ()
  "Debug function to show all relevant forge and magit variables."
  (interactive)
  (with-output-to-temp-buffer "*forge-llm-debug*"
    (let ((vars '()))
      ;; Collect global variables with forge or magit in name
      (mapatoms
       (lambda (sym)
         (when (and (boundp sym)
                    (not (keywordp sym))
                    (symbolp sym)
                    (or (string-match-p "forge" (symbol-name sym))
                        (string-match-p "magit" (symbol-name sym))))
           (push sym vars))))

      ;; Sort and print global variables
      (setq vars (sort vars (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
      (princ "=== Global Variables ===\n\n")
      (dolist (var vars)
        (princ (format "%s: %S\n\n" var (symbol-value var))))

      ;; Print local variables
      (princ "\n\n=== Buffer-Local Variables ===\n\n")
      (dolist (var (buffer-local-variables))
        (when (and (symbolp (car var))
                   (or (string-match-p "forge" (symbol-name (car var)))
                       (string-match-p "magit" (symbol-name (car var)))))
          (princ (format "%s: %S\n\n" (car var) (cdr var))))))))

;;;###autoload
(defun forge-llm-setup ()
  "Set up forge-llm integration with Forge's new-pullreq buffer."
  (interactive)
  (add-hook 'forge-post-mode-hook #'forge-llm-setup-pullreq-hook)
  (message "forge-llm has been set up successfully"))

(defun forge-llm-setup-pullreq-hook ()
  "Hook function to set up forge-llm in a new-pullreq buffer."
  ;; Only add our keybinding if this is a pull request post
  (when (and buffer-file-name
             (string-match-p "new-pullreq" buffer-file-name))
    (local-set-key (kbd "C-c C-l") #'forge-llm-hello)
    (local-set-key (kbd "C-c C-d") #'forge-llm-debug)
    (local-set-key (kbd "C-c C-g") #'forge-llm-generate-story)
    (local-set-key (kbd "C-c C-t") #'forge-llm-insert-template-at-point)))

;;; LLM Integration

(defcustom forge-llm-llm-provider nil
  "LLM provider to use.
Can be a provider object or a function that returns a provider object."
  :type '(choice
          (sexp :tag "LLM provider")
          (function :tag "Function that returns an LLM provider"))
  :group 'forge-llm)

(defcustom forge-llm-temperature nil
  "Temperature for LLM responses.
If nil, the default temperature of the LLM provider will be used."
  :type '(choice (const :tag "Use provider default" nil)
                (float :tag "Custom temperature"))
  :group 'forge-llm)

(defcustom forge-llm-max-tokens nil
  "Maximum number of tokens for LLM responses.
If nil, the default max tokens of the LLM provider will be used."
  :type '(choice (const :tag "Use provider default" nil)
                (integer :tag "Custom max tokens"))
  :group 'forge-llm)

(defcustom forge-llm-short-story-prompt
  "Write a short story (250-300 words) about an open source developer who discovers something unexpected while working on a pull request.

   The story should be professional, concise, and have a clear beginning, middle, and conclusion."
  "Prompt used to generate a short story with the LLM."
  :type 'string
  :group 'forge-llm)

(defun forge-llm--get-provider ()
  "Return the LLM provider to use.
If `forge-llm-llm-provider' is a function, call it to get the provider.
Otherwise, return the value directly."
  (if (functionp forge-llm-llm-provider)
      (funcall forge-llm-llm-provider)
    forge-llm-llm-provider))

;;;###autoload
;;; PR Template Handling

(defcustom forge-llm-pr-template-paths
  '(".github/PULL_REQUEST_TEMPLATE.md"
    ".github/pull_request_template.md"
    "docs/pull_request_template.md"
    ".gitlab/merge_request_templates/default.md")
  "List of possible paths for PR/MR templates relative to repo root."
  :type '(repeat string)
  :group 'forge-llm)

(defvar-local forge-llm--pr-template-path nil
  "Path to the PR template for the current repository.")

(defun forge-llm-find-pr-template ()
  "Find PR template for the current repository.
Updates `forge-llm--pr-template-path' with the found template path."
  (interactive)
  (let* ((default-directory (file-name-directory
                            (directory-file-name
                             (file-name-directory
                              (or buffer-file-name default-directory)))))
         (repo-root (locate-dominating-file default-directory ".git"))
         found-template)
    (when repo-root
      (let ((default-directory repo-root))
        (setq found-template
              (cl-find-if #'file-exists-p forge-llm-pr-template-paths))))

    (setq forge-llm--pr-template-path
          (when found-template (expand-file-name found-template repo-root)))

    (if found-template
        (message "Found PR template: %s" forge-llm--pr-template-path)
      (message "No PR template found in repository"))

    forge-llm--pr-template-path))

(defun forge-llm-insert-template-at-point ()
  "Insert PR template at the current point in buffer."
  (interactive)
  (if (not (derived-mode-p 'forge-post-mode))
      (message "Not in a Forge pull request buffer")
    (let* ((default-directory (file-name-directory
                              (directory-file-name
                               (file-name-directory
                                (or buffer-file-name default-directory)))))
           (repo-root (locate-dominating-file default-directory ".git"))
           template-path
           template-content)

      ;; Find template file
      (when repo-root
        (let ((default-directory repo-root))
          (setq template-path
                (cl-find-if #'file-exists-p forge-llm-pr-template-paths))))

      ;; Read template content if found
      (when template-path
        (let ((full-path (expand-file-name template-path repo-root)))
          (condition-case err
              (progn
                (with-temp-buffer
                  (insert-file-contents full-path)
                  (setq template-content (buffer-string)))
                ;; Insert the content
                (when (and template-content (not (string-empty-p template-content)))
                  (insert template-content)
                  (message "PR template inserted")))
            (error (message "Error reading template: %s" err)))))

      ;; Show message if template not found
      (unless template-path
        (message "No PR template found in repository")))))

(defvar forge-llm--active-request nil
  "The active LLM request, if any.")

(defun forge-llm--stream-insert-response (msg buffer)
  "Insert streaming LLM response.
MSG is the response text.
BUFFER is the target buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# Short Story Generated by LLM\n\n")
        (insert msg)))))

(defun forge-llm--stream-update-status (status buffer &optional error-msg)
  "Update status of the streaming response.
STATUS is one of 'success', 'error'.
BUFFER is the target buffer.
ERROR-MSG is the error message, if any."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n\n")
        (pcase status
          ('success (insert "--- Generation complete ---"))
          ('error (insert (format "Error: %s" error-msg))))
        (text-mode)))))

(defun forge-llm-cancel-request ()
  "Cancel the active LLM request, if any."
  (interactive)
  (when forge-llm--active-request
    (llm-cancel-request forge-llm--active-request)
    (setq forge-llm--active-request nil)
    (message "LLM request canceled")))

(defun forge-llm-generate-story ()
  "Generate a short story using LLM and display it in a buffer.
Only works in Forge pull request buffers."
  (interactive)
  (if (not (derived-mode-p 'forge-post-mode))
      (message "Not in a Forge pull request buffer")
    (if-let ((provider (forge-llm--get-provider)))
        (progn
          (message "Generating story with LLM...")
          (let ((buffer (get-buffer-create "*forge-llm-output*")))
            ;; Initialize buffer
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "# Short Story Generated by LLM\n\n")
                (insert "Generating...")
                (display-buffer buffer)))

            ;; Create a proper chat prompt
            (let ((prompt (llm-make-simple-chat-prompt forge-llm-short-story-prompt)))
              ;; Set temperature and max tokens if supported
              (when forge-llm-temperature
                (setf (llm-chat-prompt-temperature prompt) forge-llm-temperature))
              (when forge-llm-max-tokens
                (setf (llm-chat-prompt-max-tokens prompt) forge-llm-max-tokens))

              ;; Cancel any existing request
              (when forge-llm--active-request
                (llm-cancel-request forge-llm--active-request)
                (setq forge-llm--active-request nil))

              ;; Start new streaming request
              (setq forge-llm--active-request
                    (llm-chat-streaming
                     provider prompt
                     ;; Partial callback - called for each chunk
                     (lambda (partial-response)
                       (forge-llm--stream-insert-response partial-response buffer))
                     ;; Complete callback - called when done
                     (lambda (_full-response)
                       (forge-llm--stream-update-status 'success buffer)
                       (setq forge-llm--active-request nil))
                     ;; Error callback
                     (lambda (err-msg)
                       (forge-llm--stream-update-status 'error buffer err-msg)
                       (setq forge-llm--active-request nil)))))))
      (user-error "No LLM provider configured. Set `forge-llm-llm-provider' first"))))

;; Add a key binding to the forge-llm keybinding
(defun forge-llm-setup-story-key ()
  "Add key binding for generating stories with LLM."
  (interactive)
  (define-key forge-post-mode-map (kbd "C-c C-g") 'forge-llm-generate-story))

;;;###autoload
(defun forge-llm-setup-all ()
  "Set up all forge-llm integrations."
  (interactive)
  (forge-llm-setup)  ; Set up the basic PR branch info
  (forge-llm-setup-story-key))  ; Set up the story generation key

(provide 'forge-llm)
