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
    (local-set-key (kbd "C-c C-g") #'forge-llm-generate-pr-description)
    (local-set-key (kbd "C-c C-p") #'forge-llm-generate-pr-description-at-point)
    (local-set-key (kbd "C-c C-t") #'forge-llm-insert-template-at-point)))

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

(defcustom forge-llm-pr-description-prompt
  "Please help write a professional Pull Request description based on the following information.

INSTRUCTIONS:
1. Follow the PR template structure exactly - do not modify, add or remove sections
2. Fill each section with relevant content based on the git diff
3. Mark sections as 'N/A' if they don't apply to this specific PR
4. Focus on the logic and functionality changes, not on test implementation details
5. Be clear and concise, but provide enough detail for reviewers to understand the changes
6. Use bullet points for lists of changes where appropriate

%s

Git diff:
```
%s
```

Please generate the complete PR description, ready to submit."
  "Prompt used to generate a PR description with the LLM.
This will be formatted with the PR template (or default template) and git diff."
  :type 'string
  :group 'forge-llm)

(defcustom forge-llm-default-pr-template
  "## Description
<!-- Provide a clear and concise description of the changes -->

## Type of change
<!-- What types of changes does your code introduce? Put an 'x' in all boxes that apply -->
- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] Documentation update
- [ ] Refactoring (no functional changes)

## How Has This Been Tested?
<!-- Describe how you tested your changes -->

## Checklist
- [ ] My code follows the project's style guidelines
- [ ] I have performed a self-review of my code
- [ ] I have added tests that prove my fix is effective or my feature works
- [ ] New and existing tests pass with my changes"
  "Default PR template to use when no template is found in the repository."
  :type 'string
  :group 'forge-llm)

(defun forge-llm--get-provider ()
  "Return the LLM provider to use.
If `forge-llm-llm-provider' is a function, call it to get the provider.
Otherwise, return the value directly."
  (if (functionp forge-llm-llm-provider)
      (funcall forge-llm-llm-provider)
    forge-llm-llm-provider))

;;; Stream handling

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
        (insert "# Generated PR Description\n\n")
        (insert msg)
        ;; If markdown-mode is available, set the buffer mode
        (when (require 'markdown-mode nil t)
          (markdown-mode))))))

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
        ;; Set buffer to markdown-mode
        (when (require 'markdown-mode nil t)
          (markdown-mode))))))

(defun forge-llm-cancel-request ()
  "Cancel the active LLM request, if any."
  (interactive)
  (when forge-llm--active-request
    (llm-cancel-request forge-llm--active-request)
    (setq forge-llm--active-request nil)
    (message "LLM request canceled")))

(defun forge-llm-generate-pr-description ()
  "Generate a PR description based on the current git diff and PR template.
Only works in Forge pull request buffers."
  (interactive)
  (if (not (derived-mode-p 'forge-post-mode))
      (message "Not in a Forge pull request buffer")
    (if-let ((provider (forge-llm--get-provider)))
        (progn
          (message "Generating PR description with LLM...")
          (let* ((buffer (get-buffer-create "*forge-llm-output*"))
                 (head (and (boundp 'forge--buffer-head-branch) forge--buffer-head-branch))
                 (base (and (boundp 'forge--buffer-base-branch) forge--buffer-base-branch))
                 (default-directory (file-name-directory
                                    (directory-file-name
                                     (file-name-directory
                                      (or buffer-file-name default-directory)))))
                 (repo-root (locate-dominating-file default-directory ".git"))
                 diff-output
                 template-content
                 template-path)

            ;; Log repository root for debugging
            (message "Repository root: %s" repo-root)

            ;; Get git diff
            (when (and repo-root head base)
              (let ((default-directory repo-root))
                (with-temp-buffer
                  (call-process "git" nil t nil "diff" base head)
                  (setq diff-output (buffer-string)))))

            ;; If diff is too large, trim it
            (when (and diff-output (> (length diff-output) 12000))
              (setq diff-output (substring diff-output 0 12000))
              (setq diff-output (concat diff-output "\n\n... [diff truncated due to size] ...")))

            ;; Find PR template - enhanced with more debugging
            (when repo-root
              (let ((default-directory repo-root))
                ;; Log all potential template paths for debugging
                (dolist (path forge-llm-pr-template-paths)
                  (message "Checking for template at: %s (exists: %s)"
                           path (file-exists-p path)))

                ;; Try to find the template
                (setq template-path (cl-find-if #'file-exists-p forge-llm-pr-template-paths))

                (if template-path
                    (progn
                      (message "Found template at: %s" template-path)
                      (condition-case err
                          (progn
                            (with-temp-buffer
                              (insert-file-contents (expand-file-name template-path repo-root))
                              (setq template-content (buffer-string)))
                            (message "Template loaded, length: %d chars"
                                     (length template-content)))
                        (error (message "Error reading template: %s" err))))
                  (message "No template found in repository paths"))))

            ;; Use default template if none found
            (unless template-content
              (setq template-content forge-llm-default-pr-template)
              (message "Using default template"))

            ;; Log template info
            (message "Using template: %s"
                     (if (equal template-content forge-llm-default-pr-template)
                         "default template (no repository template found)"
                         (format "repository template at %s" template-path)))

            ;; Initialize output buffer
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "# Generating PR Description\n\n")
                (insert "Analyzing diff between: ")
                (when (and head base)
                  (insert (format "%s → %s\n\n" head base)))
                ;; Enable markdown mode right away if available
                (when (require 'markdown-mode nil t)
                  (markdown-mode))
                (display-buffer buffer)))

            ;; Create prompt with template and diff
            (let* ((pr-section (format "PR template:\n%s" template-content))
                   (formatted-prompt (format forge-llm-pr-description-prompt
                                             pr-section
                                             (or diff-output "[No diff available]"))))

              ;; Log prompt to debug buffer
              (with-current-buffer (get-buffer-create "*forge-llm-debug-prompt*")
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert "=== PR DESCRIPTION PROMPT ===\n\n")
                  (insert formatted-prompt)
                  (goto-char (point-min))))

              (message "Prompt prepared - see *forge-llm-debug-prompt* buffer for details")

              ;; Create LLM prompt object
              (let ((prompt (llm-make-simple-chat-prompt formatted-prompt)))
                ;; Set LLM parameters
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
                         (setq forge-llm--active-request nil))))))))
      (user-error "No LLM provider configured. Set `forge-llm-llm-provider' first"))))

(defun forge-llm-generate-pr-description-at-point ()
  "Generate a PR description and insert at current point, replacing any content after point.
Only works in Forge pull request buffers."
  (interactive)
  (if (not (derived-mode-p 'forge-post-mode))
      (message "Not in a Forge pull request buffer")
    (if-let ((provider (forge-llm--get-provider)))
        (progn
          (message "Generating PR description at point with LLM...")
          (let* ((head (and (boundp 'forge--buffer-head-branch) forge--buffer-head-branch))
                 (base (and (boundp 'forge--buffer-base-branch) forge--buffer-base-branch))
                 (default-directory (file-name-directory
                                    (directory-file-name
                                     (file-name-directory
                                      (or buffer-file-name default-directory)))))
                 (repo-root (locate-dominating-file default-directory ".git"))
                 (current-point (point))
                 diff-output
                 template-content
                 template-path)

            ;; Delete everything after point
            (delete-region current-point (point-max))

            ;; Insert a placeholder
            (insert "\n\n[Generating PR description...]\n\n")
            (let ((placeholder-start current-point)
                  (placeholder-end (point)))

              ;; Get git diff
              (when (and repo-root head base)
                (let ((default-directory repo-root))
                  (with-temp-buffer
                    (call-process "git" nil t nil "diff" base head)
                    (setq diff-output (buffer-string)))))

              ;; If diff is too large, trim it
              (when (and diff-output (> (length diff-output) 12000))
                (setq diff-output (substring diff-output 0 12000))
                (setq diff-output (concat diff-output "\n\n... [diff truncated due to size] ...")))

              ;; Find PR template with debugging
              (when repo-root
                (let ((default-directory repo-root))
                  ;; Try to find the template
                  (setq template-path (cl-find-if #'file-exists-p forge-llm-pr-template-paths))

                  (when template-path
                    (message "Found template at: %s" template-path)
                    (condition-case err
                        (with-temp-buffer
                          (insert-file-contents (expand-file-name template-path repo-root))
                          (setq template-content (buffer-string)))
                      (error (message "Error reading template: %s" err))))))

              ;; Use default template if none found
              (unless template-content
                (setq template-content forge-llm-default-pr-template)
                (message "Using default template"))

              ;; Log template info
              (message "Using template: %s"
                       (if (equal template-content forge-llm-default-pr-template)
                           "default template (no repository template found)"
                           (format "repository template at %s" template-path)))

              ;; Create prompt with template and diff
              (let* ((pr-section (format "PR template:\n%s" template-content))
                     (formatted-prompt (format forge-llm-pr-description-prompt
                                              pr-section
                                              (or diff-output "[No diff available]"))))

                ;; Log prompt to debug buffer
                (with-current-buffer (get-buffer-create "*forge-llm-debug-prompt*")
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (insert "=== PR DESCRIPTION PROMPT ===\n\n")
                    (insert formatted-prompt)
                    (goto-char (point-min))))

                (message "Prompt prepared - see *forge-llm-debug-prompt* buffer for details")

                ;; Create LLM prompt object
                (let ((prompt (llm-make-simple-chat-prompt formatted-prompt)))
                  ;; Set LLM parameters
                  (when forge-llm-temperature
                    (setf (llm-chat-prompt-temperature prompt) forge-llm-temperature))
                  (when forge-llm-max-tokens
                    (setf (llm-chat-prompt-max-tokens prompt) forge-llm-max-tokens))

                  ;; Cancel any existing request
                  (when forge-llm--active-request
                    (llm-cancel-request forge-llm--active-request)
                    (setq forge-llm--active-request nil))

                  ;; Start new streaming request for in-buffer generation
                  (setq forge-llm--active-request
                        (llm-chat-streaming
                         provider prompt
                         ;; Partial callback - called for each chunk
                         (lambda (partial-response)
                           (save-excursion
                             (let ((inhibit-read-only t))
                               ;; Replace placeholder with response
                               (delete-region placeholder-start placeholder-end)
                               (goto-char placeholder-start)
                               (insert "\n\n")
                               (insert partial-response)
                               (setq placeholder-end (point)))))
                         ;; Complete callback - called when done
                         (lambda (_full-response)
                           (save-excursion
                             (goto-char placeholder-end)
                             (insert "\n\n")
                             (setq forge-llm--active-request nil))
                           (message "PR description generation complete"))
                         ;; Error callback
                         (lambda (err-msg)
                           (save-excursion
                             (goto-char placeholder-end)
                             (insert (format "\n\nError: %s" err-msg))
                             (setq forge-llm--active-request nil))))))))))
      (user-error "No LLM provider configured. Set `forge-llm-llm-provider' first"))))

;;;###autoload
(defun forge-llm-setup-all ()
  "Set up all forge-llm integrations."
  (interactive)
  (forge-llm-setup)  ; Set up the basic PR branch info
  (message "All forge-llm integrations have been set up"))

(provide 'forge-llm)
