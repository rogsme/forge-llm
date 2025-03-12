;;; forge-llm.el --- LLM Integration for Forge -*- lexical-binding: t -*-

;; Copyright (C) 2025 Roger Gonzalez

;; Author: Roger Gonzalez <roger@rogs.me>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (forge "0.3.0") (llm "0.16.1"))
;; Keywords: convenience, forge, git, llm
;; URL: https://gitlab.com/rogs/forge-llm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides LLM integration for Forge.
;;

;;; Code:

(require 'forge)
(require 'llm)

;;; Customization Options

(defgroup forge-llm nil
  "LLM integration for Forge."
  :group 'forge
  :prefix "forge-llm-")

(defcustom forge-llm-pr-template-paths
  '(".github/PULL_REQUEST_TEMPLATE.md"
    ".github/pull_request_template.md"
    "docs/pull_request_template.md"
    ".gitlab/merge_request_templates/default.md")
  "List of possible paths for PR/MR templates relative to repo root."
  :type '(repeat string)
  :group 'forge-llm)

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

;;; Variables

(defvar-local forge-llm--pr-template-path nil
  "Path to the PR template for the current repository.")

(defvar forge-llm--active-request nil
  "The active LLM request, if any.")

;;; Core Functions

;;;###autoload
(defun forge-llm-setup ()
  "Set up forge-llm integration with Forge's new-pullreq buffer."
  (interactive)
  (add-hook 'forge-post-mode-hook #'forge-llm-setup-pullreq-hook)
  (message "forge-llm has been set up successfully"))

;;;###autoload
(defun forge-llm-setup-all ()
  "Set up all forge-llm integrations."
  (interactive)
  (forge-llm-setup)  ; Set up the basic PR branch info
  (message "All forge-llm integrations have been set up"))

(defun forge-llm-setup-pullreq-hook ()
  "Hook function to set up forge-llm in a new-pullreq buffer."
  ;; Only add our keybinding if this is a pull request post
  (when (and buffer-file-name
             (string-match-p "new-pullreq" buffer-file-name))
    (local-set-key (kbd "C-c C-g") #'forge-llm-generate-pr-description)
    (local-set-key (kbd "C-c C-p") #'forge-llm-generate-pr-description-at-point)
    (local-set-key (kbd "C-c C-t") #'forge-llm-insert-template-at-point)))

;;; Utility Functions

(defun forge-llm--get-repo-root ()
  "Find the git repository root directory."
  (let* ((default-directory (file-name-directory
                             (directory-file-name
                              (file-name-directory
                               (or buffer-file-name default-directory)))))
         (repo-root (locate-dominating-file default-directory ".git")))
    repo-root))

(defun forge-llm--get-branch-info ()
  "Get the head and base branch information."
  (let ((head (and (boundp 'forge--buffer-head-branch) forge--buffer-head-branch))
        (base (and (boundp 'forge--buffer-base-branch) forge--buffer-base-branch)))
    (cons head base)))

(defun forge-llm--get-git-diff (repo-root head base)
  "Get the git diff between HEAD and BASE branches.
Truncate the diff if it's too large.
REPO-ROOT is the repository root directory."
  (when (and repo-root head base)
    (let ((default-directory repo-root)
          diff-output)
      (with-temp-buffer
        (call-process "git" nil t nil "diff" base head)
        (setq diff-output (buffer-string)))

      ;; If diff is too large, trim it
      (when (and diff-output (> (length diff-output) 12000))
        (setq diff-output (substring diff-output 0 12000))
        (setq diff-output (concat diff-output "\n\n... [diff truncated due to size] ...")))

      diff-output)))

(defun forge-llm--get-provider ()
  "Return the LLM provider to use.
If `forge-llm-llm-provider' is a function, call it to get the provider.
Otherwise, return the value directly."
  (if (functionp forge-llm-llm-provider)
      (funcall forge-llm-llm-provider)
    forge-llm-llm-provider))

;;; PR Template Handling

(defun forge-llm-find-pr-template ()
  "Find PR template for the current repository.
Updates `forge-llm--pr-template-path' with the found template path.
Returns the template content or nil if not found."
  (interactive)
  (let* ((repo-root (forge-llm--get-repo-root))
         found-template
         template-content)

    (when repo-root
      (let ((default-directory repo-root))
        ;; Try to find the template
        (setq found-template
              (cl-find-if #'file-exists-p forge-llm-pr-template-paths))

        ;; Read the template content if found
        (when found-template
          (condition-case err
              (with-temp-buffer
                (insert-file-contents (expand-file-name found-template repo-root))
                (setq template-content (buffer-string)))
            (error
             (message "Error reading template: %s" err)
             (setq template-content nil))))))

    ;; Store the path for future reference
    (setq forge-llm--pr-template-path
          (when found-template (expand-file-name found-template repo-root)))

    (if found-template
        (message "Found PR template: %s" forge-llm--pr-template-path)
      (message "No PR template found in repository"))

    template-content))

(defun forge-llm--get-pr-template ()
  "Get the PR template content.
Returns the found template or default template if none found."
  (or (forge-llm-find-pr-template)
      forge-llm-default-pr-template))

(defun forge-llm-insert-template-at-point ()
  "Insert PR template at the current point in buffer."
  (interactive)
  (if (not (derived-mode-p 'forge-post-mode))
      (message "Not in a Forge pull request buffer")
    (let ((template-content (forge-llm--get-pr-template)))
      (when (and template-content (not (string-empty-p template-content)))
        (insert template-content)
        (message "PR template inserted")))))

;;; LLM Stream Handling

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

;;; PR Description Generation

(defun forge-llm--prepare-prompt ()
  "Prepare the LLM prompt for PR description generation.
Returns a cons cell with (prompt . debug-info) or nil if preparation fails."
  (if-let ((provider (forge-llm--get-provider)))
      (let* ((branch-info (forge-llm--get-branch-info))
             (head (car branch-info))
             (base (cdr branch-info))
             (repo-root (forge-llm--get-repo-root))
             (diff-output (forge-llm--get-git-diff repo-root head base))
             (template-content (forge-llm--get-pr-template))
             (debug-info nil))

        ;; Create debug info for logging
        (setq debug-info
              (cond
               ((not repo-root)
                "Failed to find git repository root")
               ((not (and head base))
                "Failed to get branch information")
               ((not diff-output)
                "Failed to generate git diff")
               ((not template-content)
                "Failed to get PR template")
               (t
                (format "Using %s, comparing %s → %s"
                        (if (equal template-content forge-llm-default-pr-template)
                            "default template (no repository template found)"
                            (format "repository template at %s" forge-llm--pr-template-path))
                        base
                        head))))

        ;; Create prompt with template and diff
        (when (and diff-output template-content)
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

            ;; Return the prepared prompt and debug info
            (cons formatted-prompt debug-info))))
    (user-error "No LLM provider configured. Set `forge-llm-llm-provider' first")))

(defun forge-llm--start-llm-request (prompt output-fn complete-fn error-fn)
  "Start an LLM request with the given PROMPT.
OUTPUT-FN is called with each response chunk.
COMPLETE-FN is called when the request completes.
ERROR-FN is called if the request fails."
  (if-let ((provider (forge-llm--get-provider)))
      (progn
        ;; Create LLM prompt object
        (let ((llm-prompt (llm-make-simple-chat-prompt prompt)))
          ;; Set LLM parameters
          (when forge-llm-temperature
            (setf (llm-chat-prompt-temperature llm-prompt) forge-llm-temperature))
          (when forge-llm-max-tokens
            (setf (llm-chat-prompt-max-tokens llm-prompt) forge-llm-max-tokens))

          ;; Cancel any existing request
          (when forge-llm--active-request
            (llm-cancel-request forge-llm--active-request)
            (setq forge-llm--active-request nil))

          ;; Start new streaming request
          (setq forge-llm--active-request
                (llm-chat-streaming
                 provider llm-prompt
                 ;; Partial callback
                 output-fn
                 ;; Complete callback
                 (lambda (full-response)
                   (funcall complete-fn full-response)
                   (setq forge-llm--active-request nil))
                 ;; Error callback
                 (lambda (err-msg)
                   (funcall error-fn err-msg)
                   (setq forge-llm--active-request nil))))))
    (user-error "No LLM provider configured. Set `forge-llm-llm-provider' first")))

(defun forge-llm-generate-pr-description ()
  "Generate a PR description based on the current git diff and PR template.
Only works in Forge pull request buffers."
  (interactive)
  (if (not (derived-mode-p 'forge-post-mode))
      (message "Not in a Forge pull request buffer")

    (let* ((prompt-info (forge-llm--prepare-prompt))
           (prompt (car prompt-info))
           (debug-info (cdr prompt-info)))

      (if prompt
          (progn
            (message "Generating PR description with LLM... %s" debug-info)
            (let ((buffer (get-buffer-create "*forge-llm-output*")))
              ;; Initialize output buffer
              (with-current-buffer buffer
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert "# Generating PR Description\n\n")
                  (insert debug-info "\n\n")
                  ;; Enable markdown mode right away if available
                  (when (require 'markdown-mode nil t)
                    (markdown-mode))
                  (display-buffer buffer)))

              ;; Start the LLM request
              (forge-llm--start-llm-request
               prompt
               ;; Output function
               (lambda (partial-response)
                 (forge-llm--stream-insert-response partial-response buffer))
               ;; Complete function
               (lambda (_full-response)
                 (forge-llm--stream-update-status 'success buffer))
               ;; Error function
               (lambda (err-msg)
                 (forge-llm--stream-update-status 'error buffer err-msg)))))

        ;; Handle prompt preparation failure
        (message "Failed to prepare LLM prompt: %s" debug-info)))))

(defun forge-llm-generate-pr-description-at-point ()
  "Generate a PR description and insert at current point.
Only works in Forge pull request buffers."
  (interactive)
  (if (not (derived-mode-p 'forge-post-mode))
      (message "Not in a Forge pull request buffer")

    (let* ((prompt-info (forge-llm--prepare-prompt))
           (prompt (car prompt-info))
           (debug-info (cdr prompt-info))
           (current-point (point)))

      (if prompt
          (progn
            (message "Generating PR description at point with LLM... %s" debug-info)

            ;; Delete everything after point
            (delete-region current-point (point-max))

            ;; Insert a placeholder
            (insert "\n\n[Generating PR description...]\n\n")
            (let ((placeholder-start current-point)
                  (placeholder-end (point)))

              ;; Start the LLM request
              (forge-llm--start-llm-request
               prompt
               ;; Output function
               (lambda (partial-response)
                 (save-excursion
                   (let ((inhibit-read-only t))
                     ;; Replace placeholder with response
                     (delete-region placeholder-start placeholder-end)
                     (goto-char placeholder-start)
                     (insert "\n\n")
                     (insert partial-response)
                     (setq placeholder-end (point)))))
               ;; Complete function
               (lambda (_full-response)
                 (save-excursion
                   (goto-char placeholder-end)
                   (insert "\n\n"))
                 (message "PR description generation complete"))
               ;; Error function
               (lambda (err-msg)
                 (save-excursion
                   (goto-char placeholder-end)
                   (insert (format "\n\nError: %s" err-msg)))))))

        ;; Handle prompt preparation failure
        (message "Failed to prepare LLM prompt: %s" debug-info)))))

(provide 'forge-llm)
;;; forge-llm.el ends here
