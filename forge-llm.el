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
    (local-set-key (kbd "C-c C-d") #'forge-llm-debug)))

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
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "Generating story...\n\n")
                (display-buffer buffer)))

            ;; Create a proper chat prompt
            (let ((prompt (llm-make-simple-chat-prompt forge-llm-short-story-prompt)))
              ;; Set temperature and max tokens if supported
              (when forge-llm-temperature
                (setf (llm-chat-prompt-temperature prompt) forge-llm-temperature))
              (when forge-llm-max-tokens
                (setf (llm-chat-prompt-max-tokens prompt) forge-llm-max-tokens))

              ;; Call llm-chat with the proper prompt object
              (condition-case err
                  (let ((response (llm-chat provider prompt)))
                    (with-current-buffer buffer
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert "# Short Story Generated by LLM\n\n")
                        (insert response)
                        (goto-char (point-min))
                        (text-mode))))
                (error
                 (with-current-buffer buffer
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (insert (format "Error generating story: %s" (error-message-string err))))))))))
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
