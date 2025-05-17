;;; package -- deviantfero's gpt.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; File to configure lsp related packages without a specific language
;;; Code:

(use-package gptel
  :after
  evil
  :config
  (setq gptel-default-mode 'org-mode)
  (setq-default gptel-model "llama3"
                gptel-backend (gptel-make-ollama "Ollama"
                                :host "localhost:11434"
                                :models '("deepseek-coder-v2")
                                :stream t))
  :bind
  (:map global-map
        ("C-c g" . gptel-send)))

;;; gpt.el ends here
