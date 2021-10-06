;;; camiot-mode.el


;; Built using the tutorial at - http://ergoemacs.org/emacs/elisp_syntax_coloring.html
;; Copyright © 2021, Abhiroop Sarkar, Joel Svensson, Robert Krook

;; Author: Abhiroop Sarkar ( sarkara@chalmers.se )
;; Version: 0.0.1
;; Created: 6 Sept 2021
;; Keywords: languages
;; Homepage: https://github.com/svenssonjoel/Sense-VM


(setq camiot-font-lock-keywords
      (let* (
            ;; define several category of keywords
             (x-keywords '("mutrec" "where" "data" "case" "let" "then" "else" "of" "in" "if"))
            (x-types '("Channel Bool" "Channel Int" "Event Bool" "Event Int" "List Int" "List a" "Float" "Bool" "Int" "->" "()"))
            (x-constants '("False" "True" "()"))
            (x-functions '("spawnExternal" "channel" "spawn" "choose" "syncT" "send" "recv"  "sync" "wrap"))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-types-regexp (regexp-opt x-types 'words))
            (x-constants-regexp (regexp-opt x-constants 'words))
            (x-functions-regexp (regexp-opt x-functions 'words)))

        `(
          (,x-types-regexp . font-lock-type-face)
          (,x-constants-regexp . font-lock-constant-face)
          (,x-functions-regexp . font-lock-function-name-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;;;###autoload
(define-derived-mode camiot-mode c-mode "camiot mode"
  "Major mode for editing the CAMIOT language"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((camiot-font-lock-keywords))))

;; add the mode to the `features' list
(provide 'camiot-mode)

;;; camiot-mode.el ends here
