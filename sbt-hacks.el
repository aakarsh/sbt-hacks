;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hack to provide light weight support for simple sbt-repl usage.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar an/sbt-path "/usr/bin/sbt")

(defvar an/sbt-hacks-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    ;; final-map to retrun
    map)
  "Basic mode map for `run-sbt-hacks'")

(defvar an/sbt-hacks--arguments "")

(defvar an/sbt-prompt-regexp "^\\(?:\\[[^@]+@[^@]+\\]\\)"
  "Prompt for `run-sbt-lit'.")

(defun an/run-sbt-hacks ()
  "Run an inferior instance of `an/sbt-hacks-' inside Emacs."
  (interactive)
  (let* ((runner an/sbt-path)
         (buffer (comint-check-proc "sbt")))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'an/sbt-hacks-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*SBT*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "SBT" buffer
             runner an/sbt-hacks--arguments)
      (an/sbt-hacks-mode))))

(defvar an/sbt-buffer-name "*sbt*")

(defvar an/sbt-buffer-skip-line-regexp '( "^package.*" ))

(defun an/sbt-skip-line-p(line)
  (let* ((found-match nil))
    (loop for regexp in an/sbt-buffer-skip-line-regexp do
          (if (string-match regexp line)
              (setq found-match t)))
  found-match))

(defun an/sbt-eval-region(start end)
  (interactive "rEvaluate region in shell buffer ")  
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((an/sbt-buffer (get-buffer an/sbt-buffer-name))
            (region (buffer-string)))
        (if (not an/sbt-buffer)
            (error "No buffer called : %s " an/sbt-buffer-name))
        (with-current-buffer an/sbt-buffer
          (loop for line in (split-string region "[\n\t\r]+" "[ \t\f\v]+" )
                if (and (> (length line) 0) (not (an/sbt-skip-line-p line)))
                do
                (insert line)
                (comint-send-input)))))))

(defun an/sbt-eval-buffer()
  (interactive)
  (an/sbt-eval-region (point-min) (point-max)))

;; use hacks for now 
(global-set-key (kbd "\C-c b") 'an/sbt-eval-buffer)
(global-set-key (kbd "\C-c r") 'an/sbt-eval-region)

