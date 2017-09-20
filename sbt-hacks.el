;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hack to provide light weight support for simple sbt-repl usage.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar an/sbt-buffer-name "*sbt*")

(defvar an/sbt-buffer-skip-line-regexp '( "^package.*" "[ \t\f\v\n\rs]+" ))

(defun an/sbt-skip-line-p(line)
  (let* ((found-match nil))
    (loop for regexp in an/sbt-buffer-skip-line-regexp do
          (if (string-match regexp line)
              (setq found-match t)))
    found-match))

(defun an/sbt-pop-to-buffer()
  (interactive)
  (pop-to-buffer (get-buffer an/sbt-buffer-name)))

(defun an/sbt-send-line(line)
  (interactive "sSend SBT : ")
    (save-excursion
    (save-restriction
      (let ((an/sbt-buffer (get-buffer an/sbt-buffer-name)))            
        (if (not an/sbt-buffer)
            (error "No buffer called : %s " an/sbt-buffer-name))
        (with-current-buffer an/sbt-buffer
          (when (> (length line) 0) (not (an/sbt-skip-line-p line))
                (insert (format "%s\n" line))
                (comint-send-input)))))))

        
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
                do
                (an/sbt-send-line line)))))))


(defun an/sbt-eval-buffer()
  (interactive)
  (an/sbt-eval-region (point-min) (point-max)))

(defun an/sbt-console-start()
  (interactive)
  (an/sbt-send-line "console"))

(defun an/sbt-console-quit()
  (interactive)
  (an/sbt-send-line ":quit"))

(defun an/sbt-console-show-type(line)
  (interactive "sType Of : ")
  (an/sbt-send-line (format ":type %s" line)))

(defun an/sbt-console-shell-cmd(line)
  (interactive "sShell Command : ")
  (an/sbt-send-line (format ":sh %s" line)))

(defun an/sbt-load-file()
  (interactive)
  (an/sbt-send-line
   (format ":load %s"
           (buffer-file-name (current-buffer)))))


(setq an/sbt-hacks-map
  (let ((map   (make-sparse-keymap)))
    (define-key map  "b" 'an/sbt-eval-buffer)
    (define-key map  "r" 'an/sbt-eval-buffer)
    (define-key map  "l" 'an/sbt-load-file)
    (define-key map  "g" 'an/sbt-pop-to-buffer)
    map))

(global-set-key (kbd "\C-c s") an/sbt-hacks-map)


;;;; TODO better comint.

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


