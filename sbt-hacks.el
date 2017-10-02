;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hack to provide light weight support for simple sbt-repl usage;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'an) 

(defvar an/sbt-buffer-name "*sbt*")

(defvar an/sbt-buffer-skip-line-regexp
  '( "^package.*" "[ \t\f\v\n\rs]+" ))

(defun an/sbt-skip-line-p(line)
  (let* ((found-match nil))
    (loop for regexp in an/sbt-buffer-skip-line-regexp do
          (if (string-match regexp line)
              (setq found-match t)))
    found-match))

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
  (an/sbt-eval-region
   (point-min)
   (point-max)))


(defun an/sbt-console-start()
  (interactive)
  (an/sbt-send-line "console"))

(defun an/sbt-console-quit()
  (interactive)
  (an/sbt-send-line ":quit"))

(defun an/sbt-run-test()
  (interactive)
  (an/sbt-send-line "test"))

(defun an/sbt-compile()
  (interactive)
  (an/sbt-send-line "compile"))

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

(defvar an/sbt-marker-file "build.sbt")

(defun an/is-rootp(fname)
  (equal "/" fname))

(defun an/dir-parent(fname)
  (file-name-directory (directory-file-name fname)))

(defun an/buffer-directory(buffer)
  (file-name-directory (buffer-file-name buffer)))

(defun an/sbt-find-root()
  "Look up till you find `sbt-marker-file` build.sbt "
  (let ((cur-dir  (an/buffer-directory (current-buffer))))
    (while (not (or (an/is-rootp cur-dir)
                     (an/file:find-files cur-dir an/sbt-marker-file)))
      (setf cur-dir (an/dir-parent cur-dir)))
    cur-dir))

(defun an/sbt-get-buffer-create()
  (let ((sbt-buffer (get-buffer an/sbt-buffer-name)))
    (if sbt-buffer
        sbt-buffer
      (let ((buffer (get-buffer-create an/sbt-buffer-name))
            (root-dir (an/sbt-find-root)))
        (if (not root-dir)
            (message "No root found for buffer !")
          (with-current-buffer buffer
            (setf default-directory root-dir)
            (shell buffer)
            (insert "sbt\n")
            (comint-send-input))
          buffer)))))

(defun an/sbt-pop-to-buffer()
  (interactive)
  (pop-to-buffer
   (an/sbt-get-buffer-create)
   'display-buffer-in-previous-window))
            

(defun an/sbt-show-class()
  (interactive)
   (an/sbt-send-line
    (format "%s.getClass.getMethods.foreach(println)" (thing-at-point 'symbol))))

(defun an/scala-thing-at-point()
  "Search scala files in current directory for thing at point 'symbol"
  (interactive)
  (save-excursion
    (rgrep (thing-at-point 'symbol)
           "*.scala"
           (file-name-directory
            (buffer-file-name (current-buffer))))))

(defvar an/sbt-navitation-map
  (let ((map (make-sparse-keymap)))
    (define-key map "." 'an/scala-thing-at-point)
    map))

(defvar an/sbt-console-commands-map
  (let ((map (make-sparse-keymap)))
    (define-key map "b" 'an/sbt-eval-buffer)
    (define-key map "r" 'an/sbt-eval-region)
    (define-key map "q" 'an/sbt-console-quit)
    (define-key map "t" 'an/sbt-console-show-type)
    (define-key map "@" 'an/sbt-show-class)
    map))

(defvar an/sbt-hacks-map
  (let ((map   (make-sparse-keymap)))
    (define-key map  "l" 'an/sbt-load-file)
    (define-key map  "g" 'an/sbt-pop-to-buffer)
    (define-key map  "s" 'an/sbt-console-start)
    (define-key map  "t" 'an/sbt-run-test)
    ;; sub-modes for navigation, command map
    (define-key map  "n" an/sbt-navitation-map)
    (define-key map  "c" an/sbt-console-commands-map)
    map))

(global-set-key (kbd "\C-c s") an/sbt-hacks-map)

;;;; TODO: Better Comint ?
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
         (buffer-name "*sbt*")
         (buffer (comint-check-proc buffer-name)))
    
    ;; Recreate the buffer if it has been lost. 
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'an/sbt-hacks-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer buffer-name))
       (current-buffer)))

    ;; Create the comint-process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "SBT" buffer
             runner an/sbt-hacks--arguments)
      (an/sbt-hacks-mode))))
