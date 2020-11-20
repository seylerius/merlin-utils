;;; merlin-utils.el --- locate usages of ocaml identifiers -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Sable Seyler
;;
;; Author: Sable Seyler <http://github/seylerius>
;; Maintainer: Sable Seyler <sable@seyleri.us>
;; Created: November 19, 2020
;; Modified: November 19, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/seylerius/merlin-locate-usage
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; merlin-locate-usages is a companion function to merlin-mode. It allows
;; you to identify other locations within a project where an a specified
;; identifier is used. Depends
;;
;;; Code:

(require 'projectile)
(require 'rg)
(require 'merlin)

(defun merlin-utils--pos-equal-p (pos1 pos2)
  "Return the equality of two merlin position data structures, POS1 and POS2."
  (and
   (equal (cdr (assq 'file pos1)) (cdr (assq 'file pos2)))
   (equal (cdr (assq 'line (assq 'pos pos1)))
          (cdr (assq 'line (assq 'pos pos2))))
   (equal (cdr (assq 'col (assq 'pos pos1)))
          (cdr (assq 'col (assq 'pos pos2))))))

(defun merlin-utils--bounds-point-p (bounds)
  "Test whether BOUNDS surrounds point."
  (let ((start (merlin--point-of-pos (cdr (assq 'start bounds))))
        (end (merlin--point-of-pos (cdr (assq 'end bounds)))))
    (and (<= start (point)) (> end (point)))))

(defun merlin-utils--identifier-at-point (&optional locate)
  "Return the identifier under point, plus start position if LOCATE is non-nil.
Disassembles a `merlin--occurrences' call to find the identifier at point,
and returns it as a string. With a non-nil argument passed to LOCATE, the
return value will instead be an assoc list, with the keys `ident' and `pos',
which is a position data structure as used by `merlin'."
  (let* ((occurrences (merlin--occurrences))
         (this-occurrence (car (seq-filter #'merlin-utils--bounds-point-p occurrences)))
         (start-point (merlin--point-of-pos (cdr (assq 'start this-occurrence))))
         (end-point (merlin--point-of-pos (cdr (assq 'end this-occurrence))))
         (identifier (buffer-substring-no-properties start-point end-point)))
    (if locate
        `((identifier . ,identifier)
          (location . ((file . ,(buffer-file-name))
                       (pos . ,(cdr (assq 'start this-occurrence))))))
      identifier)))

(defconst merlin-utils--rg-location-regexp
  (rx
   (seq bol
        (group (one-or-more (any alphanumeric "/" "-" "_" "."))) ":"
        (group (one-or-more digit))
        ":" (group (one-or-more digit)) ":"))
  "Regexp to match the filename-and-position headings on each ripgrep match.
Breaks the match into three groupings: file, line, column.")

(defun merlin-utils--rg-identifier-regexp (identifier)
  "Generate a ripgrep regexp matching the specified IDENTIFIER."
  (concat "(?:[^_'[:alnum:]])"
          "(" identifier ")"
          "(?:[^_'[:alnum:]]|$)"))

(defun merlin-utils--pos-of-result (result-line)
  "Convert the ripgrep result RESULT-LINE to a merlin position alist."
  (when-let ((match-p (string-match merlin-utils--rg-location-regexp result-line))
             (file (concat (projectile-project-root)
                           (match-string 1 result-line)))
             (line (string-to-number (match-string 2 result-line)))
             (col (string-to-number (match-string 3 result-line))))
    `((assoc)
      (file . ,file)
      (pos (assoc)
           (line . ,line)
           (col . ,col)))))

(defvar merlin-utils--results-function-closure nil
  "Closure to store results function.")

(defun merlin-utils--process-rg-results (buffer msg)
  "Call closure on usage results BUFFER and print MSG."
  (message "Ripgrep returned: %s" msg)
  (message "Buffer passed: %s" buffer)
  (funcall merlin-utils--results-function-closure buffer))

(defun merlin-utils-locate-usages ()
  "Return an interactive list of usages of the identifier at point."
  (interactive)
  (let* ((identifier-location (merlin-utils--identifier-at-point t))
         (identifier (cdr (assq 'identifier identifier-location)))
         (orig-location (cdr (assq 'location identifier-location)))
         (project-root (projectile-project-root)))
    (setq merlin-utils--results-function-closure
          (lambda (buffer)
            (message "Entered result processor")
            (with-current-buffer buffer
              ;; Possibly advance to the first search result
              (if (looking-at "^rg started at ")
                  (progn (re-search-forward
                          (rx-to-string
                           '(seq bol "rg started at "
                                 (one-or-more alpha) " "
                                 (one-or-more alpha) " "
                                 (one-or-more digit) " "
                                 (= 2 digit) ":"
                                 (= 2 digit) ":"
                                 (= 2 digit)
                                 "\n" "\n"
                                 (one-or-more (any alphanumeric "/" "-" "_" ".")))))
                         (re-search-forward
                          (rx-to-string
                           '(seq "\n" "\n")))))
              (message "Advanced to results if necessary")
              ;; Twisting a `while' into an `until'
              (while
                  (when-let*
                      ((result-line (buffer-substring-no-properties (line-beginning-position)
                                                                    (line-end-position)))
                       (result-location (merlin-utils--pos-of-result result-line))
                       (inhibit-read-only t))
                    (let (usage-p)
                      (message "Jumping to a result location")
                      (merlin--goto-file-and-point result-location)
                      (condition-case nil
                          (setq usage-p (merlin-utils--pos-equal-p orig-location (merlin/locate)))
                        (error nil))
                      (merlin-pop-stack)
                      (if usage-p
                          (forward-line 1)
                        (delete-region (line-beginning-position) (+ 1 (line-end-position)))))
                    ;; Continue until we get an empty line at the end of the results
                    (not (looking-at "^$"))))
              (remove-hook 'compilation-finish-functions 'merlin-utils--process-rg-results))
            (merlin--goto-file-and-point orig-location)))
    (add-hook 'compilation-finish-functions 'merlin-utils--process-rg-results)
    (message "%s" compilation-finish-functions)
    (setq rg-buffer-name "usages")
    (let ((rg-group-result nil))
      (rg-run
       (merlin-utils--rg-identifier-regexp identifier)
       "*.ml"
       project-root
       ;; Skipping LITERAL and CONFIRM
       nil nil
       ;; Specify flags to get jumpable line numbers, rather than groupings
       '("-n" "-H" "--no-heading")))))

(provide 'merlin-utils)
;;; merlin-utils.el ends here
