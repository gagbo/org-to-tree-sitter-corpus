;;; org-to-tree-sitter-corpus.el --- Transform an org-mode region to tree-sitter test format -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Gerry Agbobada
;;
;; Author: Gerry Agbobada <https://github.com/gagbo>
;; Maintainer: Gerry Agbobada <gerry@gagbo.net>
;; Created: September 30, 2021
;; Modified: September 30, 2021
;; Version: 0.0.1
;; Keywords: data languages tools
;; Homepage: https://github.com/gagbo/org-to-tree-sitter-corpus
;; Package-Requires: ((emacs "27.1") (org "9.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Convert a region of org-mode text directly using org into
;; a suitable tree-sitter test.
;;
;;
;;; Code:

(require 'org-element)
(require 'subr-x)
(require 'cl-lib)

(defconst org-to-tree-sitter-corpus--separator "---\n"
  "Separator between input and expected value.")

(defun org-to-tree-sitter-corpus-convert-org-file (file-path &optional delete-old)
  "Convert FILE-PATH from an org buffer to a corpus test file.

If optional DELETE-OLD is non-nil, delete the previous target file to avoid error on writes."
  (let ((corpus-file-path (file-name-with-extension file-path "txt")))
    (unless (string= (file-name-extension file-path) "org")
      (user-error "Expecting an org file"))
    (let ((input-str (with-temp-buffer (condition-case nil
                                           (progn
                                             (insert-file-contents file-path)
                                             (buffer-string))
                                         (file-error
                                          (user-error (format "Unable to read file %S" file-path))
                                          nil)))))
      (when delete-old
        (delete-file corpus-file-path))
      (with-temp-buffer
        (insert (org-to-tree-sitter-corpus-make-test input-str (string-replace "_" " " (file-name-base file-path))))
        (write-file corpus-file-path)))))

(defun org-to-tree-sitter-corpus-make-test (input-str title)
  "Format a test from INPUT-STR with TITLE."
  (let ((corpus-tree (org-to-tree-sitter-corpus--transform-tree (org-to-tree-sitter-corpus--parse-string input-str))))
    (concat (org-to-tree-sitter-corpus--test-title title)
            "\n\n"
            input-str
            "\n"
            org-to-tree-sitter-corpus--separator
            "\n"
            (format "%s" corpus-tree)
            "\n")))

(defun org-to-tree-sitter-corpus--test-title (title)
  "Return a formatted test title using TITLE."
  (let ((equal-str (thread-first title (length) (make-string ?=))))
    (format (concat equal-str "\n%s\n" equal-str) title)))

(defun org-to-tree-sitter-corpus--parse-string (content)
  "Parse CONTENT using org-element parser."
  (with-temp-buffer
    (insert content)
    (org-element-parse-buffer)))

(defun org-to-tree-sitter-corpus--transform-tree (ast)
  "Transform AST from an org-element tree to a tree-sitter corpus tree."
  (unless (eq (car ast) 'org-data)
    (user-error (format "Expecting the root of an AST, got %s" (car ast))))
  (cl-remove-if #'null
                (mapcar
                 (lambda (element)
                   (cond ((eq element 'org-data)
                          'org_data)
                         ((org-to-tree-sitter-corpus--transform--function-symbol element)
                          (funcall (org-to-tree-sitter-corpus--transform--function-symbol element) element))
                         ;; ((eq (car element) 'headline)
                         ;;  (org-to-tree-sitter-corpus--transform-headline element))
                         (t nil)))
                 ast)))

(defun org-to-tree-sitter-corpus--transform--function-symbol (element)
  "Return the function symbol that manages ELEMENT, or nil if absent."
  (symbol-function (intern (format "org-to-tree-sitter-corpus--transform-%s" (car element)))))

(defun org-to-tree-sitter-corpus--transform-headline (headline)
  "Transform HEADLINE from an org-element headline to a tree-sitter corpus tree."
  (unless (eq (car headline) 'headline)
    (user-error (format "Expecting a headline element, got %s" (car headline))))
  (let ((metadata (cadr headline)))
    (cl-remove-if #'null
                  (list 'headline
                        '(stars)
                        (when (plist-get metadata :commentedp) '(comment_marker))
                        (when (plist-get metadata :priority) '(priority_level))
                        (when (plist-get metadata :todo-keyword) '(todo_keyword))
                        (when (plist-get metadata :raw-value) '(title))
                        (when (plist-get metadata :tags) '(tags))))))

(provide 'org-to-tree-sitter-corpus)
;;; org-to-tree-sitter-corpus.el ends here
