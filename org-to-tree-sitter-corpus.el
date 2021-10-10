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
;; Package-Requires: ((emacs "28.1") (org "9.5") (compat "28.1.0.0"))
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
(require 'files)

(defconst org-to-tree-sitter-corpus--separator "---\n"
  "Separator between input and expected value.")

(defun org-to-tree-sitter-corpus-convert-org-file (file-path &optional delete-old)
  "Convert FILE-PATH from an org buffer to a corpus test file.

If optional DELETE-OLD is non-nil, delete the previous target file to
avoid error on writes."
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
  (let ((corpus-tree (org-to-tree-sitter-corpus-transform-tree (org-to-tree-sitter-corpus--parse-string input-str))))
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

(defalias 'org-to-tree-sitter-corpus-transform-tree #'org-to-tree-sitter-corpus--transform-org-data
  "Transform AST from an org-element tree to a tree-sitter corpus tree.")

(defun org-to-tree-sitter-corpus--transform-node (node)
  "Transform NODE from an org-element tree to a tree-sitter corpus tree."
  (cond
   ((org-to-tree-sitter-corpus--transform--function-symbol node)
    (funcall (org-to-tree-sitter-corpus--transform--function-symbol node) node))
   (t (user-error (format "Unmanaged node type: %s" (car node))))))

(defun org-to-tree-sitter-corpus--transform--function-symbol (element)
  "Return the function symbol that manages ELEMENT, or nil if absent."
  (let ((node-type (car element)))
    (symbol-function (intern (format "org-to-tree-sitter-corpus--transform-%s" node-type)))))

(defun org-to-tree-sitter-corpus--transform-org-data (data)
  "Transform DATA from an org-element root data to a tree-sitter corpus tree."
  (unless (eq (car data) 'org-data)
    (user-error (format "Expecting a org-data element, got %s" (car data))))
  (let ((children (cddr data)))
    (cl-remove-if #'null
                  (append '(org_data)
                          (mapcar #'org-to-tree-sitter-corpus--transform-node children)))))

(defun org-to-tree-sitter-corpus--transform-headline (headline)
  "Transform HEADLINE from an org-element headline to a tree-sitter corpus tree."
  (unless (eq (car headline) 'headline)
    (user-error (format "expecting a headline element, got %s" (car headline))))
  (let ((metadata (cadr headline))
        (children (cddr headline)))
    (cl-remove-if #'null
                  (append '(headline)
                          '((stars))
                          (when (plist-get metadata :commentedp) '((comment_marker)))
                          (when (plist-get metadata :priority) '((priority_level)))
                          (when (plist-get metadata :todo-keyword) '((todo_keyword)))
                          (when (plist-get metadata :title) '((title)))
                          (when (plist-get metadata :tags) '((tags)))
                          (mapcar #'org-to-tree-sitter-corpus--transform-node children)))))

(defun org-to-tree-sitter-corpus--transform-section (section)
  "Transform SECTION from an org-element section to a tree-sitter corpus tree."
  (unless (eq (car section) 'section)
    (user-error (format "expecting a section element, got %s" (car section))))
  (let ((children (cddr section)))
    (cl-remove-if #'null
                  (append '(section)
                          (mapcar #'org-to-tree-sitter-corpus--transform-node children)))))

(defun org-to-tree-sitter-corpus--transform-paragraph (paragraph)
  "Transform PARAGRAPH from an org-element paragraph to a tree-sitter corpus tree."
  (unless (eq (car paragraph) 'paragraph)
    (user-error (format "expecting a paragraph element, got %s" (car paragraph))))
  (let ((metadata (cadr paragraph)))
    (cl-remove-if #'null
                  (append
                   (if (plist-get metadata :results)
                       '(results)
                     '(paragraph))
                   ;; paragraph data is a non-readable struct that contains the text,
                   ;; therefore we only set the (paragraph) data there
                   ;; Maybe it will be different later
                   ))))

(defun org-to-tree-sitter-corpus--transform-src-block (src-block)
  "Transform SRC-BLOCK from an org-element src-block to a tree-sitter corpus tree."
  (unless (eq (car src-block) 'src-block)
    (user-error (format "expecting a src-block element, got %s" (car src-block))))
  (let ((metadata (cadr src-block)))
    (cl-remove-if #'null
                  (append '(src-block)
                          (when (plist-get metadata :language) '((language)))
                          (when (plist-get metadata :switches) '((switches)))
                          (when (plist-get metadata :parameters) '((parameters)))))))

(provide 'org-to-tree-sitter-corpus)
;;; org-to-tree-sitter-corpus.el ends here
