;;; org-to-tree-sitter-corpus-tests.el --- Tests for org-to-tree-sitter-corpus -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Gerry Agbobada
;;
;; Author: Gerry Agbobada <https://github.com/gagbo>
;; Maintainer: Gerry Agbobada <gerry@gagbo.net>
;; Homepage: https://github.com/gagbo/org-to-tree-sitter-corpus
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Just a test file
;;
;;; Code:

(require 'org-to-tree-sitter-corpus)
(require 'org)
(require 'subr-x)

(defun ottsct--make-tree (content)
  "Build an org-element AST with CONTENT."
  (with-temp-buffer
    (insert content)
    (org-element-parse-buffer)))

(defun ottsct--make-test (content ts-tree)
  "Return a test that asserts that CONTENT is transformed to TS-TREE."
  (let ((actual-tree (thread-first content (string-trim-left) (ottsct--make-tree) (org-to-tree-sitter-corpus-transform-tree))))
    (should (equal actual-tree ts-tree))))

(ert-deftest ottsc-test-headline ()
  "Assert that headlines are correctly transformed."
  (ottsct--make-test
   "*"
   '(org_data (headline (stars))))
  (ottsct--make-test
   "* Title"
   '(org_data
     (headline (stars) (title))))
  (ottsct--make-test
   "* Title with tags :tag1:"
   '(org_data
     (headline (stars) (title) (tags))))
  (ottsct--make-test
   "* COMMENT Title"
   '(org_data
     (headline (stars) (comment_marker) (title))))
  ;; TODO: Enable this test and fix it
  ;; (ottsct--make-test "* TODO Title" '(org_data (headline (stars) (todo) (title))))

  ;; TODO: Make this pass, if it's legal org
  ;; (ottsct--make-test
  ;;  "* :tag1:"
  ;;  '(org_data
  ;;    (headline (stars) (tags))))
  )

(ert-deftest ottsc-test-paragraph ()
  "Assert that paragraphs are correctly transformed."
  (ottsct--make-test
   "
*
Sample text"
   '(org_data (headline (stars) (section (paragraph)))))
  (ottsct--make-test
   "
#+RESULTS: test
Sample text"
   '(org_data
     (section
      (results)))))

(ert-deftest ottsc-test-keyword ()
  "Assert that keywords are correctly transformed."
  (ottsct--make-test
   "#+TITLE: Some title"
   '(org_data (keyword (key) (value))))
  (ottsct--make-test
   "
#+TITLE: Some title
#+SUBTITLE: I'm not over"
   '(org_data (keyword (key) (value))
              (keyword (key) (value)))))

;; TODO: Add support for those
(ert-deftest ottsc-test-drawers ()
  "Assert that drawers are correctly transformed."
  (ottsct--make-test
   "
* Random drawer
Text at the beginning
:CUSTOM:
Hidden arbitrary drawer
:END:
Another paragraph"
   '(org_data
     (headline
      (stars)
      (section
       (paragraph)
       (drawer (paragraph))
       (paragraph)))))
  ;; Misplaced Properties drawer is still a drawer
  (ottsct--make-test
   "
* Random drawer
Text at the beginning
:PROPERTIES:
Hidden arbitrary drawer
:END:
Another paragraph"
   '(org_data
     (headline
      (stars)
      (section
       (paragraph)
       (drawer (paragraph))
       (paragraph)))))
  (ottsct--make-test
   "
* Random drawer
:PROPERTIES:
:key: Value
:END:
The paragraph"
   '(org_data
     (headline
      (stars)
      (section
       (property_drawer (node_property (key) (value)))
       (drawer (paragraph))
       (paragraph))))))

(provide 'org-to-tree-sitter-corpus-tests)
;;; org-to-tree-sitter-corpus-tests.el ends here
