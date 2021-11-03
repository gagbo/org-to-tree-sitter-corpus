#!/usr/bin/env bash

set -e

EMACS="${EMACS:=emacs}"

NEEDED_PACKAGES="org"

INIT_PACKAGE_EL="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (push '(\"elpa\" . \"https://elpa.gnu.org/packages/\") package-archives) \
  (package-initialize) \
  (unless package-archive-contents \
     (package-refresh-contents)) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

# Refresh package archives
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL"

# Run tests
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l org-to-tree-sitter-corpus.el \
         -l org-to-tree-sitter-corpus-tests.el \
         -f ert-run-tests-batch-and-exit
