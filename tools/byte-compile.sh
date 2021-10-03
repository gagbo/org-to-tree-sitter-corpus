#!/usr/bin/env bash

set -e

EMACS="${EMACS:=emacs}"

NEEDED_PACKAGES="org"

INIT_PACKAGE_EL="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (unless package-archive-contents \
     (package-refresh-contents)) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

# Refresh package archives
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL"

# Byte-compile
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         --eval "(setq byte-compile-error-on-warn t)" \
         -f batch-byte-compile \
         org-to-tree-sitter-corpus.el
