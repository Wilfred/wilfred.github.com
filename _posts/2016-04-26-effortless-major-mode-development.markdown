--- 
layout: post
title: "Effortless Major Mode Development"
---

Emacs today has great tooling to help you develop reliable major modes
without forcing your favourite libraries on your users. In this post,
I showcase a few tips on some tools you may have overlooked.

## Use regexp-opt

As of Emacs 24, `regexp-opt` takes a `'symbols` option. You should write
your font lock keywords like this:

(defvar cask-mode-font-lock-keywords
  `((,(regexp-opt
       ;; Full list taken from http://cask.readthedocs.org/en/latest/guide/dsl.html
       '("package" "package-file" "files" "depends-on" "development" "source")
       'symbol)
     . font-lock-keyword-face)

This has two advantages. By whitelisting keywords, users can quickly
spot mistakes when editing:

<img src="/assets/cask_incorrect_keyword.png">

This also prevents a classic bug where Emacs highlights substrings
that happen to be keywords:

<img src="/assets/cask_highlight_substring.png">

## (Don't) use company

Company is quickly becoming the most popular completion backend for
Emacsers. However, not everyone uses it, and ideally you wouldn't
force company on people who don't use it.

You can have the best of both worlds by using
`completion-at-point-functions`. Your completion functionality will
work in stock Emacs, and company users will benefit too through
`company-capf`.

See racer.el for a great example of this.

Company is excellent, and 

## Write tests

Finally, you should write tests to verify that indentation and
highlighting work as intended.

This used to be awkward, but the
[assess](https://github.com/phillord/assess) library has made writing
tests much easier. It provides tons of assertions that produce useful
error messages when they fail.

For example, here's a simple indentation test from [cask-mode]():

(ert-deftest cask-mode-indent-inside-development ()
  "Ensure we correctly indent inside (development ...) blocks."
  (should (assess-indentation=
           'cask-mode
           "(development\n(depends-on \"foo\"))"
           "(development\n (depends-on \"foo\"))")))
           
Highlighting is particularly helped by assess:

(ert-deftest cask-mode-highlight-sources ()
  "Ensure we highlight known values for source."
  (should (assess-face-at=
           "(source melpa)"
           'cask-mode
           "melpa"
           'cask-mode-source-face)))
           
Here's an example of the assertion error message:

      #("Face does not match expected value
	Expected: cask-mode-source-face
	Actual: font-lock-keyword-face
	Location: 9
	Line Context: (source melpa)
	bol Position: 1
