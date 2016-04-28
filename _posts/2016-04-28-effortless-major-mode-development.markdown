--- 
layout: post
title: "Effortless Major Mode Development"
tags:
 - emacs
---

It's now easier than ever to write major modes in Emacs. If you
haven't written a major mode in a while, or you're just starting out,
here are my top tips:

## Use regexp-opt

As of Emacs 24, `regexp-opt` takes a `'symbols` option. You should write
your font-lock keywords like this:

{% highlight common-lisp %}
(defvar js-mode-font-lock-keywords
  `((,(regexp-opt
       '("var" "for" "function" "if" "else")
       'symbols)
     . font-lock-keyword-face)
{% endhighlight %}

This has two advantages. By whitelisting keywords, users can quickly
spot mistakes when editing:

<figure>
<img src="/assets/mispelled_keyword.png">
</figure>

This also prevents a classic bug where Emacs highlights substrings
that happen to be keywords:

<figure>
<img src="/assets/keyword_substring.png">
</figure>

## (Don't) use company

Company is excellent, and I highly recommend it. However, not all
Emacsers use company. You don't need to force company on your users.

Instead, you can use `completion-at-point-functions`. Your completion functionality will
work in stock Emacs, and company users will benefit too through
`company-capf`.

Ah, but what about all the extra annotations you can supply to
company? We can have our cake and eat it too:

{% highlight common-lisp %}
(defun racer-complete-at-point ()
  "Complete the symbol at point."
  (unless (nth 3 (syntax-ppss)) ;; not in string
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (beg (or (car bounds) (point)))
           (end (or (cdr bounds) (point))))
      (list beg end
            (completion-table-dynamic #'racer-complete)
            :annotation-function #'racer-complete--annotation
            :company-prefix-length (racer-complete--prefix-p beg end)
            :company-docsig #'racer-complete--docsig
            :company-location #'racer-complete--location))))
{% endhighlight %}

## Test with assess

Historically, it's been rather awkward to test major modes. Many
authors didn't bother.

That's all changed with the release of
[assess](https://github.com/phillord/assess). Assess provides great
assertions with readable error messages.

For example, here's a simple indentation test from [cask-mode](https://github.com/Wilfred/cask-mode):

{% highlight common-lisp %}
(ert-deftest cask-mode-indent-inside-development ()
  "Ensure we correctly indent inside (development ...) blocks."
  (should (assess-indentation=
           'cask-mode
           ;; before:
           "
(development
(depends-on \"foo\"))"
           ;; after:
           "
(development
 (depends-on \"foo\"))")))
{% endhighlight %}

Highlighting is particularly helped by assess:

{% highlight common-lisp %}
(ert-deftest cask-mode-highlight-sources ()
  "Ensure we highlight known values for source."
  (should (assess-face-at=
           "(source melpa)"
           'cask-mode
           "melpa"
           'cask-mode-source-face)))
{% endhighlight %}

If this test fails, we get a helpful message describing which faces
were actually used:
           
{% highlight common-lisp %}
#("Face does not match expected value
   Expected: cask-mode-source-face
   Actual: font-lock-keyword-face
   Location: 9
   Line Context: (source melpa)
   bol Position: 1"
{% endhighlight %}

Do you have any tips for major mode development?
[Leave a comment on the reddit discussion](#). 

*If you haven't written your first major mode yet, check out
[adding a new language to Emacs](/blog/2015/03/19/adding-a-new-language-to-emacs/)*.
