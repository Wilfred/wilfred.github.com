--- 
layout: post
title: "Trifle Lisp: Being Explicit With Strings"
---

What makes a good string datatype? What string features should a
programming language provide?

In this blog post, I will discuss the decisions behind strings in
Trifle. **Our goals are to be unambiguous, flexible, and Unicode
friendly**.

Supporting Unicode is crucial. Users need to be able to reason about
characters (which are Unicode code points) without worrying about
representation. Too many languages (e.g. Java, JavaScript) force you
to work out how many bytes each character is, in order to simply split
a string.

In Trifle, strings are simply sequences of characters. We also provide
a bytestring datatype for dealing with arbitrary bytes. This
separation allows users to clearly distinguish between handling text
and handling binary data. It ensures users know what encoding their
files are. It's too easy otherwise to assume ASCII and hope for the
best.

As a result of this design, I discovered that
[unix doesn't specify an encoding for file paths](http://unix.stackexchange.com/a/2111). Again,
the clear separation between bytes and text makes this unambiguous.

Trifle strings are also mutable. This is unconventional for a modern
scripting language, but it has two benefits. Firstly, it allows users
to treat strings exactly as lists. Trifle has no separate string API
-- you just use the normal list functions. Secondly, users do not need
to use special functions to manipulate strings efficiently. By
contrast, in Java, users are forced to use `StringBuilder` if they
want to build a string efficiently.

String literals in Trifle also offer a few noteworthy features. String
literals may contain newlines. For example:

    "I'm a string."
    
    "I am also a string, I can
    contain newlines with any ceremony."

Preventing newlines in strings is an arbitrary limitation that we
don't need. Trifle does also provide escape sequences, but it's much
stricter. Consider the following string:

    "\m\n\o"

What would you expect to see when you print this? Python guesses you
forgot to escape your backslashes, so it treats it as `"\\m\n\\o"`. C
treats it as `"m\no"` (with a warning). Whilst the Python behaviour
might be convenient for regular expressions, Python provides
rawstrings for exactly this purpose already. Trifle considers this a
syntax error rather than guessing what the user intended.

To sum up: Trifle presents strings as an abstract datatype, allowing
users to be clear about the data they're handling, and to manipulate
arbitrary international text with ease.
