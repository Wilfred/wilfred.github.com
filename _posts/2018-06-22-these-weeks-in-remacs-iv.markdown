--- 
layout: post
title: "These Weeks in Remacs IV"
---

It's been a while since the last Remacs update, but the Remacs project
is thriving!

Function counts.

Number of contributors.

Files removed entirely like 
cmd.c: https://github.com/Wilfred/remacs/pull/739 (also allows-var-definitions)
floatfns.c: https://github.com/Wilfred/remacs/issues/568

Periodic roll-ups with upstream Emacs master. Latest is
https://github.com/Wilfred/remacs/pull/724 and https://github.com/Wilfred/remacs/pull/729

Unit tests of Rust code: https://github.com/Wilfred/remacs/pull/537

Emacs now understands if primitives are written in Rust: https://github.com/Wilfred/remacs/pull/539

remacs supports emoji: https://github.com/Wilfred/remacs/pull/567

Keymaps: https://github.com/Wilfred/remacs/pull/660

windows, e.g. https://github.com/Wilfred/remacs/pull/680

loading and requiring: https://github.com/Wilfred/remacs/pull/635
https://github.com/Wilfred/remacs/pull/616

macroexpansion: https://github.com/Wilfred/remacs/pull/606/files
(including tests!)

while: https://github.com/Wilfred/remacs/pull/605
eval: https://github.com/Wilfred/remacs/pull/681

let/let* https://github.com/Wilfred/remacs/pull/604
boundp: https://github.com/Wilfred/remacs/pull/676

prog1/prog2 https://github.com/Wilfred/remacs/pull/599

markers: https://github.com/Wilfred/remacs/pull/666

self-insert-command! https://github.com/Wilfred/remacs/pull/707

buffers: https://github.com/Wilfred/remacs/pull/697

overlays: https://github.com/Wilfred/remacs/pull/721

minibuffer: https://github.com/Wilfred/remacs/pull/723

## Bindgen

https://github.com/Wilfred/remacs/pull/781

## Safety First

https://github.com/Wilfred/remacs/pull/753

## Platform Support

Windows: https://github.com/Wilfred/remacs/pull/445
macOs: https://github.com/Wilfred/remacs/pull/471

## simpler types

Removed Lisp_Object vs LispObject distinction
https://github.com/Wilfred/remacs/pull/730 as repr transparent has
landed https://github.com/Wilfred/remacs/issues/568

## Better macros

proc macros, can use option or boolean directly in rust, still needs
nightly

Automatic types: https://github.com/Wilfred/remacs/pull/547
