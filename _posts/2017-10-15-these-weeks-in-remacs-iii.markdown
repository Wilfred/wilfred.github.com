--- 
layout: post
title: "These Weeks in Remacs III"
---

Time for another Remacs update!

## Contributing

Since the [last update](/blog/2017/07/15/these-weeks-in-remacs-ii/),
we've seen contributions from lots of new people. We've added
@brotzeit and @shanavas786, bringing us to seven wonderful people who
can approve your PRs.

Speaking of PRs, we've merged an amazing 64 pull requests since the
last update!

If you're looking for a good feature for your first contribution,
Brotzeit has been regularly adding
[new suggestions under the 'good first issue' label](https://github.com/Wilfred/remacs/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22).

## Features

Many Emacs features have now been ported to Rust, with new Rust APIs
for accessing elisp datastructures.

Random: 

Arithmetic:
[floating point arithmetic](https://github.com/Wilfred/remacs/pull/234/files), 
[random number generation](https://github.com/Wilfred/remacs/pull/235/files) (using
a Rust RNG!),
[comparisons](https://github.com/Wilfred/remacs/pull/226/files),

Symbols:
[symbol properties](https://github.com/Wilfred/remacs/pull/235/files), 
[interning](https://github.com/Wilfred/remacs/pull/255)

Hashes: [MD5sum](https://github.com/Wilfred/remacs/pull/260/files)
(using a Rust MD5 crate)

Type checks: 

Windows:
[liveness check](https://github.com/Wilfred/remacs/pull/258/files),
[type check](https://github.com/Wilfred/remacs/pull/240/files)

Processes: [1](https://github.com/Wilfred/remacs/pull/256/files)

Buffers:
[buffer for the current thread](https://github.com/Wilfred/remacs/pull/253),
[accessing](https://github.com/Wilfred/remacs/pull/242)

Hash tables: [copying](https://github.com/Wilfred/remacs/pull/251/files)

Misc:
[prefix arguments](https://github.com/Wilfred/remacs/pull/252/files)

Fonts: [type checks](https://github.com/Wilfred/remacs/pull/248)

Characters: [multibyte conversions](https://github.com/Wilfred/remacs/pull/236/files)

## Rusty APIs

## Interesting Issues

Markers

Porting hash map.

## Logo

https://github.com/Wilfred/remacs/issues/358
