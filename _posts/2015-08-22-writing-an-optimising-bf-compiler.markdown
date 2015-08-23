--- 
layout: post
title: "Writing an Optimising BF Compiler"
---

[BF](https://en.wikipedia.org/wiki/Brainfuck) is a wonderful
language. It's Turing compiler, but only supplies six instructions,
and many interpreters and compilers exist. LLVM and libjit both
provide tutorials for writing a basic BF compiler with their toolsets!

However, what would an industrial strength compiler look like? I set
out to write a highly optimising compiler, and it's screaming
fast. Let's take look.

## Baseline Performance

I've written a basic BF interpreter in C, so we'll use that as our
reference point for BF performance. Let's compare our compiler,
without optimisations, with this interpreter.

As you can see, even without optimisations, our compiler is
comfortably outperforming the interpreter.

## Optimising Increments

Let's look at some peephole optimisations. Consider the BF program `++`. This increments current cell
twice. Our baseline compiler would convert this to:

    cell_val := load(cell_ptr)
    cell_val := cell_val + 1
    store(cell_ptr, cell_val)
    cell_val := load(cell_ptr)
    cell_val := cell_val + 1
    store(cell_ptr, cell_val)

However, the underlying computer is capable of adding numbers other
than one! We expand our intermediate representation (IR) to handle
increments, so we can compile this to:

    cell_val := load(cell_ptr)
    cell_val := cell_val + 2
    store(cell_ptr, cell_val)


