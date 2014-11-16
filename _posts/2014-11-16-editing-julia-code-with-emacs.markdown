--- 
layout: post
title: "Editing Julia code (with Emacs!)"
tags:
 - emacs
---

I'm a big admirer of the Julia programming language: it's a fast
general-purpose language with a nice syntax, macros, and a decent
package manager.

No respectable up-and-coming language should be without good editor
support. I've been polishing the Emacs mode, and learnt a lot about
the language. If you're writing Julia code, or integrating an editor,
this should be interesting to you.

## Syntax highlighting

Syntax highlighting is hard, and it's rather challenging in
Julia. We'll look at some corner cases of syntax highlighting in
Julia, so I'll show code snippets along with screenshots of how this
code is currently highlighted in Emacs.

I've written a
[complete Julia syntax highlighting test file](https://gist.github.com/Wilfred/f1aca44c61ed6e1df603)
which exercises all the different syntactic features of the
language. You can use this to test Julia support in your editor of choice.

### Highlighting function calls

Julia supports two ways of declaring functions, which the docs
describe as 'basic' and 'terse'.

    function f(x,y)
        x + y
    end

    f(x,y) = x + y

We want to highlight keywords (such as `function` and `end`) and to
highlight function names (`f` in this example). This is pretty
straightforward: we can write a regular expression that spots either
the keyword `function` or a symbol followed by something roughly like
`(.*?) =`.

<img src="/assets/julia_functions.png">

We can also define functions in an explicit namespace. This is also
straightforward, we just highlight the last symbol after the dot.

    function Foo.bar(x, y)
        x + 1
    end

<img src="/assets/julia_function_namespace.png">

A function definition may also include type variables. This isn't too
difficult to handle either, we just need to adjust our terse regular
expression to step over the curly brackets.

    elsize{T}(::AbstractArray{T}) = sizeof(T)

    function elsize{T}(::AbstractArray{T})
        sizeof(T)
    end

<img src="/assets/julia_function_type_vars.png">

However, highlighting gets harder with nested brackets. 

    cell(dims::(Integer...)) = Array(Any, convert((Int...), dims))

At this point, our naive regular expression falls down. We need to
count brackets, or write a crude parser. The Emacs editing mode doesn't
yet handle this case.

### Macro usage

Highlighting macros is easy. There are some
[awkward syntactic edge cases](https://github.com/JuliaLang/julia/issues/7232)
but these don't affect highlighting.

    @hello_world! foo

<img src="/assets/julia_macro.png">

### Built-in functions

Julia has a lot of built-in functions. After some discussion, we
felt that it wasn't worth special-casing functions that are
keywords in other languages, such as `throw` and `error`.

    throw(foo)
    error("foo", bar, "baz")

<img src="/assets/julia_builtins.png">

### Strings and characters

Julia has a lovely syntax here, but it takes a little care to
highlight correctly.

For characters, Julia uses single quotes, but it also supports `'` as
an operator. This gives very readable mathematical formulae.

    # Characters
    x = 'a'
    y = '\u0'

    # Not characters
    a = b' + c'

<img src="/assets/julia_characters.png">

Julia's string syntax allows multi-line strings, triple-quoted strings,
regular expression literals, byte array literals and (particularly
nifty) version number literals.

    x = "foo
    bar"
    x = """hello world"""
    x = "hello $user"
    x = r"foo.*"ismx
    x = v"0.1"
    x = b"DATA\xff\u2200"

<img src="/assets/julia_strings.png">

We are handling most of this syntax in the Emacs mode, but it's not
perfect yet. I think we should highlight interpolated values in
strings. See my test file for a full set of examples.

### Comments

Julia's comment syntax is also very nice. There are single-line and
multi-line comments, and they support arbitrary nesting.

    # I'm a comment.

    #= I'm a 
    multi-line comment. =#

    #= I'm a #= nested =# comment. =#

<img src="/assets/julia_comments.png">

Emacs makes it easy for us to support all this different variants, so
we've supported this for a long time.

### Type declarations

You can declare your own types in Julia.

    type Foo
        x::Bar
    end
    immutable Foo
        x::Bar
    end

    abstract Foo <: Bar

<img src="/assets/julia_type_decl.png">

This is mostly a case of knowing all the keywords for type
declaration, so it's straightforward.

The operator `<:` is particularly tricky. It is used in type
declarations to declare subtypes, but it's also used a boolean
operator to see if one value is a subtype of another `x <: y`. I
believe this is
[impossible to highlight correctly in all cases](https://github.com/JuliaLang/julia/pull/7963#issuecomment-52137831).

    # I can't see how to highlight the first 'T' here.
    same_type_numeric{T<:Number}(x::T, y::T) = true

We can cheat by having a full list of built-in types in our
highlighting code, so we highlight most subtype declarations
correctly.

### Type annotations

Julia supports optional type annotations in functions and on
variables. These are simple to highlight, but we need to get `::`
right before dealing with quoted symbols.

    f(x::FooBar) = x

    function foo()
        local x::Int8 = 5
        x
    end

<img src="/assets/julia_type_annotations.png">

### Variable declarations

Julia has a `local` keyword which lets you introduce local variable
bindings. I'd love to highlight this correctly too.

    global x = "hello world", y = 3

    let x = 1
        x + 1
    end

    function foo()
        local x = 5
        x + 1
    end

This requires parsing to handle correctly, so we don't handle it
yet. We can't simply look for commas, as there may be arbitrary Julia
expressions used.

    # 'b' is not declared as a variable here.
    global x = foo(a, b), y = 3

### Colons and quoting

The hardest part of Julia's syntax is `:`. There have also been [users
confused by this syntax](https://github.com/JuliaLang/julia/issues/5997).

    # Quoted symbols
    x = :foo
    y = :function
    foo[:baz]
    [1 :foo]

    # Not quoted symbols
    foo[bar:end]
    foo[bar:baz]
    x = :123
    for x=1:foo
        print(x)
    end

I've
[opened a pull request](https://github.com/JuliaLang/julia/pull/9024)
that enables Emacs to handle the most common usages correctly, but
this is very hard to get right in all cases.

### Numbers

Finally, Julia has a really neat numeric syntax. It supports all the
literals you could possibly want. It also lets you write `2x` as a
shorthand for `2 * x`, which makes many equations in Julia much more
similar to a maths textbook.

    x = 0x123abcdef
    x = 0o7
    x = 0b1011
    x = 2.5e-4

    # Equivalent to '2 * x'
    y = 2x

The Emacs mode currently doesn't highlight these, but we probably
should. Some Emacs modes highlight numbers, some don't, but for a
language with a focus on scientific computing, it would make sense to
highlight numbers. It's particularly helpful to help readers see that
`2x` is two separate symbols.

## Conclusions

Julia's syntax isn't completely set in stone, but I doubt much of the
syntax will change in ways that affect highlighting. The syntax
favours readability over simple parsing (a great tradeoff), so writing
a highlighter takes some careful thought.

Once you've got syntax highlighting working, it's much easier to
handle indentation. I think Emacs' ability to indent Julia is pretty
good (this blog post is plenty long enough without getting into
indentation) and this is because it can fairly robustly identify block
delimiters for highlighting.

Finally, it's also desirable to have as-you-type syntax checking and
linting. [Flycheck will add support for this using Lint.jl](https://github.com/flycheck/flycheck/pull/510)
as soon as Lint.jl/Julia performance is good enough to run on demand
without a persistent process.

If you do encounter a bug with Emacs and Julia, there's a
['julia-mode' issue label](https://github.com/JuliaLang/julia/labels/julia-mode)
to track any bugs.

Happy hacking!
