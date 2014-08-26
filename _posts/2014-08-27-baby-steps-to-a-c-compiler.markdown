--- 
layout: post
title: "Baby Steps to a C Compiler"
---

Writing a compiler, and learning a little low-level programming, is a
very powerful way of learning more about how computers work.

Compilers are often seen as daunting projects. Indeed, a
production-grade compiler is a huge task. Writing a crude but working
compiler is not.

The secret to getting started is to find a minimal viable project, and
expand your featureset from there. This is the approach discussed in
[An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf),
and it works really well. You only need to complete step 1, and you
have a compiler! It's only compiling a tiny subset of the language,
but it's a bona fide compiler. You can expand your compiler as much or
as little as you like -- you will be a better programmer with a deeper
knowledge as a result.

Inspired by this paper, I have been writing a C compiler. This is a
little harder in some ways (you have to parse C) but easier in others
(you don't need runtime type information). All you need to get started
is your Minimal Viable Compiler.

For my compiler, [babyc](https://github.com/Wilfred/babyc), I chose
this as my first basic program:

{% highlight python %}
int main() {
    return 2;
}
{% endhighlight %}

No variables, function calls, external dependencies, if statements, or
loops. Not so scary.

We need to parse this first. We'll use Flex and Bison. There are
[example C99 grammars](https://gist.github.com/codebrainz/2933703)
that we can look at, but our entire grammar is tiny. Here's the lexer:

    "{" { return '{'; }
    "}" { return '}'; }
    "(" { return '('; }
    ")" { return ')'; }
    ";" { return ';'; }
    [0-9]+ { return NUMBER; }
    "return" { return RETURN; }
    "int" { return TYPE; }
    "main" { return IDENTIFIER; }

Here's the grammar:

    function:
    	TYPE IDENTIFIER '(' ')' '{' expression '}'
    	;
        
    expression:
    	RETURN NUMBER ';'
    	;

Finally, we need to generate some assembly. We'll use 32-bit x86
assembly, because it's extremely common and it probably runs natively
on your current machine. There's a
[great x86 reference site here](http://x86.renejeschke.de/).

Here's the assembly file we need to generate:

    .text
            .global _start # Tell the loader we want to start at _start.

    _start:
            movl    $2,%ebx # The argument to our system call.
            movl    $1,%eax # The system call numer of sys_exit is 1.
            int     $0x80 # Send an interrupt

Hook up the lexer and parser, write this assembly to a file, and
congratulations! You're a compiler writer!

[Babyc started out like this, and you can see this minimal version here](https://github.com/Wilfred/babyc/tree/dffc393f3254468acfbb3539c2e0f8c464b40464).

Of course, the assembly file is no good if you can't run it. Let's
check it actually works as it should. If you've saved the file as
`out.s`:

    # Assemble the file. We explicitly assemble as 32-bit
    # to avoid confusion on x86_64 machines.
    $ as out.s -o out.o --32
    
    # Link the file, again specifying 32-bit.
    $ ld -m elf_i386 -s -o out out.o
    
    # Run it!
    $ ./out

    # What was the return code?
    $ echo $?
    2 # Woohoo!

From here, the sky's the limit. You can work through the Incremental
Approach paper and gradually make your compiler more
sophisticated. You'll need to build a more elaborate parse tree, then
walk it to generate assembly. The next steps are (1) allowing
arbitrary return values (`return 3;`) and then (2) adding support for
negating values (`return ~1;`). Each additional step will teach you
more about C, more about your computer works, and more of the world of
compilers.

This is the approach babyc is taking. Babyc now has if statements, loops,
variables, and basic arithmetic. You're welcome to
[check out the code](https://github.com/Wilfred/babyc), but I hope
I've tempted you to have a try yourself.

Don't fear the low-level stuff. It's a fascinating world down here.
