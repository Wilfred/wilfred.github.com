--- 
layout: post
title: "Getting Started With LLVM"
---

_A walkthrough of writing a basic compiler with LLVM. No prior
experience assumed._

LLVM is a great project to work with: the people are friendly, the
docs are good, and the technology itself is excellent. It's a great
tool to get started in compiler writing.

I decided to write a [BF](https://en.wikipedia.org/wiki/Brainfuck)
compiler to explore the API. In spite of a total lack of C++
experience, I was able to develop a bona fide compiler!

The LLVM team provide a great
[tutorial](http://llvm.org/docs/tutorial/index.html) and there already
a BF compiler in
[LLVM's examples directory](http://llvm.org/viewvc/llvm-project/llvm/trunk/examples/). In
this post, I'll focus on the steps needed to get up and running.

## Prerequisites

You will need LLVM itself (I'm using 3.5.1) and Clang.

## A first LLVM program

First, we need to write a skeleton LLVM program. You'll want to take a
look at the
[LLVM IR Reference Manual](http://llvm.org/docs/LangRef.html) to get a
feel of how LLVM IR works. However, **you can cheat and have Clang
write the LLVM IR for you**!

For our initial program, we'll just exit with a return code
of 42. This is easy in C:

{% highlight c %}
int main() {
    return 42;
}
{% endhighlight %}

We can generate a `forty_two.ll` file by simply running `clang -S
-emit-llvm -O3 forty_two.c`. We specify `-O3` to avoid getting
redundant instructions in our main function. Our main function now
looks like this:

{% highlight llvm %}
define i32 @main() {
  ret i32 42
}
{% endhighlight %}

Easy! We can use `lli` to run this file:

```
$ lli forty_two.ll 
$ echo $?
42
```

## Working out what to generate

Now we know how to write and run an LLVM IR program, we need to figure
out what LLVM IR we want to generate.

{% highlight llvm %}
define i32 @main() nounwind {
       %cells = call i8* @calloc(i32 3000, i32 1)
       %cell_index = alloca i8
       store i8 0, i8* %cell_index

       %cell_index_val = load i8* %cell_index

       ; we implement the BF program '+'
       %cell_ptr = getelementptr i8* %cells, i8 %cell_index_val
       %tmp = load i8* %cell_ptr
       %tmp2 = add i8 %tmp, 1
       store i8 %tmp2, i8* %cell_ptr

       ; exit the stored value, as a sanity check
       %exit_code_byte = load i8* %cell_ptr
       %exit_code = zext i8 %exit_code_byte to i32
       ret i32 %exit_code
}
{% endhighlight %}
