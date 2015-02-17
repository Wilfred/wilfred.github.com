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
-emit-llvm forty_two.c`. This looks like the following:

{% highlight c %}

define i32 @main() {
  %1 = alloca i32, align 4
  store i32 0, i32* %1
  ret i32 42
}
{% endhighlight %}


