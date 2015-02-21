--- 
layout: post
title: "My First LLVM Compiler"
---

_A walkthrough of writing a basic compiler with LLVM. No prior
experience assumed._

In a short space of time, I was able to go from zero C++ knowledge,
and no experience with LLVM, to a fully-fledged compiler. It's a lot
of fun, let me show you how!

Our compiler will accept programs written in
[BF](https://en.wikipedia.org/wiki/Brainfuck). This is a classic toy
language for compilers, and there is even a BF compiler in
[LLVM's examples directory](http://llvm.org/viewvc/llvm-project/llvm/trunk/examples/)! In
this post, I'll explain the process of writing it.

## A first LLVM program

The LLVM toolchain is built around programs written in
[LLVM IR](http://llvm.org/docs/LangRef.html). First, we'll write a
basic LLVM IR program that just exits.

Since LLVM tools already use LLVM IR, we can simply use the
`clang` compiler to show us what a basic program looks like. Here's a
simple C program:

{% highlight c %}
int main() {
    return 42;
}
{% endhighlight %}

We run this through Clang:

{% highlight bash %}
# -O3 ensures we discard any unnecessary instructions.
$ clang -S -emit-llvm -O3 forty_two.c
{% endhighlight %}

This creates a file `forty_two.ll` which looks like this:

{% highlight llvm %}
define i32 @main() {
  ret i32 42
}
{% endhighlight %}

Our first LLVM program! We can use `lli` to run it:

{% highlight bash %}
$ lli forty_two.ll 
$ echo $?
42
{% endhighlight %}


## Writing a skeleton

We'll compile BF commands to sequences of LLVM IR instructions.
However, those instruction will need to be inside a `main` function so
LLVM knows our entry point. We will also need to allocate and
initialise our memory cells and cell index.

Again, we can simply write the equivalent C and look at the LLVM IR
instructions Clang generates. Here's the skeleton we'll use:

{% highlight llvm %}
declare i8* @calloc(i32, i32)
declare void @free(i8*)

define i32 @main() {
  ; Allocate 30,000 cells on the heap.
  %cells = call i8* @calloc(i32 30000, i32 1)

  ; Allocate a stack variable to track the cell index.
  %cell_index_ptr = alloca i32
  ; Initialise it to zero.
  store i32 0, i32* %cell_index_ptr

  ;;;;;;;;;;;;;;;;;;;;
  ; Our BF code will go here!
  ;;;;;;;;;;;;;;;;;;;;

  ; Free the memory for the cells.
  call void @free(i8* %cells)
  ret i32 0
}
{% endhighlight %}

## Hand-compiling &gt;

`>` is the easiest BF command to compile. Let's fire up a text editor
and write out the LLVM IR equivalent.

If your BF is rusty, `>` simply increments the cell index.

{% highlight llvm %}
%cell_index = load i32* %cell_index_ptr
%new_cell_index = add i32 1, %cell_index
store i32 %new_cell_index, i32* %cell_index_ptr
{% endhighlight %}

To check this code is correct, we want to run it. We can simply
[wrap it in our skeleton](https://github.com/Wilfred/Brainfrack/blob/0252dc84791f5461dc8da5a67b755bd48b7cc0b4/llvm/increment_data_pointer.ll)
and run with it with `lli` to see what happens. Implementing `<` is
now straightforwards, and we write a
[test program for `<`](https://github.com/Wilfred/Brainfrack/blob/0252dc84791f5461dc8da5a67b755bd48b7cc0b4/llvm/decrement_data_pointer.ll)
too.

## Hand-compiling +

BF's `+` command increments the current cell. This
requires us to dereference the current cell, calculate new value, then
store it. In C, this would look like:

{% highlight c %}
char *cell_ptr = cells + cell_index;
char current_value = *cell_ptr;
char new_value = current_value + 1;
*cell_ptr = new_value;
{% endhighlight %}

LLVM provides the
[getelementptr instruction](http://llvm.org/docs/LangRef.html#getelementptr-instruction)
to calculate the pointer. The
translation then looks like this:

{% highlight llvm %}
%cell_index = load i8* %cell_index_ptr
%cell_ptr = getelementptr i8* %cells, i8 %cell_index

%current_value = load i8* %cell_ptr
%new_value = add i8 %current_value, 1
store i8 %new_value, i8* %cell_ptr
{% endhighlight %}

Again, we test this by
[wrapping it in our skeleton](https://github.com/Wilfred/Brainfrack/blob/a86c4ee50e35d38e3cb3cb826c1d68de8898ef06/llvm/increment.ll),
and [do the same for `-`](https://github.com/Wilfred/Brainfrack/blob/a86c4ee50e35d38e3cb3cb826c1d68de8898ef06/llvm/decrement.ll).

## I/O

BF has two I/O commands: `,` reads from stdin into a cell, and `.`
writes from a cell onto stdout. We need to call C functions for this: `putchar`
and `getchar`.

We need to declare these functions, as we did with `malloc` earlier:

{% highlight llvm %}
declare i32 @putchar(i32)
declare i32 @getchar()
{% endhighlight %}

To implement `,` we call `getchar`, truncate it to a char, and write it
to the current cell.

{% highlight llvm %}
%cell_index = load i32* %cell_index_ptr
%cell_ptr = getelementptr i8* %cells, i32 %cell_index

%input_int = call i32 @getchar()
%input_byte = trunc i32 %input_int to i8
store i8 %input_byte, i8* %cell_ptr
{% endhighlight %}

`.` is the reverse: we read the cell, sign extend it, then call `putchar`.

{% highlight llvm %}
%cell_index = load i32* %cell_index_ptr
%cell_ptr = getelementptr i8* %cells, i32 %cell_index

%current_cell = load i8* %cell_ptr
%current_cell_word = sext i8 %current_cell to i32
call i32 @putchar(i32 %current_cell_word)
{% endhighlight %}

## Loops

LLVM IR instructions are organised into basic blocks; sequences of
instructions that don't contain jumps. At the end of each basic block,
you must either jump to another basic block or return.

To compile `[ x ] y`, we need to inspect the current cell,
then either jump to `x`, which is our loop body, or to `y`. At the end of `x`,
we need to jump back to the start.

{% highlight llvm %}
loop_header:
  %cell_index = load i32* %cell_index_ptr
  %cell_ptr = getelementptr i8* %cells, i32 %cell_index
  %cell_value = load i8* %cell_ptr
  %is_zero = icmp eq i8 %cell_value, 0
  br i1 %is_zero, label %loop_after, label %loop_body

loop_body:
  ; x
  br label %loop_header

loop_after:
  ; y
{% endhighlight %}

Note that this is recursive, `x` may also contain loops. There's a
[sample loop program here](https://github.com/Wilfred/Brainfrack/blob/5a2f613f9e82bfd57be687aa6a67aca15d3d9861/llvm/loop.ll).

## Putting it all together

We've done the hard bit! All that's left is to use the LLVM API
generate these instructions. Each IR instruction has a corresponding
C++ object that you can instantiate and add to your basic block.

LLVM's API also has the convenient concept of an
[IRBuilder](http://llvm.org/docs/doxygen/html/classllvm_1_1IRBuilder.html).
The `IRBuilder` class provides a create method for every IR instruction, making IR
generation easy.

Here's the C++ for generating LLVM IR for `>`. The
[excellent LLVM tutorial](http://llvm.org/docs/tutorial/LangImpl3.html#full-code-listing)
includes instructions for compiling a basic C++ program that uses LLVM APIs.

{% highlight cpp %}
BasicBlock *BB;

// Instantiate an IRBuilder that appends to our
// current basic block.
IRBuilder<> Builder(getGlobalContext());
Builder.SetInsertPoint(BB);

// We want to increment by 1, but since cell_index is
// 32-bit, our constant must be 32-bit too.
Value *IncrementAmount =
    ConstantInt::get(getGlobalContext(), APInt(32, 1));

// Emit the load, add and store instructions.
Value *CellIndex = Builder.CreateLoad(CellIndexPtr, "cell_index");
Value *NewCellIndex =
    Builder.CreateAdd(CellIndex, IncrementAmount, "new_cell_index");
Builder.CreateStore(NewCellIndex, CellIndexPtr);
{% endhighlight %}

Compiling the other BF commands is a simple translation of our hand-compiled snippets. You can
[view the full working compiler here](https://github.com/Wilfred/Brainfrack/blob/5a2f613f9e82bfd57be687aa6a67aca15d3d9861/llvm/compiler.cpp).

## Emitting machine code

Finally, our compiler is only emitting LLVM IR. A proper compiler
emits machine code. We can use `llc` to convert to an object file,
then link it to produce an executable.

{% highlight bash %}
$ ./compiler hello_world.bf
$ llc -filetype=obj hello_world.ll
$ gcc hello_world.o -o hello_world
$ ./hello_world
Hello World!
{% endhighlight %}

And that's all there is to it!

## Addendum: Optimising

There's lots that can be done to produce faster executable from BF
programs. However, LLVM is already smart enough to compile simple loop-free BF
programs to optimal LLVM IR!

{% highlight bash %}
$ cat exclamation.bf 
+++++ +++++
+++++ +++++
+++++ +++++
+++ ASCII 33 is '!'
. Write ! to stdout
$ ./compiler exclamation.bf 
$ opt -S -O3 exclamation.ll -o optimised_exclamation.ll
{% endhighlight %}

This produces:

{% highlight llvm %}
define i32 @main() {
entry:
  %0 = tail call i32 @putchar(i32 33)
  ret i32 0
}
{% endhighlight %}
