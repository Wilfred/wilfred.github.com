--- 
layout: post
title: "An Optimising BF Compiler"
---

Almost everyone's written a
[BF](https://en.wikipedia.org/wiki/Brainfuck) implementation these
days. In fact, BF is now so widespread that LLVM and libjit both include
tutorials for a basic implementation!

However, what would an industrial strength compiler look like? I set
out to write
[bfc, a highly optimising compiler](https://github.com/Wilfred/bfc),
exploring the limits of BF optimisation. I believe some of my
optimisations are completely novel. Let's take a look.

## Baseline Performance

I've written a basic BF interpreter in C, so we'll use that as our
reference point for BF performance. Let's compare bfc,
compiling without optimisations, with this interpreter.

<figure>
<div id="interpreter-vs" style="min-width: 310px; max-width: 800px; height:500px; margin: 0 auto"></div>
</figure>

Clearly we're off to a good start, even without
optimisations. However, interpreters exist that are much faster than
this dumb implementation, so let's look at optimisations.

## Combining Increments

There are a range of peephole optimisations possible with BF. The most
commonly implemented approach is combining increments. Consider the BF
program `++`. This increments current cell twice. Our baseline
compiler would convert this to:

    cell_val := load(cell_ptr)
    cell_val := cell_val + 1
    store(cell_ptr, cell_val)
    cell_val := load(cell_ptr)
    cell_val := cell_val + 1
    store(cell_ptr, cell_val)

However, the underlying hardware is capable of adding numbers other
than one! We expand our intermediate representation (IR) to handle
increments, so we can compile this to:

    cell_val := load(cell_ptr)
    cell_val := cell_val + 2
    store(cell_ptr, cell_val)

We can do the same thing with pointers too. Rather than compiling `>>`
to this:

    cell_ptr := cell_ptr + 1
    cell_ptr := cell_ptr + 1

We compile it to this:

    cell_ptr := cell_ptr + 2

## Clear Loops

A common BF idiom is `[-]`, which zeroes the current cell. bfc's
optimiser knows about this, and it's smart enough to compile it to:

    store(cell_ptr, 0)

Setting a constant value is great because it opens up new opportunities
for collapsing instructions. We can go a step further and combine
with increments, so `[-]+++` can be compiled to:

    store(cell_ptr, 3)

## Multiply Loops

BF doesn't provide a multiply instruction, so programs have to use a
loop. `[->++<]` is equivalent to:

    cell#1 := cell#1 + 2 * cell#0
    cell#0 := 0

Our optimiser can also detect multiply loops and generate efficient
code. This also captures useful patterns like `[->+<]` (move cell 0 to
cell 1) and `[->+>+<<]` (move cell 0 to both cell 1 and cell 2).

As a result, we compile `[->++<]` to:

    cell0_val := load(cell_ptr)
    cell1_old_val := load(cell_ptr + 1)
    cell1_new_val := cell1_old_val + 2 * cell0_val
    store(cell_ptr + 1, cell1_new_val)
    store(cell_ptr, 0)

Loops are far more expensive than multiplication, so this improves
performance considerably.

## Dead Code Elimination

It's common to generate BF code with other tools, which can be
redundant. It's also common to add an introductory comment at the
beginning of a BF program:

    [Since we know that cells are initialised to zero,
     we can write whatever we want here because this
     loop is dead. We can use any of +-<>.,[] safely,
     as long as our brackets are matched.]
    actual code here!

bfc aggressively removes dead code. For example, cells in BF are
initialised to zero, so a loop at the beginning of a program is dead:

    [dead]not-dead

We also know that if a loop terminates, the current cell is zero. As a
result, consecutive loops are dead:

    foo[bar][dead][also dead]

## Redundant Code Elimination

bfc can also remove instructions if it can prove they have no effect.

We already know that `[-]+` is setting the current cell to a constant
value. Repeated instances of this will clobber the previous value. For
example, this code is assigning to the same cell three times:

    [-]+++[-]++[-]+

We can replace this with just `[-]+`.

I/O can also clobber values, making previous instructions
redundant. `,` reads a byte from stdin, and stores it in the current
cell. As a result, the following snippets can all be reduced to just
`,`:

    +,

<!-- -->

    -,

<!-- -->

    [-]+,

Instructions at the end of a program can also be redundant. The only
possible side effects in a BF program are reading, writing and
infinite loops. If we can prove the final instructions have no
observable side effects, we remove them. For example:

    foo.<[-]+>

This can be reduced to just `foo.`

Astute readers will observe that it's also possible for BF programs to
crash if they access cells out of the range permitted (between 0 and
30,000). For example, `<+`.

bfc considers out-of-range cell access to be undefined. In many cases
these programs will segfault, but in the case of `<+` it will be
optimised away by this redundant code elimination pass.

## Bounds Analysis

One novel optimisation that bfc offers is bounds analysis. In BF,
programs have 30,000 cells available, initialised to 0. However, most
programs don't use the full 30,000.

bfc uses static analysis to work out the highest cell that a program
could possibly reach. When we can prove a program only uses a smaller
range of cells, we can allocate less memory for them and need less time
to zero-initialise.

For example, this program never reaches beyond cell #2:

    >><<

If loops have a net cell movement of zero, then we're still limiting
ourselves to a fixed number of cells. This program never reaches
beyond cell #2, regardless of how many loop iterations are executed:

    [>><<]

We apply this analysis recursively, so `[><[<>]]` is still bounded in
the cells it can reach.

Of course, we can't always find a lower bound. For example, this
program may use an arbitrary number of cells:

    [,>]

In these cases, bfc provides 30,000 cells for the program to use.

## LLVM Optimisations

LLVM offers a suite of optimisations that we can run, and bfc uses all
of them by default (essentially `llc -O3 foo.ll`). Surprisingly, they don't always offer a
performance benefit.

I suspect this is due to the LLVM IR generated by bfc. bfc generates a
single large function with few external function calls, so many LLVM
optimisation passes aren't applicable.

<figure>
<div id="llvm-opt" style="min-width: 310px; max-width: 800px; height:500px; margin: 0 auto"></div>
<figcaption>Comparing bfc and LLVM optimisations. bfc was run with
speculative execution disabled.</figcaption>
</figure>

## Testing Optimisations

With all these different optimisations, how can we know that we have
combined them in the optimal order? There's a great
[Rust implementation of Quickcheck](https://github.com/BurntSushi/quickcheck)
which we use to verify the ordering. Quickcheck generates random BF
programs and reports if this test function ever returns false:

{% highlight rust %}
#[quickcheck]
fn should_be_idempotent(instrs: Vec<Instruction>) -> bool {
    // Once we've optimised once, running again shouldn't
    // reduce the instructions further.
    let minimal = optimise(instrs.clone());
    optimise(minimal.clone()) == minimal
}
{% endhighlight %}

This proved to be a fantastic way of finding corner cases in the
optimisation code. After various attempts at ordering optimisations,
Quickcheck found a BF program of the form `+>+-<-`. This showed bfc
had to run optimisations repeatedly to ensure it exploited all
available opportunities.

## Speculative Execution

Many BF programs don't take any inputs at all. For programs that do
take input, there's an initialisation phase that sets cells to certain
values first.

bfc exploits this by executing as much as it can at compile time. We
run a fixed number of steps (to avoid problems with infinite loops)
and terminate if we encounter a `,` (a read instruction).

As a result, we compile the classic BF hello world to:

{% highlight llvm %}
@known_outputs = constant [13 x i8] c"Hello World!\0A"

declare i32 @write(i32, i8*, i32)

define i32 @main() {
entry:
  %0 = call i32 @write(i32 0, i8* getelementptr inbounds ([13 x i8]* @known_outputs, i32 0, i32 0), i32 13)
  ret i32 0
}
{% endhighlight %}

bfc performs speculative execution after peephole optimisations and dead code
removal, to maximise how much we can execute before hitting the execution
step limit.

Even if we cannot speculatively execute the whole program, partial
execution is useful. We can discard any instructions executed at
compile time. We can also initialise cells to the values reached
during speculative execution.

For example, consider the program `+>+>+>++>,.`. bfc compiles this to:

{% highlight llvm %}
; Initialise cell #0, cell #1 and cell #2 to 1.
; Note that we combine adjacent cells with the
; same value into a single memset call.
%offset_cell_ptr = getelementptr i8* %cells, i32 0
call void @llvm.memset.p0i8.i32(i8* %offset_cell_ptr, i8 1, i32 3, i32 1, i1 true)

; Initialise cell #3 to 2.
%offset_cell_ptr1 = getelementptr i8* %cells, i32 3
call void @llvm.memset.p0i8.i32(i8* %offset_cell_ptr1, i8 2, i32 1, i32 1, i1 true)

; Intialise cell #4 to 0.
%offset_cell_ptr2 = getelementptr i8* %cells, i32 4
call void @llvm.memset.p0i8.i32(i8* %offset_cell_ptr2, i8 0, i32 1, i32 1, i1 true)

; Initialise the cell pointer to 4.
store i32 4, i32* %cell_index_ptr

; Compiled representation of , here.
; Compiled representation of . here.
{% endhighlight %}

## Future Work

There are a number of other optimising BF projects (notable
implementations include
[1](http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html),
[2](http://mearie.org/projects/esotope/bfc/) and
[3](https://github.com/rdebath/Brainfuck/tree/master/tritium)) and bfc
has benefited from seeing their ideas.

bfc still has scope for further optimisations: we don't apply the
'scan loops' or 'operation offsets' optimisations
[discussed by Mats Linander](http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html). We
only detect multiplication by constant values, and don't detect
division at all.

The bounds detection pass is also very pessimistic. It's currently
limited to loops with a net cell movement of zero. As a result, `[>]`
is treated as unbounded, whereas this loop cannot access cells which
haven't previously been modified.

Finally, bfc does not provide profile guided optimisation nor adaptive
optimisation (in the style of HotSpot or pypy). Its output also
assumes a 32-bit word size.

## Closing Thoughts

BF is small enough to implement in a short space of time, but it's a
real language with programs you can play with. It's a fantastic
testbed for compiler techniques.

Rust proved to be a fantastic language to use for bfc. The compiler
prevents many bugs, and warns about many others, which helps
tremendously. Its FFI makes interfacing with LLVM very
straightforward, and makes it easy to separate 'code that could
segfault' from the rest of the project.

Rust's pattern matching also makes bfc optimisations very
readable. Here's the dead code removal:

{% highlight rust %}
/// Remove any loops where we know the current cell is zero.
pub fn remove_dead_loops(instrs: Vec<Instruction>) -> Vec<Instruction> {
    instrs.into_iter().coalesce(|prev_instr, instr| {
        if let (&Set(Wrapping(0)), &Loop(_)) = (&prev_instr, &instr) {
            return Ok(Set(Wrapping(0)));
        }
        Err((prev_instr, instr))
    }).map(|instr| {
        match instr {
            Loop(body) => {
                Loop(remove_dead_loops(body))
            },
            i => i
        }
    }).collect()
}
{% endhighlight %}

Writing an effective optimising compiler for a conventional language
is a large scale project that takes years. By limiting ourselves to a
very small language, we can explore the entire compiler development
process.

<script src="/bower_components/jquery/dist/jquery.min.js"></script>
<script src="/bower_components/highcharts/highcharts.js"></script>
<script src="/bower_components/highcharts/modules/exporting.js"></script>

<script>
function plot(selector, categories, series, opts) {
    opts = opts || {};
    $(selector).highcharts({
        chart: {
            type: 'bar'
        },
        title: {
            text: null
        },
        xAxis: {
            categories: categories,
        },
        yAxis: {
            min: 0,
            max: opts.ymax || null,
            title: {
                text: 'Runtime in seconds (fastest of 10 runs)',
                align: 'high'
            },
        },
        tooltip: {
            valueSuffix: ' seconds',
            // The default pointFormat but with numbers rounded to 5dp.
            pointFormat: '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.5f}</b><br/>'
        },
        plotOptions: {
            bar: {
            }
        },
        exporting: {
            enabled: false
        },
        credits: {
            enabled: false
        },
        series: series
    });
}

// Based on http://stackoverflow.com/a/26674173/509706
Highcharts.seriesTypes.bar.prototype.pointAttrToOptions.dashstyle = 'dashStyle';

plot("#interpreter-vs",
     ['Hello world', '99 Bottles', 'Squares', 'Fibs'],
     [{
         name: 'Interpreter',
         data: [0.0063936710357666016,
                {y: 2.923617362976074, borderWidth: 5, borderColor: 'black', dashStyle: 'dash'},
                0.1846942901611328, 0.14739513397216797]
     }, {
         name: 'Compiler',
         data: [0.0065386295318603516, 0.011064291000366211, 0.01006627082824707, 0.008800506591796875]
     }],
     {ymax: 0.25}
    );

plot("#llvm-opt",
     ['Hello world', '99 Bottles', 'Squares', 'Fibs'],
     [{
         name: 'No optimisations',
         data: [0.006108760833740234, 0.011005878448486328, 0.009813070297241211, 0.008805036544799805]
     }, {
         name: 'bfc optimisations',
         data: [0.006061553955078125, 0.008074283599853516, 0.006600856781005859, 0.006422281265258789]
     }, {
         name: 'LLVM optimisations',
         data: [0.006047248840332031, 0.00925898551940918, 0.008416414260864258, 0.007428884506225586]
     }, {
         name: 'Both LLVM and bfc optimisations',
         data: [0.006059885025024414, 0.0071756839752197266, 0.007098197937011719, 0.006128549575805664]
     }]
    );</script>
