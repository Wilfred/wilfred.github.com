--- 
layout: post
title: "RPython for Fun and Profit"
---

So, you're thinking of writing a program in RPython. It's a great language, it produces fast programs, but has a few pitfalls. Here's what I've learnt.

**Make sure you're using it for the right purpose**. RPython is intended for writing interpreters. There are already RPython interpreters (in varying states of completeness) for Python, Ruby, PHP, Scheme, Smalltalk and various experimental languages.

**Start your project by working through [this tutorial](http://morepypy.blogspot.co.uk/2011/04/tutorial-writing-interpreter-with-pypy.html)**. It's a small and self-contained example that gets you started nicely.

**Test in CPython first**. An interpreter is a non-trivial project and normal CPython has lots of excellent tools to help you. Get your code working in CPython, ensure your test coverage is good, and only compile afterwards. You can go wild with mocks, magic methods and dynamic code in your tests, since RPython ignores code that isn't called from your `main` function.

**Build frequently**. RPython is a limited, statically typed subset of Python. Just because your tests pass on CPython, doesn't mean your code compiles. The compiler errors [can be confusing](https://bitbucket.org/pypy/pypy/issue/1689) so the best approach is to make sure every little changes still compiles. For my interpreter, I have [Travis compiling on every commit](https://github.com/Wilfred/trifle/blob/430f2d9a910cec22cefb2359aa2901d773a3172b/.travis.yml#L13) to keep me honest.

**Assert your types**. CPython is pretty forgiving about types. For example: `u"foo %s" % "bar"` is legal CPython, but RPython won't allow it. Since debugging CPython is easier, I like to write my classes with type assertions:

{% highlight python %}
def BoxedInteger(object):
    def __init__(self, value):
        assert isinstance(value, int), "Expected a string but got a: %s" % value
        self.value = value
{% endhighlight %}

**Use RPython's rlib**. RPython is limited in many respects. For example, its integer types are fixed size and can overflow, unlike CPython. However, rlib [includes a selection of useful modules](https://bitbucket.org/pypy/pypy/src/48e079c6da38d8a6245861844f6ad75fd2ecc86f/rpython/rlib/?at=default), including a bigint implementation.

**Reach out to the community**. The Pypy team (and wider community) is friendly, helpful and smart. There's a mailing list and an active IRC channel. You will probably get stuck from time to time, but I've found the team patiently answers all my questions.

I'm a happy RPython user. As of Pypy 2.3.0, [RPython is completely separate from the Pypy python interpreter](http://doc.pypy.org/en/latest/release-2.3.0.html#new-platforms-and-features), as the number of happy RPython users is growing.
