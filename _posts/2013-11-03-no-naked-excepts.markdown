--- 
layout: post
title: "No Naked Excepts"
---

You probably shouldn't be using `except:` or `except Exception:` in
Python, sometimes called 'catch-all exceptions' or 'naked excepts'.

I've tried to find a good article or blog post discussing this, but I
haven't found any thorough discussions of the problems that arise.

Let's take a look at exceptions. Python is excellent at throwing
exceptions, and errs on the side of more exceptions rather than
less. For example, this code will raise `KeyError`:

{% highlight python %}
d = {'foobar': 'Hello world'}
print d['fooba'] # spelling mistake!
{% endhighlight %}

The equivalent code in other languages will just return a sentinel
value. For example, JavaScript:

{% highlight javascript %}
var d = {'foobar': 'Hello world'};
console.log(d['fooba']); // prints 'undefined'
{% endhighlight %}

The main disadvantage of the JavaScript approach is that it's easy to
get confused when you have your sentinel value in the hashmap. It's
hard to distinguish `{}` from `{'foobar': undefined}`.

So, there are a large number of situations that will throw exceptions
in Python.

Let's consider the following piece of Django code, which fetches a
page, if it exists.

{% highlight python %}
try:
    page = Page.objects.get(name="home")
except Page.DoesNotExist:
    page = None
{% endhighlight %}

This works as expected. However, can you see the mistake in this
version?

{% highlight python %}
try:
    page = Page.object.get(name="home")
except:
    page = None
{% endhighlight %}

This code will actually throw `AttributeError`, since `Page.object`
doesn't exist. However, since we used a catch-all `except`, we don't
get any error. Subtle bugs like these can be a pain to track down
later.

The solution is to always name the specific exception you're
expecting. Note there's a nasty gotcha with catching multiple
exceptions:

{% highlight python %}
try:
    do_something()
except SomeException, OtherException:
    # This only catches SomeException and binds it to
    # the variable OtherException
    pass
{% endhighlight %}

The correct way to write this is:

{% highlight python %}
try:
    do_something()
except (SomeException, OtherException):
    # the tuple ensures we catch both exceptions
    pass
{% endhighlight %}

To avoid this gotcha completely, you can either use the excellent
[Pyflakes](https://pypi.python.org/pypi/pyflakes) to check your code,
or use the `as` syntax:

{% highlight python %}
try:
    do_something()
except (SomeException, OtherException) as e:
    # not using a tuple here is a syntax error
    pass
{% endhighlight %}

As with every rule, there are a few exceptions. The first is when
you're allowing exceptions to propagate.

{% highlight python %}
try:
    some_resource = open_resource()
    process_resource(some_resource)
except:
    # cleanup resource before propagating exception
    some_resource.close()
    # re-raise the original exception
    raise
{% endhighlight %}

The other case is long running process, such as daemons:

{% highlight python %}
while True:
    try:
        do_daemon_stuff()
    except:
        log_exception_somewhere()
{% endhighlight %}

In this case, you only want a naked except at the very top level of
your program, and you want the exception to go somewhere. I like to
log to [Sentry](https://getsentry.com/) or to a log file monitored by
[Papertrail](https://papertrailapp.com/).

To sum up: Only catch the exceptions you're expecting to see. For all
other exceptions, you want to be informed about it so you can fix it.
