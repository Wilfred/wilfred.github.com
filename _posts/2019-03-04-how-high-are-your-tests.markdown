--- 
layout: post
title: "How High Are Your Tests?"
---

How big can a unit test be? How small can an integration test be?

It's easy to argue about whether a test is a 'true' unit test or
not. If we test several classes together, is it still a unit test? If
we use a small external API, must we call it an integration test?

The problem is that testing is a **spectrum**, but our terminology only
allows discrete levels.

## Putting A Number On It

I propose we treat testing as a **numerical scale** instead. Let's
introduce a concept of **test height**, from 0 to 100.

At 0, we have low-level, isolated unit tests. They run quickly, they run
in process, and they're extremely reliable. No setup or teardown is
required.

At 100, we have high-level tests of full computer systems. We spin up
an elaborate infrastructure of databases, external services, and
exercise real protocols. Many processes run, runtime can be very
variable, and flakiness is a continuing challenge.

Let's look at some examples! We'll look at some code for a wiki
website, and discuss different tests we could write.

## 20: An Isolated Test

At the lowest level, we have code that only depends on its inputs.

{% highlight python %}
def slugify(value):
    if not value:
        return value
    
    # we don't want /, # or ? in our URL
    value = value.replace('/', '')
      .replace('#', '')
      .replace('?', '')

    # replace whitespace with underscores
    return re.sub('[-\\s]+', '_', value)
{% endhighlight %}

We can easily write a small unit test for a function like this. There
are no dependencies on external resources, or even external
libraries.

{% highlight python %}
def test_slugify():
    assert slugify('foo/bar baz?') == 'foobar_baz'
{% endhighlight %}

This test only requires a single process, and our test assertion is simply
looking at runtime values.

## 40: Running With A Scratch Database

Our wiki stores its data in a database. Let's look at some view code
that ultimately creates database rows.

{% highlight python %}
def create_user(request):
    if request.POST:
        form = UserForm(request.POST)

        if form.is_valid():
            form.save()
            return HttpResponseRedirect(reverse('all_users'))
    else:
        form = UserForm()

    template_vars = {'form': form}
    return render_to_response(
        "users/create_user.html", template_vars,
        RequestContext(request))
{% endhighlight %}

We can test this after creating a new database to write to.

{% highlight python %}
class UserCreateTest(django.test.TestCase):
    def test_create_user(self):
        self.client.post(reverse('create_user'),
                         {'username': 'someuser',
                          'email': 'some@example.com'})

        self.assertTrue(
            User.objects.get(username='someuser'))
{% endhighlight %}

We're now running two processes during our tests, and we need to
ensure we clean up properly after each test. Assertions are now
verifying that data is being written to the database. Our test is
definitely slower than level 10.

## 60: Full Protocol Test

Web frameworks make testing easy, and allow you to call view functions
directly. If we want to exercise a full web request, we need something
that speaks HTTP. This is usually a headless web browser.

{% highlight python %}
class UserCreateTest(TestCase):
    def setUp(self):
        self.driver = webdriver.PhantomJS()

    def test_create_user(self):
        self.driver.get("http://localhost/"
                        + reverse('create_user'))
        self.driver.find_element_by_id(
            'username').send_keys("someuser")
        self.driver.find_element_by_id(
            'email').send_keys("some@example.com")
        self.driver.find_element_by_css_selector("[type=submit]")
            .click()

        # We should have a table listing our new user.
        matching_rows = [
          tag for tag in self.driver.find_elements_by_name('td')
          if tag.text == 'someuser'
        ]
        self.assertTrue(matching_rows)

    def tearDown(self):
        self.driver.quit()
{% endhighlight %}

This test is even slower still: we're running the full webserver stack
and driving a browser from our tests. We have to start worrying about
asynchronous responses: when do we know that the browser has fully
loaded the page, so we can look for `td` tags?

If this test fails, any errors or exceptions are less likely to be
useful. We'll probably need to open a browser ourselves and step
through the process to see what happened.

These tests are definitely useful! They're higher maintenance, but
they exercise more of your code in a more realistic way.

## 80: System Test

Finally, we simulate the whole system. We could spin up VMs, configure
HTTPS certificates, automatically building the infrastructure for an
entire website.

(But isn't this just a staging area? In practice, the line between
testing and staging is extremely blurry anyway.)

This can give a very high degree of confidence, but it's even worse
than level 60. It's slower, even more to maintain, and even flakier.

I like to have a couple of sanity check tests at this level. For
example, spinning up a full server and checking that the homepage
loads is a great way of catching infrastructure issues.

## Why Don't We Use A Scale?

If we think about tests on a scale, then we acknowledge there are
extremes, and points in-between. This is why I don't have any examples
with a height of 0 (not even using the standard library?) or 100 (simulating
an entire data centre?).

Testing a function that only uses the standard library could be 20,
but if our function also calls other helper functions we've written,
it might be 25.

If our program require a database, but we never written to, we might
only give it a height of 35. It requires no teardown and tests can
even safely run in parallel.

**Our tooling forces discrete levels upon us**. The tests exist on a
scale, but we have discrete mocking features we can choose to use or
not. A Django test must decide whether it inherits from
`django.test.TestCase`, which does database setup/teardown, or
`unittest.TestCase`, which does not.

**Sometimes test levels just mean performance**. Higher level tests
are slower, so projects often split tests into `unit/` and
`integration/`, or `quick/` and `slow/`. These are *arbitrary
designations*, chosen to make testing convenient. They're not
architectural!

## Beyond Discrete Categories

I hope this scale is a useful tool for you to think about how you
test. What's right for you depends on what problems you're facing, the
number of services you depend on, and the complexity of your
stack. Good luck.
