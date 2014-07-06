--- 
layout: post
title: "Testing Django Websites"
---

I've recently developed some Django sites where I was able to ensure
all features were thoroughly tested. I learnt a lot about writing
maintainable tests, and I'd like to share with you.

## A basic view

When you're starting out developing your site, your views will be
simple. Your index view probably looks something like this:

{% highlight python %}
from django.shortcuts import render

def index(request):
    return render(request, "index.html")
{% endhighlight %}

Now, what properties do we want this view to have? We want it to render without errors. Here's a test:

{% highlight python %}
from django.test import TestCase
from django.core.urlresolvers import reverse

class IndexTests(TestCase):
    def test_index_renders(self):
        response = self.client.get(reverse('index'))
        self.assertEqual(response.status_code, 200)
{% endhighlight %}

Hey presto, your index view is tested!

When thinking about testing, you should ask yourself: _'what properties do I want this view to have?'_ In this case, we just want
to check the view renders. This helps us catch any silly errors (such
as incorrect template names) that may occur.

What's nice about this test is that **no legitimate refactoring will
cause the test to fail**. Django provides many specific assertion
methods, such as `assertTemplateUsed` and
`assertHTMLEqual`. These have their uses, but they limit you. If
you use `assertTemplateUsed` and refactor your templates, you will
have to update your tests. Grrr.

## Testing a form

Suppose we're writing a form for users to sign up. Here's what our
view looks like:

{% highlight python %}
from django.shortcuts import render
from .forms import UserRegistrationForm

def register_user(request):

    if request.POST:
        form = UserRegistrationForm(request.POST)

        if form.is_valid():
            form.save()
            return redirect('completed-signup')
    else:
        form = UserRegistrationForm()

    return render(request, "register_user.html",
                  {'form': form})
{% endhighlight %}

What are we interested in here? We want the page to load, we want
users to be able to register, and we want form validation. I've
written
[django-test-mixins](https://github.com/Wilfred/django-test-mixins) to
provide these assertions.

{% highlight python %}
from django.core.urlresolvers import reverse
from django_test_mixins import HttpCodeTestCase, FormValidationTestCase

class RegistrationTests(HttpCodeTestCase, FormValidationTestCase):
    def form_params(self):
        return {'username': 'foobar',
                'password': 'helloworld1'}

    def test_view_renders(self):
        response = self.client.get(reverse('user_registration'))
        self.assertHttpOK(response)

    def test_user_created(self):
        params = self.form_params()
        expected_username = params['username']

        response = self.client.post(
            reverse('user_registration'), self.form_params())

        self.assertTrue(
            User.objects.filter(username=expected_username).exists(),
            "User was not created.")

    def test_password_required(self):
        params = self.form_params()
        params.pop('password')

        response = self.client.post(
            reverse('user_registration'), params)
        self.assertFormInvalid(response)
{% endhighlight %}

We don't want tests to become a burden in later development. There are
two things we've done to ensure our tests are future-friendly.

Firstly, we're careful to only make an assertion about the user we're
interested in. We don't want to assume how many users exist in the
system. Websites often start off with a single admin user, but we
shouldn't bake that assumption into our tests. This assertion:

{% highlight python %}
# I have been guilty of this.
self.assertEqual(User.objects.count(), 2)
{% endhighlight %}

is worse, because we are making assumptions about the state of our
database.

Secondly, we've factored out the form parameters. The exact number of
form parameters often changes as features are added. It's a pain to
change lots of tests because you've added one field to form, so keep
it DRY.

## Testing protected views

Finally, let's look at a more complex view.

{% highlight python %}
from django.contrib.admin.views.decorators import staff_member_required
from django.shortcuts import get_object_or_404, redirect, render

from .models import BlogPost
from .forms import BlogPostForm

@staff_member_required
def edit_post(request, post_id):
    blog_post = get_object_or_404(BlogPostForm, id=post_id)

    if request.POST:
        form = BlogPostForm(request.POST, instance=blog_post)

        if form.is_valid():
            form.save()
            return redirect('view_post', blog_post.id)
    else:
        form = BlogPostForm(instance=blog_post)

    return render(request, "pages/page_edit.html"
                  {'form': form, 'blog_post': blog_post})
{% endhighlight %}

We have authentication here and we need some existing models to test
the view.

We need a blog post to test this view, and we could explicitly create
one. This is tedious, and
[milkman](https://github.com/ccollins/milkman) can do this for us.

Finally, we want an easy way of creating users to test that our view
applies authentication correctly.

{% highlight python %}
from django.test import TestCase
from django.contrib.auth.models import User

from milkman.dairy import milkman

class UserTestCase(TestCase):
    TEST_PASSWORD = "password"

    def create_user(self, **kwargs):
        return milkman(User, password=self.TEST_PASSWORD, **kwargs)

    def create_staff_user(self, **kwargs):
        return self.create_user(is_staff=True, **kwargs)

    def log_in(self, user):
        self.client.login(username=user.username,
                          password=self.TEST_PASSWORD)
{% endhighlight %}

We factor out user creation so we have a single place in our tests
that provides reusable methods for creating users according to th
eneeds of our site.

We want to test that our view validates users correctly and that it
modifies blog posts correctly. Since each test should only verify one
thing, we will write three tests.

{% highlight python %}
from django_test_mixins import HttpCodeTestCase

from milkman.dairy import milkman
from .models import BlogPost

class EditBlogPostTest(HttpCodeTestCase, UserTestCase):
    def test_view_renders(self):
        user = self.create_staff_user()
        self.log_in(user)

        post = milkman.deliver(BlogPost)
        response = self.client.get(reverse('edit_post', args=[post.id]))
        self.assertHttpOK(response)

    def test_view_requires_staff_user(self):
        user = self.create_user()
        self.log_in(user)

        post = milkman.deliver(BlogPost)
        response = self.client.get(reverse('edit_post', args=[post.id]))
        self.assertHttpForbidden(response)

    def form_params(self):
        return {'title': 'a test title',
                'content': 'some test content'}

    def test_blog_post_edited(self):
        user = self.create_staff_user()
        self.log_in(user)

        post = milkman.deliver(BlogPost)

        params = self.form_params()

        response = self.client.post(
            reverse('edit_post', args=[post.id]), params)

        # Reload the post from the DB.
        post = BlogPost.objects.get(id=post.id)

        self.assertEqual(post.title, params['title'])
        self.assertEqual(post.content, params['content'])
{% endhighlight %}

This is how I write tests with Django. Don't worry about writing
perfect tests, having tests at all is a huge boon. Once you have them,
don't be afraid to keep iterating on them. Always seek out ways to
make testing easier next time.
