--- 
layout: post
title: "The Strange World of Directory Scope"
---

Most languages today use
[lexical scope](https://en.wikipedia.org/wiki/Scope_%28computer_science%29#Lexical_scoping). A
few older languages use
[dynamic scope](https://en.wikipedia.org/wiki/Scope_%28computer_science%29#Dynamic_scoping). Imagine
my surprise this week, when debugging a
[Handlebars](http://www.handlebarsjs.com) template, finding a totally
new approach to scope!

What if we required programmers to access scopes explicitly? Sound
crazy? In Handlebars, this is a reality!

Handlebars *does not let you access variables in outer scopes*. Suppose we
have the following context:

```js
var context = {user: "Alice",
               friends: ["Bob", "Charlie", "Diane"]};
```
     
We can access `user` in the top-level scope:

{% raw %}
```handlebars
This user is {{user}}.

renders as:

This user is Alice.
```
{% endraw %}
    
However, we cannot access `user` from `#each` blocks:

{% raw %}
```handlebars
{{#each friends}}
{{user}} is friends with {{this}}.
{{/each}}

renders as:

is friends with Bob.
is friends with Charlie.
is friends with Diane.
```
{% endraw %}

Handlebars
[provides filesystem style syntax](http://handlebarsjs.com/#paths) to
access the parent scope:

{% raw %}
```handlebars
{{#each friends}}
{{../user}} is friends with {{this}}.
{{/each}}

renders as:

Alice is friends with Bob.
Alice is friends with Charlie.
Alice is friends with Diane.
```
{% endraw %}
    
Wow! I'd believed there was nothing beyond dynamic and lexical
scoping. I'm not aware of a name for this style of scope, so I'm
going to call it 'directory scope' for the sake of this post.

Directory scope does have its downsides. It makes refactoring more
difficult, because templates are very sensitive to context. If you
introduce a new block, or move code around, you may need to change how
you access your variables.

It has a number of benefits too though. You never need to worry about
shadowing variables. You can still access variables in outer scopes,
even if they have the same name! No namespacing required.

If you're not worried about shadowing, then writing new control
structures, lisp-style, is easy. A lisp macro author would worry about
hygiene in blocks, because they could end up shadowing user-defined
variables.

A Handlebars block author can inject variables with gay
abandon. Suppose we defined a `#with-user` block. This could inject a
`user` variable without breaking any of our code examples above.

The syntax choice is also clever. You can probably guess what `./foo`
means (access the variable in the current scope) and what `../../foo`
means (access the variable in the grandparent scope).

This illustrates the value of regularly learning new
languages. Starting from a bug — why isn't my variable in scope? — I
ended up a exploring a radically different approach to scoping with a
fresh set of tradeoffs.

