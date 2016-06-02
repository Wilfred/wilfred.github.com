--- 
layout: post
title: "The Strange World of Directory Scope"
---

Most languages today use [lexical scope](#todo). A few older languages
use [dynamic scope](#todo). Imagine my surprise this week, when debugging a
[Handlebars](http://www.handlebarsjs.com) template, to find a totally
new approach to scope!

In Handlebars, you cannot access variables in outer scopes. Suppose we
have the following context:

    var context = {user: "Alice",
                   friends: ["Bob", "Charlie", "Diane"]};
                   
We can access `user` in the top-level scope:

    This user is {{user}}.
    
    ->
    
    This user is Alice.
    
However, we cannot access `user` from `#each` blocks:

    {{#each friends}}
    {{user}} is friends with {{this}}.
    {{/each}}
    
    ->
    
    is friends with Bob.
    is friends with Charlie.
    is friends with Diane.

Handlebars [provides a solution](#todo), which is a filesystem style
reference to the parent scope:

    {{#each friends}}
    {{../user}} is friends with {{this}}.
    {{/each}}
    
    ->
    
    Alice is friends with Bob.
    Alice is friends with Charlie.
    Alice is friends with Diane.
    
Wow! I'd believed there was nothing beyond dynamic and lexical
scoping. I don't know what this scoping technique is called, so I'm
going to call it 'directory scope' for the sake of this post.

Directory scope has some interesting consequences. Firstly, it makes
refactoring more difficult, since your template is very sensitive to
context. If you introduce a new block, or move code around, you may
need to change how you access your variables.

It has a number of benefits too though. You don't need to worry about
shadowing variables, because you can still access identically named
variables in outer scope!

If we're not worried about shadowing, we can write new control
structures with ease. A lisp macro author would worry about hygiene in
blocks, because they could end up shadowing user-defined variables. A
Handlebars block author can inject variables with gay abandon. Any
block could introduce a `user` variable, because the programmer can
only access outer variables with explicit `../foo`!

It's not often simple debugging teaches you entirely new ways to handle
variable scope.
