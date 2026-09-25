This git repository houses the static blog content of the
wilfred.me.uk blog. All content is under the GFDL 1.3.

The site is built and deployed by GitHub Pages on every push to
`master`. A GitHub Actions workflow builds each push too, so Markdown
and Liquid errors show up as a failed check rather than a silent
deploy failure.

Styling is based on [pixyll](https://github.com/johnotander/pixyll).

## Running the server

    $ bundle install
    $ bundle exec jekyll serve --watch

Then open <http://localhost:4000>.

Note that changes to `_config.yml` require restarting the server.

`bundle install` resolves the [github-pages
gem](https://github.com/github/pages-gem), which pins the same Jekyll
version GitHub Pages uses. `Gemfile.lock` is deliberately not
committed: GitHub Pages ignores it, so committing it would only mean
keeping two sets of pins in sync.

## Writing posts

    $ bundle exec rake post['my new post']
    $ bundle exec rake draft['my new draft']

Drafts live in `_drafts` and are shown by `jekyll serve --drafts`.

## bower_components

`bower_components` contains vendored copies of jQuery and Highcharts,
used by the charts in the four brainfuck/bigint posts. Bower itself is
long dead; the files are checked in and need no tooling, so leave them
be unless those posts change.

## Contact form

The contact page posts to [Formspree](https://formspree.io). Set
`formspree_form_id` in `_config.yml` to the ID of the form in the
Formspree dashboard; the page falls back to a mailto link while it is
blank.
