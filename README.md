This git respository houses the static blog content of the
wilfred.me.uk blog. All content is under the GFDL 1.3.

[![Build Status](https://travis-ci.org/Wilfred/wilfred.github.com.png?branch=master)](https://travis-ci.org/Wilfred/wilfred.github.com)

Styling is based on [pixyll](https://github.com/johnotander/pixyll)

## Running the server

You need Jekyll installed:

    $ gem install --user-install bundler
    $ bundle install --path vendor/bundle
    
Start Jekyll:

    $ bundle exec jekyll serve --watch

Note that changes to `_config.yml` may require restarting the server.

### Catching Markdown Errors

GitHub
[documents how to set up travis to test the build](https://help.github.com/articles/pages-don-t-build-unable-to-run-jekyll),
so you may find
[the travis build results](https://travis-ci.org/Wilfred/wilfred.github.com)
useful.

## Known gotchas

Syntax highlighting fails if `python` doesn't point to a
Python 2.x executable (see
[octopress/1028](https://github.com/imathis/octopress/issues/1028),
amongst others).
