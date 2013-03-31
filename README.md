This git respository houses the static blog content of the
wilfred.me.uk blog. All content is under the GFDL 1.3.

## Running the server

You need Jekyll installed:

    $ sudo gem install jekyll
    
Start Jekyll:

    $ jekyll --server

Note that changes to `_config.yml` may require restarting the server.

### Minifying CSS

I use clean-css, which requires node.js:

    $ sudo npm install -g clean-css
    $ cd static
    $ cat style.css syntax.css wilfred.css | cleancss -o min.css

## Known gotchas

Note that the traceback in stock Jekyll is not as always helpful as it
could be ([issue #388](https://github.com/mojombo/jekyll/issues/388)).

Furthermore, no warnings or errors are produced when a layout is named
incorrectly
([issue #353](https://github.com/mojombo/jekyll/issues/353)).
