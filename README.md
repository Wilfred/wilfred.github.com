This git respository houses the static blog content of the
wilfred.me.uk blog. All content is under the GFDL 1.3.

## Running the server

    $ jekyll --server

Note that changes to `_config.yml` may require restarting the server.

## Known gotchas

Liquid templating errors do not produce stack traces when running
Jekyll as a server
([ticket #46](https://github.com/mojombo/jekyll/issues/46)). To see a
full stack trace, do:

    $ jekyll --no-auto
    
Note that the traceback in stock Jekyll is not as always helpful as it
could be ([issue #388](https://github.com/mojombo/jekyll/issues/388)).

Furthermore, no warnings or errors are produced when a layout is named
incorrectly
([issue #353](https://github.com/mojombo/jekyll/issues/353)).
