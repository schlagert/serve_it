serve_it
========

[![Build Status](https://travis-ci.org/schlagert/serve_it.png?branch=master)](https://travis-ci.org/schlagert/serve_it)

A super simple webserver based on [cowboy](https://github.com/ninenines/cowboy).
Basically this application provides a `cowboy_http_handler` implementation with
support for `range` headers, index files and (as a fallback) directory listings.

Mimetypes will be detected using the `cow_mimetypes:all/1` function, so it
should be sufficient for all your needs from website development to audio/video
streaming (and of course it supports resumable downloads, think of ISOs).

Although this project is meant to be an example for the
[rebar_escript_plugin](https://github.com/schlagert/rebar_escript_plugin) it
can really be a handy tool if Erlang/OTP is installed on the executing machine.

Get it
------

Of course this is done the usual way:
```
git clone https://github.com/schlagert/serve_it.git
```

Build it
--------

It is intended to issue the `compile` command twice. Unfortunately this is
needed when initially cloning dependencies. This is not needed when recompiling.
```
rebar get-deps
rebar compile
rebar compile
```

Configure it (optional)
-----------------------

The application can of course be configured through the application environment.
However, this is only useful when you want to include `serve_it` in an Erlang
release. The following configuration options are implemented:

* `{port, inet:port_number()}`

  Specifies the server listen port. Default is (guess what) `8080`.

* `{base_dir, DirectoryPath :: string()}`

  Specifies the server's root directory. Defaults to the current working directory.

* `{index_file, Basename :: string()}`

  Specifies the basename of the webserver directory files to serve. The basename
  may contain `*` as wildcard. Default is `"index.*"`.

* `{show_hidden_files, boolean()}`

  Indicating whether hidden files should be shown in directory listings (they
  will always be served). _Hidden files_ is defined the `unix` way
  (files/directories with names starting with a dot). Default is `false`.

Serve it
--------

After compilation you'll find an executable, self-contained `escript` called
`serve_it` in the project's top level directory. To run this script, all you
need is having Erlang/OTP installed and in your path. This means the script is
fully relocatable. Copy the script somewhere into your path or onto another
machine (maybe even a Windows machine, didn't try it yet) and call it from the
directory you want to serve. Done!

To browse your directory, fire up a browser and visit
[localhost:8080](http://localhost:8080).
