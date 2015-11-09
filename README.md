[![Build Status](https://travis-ci.org/diagrams/diagrams-pandoc.svg?branch=master)](https://travis-ci.org/diagrams/diagrams-pandoc)

A [pandoc](http://johnmacfarlane.net/pandoc/) filter to express
diagrams inline using the haskell EDSL
[diagrams](http://projects.haskell.org/diagrams/).

## Usage

Create an input file called `demo.md` with the following text:

``` markdown
    This is how to draw a circle:
    ~~~ diagram
    example = circle 1
    ~~~
```

Install diagrams-pandoc, then run pandoc as follows:

``` shell
    pandoc -t html demo.md --filter diagrams-pandoc -o demo.html -s
```

The file demo.html should now have an img tag pointing at a PNG of a circle.

## Attributes

You can specify attributes to control how the diagram is generated.
The following, for example,
``` markdown
    ~~~ {.diagram width=800 height=400}
    example = circle 1
    ~~~
```
will override the default width and height of the generated diagram.
The following attributes are supported:

* `width`: The width of the generated diagram, in pixels. The default is 500.
* `height`: The height of the generated diagram, in pixels. The default is 200.

## Details

`diagrams-pandoc` compiles code blocks containing diagrams expressions
and includes the resulting images in the pandoc markup.  It is meant
to be run as a
[pandoc filter](http://johnmacfarlane.net/pandoc/scripting.html) as
shown above.

`diagrams-pandoc` evaluates the diagrams expression `example` by
default. This can be modified by passing a command line argument.

`diagrams-pandoc` is aware of two code block classes.  A block with
the `diagram` class will be replaced by the resulting image---the code
will not appear in the output.  A block with the `diagram-haskell`
class will produce both an image and a (syntax highlighted) code
block.  The input block is replaced by image appears before the code
block, and the `diagram-haskell` class is replaced by the `haskell`
class, so that pandoc can perform syntax highlighting as usual.

`diagrams-pandoc` produces images in the `pdf` format when used with
the `latex` and `beamer` writers of `pandoc` and produced `png` output
otherwise.

I have only tested with pandoc's markdown reader.  In particular, the
rst reader does not attach classes to code blocks, only to Div elements.

## Installing

`diagrams-pandoc` is on Hackage.  To install, run `cabal install diagrams-pandoc`

## TODO

* use pandoc output type to pick an image file format
* for formats which are more human-readable (eg, markdown, rst), leave
  code block alone?
* provide command-line flags to override default behavior
* add Backends besides Cairo
* Support RST by handling `Div class=diagram [CodeBlock foo bar]` the same as `CodeBlock class=diagram bar`
* Alternate install directions using `stack`
