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

## Details

`diagrams-pandoc` compiles code blocks containing diagrams expressions
and includes the resulting images in the pandoc markup.  It is meant
to be run as a
[pandoc filter](http://johnmacfarlane.net/pandoc/scripting.html) as
shown above.

`diagrams-pandoc` is aware of two code block classes.  A block with
the `diagram` class will be replaced by the resulting image---the code
will not appear in the output.  A block with the `diagram-haskell`
class will produce both an image and a (syntax highlighted) code
block.  The input block is replaced by image appears before the code
block, and the `diagram-haskell` class is replaced by the `haskell`
class, so that pandoc can perform syntax highlighting as usual.

I have only tested with pandoc's markdown reader.  In particular, the
rst reader doesn't seem to currently support adding classes to code
blocks.

## Installing

`diagrams-pandoc` is not yet on Hackage.  To install, clone this
repository, and install with cabal.

``` shell
    git clone https://github.com/bergey/diagrams-pandoc.git
    cd diagrams-pandoc
    cabal install
```

## TODO

* use pandoc output type to pick an image file format
* for formats which are more human-readable (eg, markdown, rst), leave
  code block alone?
* provide command-line flags to override default behavior
* add Backends besides Cairo
