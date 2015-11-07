# run from the `test` directory

# diagrams-pandoc will use packages installed in a sandbox in the
# directory from which it's run.  It's sufficient that the `test`
# directory have a symlink to the sandbox.

#test diagram
pandoc -t html test1.md --filter diagrams-pandoc -o test1.html -s

#test diagram with code
pandoc -t html test2.md --filter diagrams-pandoc -o test2.html -s

#test diagram with attributes
pandoc -t html test3.md --filter diagrams-pandoc -o test3.html -s

#test creation of multiple directories
pandoc -t json test1.md | \
    diagrams-pandoc -o images/some/dir/deep/down | \
    pandoc -f json -t html -o test1-b.html -s
