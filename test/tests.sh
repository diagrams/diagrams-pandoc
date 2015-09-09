# run from the `test` directory

# diagrams-pandoc will use packages installed in a sandbox in the
# directory from which it's run.  It's sufficient that the `test`
# directory have a symlink to the sandbox.

#test diagram
pandoc -t html test1.md --filter diagrams-pandoc -o test1.html -s

#test diagram with code
pandoc -t html test2.md --filter diagrams-pandoc -o test2.html -s
