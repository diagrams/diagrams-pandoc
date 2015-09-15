#test diagram
pandoc -t html test1.md --filter diagrams-pandoc -o test1.html -s

#test diagram with code
pandoc -t html test2.md --filter diagrams-pandoc -o test2.html -s
