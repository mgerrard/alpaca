#!/bin/bash

pandoc -s --mathjax -o index.html --bibliography biblio.bib --citeproc index.md
pandoc ../aca-framework/app/Main.lhs -f markdown+lhs -t html -s -o app/Main.html

for f in ../aca-framework/src/*.lhs; do
    html_f=`basename $f .lhs`.html
    pandoc $f -f markdown+lhs -t html -s -o src/$html_f
done
