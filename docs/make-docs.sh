#!/bin/bash

pandoc ../aca-framework/app/Main.lhs -f markdown+lhs -t html -s -o app/Main.html

for f in ../aca-framework/src/*.lhs; do
    html_f=`basename $f .lhs`.html
    pandoc $f -f markdown+lhs -t html -s -o src/$html_f
done

git add .
git commit -m 'auto-generated documentation'
