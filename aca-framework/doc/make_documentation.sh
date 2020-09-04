#!/bin/bash

doc="Documentation.lhs"
> $doc # empty file

cat >> $doc <<- EOM
\documentclass{article}
%include polycode.fmt
\usepackage{amsmath}
\usepackage[margin=0.5in]{geometry}
\begin{document}
EOM

cat ../src/BinarySearch.lhs >> $doc

echo "\end{document}" >> $doc

lhs2TeX -o Documentation.tex Documentation.lhs
pdflatex Documentation.tex

rm -f *.ptb
rm -f *.aux
rm -f *.log
rm -f *.*~

