#!/bin/bash
for inf in *.out
do
Multiwfn ${inf} < UV-Vis.txt > /dev/null
mv -f dislin.png ${inf//out/png}
done
