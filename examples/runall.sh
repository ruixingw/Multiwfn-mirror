#!/bin/bash
icc=0
nfile=`ls ./*.gjf|wc -l`
for inf in *.gjf
do
((icc++))
echo Running ${inf} ... \($icc of $nfile\)
time g16 < ${inf} > ${inf//gjf/out}
echo ${inf} has finished
echo
done
