#This script invokes Multiwfn to compute ESP minimum and maximum values
#based on grid data for all .fchk files in current folder. Written by Tian Lu, 2020-Jul-2

#!/bin/bash
icc=0
nfile=`ls ./*.fchk|wc -l`
for inf in *.fchk
do
((icc++))
echo Running ${inf} ... \($icc of $nfile\)
Multiwfn ${inf} << EOF |grep "The minimum is" -A 1 > ${inf//fchk/txt}
5
12
4
0.2
0
q
EOF
echo ${inf} has finished
echo
done
