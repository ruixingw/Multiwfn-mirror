#This script invokes Multiwfn to compute molecular vdW surface ESP descriptors
#for all .fchk files in current folder. Written by Tian Lu, 2020-Jun-28

#!/bin/bash
icc=0
nfile=`ls ./*.fchk|wc -l`
for inf in *.fchk
do
((icc++))
echo Running ${inf} ... \($icc of $nfile\)
Multiwfn ${inf} << EOF |grep "Summary of surface analysis" -A 20 > ${inf//fchk/txt}
12
0
-1
-1
q
EOF
echo ${inf} has finished
echo
done
