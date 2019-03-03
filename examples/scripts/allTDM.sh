#!/bin/bash
for ((i=1;i<=7;i=i+1))
do
cat << EOF >> allTDM.txt
18
2
tdmat.out
$i
n
1
-1
0
tdmfrag.txt
2
EOF
./Multiwfn tdmat.fchk < allTDM.txt
rm ./allTDM.txt -f
mv dislin.png $i.png -f
done
