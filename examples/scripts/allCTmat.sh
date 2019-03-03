#!/bin/bash
for ((i=1;i<=7;i=i+1))
do
cat << EOF >> allCTmat.txt
18
8
tdmat.out
$i
-1
2
atmCTmat.txt
-1
0
tdmfrag.txt
2
EOF
./Multiwfn tdmat.fchk < allCTmat.txt
rm ./allCTmat.txt ./atmCTmat.txt -f
mv dislin.png $i.png -f
done
