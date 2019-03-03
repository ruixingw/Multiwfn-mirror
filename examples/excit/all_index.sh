#!/bin/bash
cat << EOF > calcall.txt
18
1
D-pi-A.out
EOF
for ((i=1;i<=3;i=i+1))  #Range of excited states
do
cat << EOF >> calcall.txt
$i
1
2
0
0
1
EOF
done

./Multiwfn D-pi-A.fchk < calcall.txt |tee out.txt  #Running command
rm ./calcall.txt ./result.txt -f

grep "Sr index" ./out.txt |nl >> result.txt;echo >> result.txt
grep "D index" ./out.txt |nl >> result.txt;echo >> result.txt
grep "RMSD of hole in" ./out.txt |nl >> result.txt;echo >> result.txt
grep "RMSD of electron in" ./out.txt |nl >> result.txt;echo >> result.txt
grep "H index" ./out.txt |nl >> result.txt;echo >> result.txt
grep "t index" ./out.txt |nl >> result.txt
echo
echo "Finished!"
