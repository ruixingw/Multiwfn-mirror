#Convertting all .chk files in current folder to .gbw
#Gaussian, Multiwfn and ORCA must have been installed on current system
#Written by Tian Lu (sobereva@sina.com), 2020-Jun-1
#!/bin/bash
icc=0
nfile=`ls *.chk|wc -l`
for inf in *.chk
do
((icc++))
echo Converting ${inf} to ${inf//chk/gbw} ... \($icc of $nfile\)
fchk=${inf//chk/fchk}
mkl=${inf//chk/mkl}
inp=${inf//chk/inp}
formchk ${inf} $fchk > /dev/null
Multiwfn $fchk << EOF > /dev/null
100
2
9
$mkl
y
2
12
$inp
1
0
q
EOF
orca_2mkl ${mkl%.*} -gbw > /dev/null
gbw=${inf%.*}_Gau.gbw
mv ${inf//chk/gbw} $gbw
sed -i "2i %moinp \"$gbw\"" $inp
awk '{if (NR==1) {print $0 " moread"} else{print $0}}' $inp > "$inp"2
mv "$inp"2 $inp
rm -f $mkl $fchk
done
