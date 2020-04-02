#Convert all .xyz files to .gjf file by Multiwfn
#!/bin/bash
icc=0
nfile=`ls *.xyz|wc -l`
for inf in *.xyz
do
((icc++))
echo Converting ${inf} to ${inf//xyz/gjf} ... \($icc of $nfile\)
Multiwfn ${inf} << EOF > /dev/null
100
2
10
${inf//xyz/gjf}
0
q
EOF
done
