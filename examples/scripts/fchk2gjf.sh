#Convert geometry in all Gaussian .fchk files in current folder to .gjf file by Multiwfn
#!/bin/bash
icc=0
nfile=`ls *.fchk|wc -l`
for inf in *.fchk
do
((icc++))
echo Converting ${inf} to ${inf//fchk/gjf} ... \($icc of $nfile\)
Multiwfn ${inf} << EOF > /dev/null
100
2
10
${inf//fchk/gjf}
n
0
q
EOF
#Below line replaces the default keyword B3LYP/6-31G* to e.g. CCSD/def2TZVP density
#sed -i "s/B3LYP\/6-31G\*/CCSD\/def2TZVP density/g" ${inf//fchk/gjf}
done
