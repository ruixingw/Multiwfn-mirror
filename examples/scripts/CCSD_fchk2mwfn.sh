#Convert .fchk carrying CCSD density matrix to .mwfn file containing corresponding natural orbitals
#All .fchk files in current folder will be considered, the .mwfn file has identical name as .fchk file
#!/bin/bash
icc=0
nfile=`ls *.fchk|wc -l`
for inf in *.fchk
do
((icc++))
echo Converting ${inf} to ${inf//fchk/mwfn} ... \($icc of $nfile\)
Multiwfn ${inf} << EOF > /dev/null
200
16
CC
y
0
q
EOF
mv new.mwfn ${inf//fchk/mwfn}
done
