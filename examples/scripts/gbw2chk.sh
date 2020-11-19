#!/bin/bash
for inf in *.gbw
do
echo converting ${inf} ...
name=${inf//.gbw}
orca_2mkl $name -molden
Multiwfn $name.molden.input << EOF > /dev/null
100
2
7
$name.fch
0
q
EOF
unfchk $name.fch
echo Finished!
echo
done
