# A script to calculate RESP charge based on Gaussian and Multiwfn
# Written by Tian Lu (sobereva@sina.com), 2019-Apr-17
# You should properly modify content to use proper calculation level and solvent
# Example:
# Calculating neutral singlet molecule: RESP.sh H2O.xyz
# Calculating anionic singlet molecule: RESP.sh ani.pdb -1 1

#!/bin/bash

keyword_opt="# B3LYP/def2SVP em=GD3BJ scrf(solvent=water) opt=loose"
keyword_SP="# B3LYP/def2TZVP em=GD3BJ scrf(solvent=water) pop=MK IOp(6/33=2,6/42=6)"
Gaussian=g09

export inname=$1
filename=${inname%.*}
suffix=${inname##*.}
if [ $2 ];then
	echo "Net charge = $2"
	chg=$2
else
	echo "Net charge was not defined. Default to 0"
	chg=0
fi
if [ $3 ];then
	echo "Spin multiplicity = $3"
	multi=$3
else
	echo "Spin multiplicity was not defined. Default to 1"
	multi=1
fi

Multiwfn $1 > /dev/null << EOF
100
2
2
tmp.xyz
0
q
EOF

cat << EOF > gau.gjf
%chk=gau.chk
$keyword_opt

test

$chg $multi
EOF
awk '{if (NR>2) print }' tmp.xyz >> gau.gjf
cat << EOF >> gau.gjf


EOF
rm tmp.xyz

echo Running optimization task via Gaussian...
$Gaussian < gau.gjf > gau.out

cat << EOF > gau.gjf
%chk=gau.chk
$keyword_SP geom=allcheck guess=read


EOF

$Gaussian < gau.gjf > gau.out

if grep -Fq "Normal termination" gau.out
then
	echo Done!
else
	echo The task has failed! Exit the script...
	exit 1
fi

echo Running single point task via Gaussian...
$Gaussian < gau.gjf > gau.out

if grep -Fq "Normal termination" gau.out
then
	echo Done!
else
	echo The task has failed! Exit the script...
	exit 1
fi

echo Running formchk...
formchk gau.chk> /dev/null

echo Running Multiwfn...
Multiwfn gau.fchk > /dev/null << EOF
7
18
8
1
gau.out
y
0
0
q
EOF

rm gau.gjf gau.fchk gau.chk gau.out
chgname=${1//$suffix/chg}
mv gau.chg $chgname

echo Finished! The optimized atomic coordinates with RESP charges \(the last column\) have been exported to $chgname in current folder
