# The same as RESP.sh, but without automatic geometry optimization
# Written by Tian Lu (sobereva@sina.com), last update: 2022-Dec-15
# Example:
# Calculating neutral singlet molecule in water: RESP_noopt.sh H2O.xyz
# Calculating anionic singlet molecule in water: RESP_noopt.sh ani.pdb -1 1
# Calculating neutral triplet molecule in ethanol: RESP_noopt.sh nico.xyz 0 3 ethanol
# Calculating cation doublet molecule in vacuum: RESP_noopt.sh maki.xyz 0 2 gas

#!/bin/bash

keyword_SP="# B3LYP/def2TZVP pop=MK IOp(6/33=2,6/42=6)"
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

if [ $4 ];then
	if [[ $4 == gas ]] ; then
		echo Calculations will be done in vacuum
		solvent=""
	else
		echo Solvent is $4
		solvent="scrf(solvent="$4")"
	fi
else
	solvent="scrf(solvent=water)"
	echo "Solvent name was not defined. Default to water"
fi
keyword_SP=$keyword_SP" "$solvent

#Obtain tmp.txt containing inputted geometry
Multiwfn $1 > /dev/null << EOF
100
2
2
tmp.xyz
0
q
EOF

#Generate gau.gjf for single point calculation
cat << EOF > gau.gjf
%chk=gau.chk
$keyword_SP

test

$chg $multi
EOF
awk '{if (NR>2) print }' tmp.xyz >> gau.gjf
cat << EOF >> gau.gjf


EOF
rm tmp.xyz

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
formchk gau.chk > /dev/null

echo Running Multiwfn...
Multiwfn gau.fchk -ispecial 1 > /dev/null << EOF
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

echo Finished! The inputted atomic coordinates with RESP charges \(the last column\) have been exported to $chgname in current folder
echo Please properly cite Multiwfn in your publication according to \"How to cite Multiwfn.pdf\" in Multiwfn package
