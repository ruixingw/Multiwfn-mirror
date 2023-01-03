# A script to calculate 1.2*CM5 charge based on Gaussian 16 and Multiwfn
# Written by Tian Lu (sobereva@sina.com), 2021-Jan-28
#
# Usage: 1.2CM5.sh [geometry file] [net charge] [spin multiplicity]
# "geometry file" should be any format that Multiwfn supports, e.g. xyz/mol/mol2/pdb/fch/molden/wfn/wfx/mwfn/gjf...
# "net charge" is default to 0 if not given
# "spin multiplicity" is default to 1 if not given
#
# Example:
# Calculating neutral singlet molecule: 1.2CM5.sh H2O.xyz
# Calculating anionic singlet molecule: 1.2CM5.sh ani.pdb -1 1
#
# Note:
#   This script invokes g16, if you are using other version of Gaussian, you should modify "Gaussian" variable at line 23
#   If this script does not run successfully, please check if Gaussian and Multiwfn have been 
# properly installed, and check content of the Gaussian output file "gau.out" in current folder
#   Optimization and calculation of CM5 charges will be conducted in vacuum under B3LYP-D3(BJ)/def2-SVP
# level. The finally produced 1.2*CM5 charges are suitable for OPLS-AA forcefield

#!/bin/bash

keyword_opt="# B3LYP/def2SVP em=GD3BJ opt=loose"
Gaussian=g16

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

if grep -Fq "Normal termination" gau.out
then
	echo Done!
else
	echo The task has failed! Please check gau.out to understand the reason. Exit the script...
	exit 1
fi

echo Running formchk...
formchk gau.chk> /dev/null

echo Running Multiwfn...
Multiwfn gau.fchk > /dev/null << EOF
7
16
1
y
0
q
EOF

rm gau.gjf gau.fchk gau.chk gau.out
chgname=${1//$suffix/chg}
awk '{printf ("%-3s %12.6f %12.6f %12.6f %15.10f\n",$1,$2,$3,$4,1.2*$5)}' gau.chg > $chgname
rm gau.chg

echo Finished! The optimized atomic coordinates with 1.2*CM5 charges \(the last column\) have been exported to $chgname in current folder
echo Please properly cite Multiwfn in your publication according to \"How to cite Multiwfn.pdf\" in Multiwfn package