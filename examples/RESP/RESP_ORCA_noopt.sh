# The same as RESP_ORCA.sh, but without automatic geometry optimization
# Written by Tian Lu (sobereva@sina.com), last update: 2022-Aug-6
# Examples:
# Calculating neutral singlet molecule in water: RESP_ORCA.sh H2O.xyz
# Calculating anionic singlet molecule in water: RESP_ORCA.sh ani.pdb -1 1
# Calculating neutral triplet molecule in ethanol: RESP_ORCA.sh nico.xyz 0 3 ethanol
# Calculating neutral singlet molecule in ETHYL ETHANOATE: RESP_ORCA.sh nico.xyz 0 1 "ETHYL ETHANOATE"
# Calculating cation doublet molecule in vacuum: RESP_ORCA.sh maki.xyz 0 2 gas

#!/bin/bash

#Set actual paths of ORCA and orca_2mkl utility here
ORCA="/sob/orca503/orca"
orca_2mkl="/sob/orca503/orca_2mkl"

#Set number of CPU cores used in calculation here
nprocs=8
maxcore=1000

keyword_SP="! B3LYP/G D3 def2-TZVP def2/J RIJCOSX"

export inname=$1
filename=${inname%.*}
suffix=${inname##*.}

### Parse arguments
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

if [ "$4" ];then
	if [[ $4 == gas ]] ; then
  		echo Calculations will be done in vacuum
  		solvent=""
	else
		echo Solvent is $4
  	solvent=$4
	fi
else
	echo "Solvent name was not defined. Default to water"
  solvent="Water"
fi

### Convert current file to tmp.xyz
Multiwfn $1 > /dev/null << EOF
100
2
2
tmp.xyz
0
q
EOF

### Create input file for single point (SP.inp)
cat << EOF > SP.inp
$keyword_SP
%maxcore $maxcore
%pal nprocs $nprocs end
EOF
if [[ $solvent != "" ]] ; then
cat << EOF >> SP.inp
%cpcm
smd true
SMDsolvent "$solvent"
end
EOF
fi
echo "* xyz $chg $multi" >> SP.inp
awk '{if (NR>2) print }' tmp.xyz >> SP.inp
echo "*" >> SP.inp
rm tmp.xyz

### Run single point
echo Running single point task via ORCA...
$ORCA SP.inp > SP.out

if grep -Fq "ORCA TERMINATED NORMALLY" SP.out
then
	echo Done!
else
	echo The single point task has failed! Please check content of SP.out to find reason
	echo The script is terminated
	mv SP.out tmp.out
	rm SP.* SP_*
	mv tmp.out SP.out
	exit 1
fi

### Convert to .molden file
echo Running orca_2mkl...
$orca_2mkl SP -molden > /dev/null

#Field containing number of valence electrons of def2 pseudopotential basis set
cat << EOF > Nval.txt
[Nval]
Rb  9
Sr 10
Y  11
Zr 12
Nb 13
Mo 14
Tc 15
Ru 16
Rh 17
Pd 18
Ag 19
Cd 20
In 21
Sn 22
Sb 23
Te 24
I  25
Xe 26
Cs  9
Ba 10
La 11
Ce 30
Pr 31
Nd 32
Pm 33
Sm 34
Eu 35
Gd 36
Tb 37
Dy 38
Ho 39
Er 40
Tm 41
Yb 42
Lu 43
Hf 12
Ta 13
W  14
Re 15
Os 16
Ir 17
Pt 18
Au 19
Hg 20
Tl 21
Pb 22
Bi 23
Po 24
At 25
Rn 26
EOF

cat Nval.txt SP.molden.input > SP.molden

### Calculate RESP charges
echo Running Multiwfn...
Multiwfn SP.molden -ispecial 1 > /dev/null << EOF
7
18
1
y
0
0
q
EOF

### Finalize
chgname=${1//$suffix/chg}
mv SP.chg $chgname
rm SP.* SP_* Nval.txt

echo Finished! The inputted atomic coordinates with RESP charges \(the last column\) have been exported to $chgname in current folder
echo Please properly cite Multiwfn in your publication according to \"How to cite Multiwfn.pdf\" in Multiwfn package
