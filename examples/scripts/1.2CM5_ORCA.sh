# A script to calculate 1.2*CM5 charge based on ORCA and Multiwfn
# Written by Tian Lu (sobereva@sina.com), 2022-Mar-8
#
# Optimization and calculation of CM5 charges will be conducted in vacuum under B3LYP-D3(BJ)/def2-SVP
# level. The finally produced 1.2*CM5 charges are suitable for OPLS-AA forcefield.
#
# Usage: 1.2CM5.sh [geometry file] [net charge] [spin multiplicity]
# "geometry file" should be any format that Multiwfn supports, e.g. xyz/mol/mol2/pdb/fch/molden/wfn/wfx/mwfn/gjf...
# "net charge" is default to 0 if not given
# "spin multiplicity" is default to 1 if not given
#
# Example:
# Calculating neutral singlet molecule: 1.2CM5.sh H2O.xyz
# Calculating anionic singlet molecule: 1.2CM5.sh ani.pdb -1 1

#!/bin/bash

#Set actual paths of ORCA and orca_2mkl utility here
ORCA="/sob/orca503/orca"
orca_2mkl="/sob/orca503/orca_2mkl"

#Set number of CPU cores used in calculation here
nprocs=8
maxcore=1000

keyword="! B3LYP/G D3 def2-SVP def2/J RIJCOSX"

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

### Convert current file to tmp.xyz
Multiwfn $1 > /dev/null << EOF
100
2
2
tmp.xyz
0
q
EOF

### Create input file for optimization (opt.inp)
cat << EOF > opt.inp
$keyword
%maxcore $maxcore
%pal nprocs $nprocs end
%geom Convergence loose end
EOF
echo "* xyz $chg $multi" >> opt.inp
awk '{if (NR>2) print }' tmp.xyz >> opt.inp
echo "*" >> opt.inp
rm tmp.xyz

### Run optimization
echo Running optimization task via ORCA...
$ORCA opt.inp > opt.out

if grep -Fq "ORCA TERMINATED NORMALLY" opt.out
then
	echo Done!
else
	echo The optimization task has failed! Please check content of opt.out to find reason
	echo The script is terminated
	mv opt.out tmp.out
	rm opt.* opt_*
	mv tmp.out opt.out
	exit 1
fi

### Convert to .molden file
echo Running orca_2mkl...
$orca_2mkl opt -molden > /dev/null

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

cat Nval.txt opt.molden.input > opt.molden

### Calculate 1.2*CM5 charges
echo Running Multiwfn...
Multiwfn opt.molden > /dev/null << EOF
7
-16
1
y
0
q
EOF

chgname=${1//$suffix/chg}

mv opt.chg $chgname
rm opt.* opt_* Nval.txt

echo Finished! The optimized atomic coordinates with 1.2*CM5 charges \(the last column\) have been exported to $chgname in current folder
echo Please properly cite Multiwfn in your publication according to \"How to cite Multiwfn.pdf\" in Multiwfn package