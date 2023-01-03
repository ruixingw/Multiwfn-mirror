#Written by Tian Lu (sobereva@sina.com), 2022-Apr-18
#This is a VMD script defining a command for plotting atomic quadrupole moment outputted by Multiwfn
#The atom_moment.txt exported by Multiwfn should be put to VMD folder first, which will be loaded
#Before invoking the "atomquad" command, structure of present system should be loaded to VMD first
#
#Usage: atomquad [selection] [color] [scale] [mode] [lowaniso]
# selection: Standard VMD selection syntax to select the atoms to be considered
# color: Standard color name in VMD for plotting ellipsoids, e.g. red, yellow, orange, green
# scale: Sum of three semi-axis lengths
# mode: 1 = Lengths of ellipsoid axes exhibit magnitude of traceless atomic quadrupole moment in different directions
#       2 = Lengths of ellipsoid axes exhibit spatial extent of electron density in different directions
# resolution: Resolution of ellipsoid, the larger the value, the smoother the ellipsoid, but higher the time-consuming in plotting
#             30 is acceptable, 50 is perfect
# lowaniso: 0 = Default case; 1 = The ellipsoid will more look like sphere (less anisotropic)
#  
#
#Simply running "atomquad" corresponding to "atomquad all yellow 1 1 30 0"
#An example of representing electron elongation tendency: atomquad all green 1 2


proc atomquad {{selection all} {color yellow} {scale 1} {mode 1} {resolution 30} {lowaniso 0}} {
display rendermode GLSL
light 3 on
#color Display Background white
draw delete all
draw material Transparent
set sel [atomselect top $selection]
#Cycle selected atoms
foreach idx [$sel list] {
	set file [open atom_moment.txt r]
	set foundatm 0
	set iatm [expr $idx+1]
	set labelname [format "Atom%6d" $iatm]
	while {[gets $file line] != -1} {
		if {[string first $labelname $line] != -1} {
			set foundatm 1
			puts " Information of atom $iatm"
			set atmx [[atomselect top "serial $iatm"] get x]
			set atmy [[atomselect top "serial $iatm"] get y]
			set atmz [[atomselect top "serial $iatm"] get z]
			gets $file line
			gets $file line
			for {set i 1} {$i <= 3} {incr i} {
				gets $file line
				scan $line " Eigenvalue $i:%f  Eigenvector:%f %f %f" eigval($i) eigvec(1,$i) eigvec(2,$i) eigvec(3,$i)
				#puts [format " Eigenvalue 1:%8.3f  Eigenvector:%8.3f %8.3f %8.3f" $eigval($i) $eigvec(1,$i) $eigvec(2,$i) $eigvec(3,$i)]
				if {$i == 1} {set eigvalmin $eigval(1)}
				if {$mode == 1} {
					if {$lowaniso == 0} {
						set eigval($i) [expr $eigval($i)+(1-$eigvalmin)]
					} else {
						set eigval($i) [expr sqrt($eigval($i)+(1-$eigvalmin))]
					}
				} else {
					if {$lowaniso == 0} {
						set eigval($i) [expr 1.0/($eigval($i)+(1-$eigvalmin))]
					} else {
						set eigval($i) [expr 1.0/sqrt($eigval($i)+(1-$eigvalmin))]
					}
				}
			}
			set fac [ expr $scale/($eigval(1)+$eigval(2)+$eigval(3)) ]
			for {set i 1} {$i <= 3} {incr i} {
				set eigval($i) [expr $fac*$eigval($i)]
				puts [format " Principal axis $i:%8.3f %8.3f %8.3f  Semi-axis length: %8.3f" $eigvec(1,$i) $eigvec(2,$i) $eigvec(3,$i) $eigval($i)]
			}
			ellipsoid $eigval(1) $eigval(2) $eigval(3) $atmx $atmy $atmz $color $resolution \
			$eigvec(1,1) $eigvec(1,2) $eigvec(1,3) $eigvec(2,1) $eigvec(2,2) $eigvec(2,3) $eigvec(3,1) $eigvec(3,2) $eigvec(3,3)
			break
		}
	}
	close $file
	puts " "
	if {$foundatm == 0} {puts " Error: Unable to find information of atom $iatm"}
}
puts Done!
}

# a = Length of semi-axis of ellipse in x direction
# b = Length of semi-axis of ellipse in y direction
# c = Length of semi-axis of ellipse in z direction
# x = X position of ellipse center
# y = Y position of ellipse center
# z = Z position of ellipse center

proc ellipsoid {a b c x y z color numvert a11 a12 a13 a21 a22 a23 a31 a32 a33} {
  global M_PI

  set minu 0
  set maxu [expr $M_PI * 2]
  set stepu [expr $maxu / $numvert]
  set minv [expr -1*$M_PI]
  set maxv [expr $M_PI]
  set stepv [expr $maxv / $numvert]
  set tiny 0.00001

  # Calculate X,Y,Z of ellipse vertices as xdata, ydata, zdata
  # u,v are azimuthal and polar angle
  for {set u $minu} {$u < [expr $maxu + $tiny]} {set u [expr $u + $stepu]} {
    for {set v $minv} {$v < [expr $maxv + $tiny]} {set v [expr $v + $stepv]} {
      set xdata($u,$v) [uv2x $a $u $v]
      set ydata($u,$v) [uv2y $b $u $v]
      set zdata($u,$v) [uv2z $c $u $v]     
    }
  }

  # Apply rotation and translation transformations on ellipse vertices
  for {set u $minu} {$u < [expr $maxu + $tiny]} {set u [expr $u + $stepu]} {
    for {set v $minv} {$v < [expr $maxv + $tiny]} {set v [expr $v + $stepv]} {
      set xdata_r($u,$v) [expr {($xdata($u,$v)*$a11+$ydata($u,$v)*$a12+$zdata($u,$v)*$a13) + $x}]
      set ydata_r($u,$v) [expr {($xdata($u,$v)*$a21+$ydata($u,$v)*$a22+$zdata($u,$v)*$a23) + $y}]
      set zdata_r($u,$v) [expr {($xdata($u,$v)*$a31+$ydata($u,$v)*$a32+$zdata($u,$v)*$a33) + $z}]
    }
  }

	draw color $color
  # Render triangles for ellipse vertices
  for {set u $minu} {$u < [expr $maxu + $tiny - $stepu]} {set u [expr $u + $stepu]} {
    for {set v $minv} {$v < [expr $maxv + $tiny - $stepv]} {set v [expr $v + $stepv]} {
      # Get the next two vertices
      set u2 [expr $u + $stepu]
      set v2 [expr $v + $stepv]
      draw triangle "$xdata_r($u,$v)  $ydata_r($u,$v)  $zdata_r($u,$v)" \
                    "$xdata_r($u2,$v)  $ydata_r($u2,$v)  $zdata_r($u2,$v)" \
                    "$xdata_r($u2,$v2) $ydata_r($u2,$v2) $zdata_r($u2,$v2)"
      draw triangle "$xdata_r($u2,$v2) $ydata_r($u2,$v2) $zdata_r($u2,$v2)" \
                    "$xdata_r($u,$v2) $ydata_r($u,$v2) $zdata_r($u,$v2)" \
                    "$xdata_r($u,$v)  $ydata_r($u,$v)  $zdata_r($u,$v)"
    }
  }
}

proc uv2x {a u v} {
  expr {$a*sin($u) * cos($v)} 
}  
proc uv2y {b u v} {
  expr {$b*cos($u) * cos($v)}
}
proc uv2z {c u v} {
  expr {$c*sin($v)} 
}

