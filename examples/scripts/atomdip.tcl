#Written by Tian Lu (sobereva@sina.com), 2022-Apr-18
#This is a VMD script defining a command for plotting atomic dipole moment outputted by Multiwfn
#The atom_moment.txt exported by Multiwfn should be put to VMD folder first, which will be loaded
#Before invoking the "atomdip" command, structure of present system should be loaded to VMD first
#
#Usage: atomdip [selection] [color] [scale] [radius]
# selection: Standard VMD selection syntax to select the atoms to be considered
# color: Standard color name in VMD for plotting ellipsoids, e.g. red, yellow, orange, green
# scale: Scale factor of arrow length
# radius: Radius of arrows
#
#Simply running "atomdip" corresponding to "atomdip all yellow 3 0.1"


proc atomdip {{selection all} {color yellow} {scale 3} {radius 0.1}} {
display rendermode GLSL
light 3 on
#color Display Background white
draw delete all
#draw material Transparent
draw color $color
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
			scan $line " Atomic dipole moment:%f %f %f" vecx vecy vecz
			set norm [expr sqrt($vecx**2 + $vecy**2 + $vecz**2)]
			puts [format " Atomic dipole moment:%8.3f %8.3f %8.3f  Norm:%8.3f" $vecx $vecy $vecz $norm]
			drawarrow "serial $iatm" $vecx $vecy $vecz $scale $radius 0
			break
		}
	}
	close $file
	puts " "
	if {$foundatm == 0} {puts " Error: Unable to find information of atom $iatm"}
}
puts Done!
}


#A script for plotting arrow, released as a part of Multiwfn
#Written by Tian Lu (sobereva@sina.com, Beijing Kein Research Center for Natural Sciences), 2020-Sep-8

proc drawarrow {atmrange fragdx fragdy fragdz {scl 1} {rad 0.2} {showgoc 1}} {
#Determine arrow center
set sel [atomselect top $atmrange]
set cen [measure center $sel]
set cenx [lindex $cen 0]
set ceny [lindex $cen 1]
set cenz [lindex $cen 2]
if {$showgoc==1} {puts "Geometry center: $cenx $ceny $cenz"}
#Scale vector
set fragdx [expr $fragdx*$scl]
set fragdy [expr $fragdy*$scl]
set fragdz [expr $fragdz*$scl]
#Draw arrow
set body 0.75
set begx [expr $cenx-$fragdx/2]
set begy [expr $ceny-$fragdy/2]
set begz [expr $cenz-$fragdz/2]
set endx [expr $cenx+$fragdx*$body-$fragdx/2]
set endy [expr $ceny+$fragdy*$body-$fragdy/2]
set endz [expr $cenz+$fragdz*$body-$fragdz/2]
draw cylinder "$begx $begy $begz" "$endx $endy $endz" radius $rad filled yes resolution 20
set endx2 [expr $cenx+$fragdx/2]
set endy2 [expr $ceny+$fragdy/2]
set endz2 [expr $cenz+$fragdz/2]
draw cone "$endx $endy $endz" "$endx2 $endy2 $endz2" radius [expr $rad*2.5] resolution 20
}
