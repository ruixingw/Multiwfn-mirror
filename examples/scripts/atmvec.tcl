set filename in-plane_atmdip.txt

#Scale factor of arrow length
set sclfac 40
#Radius of arrow
set rad 0.1
#Color of arrow
set color yellow
#Criterion of drawing arrow
set crit 0.

# Load atomic vector
set myfile [open $filename r]
set natm [molinfo top get numatoms]
for {set i 1} {$i<=$natm} {incr i} {
gets $myfile line
scan $line "%f %f %f" fx($i) fy($i) fz($i)
}
close $myfile

set maxf 0
for {set i 1} {$i<=$natm} {incr i} {
set norm [expr sqrt($fx($i)**2+$fy($i)**2+$fz($i)**2)]
if {$norm>$maxf} {set maxf $norm}
}

source drawarrow.tcl
draw delete all
draw color $color
for {set i 1} {$i<=$natm} {incr i} {
set norm [expr sqrt($fx($i)**2+$fy($i)**2+$fz($i)**2)]
if {$norm>[expr $crit*$maxf]} { 
drawarrow "serial $i" $fx($i) $fy($i) $fz($i) $sclfac $rad 0
}
}

