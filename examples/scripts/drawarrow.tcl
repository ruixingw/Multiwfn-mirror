proc drawarrow {atmrange fragdx fragdy fragdz {scl 1}} {
#Determine arrow center
set sel [atomselect top $atmrange]
set cen [measure center $sel]
set cenx [lindex $cen 0]
set ceny [lindex $cen 1]
set cenz [lindex $cen 2]
puts "Geometry center: $cenx $ceny $cenz"
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
draw cylinder "$begx $begy $begz" "$endx $endy $endz" radius 0.2 filled yes resolution 20
set endx2 [expr $cenx+$fragdx/2]
set endy2 [expr $ceny+$fragdy/2]
set endz2 [expr $cenz+$fragdz/2]
draw cone "$endx $endy $endz" "$endx2 $endy2 $endz2" radius 0.5 resolution 20
}

