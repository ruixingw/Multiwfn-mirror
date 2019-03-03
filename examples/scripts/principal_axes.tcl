set lena [molinfo top get a]
set lenb [molinfo top get b]
set lenc [molinfo top get c]
set cenx [expr $lena/2]
set ceny [expr $lenb/2]
set cenz [expr $lenc/2]
draw color red
draw cylinder "$cenx $ceny 0" "$cenx $ceny $lenc" radius 0.15 resolution 20
draw color green
draw cylinder "0 $ceny $cenz" "$lena $ceny $cenz" radius 0.15 resolution 20
draw color blue
draw cylinder "$cenx 0 $cenz" "$cenx $lenb $cenz" radius 0.15 resolution 20
