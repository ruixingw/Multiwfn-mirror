set nfps [molinfo top get numframes]
set natm [molinfo top get numatoms]
set topid [molinfo top get id]
set selall [atomselect top all]

# Load ds values of all atoms in all frames from ds.pqr into dsarray 2D array
set dsfile [open ds.pqr r]
for {set ifps 0} {$ifps < $nfps} {incr ifps} {
for {set iatm 1} {$iatm <= $natm} {incr iatm} {
gets $dsfile line
set tmpstr [string range $line 54 67]
set dsarray($ifps,$iatm) $tmpstr 
}
gets $dsfile empty
}
close $dsfile
puts "ds values have been successfully loaded from ds.pqr!"

# Update "charge" property when frame of top system is changed
trace variable vmd_frame($topid) w update_ds
proc update_ds {args} {
global vmd_frame dsarray natm topid selall
if {[info exist tmparr]==1} {unset tmparr}
for {set iatm 1} {$iatm <= $natm} {incr iatm} {
lappend tmparr $dsarray($vmd_frame($topid),$iatm)
}
$selall set charge $tmparr
}

# Set default representation
mol modcolor 0 $topid Charge
mol scaleminmax $topid 0 -0.4 0.4
mol modstyle 0 $topid CPK 0.8 0.3 12.0 12.0

