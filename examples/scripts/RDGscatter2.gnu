set terminal postscript landscape enhanced color 'Helvetica' 20
set encoding iso_8859_1
set output 'RDGscatter.ps' 
set key 
set ylabel 'RDG' font "Helvetica, 20" 
set xlabel 'sign({/Symbol-Oblique l}_2){/Symbol-Oblique r} (a.u.)' font "Helvetica, 20"
set pm3d map
set palette defined (-0.04 "blue",0 "green", 0.02 "red")
set format y "%.1f"
set format x "%.2f"
set format cb "%.2f"
set border lw 2
set xtic  -0.05,0.01,0.05 nomirror rotate font "Helvetica"
set ytic   0.0,0.2,2.0 nomirror font "Helvetica"
set cbtic  -0.04,0.01,0.02 nomirror font "Helvetica"
set xrange [-0.05:0.05]  
set yrange [0.0:2.0] 
set cbrange [-0.04:0.02]
plot 'output.txt' u 4:5:4 with points pointtype 31 pointsize 0.3 palette t ''  



