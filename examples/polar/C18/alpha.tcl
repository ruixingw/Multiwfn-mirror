color Display Background white
set j 0                                         
for {set i 1} {$i<=500} {incr i} {              
incr j                                          
set red [expr double($j)/500]                       
set green [expr double($j)/500]                     
set blue 1                                      
color change rgb [expr $i+50] $red $green $blue
}                                               
set j 0                                         
for {set i 501} {$i<=1000} {incr i} {            
incr j                                          
set red 1                                       
set green [expr double(500-$j)/500]               
set blue [expr double(500-$j)/500]                
color change rgb [expr $i+50] $red $green $blue
}                                               
draw color   51
draw cylinder {   0.000   0.000   5.908} {   0.000   0.000   6.225} radius 0.03 filled yes resolution 20
draw cone {   0.000   0.000   6.225} {   0.000   0.000   6.396} radius 0.06 resolution 20
draw color  102
draw cylinder {   0.440   0.763   5.842} {   0.535   0.927   6.156} radius 0.03 filled yes resolution 20
draw cone {   0.535   0.927   6.156} {   0.586   1.016   6.325} radius 0.06 resolution 20
draw color  102
draw cylinder {  -0.440   0.763   5.842} {  -0.535   0.927   6.156} radius 0.03 filled yes resolution 20
draw cone {  -0.535   0.927   6.156} {  -0.586   1.016   6.325} radius 0.06 resolution 20
draw color  102
draw cylinder {  -0.881   0.000   5.842} {  -1.070   0.000   6.156} radius 0.03 filled yes resolution 20
draw cone {  -1.070   0.000   6.156} {  -1.173   0.000   6.325} radius 0.06 resolution 20
draw color  102
draw cylinder {  -0.440  -0.763   5.842} {  -0.535  -0.927   6.156} radius 0.03 filled yes resolution 20
draw cone {  -0.535  -0.927   6.156} {  -0.586  -1.016   6.325} radius 0.06 resolution 20
draw color  102
draw cylinder {   0.440  -0.763   5.842} {   0.535  -0.927   6.156} radius 0.03 filled yes resolution 20
draw cone {   0.535  -0.927   6.156} {   0.586  -1.016   6.325} radius 0.06 resolution 20
draw color  102
draw cylinder {   0.881  -0.000   5.842} {   1.070  -0.000   6.156} radius 0.03 filled yes resolution 20
draw cone {   1.070  -0.000   6.156} {   1.173  -0.000   6.325} radius 0.06 resolution 20
draw color  223
draw cylinder {   1.508   0.871   5.645} {   1.833   1.058   5.949} radius 0.03 filled yes resolution 20
draw cone {   1.833   1.058   5.949} {   2.008   1.160   6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {   0.871   1.508   5.645} {   1.058   1.833   5.949} radius 0.03 filled yes resolution 20
draw cone {   1.058   1.833   5.949} {   1.160   2.008   6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {   0.000   1.741   5.645} {   0.000   2.117   5.949} radius 0.03 filled yes resolution 20
draw cone {   0.000   2.117   5.949} {   0.000   2.319   6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {  -0.871   1.508   5.645} {  -1.058   1.833   5.949} radius 0.03 filled yes resolution 20
draw cone {  -1.058   1.833   5.949} {  -1.160   2.008   6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {  -1.508   0.871   5.645} {  -1.833   1.058   5.949} radius 0.03 filled yes resolution 20
draw cone {  -1.833   1.058   5.949} {  -2.008   1.160   6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {  -1.741   0.000   5.645} {  -2.117   0.000   5.949} radius 0.03 filled yes resolution 20
draw cone {  -2.117   0.000   5.949} {  -2.319   0.000   6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {  -1.508  -0.871   5.645} {  -1.833  -1.058   5.949} radius 0.03 filled yes resolution 20
draw cone {  -1.833  -1.058   5.949} {  -2.008  -1.160   6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {  -0.871  -1.508   5.645} {  -1.058  -1.833   5.949} radius 0.03 filled yes resolution 20
draw cone {  -1.058  -1.833   5.949} {  -1.160  -2.008   6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {  -0.000  -1.741   5.645} {  -0.000  -2.117   5.949} radius 0.03 filled yes resolution 20
draw cone {  -0.000  -2.117   5.949} {  -0.000  -2.319   6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {   0.871  -1.508   5.645} {   1.058  -1.833   5.949} radius 0.03 filled yes resolution 20
draw cone {   1.058  -1.833   5.949} {   1.160  -2.008   6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {   1.508  -0.871   5.645} {   1.833  -1.058   5.949} radius 0.03 filled yes resolution 20
draw cone {   1.833  -1.058   5.949} {   2.008  -1.160   6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {   1.741  -0.000   5.645} {   2.117  -0.000   5.949} radius 0.03 filled yes resolution 20
draw cone {   2.117  -0.000   5.949} {   2.319  -0.000   6.112} radius 0.06 resolution 20
draw color  370
draw cylinder {   2.409   0.877   5.323} {   2.928   1.066   5.609} radius 0.03 filled yes resolution 20
draw cone {   2.928   1.066   5.609} {   3.208   1.168   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   1.964   1.648   5.323} {   2.387   2.003   5.609} radius 0.03 filled yes resolution 20
draw cone {   2.387   2.003   5.609} {   2.615   2.194   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   1.282   2.220   5.323} {   1.558   2.699   5.609} radius 0.03 filled yes resolution 20
draw cone {   1.558   2.699   5.609} {   1.707   2.956   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   0.445   2.524   5.323} {   0.541   3.069   5.609} radius 0.03 filled yes resolution 20
draw cone {   0.541   3.069   5.609} {   0.593   3.362   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -0.445   2.524   5.323} {  -0.541   3.069   5.609} radius 0.03 filled yes resolution 20
draw cone {  -0.541   3.069   5.609} {  -0.593   3.362   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -1.282   2.220   5.323} {  -1.558   2.699   5.609} radius 0.03 filled yes resolution 20
draw cone {  -1.558   2.699   5.609} {  -1.707   2.956   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -1.964   1.648   5.323} {  -2.387   2.003   5.609} radius 0.03 filled yes resolution 20
draw cone {  -2.387   2.003   5.609} {  -2.615   2.194   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -2.409   0.877   5.323} {  -2.928   1.066   5.609} radius 0.03 filled yes resolution 20
draw cone {  -2.928   1.066   5.609} {  -3.208   1.168   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -2.563   0.000   5.323} {  -3.116   0.000   5.609} radius 0.03 filled yes resolution 20
draw cone {  -3.116   0.000   5.609} {  -3.414   0.000   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -2.409  -0.877   5.323} {  -2.928  -1.066   5.609} radius 0.03 filled yes resolution 20
draw cone {  -2.928  -1.066   5.609} {  -3.208  -1.168   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -1.964  -1.648   5.323} {  -2.387  -2.003   5.609} radius 0.03 filled yes resolution 20
draw cone {  -2.387  -2.003   5.609} {  -2.615  -2.194   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -1.282  -2.220   5.323} {  -1.558  -2.699   5.609} radius 0.03 filled yes resolution 20
draw cone {  -1.558  -2.699   5.609} {  -1.707  -2.956   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -0.445  -2.524   5.323} {  -0.541  -3.069   5.609} radius 0.03 filled yes resolution 20
draw cone {  -0.541  -3.069   5.609} {  -0.593  -3.362   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   0.445  -2.524   5.323} {   0.541  -3.069   5.609} radius 0.03 filled yes resolution 20
draw cone {   0.541  -3.069   5.609} {   0.593  -3.362   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   1.282  -2.220   5.323} {   1.558  -2.699   5.609} radius 0.03 filled yes resolution 20
draw cone {   1.558  -2.699   5.609} {   1.707  -2.956   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   1.964  -1.648   5.323} {   2.387  -2.003   5.609} radius 0.03 filled yes resolution 20
draw cone {   2.387  -2.003   5.609} {   2.615  -2.194   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   2.409  -0.877   5.323} {   2.928  -1.066   5.609} radius 0.03 filled yes resolution 20
draw cone {   2.928  -1.066   5.609} {   3.208  -1.168   5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   2.563  -0.000   5.323} {   3.116  -0.000   5.609} radius 0.03 filled yes resolution 20
draw cone {   3.116  -0.000   5.609} {   3.414  -0.000   5.763} radius 0.06 resolution 20
draw color  519
draw cylinder {   3.215   0.861   4.881} {   3.908   1.047   5.144} radius 0.03 filled yes resolution 20
draw cone {   3.908   1.047   5.144} {   4.281   1.147   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   2.882   1.664   4.881} {   3.504   2.023   5.144} radius 0.03 filled yes resolution 20
draw cone {   3.504   2.023   5.144} {   3.838   2.216   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   2.353   2.353   4.881} {   2.861   2.861   5.144} radius 0.03 filled yes resolution 20
draw cone {   2.861   2.861   5.144} {   3.134   3.134   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   1.664   2.882   4.881} {   2.023   3.504   5.144} radius 0.03 filled yes resolution 20
draw cone {   2.023   3.504   5.144} {   2.216   3.838   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   0.861   3.215   4.881} {   1.047   3.908   5.144} radius 0.03 filled yes resolution 20
draw cone {   1.047   3.908   5.144} {   1.147   4.281   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   0.000   3.328   4.881} {   0.000   4.046   5.144} radius 0.03 filled yes resolution 20
draw cone {   0.000   4.046   5.144} {   0.000   4.432   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -0.861   3.215   4.881} {  -1.047   3.908   5.144} radius 0.03 filled yes resolution 20
draw cone {  -1.047   3.908   5.144} {  -1.147   4.281   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -1.664   2.882   4.881} {  -2.023   3.504   5.144} radius 0.03 filled yes resolution 20
draw cone {  -2.023   3.504   5.144} {  -2.216   3.838   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -2.353   2.353   4.881} {  -2.861   2.861   5.144} radius 0.03 filled yes resolution 20
draw cone {  -2.861   2.861   5.144} {  -3.134   3.134   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -2.882   1.664   4.881} {  -3.504   2.023   5.144} radius 0.03 filled yes resolution 20
draw cone {  -3.504   2.023   5.144} {  -3.838   2.216   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -3.215   0.861   4.881} {  -3.908   1.047   5.144} radius 0.03 filled yes resolution 20
draw cone {  -3.908   1.047   5.144} {  -4.281   1.147   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -3.328   0.000   4.881} {  -4.046   0.000   5.144} radius 0.03 filled yes resolution 20
draw cone {  -4.046   0.000   5.144} {  -4.432   0.000   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -3.215  -0.861   4.881} {  -3.908  -1.047   5.144} radius 0.03 filled yes resolution 20
draw cone {  -3.908  -1.047   5.144} {  -4.281  -1.147   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -2.882  -1.664   4.881} {  -3.504  -2.023   5.144} radius 0.03 filled yes resolution 20
draw cone {  -3.504  -2.023   5.144} {  -3.838  -2.216   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -2.353  -2.353   4.881} {  -2.861  -2.861   5.144} radius 0.03 filled yes resolution 20
draw cone {  -2.861  -2.861   5.144} {  -3.134  -3.134   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -1.664  -2.882   4.881} {  -2.023  -3.504   5.144} radius 0.03 filled yes resolution 20
draw cone {  -2.023  -3.504   5.144} {  -2.216  -3.838   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -0.861  -3.215   4.881} {  -1.047  -3.908   5.144} radius 0.03 filled yes resolution 20
draw cone {  -1.047  -3.908   5.144} {  -1.147  -4.281   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -0.000  -3.328   4.881} {  -0.000  -4.046   5.144} radius 0.03 filled yes resolution 20
draw cone {  -0.000  -4.046   5.144} {  -0.000  -4.432   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   0.861  -3.215   4.881} {   1.047  -3.908   5.144} radius 0.03 filled yes resolution 20
draw cone {   1.047  -3.908   5.144} {   1.147  -4.281   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   1.664  -2.882   4.881} {   2.023  -3.504   5.144} radius 0.03 filled yes resolution 20
draw cone {   2.023  -3.504   5.144} {   2.216  -3.838   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   2.353  -2.353   4.881} {   2.861  -2.861   5.144} radius 0.03 filled yes resolution 20
draw cone {   2.861  -2.861   5.144} {   3.134  -3.134   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   2.882  -1.664   4.881} {   3.504  -2.023   5.144} radius 0.03 filled yes resolution 20
draw cone {   3.504  -2.023   5.144} {   3.838  -2.216   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   3.215  -0.861   4.881} {   3.908  -1.047   5.144} radius 0.03 filled yes resolution 20
draw cone {   3.908  -1.047   5.144} {   4.281  -1.147   5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   3.328  -0.000   4.881} {   4.046  -0.000   5.144} radius 0.03 filled yes resolution 20
draw cone {   4.046  -0.000   5.144} {   4.432  -0.000   5.285} radius 0.06 resolution 20
draw color  658
draw cylinder {   3.924   0.864   4.331} {   4.771   1.050   4.564} radius 0.03 filled yes resolution 20
draw cone {   4.771   1.050   4.564} {   5.226   1.150   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   3.647   1.687   4.331} {   4.433   2.051   4.564} radius 0.03 filled yes resolution 20
draw cone {   4.433   2.051   4.564} {   4.857   2.247   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   3.199   2.432   4.331} {   3.889   2.956   4.564} radius 0.03 filled yes resolution 20
draw cone {   3.889   2.956   4.564} {   4.260   3.239   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   2.601   3.063   4.331} {   3.162   3.723   4.564} radius 0.03 filled yes resolution 20
draw cone {   3.162   3.723   4.564} {   3.464   4.079   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   1.882   3.550   4.331} {   2.288   4.316   4.564} radius 0.03 filled yes resolution 20
draw cone {   2.288   4.316   4.564} {   2.507   4.728   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   1.075   3.872   4.331} {   1.307   4.707   4.564} radius 0.03 filled yes resolution 20
draw cone {   1.307   4.707   4.564} {   1.432   5.156   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   0.218   4.012   4.331} {   0.264   4.878   4.564} radius 0.03 filled yes resolution 20
draw cone {   0.264   4.878   4.564} {   0.290   5.344   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -0.650   3.965   4.331} {  -0.790   4.821   4.564} radius 0.03 filled yes resolution 20
draw cone {  -0.790   4.821   4.564} {  -0.866   5.281   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -1.487   3.733   4.331} {  -1.808   4.538   4.564} radius 0.03 filled yes resolution 20
draw cone {  -1.808   4.538   4.564} {  -1.981   4.971   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -2.255   3.326   4.331} {  -2.741   4.043   4.564} radius 0.03 filled yes resolution 20
draw cone {  -2.741   4.043   4.564} {  -3.003   4.429   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -2.917   2.763   4.331} {  -3.546   3.359   4.564} radius 0.03 filled yes resolution 20
draw cone {  -3.546   3.359   4.564} {  -3.885   3.680   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -3.443   2.072   4.331} {  -4.186   2.518   4.564} radius 0.03 filled yes resolution 20
draw cone {  -4.186   2.518   4.564} {  -4.585   2.759   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -3.808   1.283   4.331} {  -4.629   1.560   4.564} radius 0.03 filled yes resolution 20
draw cone {  -4.629   1.560   4.564} {  -5.071   1.709   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -3.995   0.434   4.331} {  -4.856   0.528   4.564} radius 0.03 filled yes resolution 20
draw cone {  -4.856   0.528   4.564} {  -5.320   0.579   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -3.995  -0.434   4.331} {  -4.856  -0.528   4.564} radius 0.03 filled yes resolution 20
draw cone {  -4.856  -0.528   4.564} {  -5.320  -0.579   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -3.808  -1.283   4.331} {  -4.629  -1.560   4.564} radius 0.03 filled yes resolution 20
draw cone {  -4.629  -1.560   4.564} {  -5.071  -1.709   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -3.443  -2.072   4.331} {  -4.186  -2.518   4.564} radius 0.03 filled yes resolution 20
draw cone {  -4.186  -2.518   4.564} {  -4.585  -2.759   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -2.917  -2.763   4.331} {  -3.546  -3.359   4.564} radius 0.03 filled yes resolution 20
draw cone {  -3.546  -3.359   4.564} {  -3.885  -3.680   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -2.255  -3.326   4.331} {  -2.741  -4.043   4.564} radius 0.03 filled yes resolution 20
draw cone {  -2.741  -4.043   4.564} {  -3.003  -4.429   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -1.487  -3.733   4.331} {  -1.808  -4.538   4.564} radius 0.03 filled yes resolution 20
draw cone {  -1.808  -4.538   4.564} {  -1.981  -4.971   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -0.650  -3.965   4.331} {  -0.790  -4.821   4.564} radius 0.03 filled yes resolution 20
draw cone {  -0.790  -4.821   4.564} {  -0.866  -5.281   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   0.218  -4.012   4.331} {   0.264  -4.878   4.564} radius 0.03 filled yes resolution 20
draw cone {   0.264  -4.878   4.564} {   0.290  -5.344   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   1.075  -3.872   4.331} {   1.307  -4.707   4.564} radius 0.03 filled yes resolution 20
draw cone {   1.307  -4.707   4.564} {   1.432  -5.156   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   1.882  -3.550   4.331} {   2.288  -4.316   4.564} radius 0.03 filled yes resolution 20
draw cone {   2.288  -4.316   4.564} {   2.507  -4.728   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   2.601  -3.063   4.331} {   3.162  -3.723   4.564} radius 0.03 filled yes resolution 20
draw cone {   3.162  -3.723   4.564} {   3.464  -4.079   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   3.199  -2.432   4.331} {   3.889  -2.956   4.564} radius 0.03 filled yes resolution 20
draw cone {   3.889  -2.956   4.564} {   4.260  -3.239   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   3.647  -1.687   4.331} {   4.433  -2.051   4.564} radius 0.03 filled yes resolution 20
draw cone {   4.433  -2.051   4.564} {   4.857  -2.247   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   3.924  -0.864   4.331} {   4.771  -1.050   4.564} radius 0.03 filled yes resolution 20
draw cone {   4.771  -1.050   4.564} {   5.226  -1.150   4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   4.018  -0.000   4.331} {   4.885  -0.000   4.564} radius 0.03 filled yes resolution 20
draw cone {   4.885  -0.000   4.564} {   5.351  -0.000   4.689} radius 0.06 resolution 20
draw color  782
draw cylinder {   4.535   0.874   3.683} {   5.514   1.063   3.881} radius 0.03 filled yes resolution 20
draw cone {   5.514   1.063   3.881} {   6.040   1.164   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   4.288   1.717   3.683} {   5.213   2.087   3.881} radius 0.03 filled yes resolution 20
draw cone {   5.213   2.087   3.881} {   5.711   2.286   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   3.886   2.497   3.683} {   4.724   3.036   3.881} radius 0.03 filled yes resolution 20
draw cone {   4.724   3.036   3.881} {   5.175   3.326   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   3.343   3.187   3.683} {   4.064   3.875   3.881} radius 0.03 filled yes resolution 20
draw cone {   4.064   3.875   3.881} {   4.452   4.245   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   2.679   3.762   3.683} {   3.257   4.574   3.881} radius 0.03 filled yes resolution 20
draw cone {   3.257   4.574   3.881} {   3.568   5.011   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   1.919   4.202   3.683} {   2.333   5.108   3.881} radius 0.03 filled yes resolution 20
draw cone {   2.333   5.108   3.881} {   2.555   5.595   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   1.089   4.489   3.683} {   1.324   5.457   3.881} radius 0.03 filled yes resolution 20
draw cone {   1.324   5.457   3.881} {   1.450   5.978   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   0.220   4.614   3.683} {   0.267   5.609   3.881} radius 0.03 filled yes resolution 20
draw cone {   0.267   5.609   3.881} {   0.293   6.144   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -0.657   4.572   3.683} {  -0.799   5.558   3.881} radius 0.03 filled yes resolution 20
draw cone {  -0.799   5.558   3.881} {  -0.875   6.089   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -1.511   4.365   3.683} {  -1.836   5.306   3.881} radius 0.03 filled yes resolution 20
draw cone {  -1.836   5.306   3.881} {  -2.012   5.813   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -2.309   4.000   3.683} {  -2.807   4.863   3.881} radius 0.03 filled yes resolution 20
draw cone {  -2.807   4.863   3.881} {  -3.076   5.327   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -3.025   3.491   3.683} {  -3.677   4.244   3.881} radius 0.03 filled yes resolution 20
draw cone {  -3.677   4.244   3.881} {  -4.028   4.649   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -3.631   2.855   3.683} {  -4.414   3.471   3.881} radius 0.03 filled yes resolution 20
draw cone {  -4.414   3.471   3.881} {  -4.835   3.802   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -4.105   2.117   3.683} {  -4.991   2.573   3.881} radius 0.03 filled yes resolution 20
draw cone {  -4.991   2.573   3.881} {  -5.468   2.819   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -4.432   1.301   3.683} {  -5.388   1.582   3.881} radius 0.03 filled yes resolution 20
draw cone {  -5.388   1.582   3.881} {  -5.902   1.733   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -4.598   0.439   3.683} {  -5.590   0.534   3.881} radius 0.03 filled yes resolution 20
draw cone {  -5.590   0.534   3.881} {  -6.123   0.585   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -4.598  -0.439   3.683} {  -5.590  -0.534   3.881} radius 0.03 filled yes resolution 20
draw cone {  -5.590  -0.534   3.881} {  -6.123  -0.585   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -4.432  -1.301   3.683} {  -5.388  -1.582   3.881} radius 0.03 filled yes resolution 20
draw cone {  -5.388  -1.582   3.881} {  -5.902  -1.733   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -4.105  -2.117   3.683} {  -4.991  -2.573   3.881} radius 0.03 filled yes resolution 20
draw cone {  -4.991  -2.573   3.881} {  -5.468  -2.819   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -3.631  -2.855   3.683} {  -4.414  -3.471   3.881} radius 0.03 filled yes resolution 20
draw cone {  -4.414  -3.471   3.881} {  -4.835  -3.802   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -3.025  -3.491   3.683} {  -3.677  -4.244   3.881} radius 0.03 filled yes resolution 20
draw cone {  -3.677  -4.244   3.881} {  -4.028  -4.649   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -2.309  -4.000   3.683} {  -2.807  -4.863   3.881} radius 0.03 filled yes resolution 20
draw cone {  -2.807  -4.863   3.881} {  -3.076  -5.327   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -1.511  -4.365   3.683} {  -1.836  -5.306   3.881} radius 0.03 filled yes resolution 20
draw cone {  -1.836  -5.306   3.881} {  -2.012  -5.813   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -0.657  -4.572   3.683} {  -0.799  -5.558   3.881} radius 0.03 filled yes resolution 20
draw cone {  -0.799  -5.558   3.881} {  -0.875  -6.089   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   0.220  -4.614   3.683} {   0.267  -5.609   3.881} radius 0.03 filled yes resolution 20
draw cone {   0.267  -5.609   3.881} {   0.293  -6.144   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   1.089  -4.489   3.683} {   1.324  -5.457   3.881} radius 0.03 filled yes resolution 20
draw cone {   1.324  -5.457   3.881} {   1.450  -5.978   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   1.919  -4.202   3.683} {   2.333  -5.108   3.881} radius 0.03 filled yes resolution 20
draw cone {   2.333  -5.108   3.881} {   2.555  -5.595   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   2.679  -3.762   3.683} {   3.257  -4.574   3.881} radius 0.03 filled yes resolution 20
draw cone {   3.257  -4.574   3.881} {   3.568  -5.011   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   3.343  -3.187   3.683} {   4.064  -3.875   3.881} radius 0.03 filled yes resolution 20
draw cone {   4.064  -3.875   3.881} {   4.452  -4.245   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   3.886  -2.497   3.683} {   4.724  -3.036   3.881} radius 0.03 filled yes resolution 20
draw cone {   4.724  -3.036   3.881} {   5.175  -3.326   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   4.288  -1.717   3.683} {   5.213  -2.087   3.881} radius 0.03 filled yes resolution 20
draw cone {   5.213  -2.087   3.881} {   5.711  -2.286   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   4.535  -0.874   3.683} {   5.514  -1.063   3.881} radius 0.03 filled yes resolution 20
draw cone {   5.514  -1.063   3.881} {   6.040  -1.164   3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   4.619  -0.000   3.683} {   5.615  -0.000   3.881} radius 0.03 filled yes resolution 20
draw cone {   5.615  -0.000   3.881} {   6.151  -0.000   3.988} radius 0.06 resolution 20
draw color  886
draw cylinder {   5.043   0.865   2.954} {   6.130   1.051   3.113} radius 0.03 filled yes resolution 20
draw cone {   6.130   1.051   3.113} {   6.716   1.152   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   4.824   1.704   2.954} {   5.864   2.072   3.113} radius 0.03 filled yes resolution 20
draw cone {   5.864   2.072   3.113} {   6.425   2.270   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   4.467   2.495   2.954} {   5.430   3.033   3.113} radius 0.03 filled yes resolution 20
draw cone {   5.430   3.033   3.113} {   5.948   3.323   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   3.981   3.214   2.954} {   4.839   3.907   3.113} radius 0.03 filled yes resolution 20
draw cone {   4.839   3.907   3.113} {   5.301   4.281   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   3.380   3.841   2.954} {   4.109   4.669   3.113} radius 0.03 filled yes resolution 20
draw cone {   4.109   4.669   3.113} {   4.502   5.115   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   2.683   4.357   2.954} {   3.261   5.296   3.113} radius 0.03 filled yes resolution 20
draw cone {   3.261   5.296   3.113} {   3.572   5.802   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   1.908   4.747   2.954} {   2.319   5.771   3.113} radius 0.03 filled yes resolution 20
draw cone {   2.319   5.771   3.113} {   2.541   6.322   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   1.078   5.002   2.954} {   1.310   6.080   3.113} radius 0.03 filled yes resolution 20
draw cone {   1.310   6.080   3.113} {   1.436   6.661   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   0.217   5.112   2.954} {   0.264   6.214   3.113} radius 0.03 filled yes resolution 20
draw cone {   0.264   6.214   3.113} {   0.289   6.808   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -0.650   5.075   2.954} {  -0.790   6.169   3.113} radius 0.03 filled yes resolution 20
draw cone {  -0.790   6.169   3.113} {  -0.865   6.759   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -1.498   4.892   2.954} {  -1.821   5.947   3.113} radius 0.03 filled yes resolution 20
draw cone {  -1.821   5.947   3.113} {  -1.995   6.515   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -2.303   4.569   2.954} {  -2.800   5.554   3.113} radius 0.03 filled yes resolution 20
draw cone {  -2.800   5.554   3.113} {  -3.068   6.084   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -3.042   4.114   2.954} {  -3.698   5.001   3.113} radius 0.03 filled yes resolution 20
draw cone {  -3.698   5.001   3.113} {  -4.052   5.478   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -3.694   3.540   2.954} {  -4.490   4.304   3.113} radius 0.03 filled yes resolution 20
draw cone {  -4.490   4.304   3.113} {  -4.919   4.715   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -4.239   2.865   2.954} {  -5.153   3.483   3.113} radius 0.03 filled yes resolution 20
draw cone {  -5.153   3.483   3.113} {  -5.645   3.816   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -4.662   2.107   2.954} {  -5.668   2.562   3.113} radius 0.03 filled yes resolution 20
draw cone {  -5.668   2.562   3.113} {  -6.209   2.807   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -4.951   1.289   2.954} {  -6.019   1.567   3.113} radius 0.03 filled yes resolution 20
draw cone {  -6.019   1.567   3.113} {  -6.594   1.717   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -5.098   0.434   2.954} {  -6.197   0.527   3.113} radius 0.03 filled yes resolution 20
draw cone {  -6.197   0.527   3.113} {  -6.789   0.578   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -5.098  -0.434   2.954} {  -6.197  -0.527   3.113} radius 0.03 filled yes resolution 20
draw cone {  -6.197  -0.527   3.113} {  -6.789  -0.578   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -4.951  -1.289   2.954} {  -6.019  -1.567   3.113} radius 0.03 filled yes resolution 20
draw cone {  -6.019  -1.567   3.113} {  -6.594  -1.717   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -4.662  -2.107   2.954} {  -5.668  -2.562   3.113} radius 0.03 filled yes resolution 20
draw cone {  -5.668  -2.562   3.113} {  -6.209  -2.807   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -4.239  -2.865   2.954} {  -5.153  -3.483   3.113} radius 0.03 filled yes resolution 20
draw cone {  -5.153  -3.483   3.113} {  -5.645  -3.816   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -3.694  -3.540   2.954} {  -4.490  -4.304   3.113} radius 0.03 filled yes resolution 20
draw cone {  -4.490  -4.304   3.113} {  -4.919  -4.715   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -3.042  -4.114   2.954} {  -3.698  -5.001   3.113} radius 0.03 filled yes resolution 20
draw cone {  -3.698  -5.001   3.113} {  -4.052  -5.478   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -2.303  -4.569   2.954} {  -2.800  -5.554   3.113} radius 0.03 filled yes resolution 20
draw cone {  -2.800  -5.554   3.113} {  -3.068  -6.084   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -1.498  -4.892   2.954} {  -1.821  -5.947   3.113} radius 0.03 filled yes resolution 20
draw cone {  -1.821  -5.947   3.113} {  -1.995  -6.515   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -0.650  -5.075   2.954} {  -0.790  -6.169   3.113} radius 0.03 filled yes resolution 20
draw cone {  -0.790  -6.169   3.113} {  -0.865  -6.759   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   0.217  -5.112   2.954} {   0.264  -6.214   3.113} radius 0.03 filled yes resolution 20
draw cone {   0.264  -6.214   3.113} {   0.289  -6.808   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   1.078  -5.002   2.954} {   1.310  -6.080   3.113} radius 0.03 filled yes resolution 20
draw cone {   1.310  -6.080   3.113} {   1.436  -6.661   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   1.908  -4.747   2.954} {   2.319  -5.771   3.113} radius 0.03 filled yes resolution 20
draw cone {   2.319  -5.771   3.113} {   2.541  -6.322   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   2.683  -4.357   2.954} {   3.261  -5.296   3.113} radius 0.03 filled yes resolution 20
draw cone {   3.261  -5.296   3.113} {   3.572  -5.802   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   3.380  -3.841   2.954} {   4.109  -4.669   3.113} radius 0.03 filled yes resolution 20
draw cone {   4.109  -4.669   3.113} {   4.502  -5.115   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   3.981  -3.214   2.954} {   4.839  -3.907   3.113} radius 0.03 filled yes resolution 20
draw cone {   4.839  -3.907   3.113} {   5.301  -4.281   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   4.467  -2.495   2.954} {   5.430  -3.033   3.113} radius 0.03 filled yes resolution 20
draw cone {   5.430  -3.033   3.113} {   5.948  -3.323   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   4.824  -1.704   2.954} {   5.864  -2.072   3.113} radius 0.03 filled yes resolution 20
draw cone {   5.864  -2.072   3.113} {   6.425  -2.270   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   5.043  -0.865   2.954} {   6.130  -1.051   3.113} radius 0.03 filled yes resolution 20
draw cone {   6.130  -1.051   3.113} {   6.716  -1.152   3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   5.116  -0.000   2.954} {   6.220  -0.000   3.113} radius 0.03 filled yes resolution 20
draw cone {   6.220  -0.000   3.113} {   6.814  -0.000   3.198} radius 0.06 resolution 20
draw color  967
draw cylinder {   5.432   0.860   2.158} {   6.603   1.046   2.274} radius 0.03 filled yes resolution 20
draw cone {   6.603   1.046   2.274} {   7.234   1.146   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   5.230   1.699   2.158} {   6.358   2.066   2.274} radius 0.03 filled yes resolution 20
draw cone {   6.358   2.066   2.274} {   6.965   2.263   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   4.900   2.497   2.158} {   5.957   3.035   2.274} radius 0.03 filled yes resolution 20
draw cone {   5.957   3.035   2.274} {   6.526   3.325   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   4.449   3.233   2.158} {   5.409   3.930   2.274} radius 0.03 filled yes resolution 20
draw cone {   5.409   3.930   2.274} {   5.925   4.305   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   3.889   3.889   2.158} {   4.727   4.727   2.274} radius 0.03 filled yes resolution 20
draw cone {   4.727   4.727   2.274} {   5.179   5.179   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   3.233   4.449   2.158} {   3.930   5.409   2.274} radius 0.03 filled yes resolution 20
draw cone {   3.930   5.409   2.274} {   4.305   5.925   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   2.497   4.900   2.158} {   3.035   5.957   2.274} radius 0.03 filled yes resolution 20
draw cone {   3.035   5.957   2.274} {   3.325   6.526   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   1.699   5.230   2.158} {   2.066   6.358   2.274} radius 0.03 filled yes resolution 20
draw cone {   2.066   6.358   2.274} {   2.263   6.965   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   0.860   5.432   2.158} {   1.046   6.603   2.274} radius 0.03 filled yes resolution 20
draw cone {   1.046   6.603   2.274} {   1.146   7.234   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   0.000   5.499   2.158} {   0.000   6.685   2.274} radius 0.03 filled yes resolution 20
draw cone {   0.000   6.685   2.274} {   0.000   7.324   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -0.860   5.432   2.158} {  -1.046   6.603   2.274} radius 0.03 filled yes resolution 20
draw cone {  -1.046   6.603   2.274} {  -1.146   7.234   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -1.699   5.230   2.158} {  -2.066   6.358   2.274} radius 0.03 filled yes resolution 20
draw cone {  -2.066   6.358   2.274} {  -2.263   6.965   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -2.497   4.900   2.158} {  -3.035   5.957   2.274} radius 0.03 filled yes resolution 20
draw cone {  -3.035   5.957   2.274} {  -3.325   6.526   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -3.233   4.449   2.158} {  -3.930   5.409   2.274} radius 0.03 filled yes resolution 20
draw cone {  -3.930   5.409   2.274} {  -4.305   5.925   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -3.889   3.889   2.158} {  -4.727   4.727   2.274} radius 0.03 filled yes resolution 20
draw cone {  -4.727   4.727   2.274} {  -5.179   5.179   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -4.449   3.233   2.158} {  -5.409   3.930   2.274} radius 0.03 filled yes resolution 20
draw cone {  -5.409   3.930   2.274} {  -5.925   4.305   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -4.900   2.497   2.158} {  -5.957   3.035   2.274} radius 0.03 filled yes resolution 20
draw cone {  -5.957   3.035   2.274} {  -6.526   3.325   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -5.230   1.699   2.158} {  -6.358   2.066   2.274} radius 0.03 filled yes resolution 20
draw cone {  -6.358   2.066   2.274} {  -6.965   2.263   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -5.432   0.860   2.158} {  -6.603   1.046   2.274} radius 0.03 filled yes resolution 20
draw cone {  -6.603   1.046   2.274} {  -7.234   1.146   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -5.499   0.000   2.158} {  -6.685   0.000   2.274} radius 0.03 filled yes resolution 20
draw cone {  -6.685   0.000   2.274} {  -7.324   0.000   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -5.432  -0.860   2.158} {  -6.603  -1.046   2.274} radius 0.03 filled yes resolution 20
draw cone {  -6.603  -1.046   2.274} {  -7.234  -1.146   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -5.230  -1.699   2.158} {  -6.358  -2.066   2.274} radius 0.03 filled yes resolution 20
draw cone {  -6.358  -2.066   2.274} {  -6.965  -2.263   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -4.900  -2.497   2.158} {  -5.957  -3.035   2.274} radius 0.03 filled yes resolution 20
draw cone {  -5.957  -3.035   2.274} {  -6.526  -3.325   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -4.449  -3.233   2.158} {  -5.409  -3.930   2.274} radius 0.03 filled yes resolution 20
draw cone {  -5.409  -3.930   2.274} {  -5.925  -4.305   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -3.889  -3.889   2.158} {  -4.727  -4.727   2.274} radius 0.03 filled yes resolution 20
draw cone {  -4.727  -4.727   2.274} {  -5.179  -5.179   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -3.233  -4.449   2.158} {  -3.930  -5.409   2.274} radius 0.03 filled yes resolution 20
draw cone {  -3.930  -5.409   2.274} {  -4.305  -5.925   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -2.497  -4.900   2.158} {  -3.035  -5.957   2.274} radius 0.03 filled yes resolution 20
draw cone {  -3.035  -5.957   2.274} {  -3.325  -6.526   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -1.699  -5.230   2.158} {  -2.066  -6.358   2.274} radius 0.03 filled yes resolution 20
draw cone {  -2.066  -6.358   2.274} {  -2.263  -6.965   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -0.860  -5.432   2.158} {  -1.046  -6.603   2.274} radius 0.03 filled yes resolution 20
draw cone {  -1.046  -6.603   2.274} {  -1.146  -7.234   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -0.000  -5.499   2.158} {  -0.000  -6.685   2.274} radius 0.03 filled yes resolution 20
draw cone {  -0.000  -6.685   2.274} {  -0.000  -7.324   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   0.860  -5.432   2.158} {   1.046  -6.603   2.274} radius 0.03 filled yes resolution 20
draw cone {   1.046  -6.603   2.274} {   1.146  -7.234   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   1.699  -5.230   2.158} {   2.066  -6.358   2.274} radius 0.03 filled yes resolution 20
draw cone {   2.066  -6.358   2.274} {   2.263  -6.965   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   2.497  -4.900   2.158} {   3.035  -5.957   2.274} radius 0.03 filled yes resolution 20
draw cone {   3.035  -5.957   2.274} {   3.325  -6.526   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   3.233  -4.449   2.158} {   3.930  -5.409   2.274} radius 0.03 filled yes resolution 20
draw cone {   3.930  -5.409   2.274} {   4.305  -5.925   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   3.889  -3.889   2.158} {   4.727  -4.727   2.274} radius 0.03 filled yes resolution 20
draw cone {   4.727  -4.727   2.274} {   5.179  -5.179   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   4.449  -3.233   2.158} {   5.409  -3.930   2.274} radius 0.03 filled yes resolution 20
draw cone {   5.409  -3.930   2.274} {   5.925  -4.305   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   4.900  -2.497   2.158} {   5.957  -3.035   2.274} radius 0.03 filled yes resolution 20
draw cone {   5.957  -3.035   2.274} {   6.526  -3.325   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   5.230  -1.699   2.158} {   6.358  -2.066   2.274} radius 0.03 filled yes resolution 20
draw cone {   6.358  -2.066   2.274} {   6.965  -2.263   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   5.432  -0.860   2.158} {   6.603  -1.046   2.274} radius 0.03 filled yes resolution 20
draw cone {   6.603  -1.046   2.274} {   7.234  -1.146   2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   5.499  -0.000   2.158} {   6.685  -0.000   2.274} radius 0.03 filled yes resolution 20
draw cone {   6.685  -0.000   2.274} {   7.324  -0.000   2.337} radius 0.06 resolution 20
draw color 1022
draw cylinder {   5.692   0.879   1.315} {   6.920   1.069   1.385} radius 0.03 filled yes resolution 20
draw cone {   6.920   1.069   1.385} {   7.581   1.171   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   5.491   1.738   1.315} {   6.675   2.113   1.385} radius 0.03 filled yes resolution 20
draw cone {   6.675   2.113   1.385} {   7.313   2.314   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   5.162   2.556   1.315} {   6.275   3.107   1.385} radius 0.03 filled yes resolution 20
draw cone {   6.275   3.107   1.385} {   6.874   3.404   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   4.711   3.314   1.315} {   5.727   4.028   1.385} radius 0.03 filled yes resolution 20
draw cone {   5.727   4.028   1.385} {   6.274   4.413   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   4.150   3.994   1.315} {   5.045   4.855   1.385} radius 0.03 filled yes resolution 20
draw cone {   5.045   4.855   1.385} {   5.527   5.319   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   3.492   4.581   1.315} {   4.245   5.568   1.385} radius 0.03 filled yes resolution 20
draw cone {   4.245   5.568   1.385} {   4.650   6.100   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   2.752   5.060   1.315} {   3.345   6.151   1.385} radius 0.03 filled yes resolution 20
draw cone {   3.345   6.151   1.385} {   3.664   6.739   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   1.947   5.421   1.315} {   2.367   6.590   1.385} radius 0.03 filled yes resolution 20
draw cone {   2.367   6.590   1.385} {   2.593   7.219   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   1.097   5.654   1.315} {   1.333   6.874   1.385} radius 0.03 filled yes resolution 20
draw cone {   1.333   6.874   1.385} {   1.460   7.530   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   0.221   5.756   1.315} {   0.268   6.997   1.385} radius 0.03 filled yes resolution 20
draw cone {   0.268   6.997   1.385} {   0.294   7.665   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -0.661   5.722   1.315} {  -0.803   6.956   1.385} radius 0.03 filled yes resolution 20
draw cone {  -0.803   6.956   1.385} {  -0.880   7.620   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -1.526   5.554   1.315} {  -1.855   6.751   1.385} radius 0.03 filled yes resolution 20
draw cone {  -1.855   6.751   1.385} {  -2.033   7.396   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -2.356   5.256   1.315} {  -2.864   6.389   1.385} radius 0.03 filled yes resolution 20
draw cone {  -2.864   6.389   1.385} {  -3.138   6.999   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -3.131   4.835   1.315} {  -3.806   5.877   1.385} radius 0.03 filled yes resolution 20
draw cone {  -3.806   5.877   1.385} {  -4.169   6.438   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -3.832   4.300   1.315} {  -4.658   5.227   1.385} radius 0.03 filled yes resolution 20
draw cone {  -4.658   5.227   1.385} {  -5.103   5.726   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -4.444   3.665   1.315} {  -5.402   4.455   1.385} radius 0.03 filled yes resolution 20
draw cone {  -5.402   4.455   1.385} {  -5.918   4.880   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -4.951   2.943   1.315} {  -6.019   3.578   1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.019   3.578   1.385} {  -6.593   3.920   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -5.342   2.153   1.315} {  -6.494   2.617   1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.494   2.617   1.385} {  -7.114   2.867   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -5.608   1.312   1.315} {  -6.818   1.595   1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.818   1.595   1.385} {  -7.469   1.748   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -5.743   0.441   1.315} {  -6.981   0.536   1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.981   0.536   1.385} {  -7.648   0.587   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -5.743  -0.441   1.315} {  -6.981  -0.536   1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.981  -0.536   1.385} {  -7.648  -0.587   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -5.608  -1.312   1.315} {  -6.818  -1.595   1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.818  -1.595   1.385} {  -7.469  -1.748   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -5.342  -2.153   1.315} {  -6.494  -2.617   1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.494  -2.617   1.385} {  -7.114  -2.867   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -4.951  -2.943   1.315} {  -6.019  -3.578   1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.019  -3.578   1.385} {  -6.593  -3.920   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -4.444  -3.665   1.315} {  -5.402  -4.455   1.385} radius 0.03 filled yes resolution 20
draw cone {  -5.402  -4.455   1.385} {  -5.918  -4.880   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -3.832  -4.300   1.315} {  -4.658  -5.227   1.385} radius 0.03 filled yes resolution 20
draw cone {  -4.658  -5.227   1.385} {  -5.103  -5.726   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -3.131  -4.835   1.315} {  -3.806  -5.877   1.385} radius 0.03 filled yes resolution 20
draw cone {  -3.806  -5.877   1.385} {  -4.169  -6.438   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -2.356  -5.256   1.315} {  -2.864  -6.389   1.385} radius 0.03 filled yes resolution 20
draw cone {  -2.864  -6.389   1.385} {  -3.138  -6.999   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -1.526  -5.554   1.315} {  -1.855  -6.751   1.385} radius 0.03 filled yes resolution 20
draw cone {  -1.855  -6.751   1.385} {  -2.033  -7.396   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -0.661  -5.722   1.315} {  -0.803  -6.956   1.385} radius 0.03 filled yes resolution 20
draw cone {  -0.803  -6.956   1.385} {  -0.880  -7.620   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   0.221  -5.756   1.315} {   0.268  -6.997   1.385} radius 0.03 filled yes resolution 20
draw cone {   0.268  -6.997   1.385} {   0.294  -7.665   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   1.097  -5.654   1.315} {   1.333  -6.874   1.385} radius 0.03 filled yes resolution 20
draw cone {   1.333  -6.874   1.385} {   1.460  -7.530   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   1.947  -5.421   1.315} {   2.367  -6.590   1.385} radius 0.03 filled yes resolution 20
draw cone {   2.367  -6.590   1.385} {   2.593  -7.219   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   2.752  -5.060   1.315} {   3.345  -6.151   1.385} radius 0.03 filled yes resolution 20
draw cone {   3.345  -6.151   1.385} {   3.664  -6.739   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   3.492  -4.581   1.315} {   4.245  -5.568   1.385} radius 0.03 filled yes resolution 20
draw cone {   4.245  -5.568   1.385} {   4.650  -6.100   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   4.150  -3.994   1.315} {   5.045  -4.855   1.385} radius 0.03 filled yes resolution 20
draw cone {   5.045  -4.855   1.385} {   5.527  -5.319   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   4.711  -3.314   1.315} {   5.727  -4.028   1.385} radius 0.03 filled yes resolution 20
draw cone {   5.727  -4.028   1.385} {   6.274  -4.413   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   5.162  -2.556   1.315} {   6.275  -3.107   1.385} radius 0.03 filled yes resolution 20
draw cone {   6.275  -3.107   1.385} {   6.874  -3.404   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   5.491  -1.738   1.315} {   6.675  -2.113   1.385} radius 0.03 filled yes resolution 20
draw cone {   6.675  -2.113   1.385} {   7.313  -2.314   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   5.692  -0.879   1.315} {   6.920  -1.069   1.385} radius 0.03 filled yes resolution 20
draw cone {   6.920  -1.069   1.385} {   7.581  -1.171   1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   5.760  -0.000   1.315} {   7.002  -0.000   1.385} radius 0.03 filled yes resolution 20
draw cone {   7.002  -0.000   1.385} {   7.671  -0.000   1.423} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.826   0.878   0.441} {   7.082   1.067   0.465} radius 0.03 filled yes resolution 20
draw cone {   7.082   1.067   0.465} {   7.758   1.169   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.630   1.737   0.441} {   6.844   2.111   0.465} radius 0.03 filled yes resolution 20
draw cone {   6.844   2.111   0.465} {   7.497   2.313   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.308   2.556   0.441} {   6.453   3.107   0.465} radius 0.03 filled yes resolution 20
draw cone {   6.453   3.107   0.465} {   7.069   3.404   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   4.868   3.319   0.441} {   5.917   4.034   0.465} radius 0.03 filled yes resolution 20
draw cone {   5.917   4.034   0.465} {   6.483   4.420   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   4.319   4.007   0.441} {   5.250   4.871   0.465} radius 0.03 filled yes resolution 20
draw cone {   5.250   4.871   0.465} {   5.751   5.337   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   3.673   4.606   0.441} {   4.465   5.599   0.465} radius 0.03 filled yes resolution 20
draw cone {   4.465   5.599   0.465} {   4.892   6.134   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   2.946   5.102   0.441} {   3.581   6.202   0.465} radius 0.03 filled yes resolution 20
draw cone {   3.581   6.202   0.465} {   3.923   6.795   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   2.152   5.484   0.441} {   2.616   6.667   0.465} radius 0.03 filled yes resolution 20
draw cone {   2.616   6.667   0.465} {   2.866   7.303   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   1.311   5.744   0.441} {   1.594   6.982   0.465} radius 0.03 filled yes resolution 20
draw cone {   1.594   6.982   0.465} {   1.746   7.649   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   0.440   5.875   0.441} {   0.535   7.142   0.465} radius 0.03 filled yes resolution 20
draw cone {   0.535   7.142   0.465} {   0.586   7.824   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -0.440   5.875   0.441} {  -0.535   7.142   0.465} radius 0.03 filled yes resolution 20
draw cone {  -0.535   7.142   0.465} {  -0.586   7.824   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -1.311   5.744   0.441} {  -1.594   6.982   0.465} radius 0.03 filled yes resolution 20
draw cone {  -1.594   6.982   0.465} {  -1.746   7.649   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -2.152   5.484   0.441} {  -2.616   6.667   0.465} radius 0.03 filled yes resolution 20
draw cone {  -2.616   6.667   0.465} {  -2.866   7.303   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -2.946   5.102   0.441} {  -3.581   6.202   0.465} radius 0.03 filled yes resolution 20
draw cone {  -3.581   6.202   0.465} {  -3.923   6.795   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -3.673   4.606   0.441} {  -4.465   5.599   0.465} radius 0.03 filled yes resolution 20
draw cone {  -4.465   5.599   0.465} {  -4.892   6.134   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -4.319   4.007   0.441} {  -5.250   4.871   0.465} radius 0.03 filled yes resolution 20
draw cone {  -5.250   4.871   0.465} {  -5.751   5.337   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -4.868   3.319   0.441} {  -5.917   4.034   0.465} radius 0.03 filled yes resolution 20
draw cone {  -5.917   4.034   0.465} {  -6.483   4.420   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.308   2.556   0.441} {  -6.453   3.107   0.465} radius 0.03 filled yes resolution 20
draw cone {  -6.453   3.107   0.465} {  -7.069   3.404   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.630   1.737   0.441} {  -6.844   2.111   0.465} radius 0.03 filled yes resolution 20
draw cone {  -6.844   2.111   0.465} {  -7.497   2.313   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.826   0.878   0.441} {  -7.082   1.067   0.465} radius 0.03 filled yes resolution 20
draw cone {  -7.082   1.067   0.465} {  -7.758   1.169   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.891   0.000   0.441} {  -7.162   0.000   0.465} radius 0.03 filled yes resolution 20
draw cone {  -7.162   0.000   0.465} {  -7.846   0.000   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.826  -0.878   0.441} {  -7.082  -1.067   0.465} radius 0.03 filled yes resolution 20
draw cone {  -7.082  -1.067   0.465} {  -7.758  -1.169   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.630  -1.737   0.441} {  -6.844  -2.111   0.465} radius 0.03 filled yes resolution 20
draw cone {  -6.844  -2.111   0.465} {  -7.497  -2.313   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.308  -2.556   0.441} {  -6.453  -3.107   0.465} radius 0.03 filled yes resolution 20
draw cone {  -6.453  -3.107   0.465} {  -7.069  -3.404   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -4.868  -3.319   0.441} {  -5.917  -4.034   0.465} radius 0.03 filled yes resolution 20
draw cone {  -5.917  -4.034   0.465} {  -6.483  -4.420   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -4.319  -4.007   0.441} {  -5.250  -4.871   0.465} radius 0.03 filled yes resolution 20
draw cone {  -5.250  -4.871   0.465} {  -5.751  -5.337   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -3.673  -4.606   0.441} {  -4.465  -5.599   0.465} radius 0.03 filled yes resolution 20
draw cone {  -4.465  -5.599   0.465} {  -4.892  -6.134   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -2.946  -5.102   0.441} {  -3.581  -6.202   0.465} radius 0.03 filled yes resolution 20
draw cone {  -3.581  -6.202   0.465} {  -3.923  -6.795   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -2.152  -5.484   0.441} {  -2.616  -6.667   0.465} radius 0.03 filled yes resolution 20
draw cone {  -2.616  -6.667   0.465} {  -2.866  -7.303   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -1.311  -5.744   0.441} {  -1.594  -6.982   0.465} radius 0.03 filled yes resolution 20
draw cone {  -1.594  -6.982   0.465} {  -1.746  -7.649   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -0.440  -5.875   0.441} {  -0.535  -7.142   0.465} radius 0.03 filled yes resolution 20
draw cone {  -0.535  -7.142   0.465} {  -0.586  -7.824   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   0.440  -5.875   0.441} {   0.535  -7.142   0.465} radius 0.03 filled yes resolution 20
draw cone {   0.535  -7.142   0.465} {   0.586  -7.824   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   1.311  -5.744   0.441} {   1.594  -6.982   0.465} radius 0.03 filled yes resolution 20
draw cone {   1.594  -6.982   0.465} {   1.746  -7.649   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   2.152  -5.484   0.441} {   2.616  -6.667   0.465} radius 0.03 filled yes resolution 20
draw cone {   2.616  -6.667   0.465} {   2.866  -7.303   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   2.946  -5.102   0.441} {   3.581  -6.202   0.465} radius 0.03 filled yes resolution 20
draw cone {   3.581  -6.202   0.465} {   3.923  -6.795   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   3.673  -4.606   0.441} {   4.465  -5.599   0.465} radius 0.03 filled yes resolution 20
draw cone {   4.465  -5.599   0.465} {   4.892  -6.134   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   4.319  -4.007   0.441} {   5.250  -4.871   0.465} radius 0.03 filled yes resolution 20
draw cone {   5.250  -4.871   0.465} {   5.751  -5.337   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   4.868  -3.319   0.441} {   5.917  -4.034   0.465} radius 0.03 filled yes resolution 20
draw cone {   5.917  -4.034   0.465} {   6.483  -4.420   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.308  -2.556   0.441} {   6.453  -3.107   0.465} radius 0.03 filled yes resolution 20
draw cone {   6.453  -3.107   0.465} {   7.069  -3.404   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.630  -1.737   0.441} {   6.844  -2.111   0.465} radius 0.03 filled yes resolution 20
draw cone {   6.844  -2.111   0.465} {   7.497  -2.313   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.826  -0.878   0.441} {   7.082  -1.067   0.465} radius 0.03 filled yes resolution 20
draw cone {   7.082  -1.067   0.465} {   7.758  -1.169   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.891  -0.000   0.441} {   7.162  -0.000   0.465} radius 0.03 filled yes resolution 20
draw cone {   7.162  -0.000   0.465} {   7.846  -0.000   0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.826   0.878  -0.441} {   7.082   1.067  -0.465} radius 0.03 filled yes resolution 20
draw cone {   7.082   1.067  -0.465} {   7.758   1.169  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.630   1.737  -0.441} {   6.844   2.111  -0.465} radius 0.03 filled yes resolution 20
draw cone {   6.844   2.111  -0.465} {   7.497   2.313  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.308   2.556  -0.441} {   6.453   3.107  -0.465} radius 0.03 filled yes resolution 20
draw cone {   6.453   3.107  -0.465} {   7.069   3.404  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   4.868   3.319  -0.441} {   5.917   4.034  -0.465} radius 0.03 filled yes resolution 20
draw cone {   5.917   4.034  -0.465} {   6.483   4.420  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   4.319   4.007  -0.441} {   5.250   4.871  -0.465} radius 0.03 filled yes resolution 20
draw cone {   5.250   4.871  -0.465} {   5.751   5.337  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   3.673   4.606  -0.441} {   4.465   5.599  -0.465} radius 0.03 filled yes resolution 20
draw cone {   4.465   5.599  -0.465} {   4.892   6.134  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   2.946   5.102  -0.441} {   3.581   6.202  -0.465} radius 0.03 filled yes resolution 20
draw cone {   3.581   6.202  -0.465} {   3.923   6.795  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   2.152   5.484  -0.441} {   2.616   6.667  -0.465} radius 0.03 filled yes resolution 20
draw cone {   2.616   6.667  -0.465} {   2.866   7.303  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   1.311   5.744  -0.441} {   1.594   6.982  -0.465} radius 0.03 filled yes resolution 20
draw cone {   1.594   6.982  -0.465} {   1.746   7.649  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   0.440   5.875  -0.441} {   0.535   7.142  -0.465} radius 0.03 filled yes resolution 20
draw cone {   0.535   7.142  -0.465} {   0.586   7.824  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -0.440   5.875  -0.441} {  -0.535   7.142  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -0.535   7.142  -0.465} {  -0.586   7.824  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -1.311   5.744  -0.441} {  -1.594   6.982  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -1.594   6.982  -0.465} {  -1.746   7.649  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -2.152   5.484  -0.441} {  -2.616   6.667  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -2.616   6.667  -0.465} {  -2.866   7.303  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -2.946   5.102  -0.441} {  -3.581   6.202  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -3.581   6.202  -0.465} {  -3.923   6.795  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -3.673   4.606  -0.441} {  -4.465   5.599  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -4.465   5.599  -0.465} {  -4.892   6.134  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -4.319   4.007  -0.441} {  -5.250   4.871  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -5.250   4.871  -0.465} {  -5.751   5.337  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -4.868   3.319  -0.441} {  -5.917   4.034  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -5.917   4.034  -0.465} {  -6.483   4.420  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.308   2.556  -0.441} {  -6.453   3.107  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -6.453   3.107  -0.465} {  -7.069   3.404  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.630   1.737  -0.441} {  -6.844   2.111  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -6.844   2.111  -0.465} {  -7.497   2.313  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.826   0.878  -0.441} {  -7.082   1.067  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -7.082   1.067  -0.465} {  -7.758   1.169  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.891   0.000  -0.441} {  -7.162   0.000  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -7.162   0.000  -0.465} {  -7.846   0.000  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.826  -0.878  -0.441} {  -7.082  -1.067  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -7.082  -1.067  -0.465} {  -7.758  -1.169  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.630  -1.737  -0.441} {  -6.844  -2.111  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -6.844  -2.111  -0.465} {  -7.497  -2.313  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.308  -2.556  -0.441} {  -6.453  -3.107  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -6.453  -3.107  -0.465} {  -7.069  -3.404  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -4.868  -3.319  -0.441} {  -5.917  -4.034  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -5.917  -4.034  -0.465} {  -6.483  -4.420  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -4.319  -4.007  -0.441} {  -5.250  -4.871  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -5.250  -4.871  -0.465} {  -5.751  -5.337  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -3.673  -4.606  -0.441} {  -4.465  -5.599  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -4.465  -5.599  -0.465} {  -4.892  -6.134  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -2.946  -5.102  -0.441} {  -3.581  -6.202  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -3.581  -6.202  -0.465} {  -3.923  -6.795  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -2.152  -5.484  -0.441} {  -2.616  -6.667  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -2.616  -6.667  -0.465} {  -2.866  -7.303  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -1.311  -5.744  -0.441} {  -1.594  -6.982  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -1.594  -6.982  -0.465} {  -1.746  -7.649  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -0.440  -5.875  -0.441} {  -0.535  -7.142  -0.465} radius 0.03 filled yes resolution 20
draw cone {  -0.535  -7.142  -0.465} {  -0.586  -7.824  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   0.440  -5.875  -0.441} {   0.535  -7.142  -0.465} radius 0.03 filled yes resolution 20
draw cone {   0.535  -7.142  -0.465} {   0.586  -7.824  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   1.311  -5.744  -0.441} {   1.594  -6.982  -0.465} radius 0.03 filled yes resolution 20
draw cone {   1.594  -6.982  -0.465} {   1.746  -7.649  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   2.152  -5.484  -0.441} {   2.616  -6.667  -0.465} radius 0.03 filled yes resolution 20
draw cone {   2.616  -6.667  -0.465} {   2.866  -7.303  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   2.946  -5.102  -0.441} {   3.581  -6.202  -0.465} radius 0.03 filled yes resolution 20
draw cone {   3.581  -6.202  -0.465} {   3.923  -6.795  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   3.673  -4.606  -0.441} {   4.465  -5.599  -0.465} radius 0.03 filled yes resolution 20
draw cone {   4.465  -5.599  -0.465} {   4.892  -6.134  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   4.319  -4.007  -0.441} {   5.250  -4.871  -0.465} radius 0.03 filled yes resolution 20
draw cone {   5.250  -4.871  -0.465} {   5.751  -5.337  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   4.868  -3.319  -0.441} {   5.917  -4.034  -0.465} radius 0.03 filled yes resolution 20
draw cone {   5.917  -4.034  -0.465} {   6.483  -4.420  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.308  -2.556  -0.441} {   6.453  -3.107  -0.465} radius 0.03 filled yes resolution 20
draw cone {   6.453  -3.107  -0.465} {   7.069  -3.404  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.630  -1.737  -0.441} {   6.844  -2.111  -0.465} radius 0.03 filled yes resolution 20
draw cone {   6.844  -2.111  -0.465} {   7.497  -2.313  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.826  -0.878  -0.441} {   7.082  -1.067  -0.465} radius 0.03 filled yes resolution 20
draw cone {   7.082  -1.067  -0.465} {   7.758  -1.169  -0.478} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.891  -0.000  -0.441} {   7.162  -0.000  -0.465} radius 0.03 filled yes resolution 20
draw cone {   7.162  -0.000  -0.465} {   7.846  -0.000  -0.478} radius 0.06 resolution 20
draw color 1022
draw cylinder {   5.692   0.879  -1.315} {   6.920   1.069  -1.385} radius 0.03 filled yes resolution 20
draw cone {   6.920   1.069  -1.385} {   7.581   1.171  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   5.491   1.738  -1.315} {   6.675   2.113  -1.385} radius 0.03 filled yes resolution 20
draw cone {   6.675   2.113  -1.385} {   7.313   2.314  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   5.162   2.556  -1.315} {   6.275   3.107  -1.385} radius 0.03 filled yes resolution 20
draw cone {   6.275   3.107  -1.385} {   6.874   3.404  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   4.711   3.314  -1.315} {   5.727   4.028  -1.385} radius 0.03 filled yes resolution 20
draw cone {   5.727   4.028  -1.385} {   6.274   4.413  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   4.150   3.994  -1.315} {   5.045   4.855  -1.385} radius 0.03 filled yes resolution 20
draw cone {   5.045   4.855  -1.385} {   5.527   5.319  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   3.492   4.581  -1.315} {   4.245   5.568  -1.385} radius 0.03 filled yes resolution 20
draw cone {   4.245   5.568  -1.385} {   4.650   6.100  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   2.752   5.060  -1.315} {   3.345   6.151  -1.385} radius 0.03 filled yes resolution 20
draw cone {   3.345   6.151  -1.385} {   3.664   6.739  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   1.947   5.421  -1.315} {   2.367   6.590  -1.385} radius 0.03 filled yes resolution 20
draw cone {   2.367   6.590  -1.385} {   2.593   7.219  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   1.097   5.654  -1.315} {   1.333   6.874  -1.385} radius 0.03 filled yes resolution 20
draw cone {   1.333   6.874  -1.385} {   1.460   7.530  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   0.221   5.756  -1.315} {   0.268   6.997  -1.385} radius 0.03 filled yes resolution 20
draw cone {   0.268   6.997  -1.385} {   0.294   7.665  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -0.661   5.722  -1.315} {  -0.803   6.956  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -0.803   6.956  -1.385} {  -0.880   7.620  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -1.526   5.554  -1.315} {  -1.855   6.751  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -1.855   6.751  -1.385} {  -2.033   7.396  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -2.356   5.256  -1.315} {  -2.864   6.389  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -2.864   6.389  -1.385} {  -3.138   6.999  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -3.131   4.835  -1.315} {  -3.806   5.877  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -3.806   5.877  -1.385} {  -4.169   6.438  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -3.832   4.300  -1.315} {  -4.658   5.227  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -4.658   5.227  -1.385} {  -5.103   5.726  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -4.444   3.665  -1.315} {  -5.402   4.455  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -5.402   4.455  -1.385} {  -5.918   4.880  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -4.951   2.943  -1.315} {  -6.019   3.578  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.019   3.578  -1.385} {  -6.593   3.920  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -5.342   2.153  -1.315} {  -6.494   2.617  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.494   2.617  -1.385} {  -7.114   2.867  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -5.608   1.312  -1.315} {  -6.818   1.595  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.818   1.595  -1.385} {  -7.469   1.748  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -5.743   0.441  -1.315} {  -6.981   0.536  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.981   0.536  -1.385} {  -7.648   0.587  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -5.743  -0.441  -1.315} {  -6.981  -0.536  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.981  -0.536  -1.385} {  -7.648  -0.587  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -5.608  -1.312  -1.315} {  -6.818  -1.595  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.818  -1.595  -1.385} {  -7.469  -1.748  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -5.342  -2.153  -1.315} {  -6.494  -2.617  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.494  -2.617  -1.385} {  -7.114  -2.867  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -4.951  -2.943  -1.315} {  -6.019  -3.578  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -6.019  -3.578  -1.385} {  -6.593  -3.920  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -4.444  -3.665  -1.315} {  -5.402  -4.455  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -5.402  -4.455  -1.385} {  -5.918  -4.880  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -3.832  -4.300  -1.315} {  -4.658  -5.227  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -4.658  -5.227  -1.385} {  -5.103  -5.726  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -3.131  -4.835  -1.315} {  -3.806  -5.877  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -3.806  -5.877  -1.385} {  -4.169  -6.438  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -2.356  -5.256  -1.315} {  -2.864  -6.389  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -2.864  -6.389  -1.385} {  -3.138  -6.999  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -1.526  -5.554  -1.315} {  -1.855  -6.751  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -1.855  -6.751  -1.385} {  -2.033  -7.396  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {  -0.661  -5.722  -1.315} {  -0.803  -6.956  -1.385} radius 0.03 filled yes resolution 20
draw cone {  -0.803  -6.956  -1.385} {  -0.880  -7.620  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   0.221  -5.756  -1.315} {   0.268  -6.997  -1.385} radius 0.03 filled yes resolution 20
draw cone {   0.268  -6.997  -1.385} {   0.294  -7.665  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   1.097  -5.654  -1.315} {   1.333  -6.874  -1.385} radius 0.03 filled yes resolution 20
draw cone {   1.333  -6.874  -1.385} {   1.460  -7.530  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   1.947  -5.421  -1.315} {   2.367  -6.590  -1.385} radius 0.03 filled yes resolution 20
draw cone {   2.367  -6.590  -1.385} {   2.593  -7.219  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   2.752  -5.060  -1.315} {   3.345  -6.151  -1.385} radius 0.03 filled yes resolution 20
draw cone {   3.345  -6.151  -1.385} {   3.664  -6.739  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   3.492  -4.581  -1.315} {   4.245  -5.568  -1.385} radius 0.03 filled yes resolution 20
draw cone {   4.245  -5.568  -1.385} {   4.650  -6.100  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   4.150  -3.994  -1.315} {   5.045  -4.855  -1.385} radius 0.03 filled yes resolution 20
draw cone {   5.045  -4.855  -1.385} {   5.527  -5.319  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   4.711  -3.314  -1.315} {   5.727  -4.028  -1.385} radius 0.03 filled yes resolution 20
draw cone {   5.727  -4.028  -1.385} {   6.274  -4.413  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   5.162  -2.556  -1.315} {   6.275  -3.107  -1.385} radius 0.03 filled yes resolution 20
draw cone {   6.275  -3.107  -1.385} {   6.874  -3.404  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   5.491  -1.738  -1.315} {   6.675  -2.113  -1.385} radius 0.03 filled yes resolution 20
draw cone {   6.675  -2.113  -1.385} {   7.313  -2.314  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   5.692  -0.879  -1.315} {   6.920  -1.069  -1.385} radius 0.03 filled yes resolution 20
draw cone {   6.920  -1.069  -1.385} {   7.581  -1.171  -1.423} radius 0.06 resolution 20
draw color 1022
draw cylinder {   5.760  -0.000  -1.315} {   7.002  -0.000  -1.385} radius 0.03 filled yes resolution 20
draw cone {   7.002  -0.000  -1.385} {   7.671  -0.000  -1.423} radius 0.06 resolution 20
draw color  967
draw cylinder {   5.432   0.860  -2.158} {   6.603   1.046  -2.274} radius 0.03 filled yes resolution 20
draw cone {   6.603   1.046  -2.274} {   7.234   1.146  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   5.230   1.699  -2.158} {   6.358   2.066  -2.274} radius 0.03 filled yes resolution 20
draw cone {   6.358   2.066  -2.274} {   6.965   2.263  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   4.900   2.497  -2.158} {   5.957   3.035  -2.274} radius 0.03 filled yes resolution 20
draw cone {   5.957   3.035  -2.274} {   6.526   3.325  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   4.449   3.233  -2.158} {   5.409   3.930  -2.274} radius 0.03 filled yes resolution 20
draw cone {   5.409   3.930  -2.274} {   5.925   4.305  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   3.889   3.889  -2.158} {   4.727   4.727  -2.274} radius 0.03 filled yes resolution 20
draw cone {   4.727   4.727  -2.274} {   5.179   5.179  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   3.233   4.449  -2.158} {   3.930   5.409  -2.274} radius 0.03 filled yes resolution 20
draw cone {   3.930   5.409  -2.274} {   4.305   5.925  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   2.497   4.900  -2.158} {   3.035   5.957  -2.274} radius 0.03 filled yes resolution 20
draw cone {   3.035   5.957  -2.274} {   3.325   6.526  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   1.699   5.230  -2.158} {   2.066   6.358  -2.274} radius 0.03 filled yes resolution 20
draw cone {   2.066   6.358  -2.274} {   2.263   6.965  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   0.860   5.432  -2.158} {   1.046   6.603  -2.274} radius 0.03 filled yes resolution 20
draw cone {   1.046   6.603  -2.274} {   1.146   7.234  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   0.000   5.499  -2.158} {   0.000   6.685  -2.274} radius 0.03 filled yes resolution 20
draw cone {   0.000   6.685  -2.274} {   0.000   7.324  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -0.860   5.432  -2.158} {  -1.046   6.603  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -1.046   6.603  -2.274} {  -1.146   7.234  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -1.699   5.230  -2.158} {  -2.066   6.358  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -2.066   6.358  -2.274} {  -2.263   6.965  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -2.497   4.900  -2.158} {  -3.035   5.957  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -3.035   5.957  -2.274} {  -3.325   6.526  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -3.233   4.449  -2.158} {  -3.930   5.409  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -3.930   5.409  -2.274} {  -4.305   5.925  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -3.889   3.889  -2.158} {  -4.727   4.727  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -4.727   4.727  -2.274} {  -5.179   5.179  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -4.449   3.233  -2.158} {  -5.409   3.930  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -5.409   3.930  -2.274} {  -5.925   4.305  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -4.900   2.497  -2.158} {  -5.957   3.035  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -5.957   3.035  -2.274} {  -6.526   3.325  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -5.230   1.699  -2.158} {  -6.358   2.066  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -6.358   2.066  -2.274} {  -6.965   2.263  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -5.432   0.860  -2.158} {  -6.603   1.046  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -6.603   1.046  -2.274} {  -7.234   1.146  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -5.499   0.000  -2.158} {  -6.685   0.000  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -6.685   0.000  -2.274} {  -7.324   0.000  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -5.432  -0.860  -2.158} {  -6.603  -1.046  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -6.603  -1.046  -2.274} {  -7.234  -1.146  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -5.230  -1.699  -2.158} {  -6.358  -2.066  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -6.358  -2.066  -2.274} {  -6.965  -2.263  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -4.900  -2.497  -2.158} {  -5.957  -3.035  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -5.957  -3.035  -2.274} {  -6.526  -3.325  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -4.449  -3.233  -2.158} {  -5.409  -3.930  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -5.409  -3.930  -2.274} {  -5.925  -4.305  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -3.889  -3.889  -2.158} {  -4.727  -4.727  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -4.727  -4.727  -2.274} {  -5.179  -5.179  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -3.233  -4.449  -2.158} {  -3.930  -5.409  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -3.930  -5.409  -2.274} {  -4.305  -5.925  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -2.497  -4.900  -2.158} {  -3.035  -5.957  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -3.035  -5.957  -2.274} {  -3.325  -6.526  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -1.699  -5.230  -2.158} {  -2.066  -6.358  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -2.066  -6.358  -2.274} {  -2.263  -6.965  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -0.860  -5.432  -2.158} {  -1.046  -6.603  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -1.046  -6.603  -2.274} {  -1.146  -7.234  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {  -0.000  -5.499  -2.158} {  -0.000  -6.685  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -0.000  -6.685  -2.274} {  -0.000  -7.324  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   0.860  -5.432  -2.158} {   1.046  -6.603  -2.274} radius 0.03 filled yes resolution 20
draw cone {   1.046  -6.603  -2.274} {   1.146  -7.234  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   1.699  -5.230  -2.158} {   2.066  -6.358  -2.274} radius 0.03 filled yes resolution 20
draw cone {   2.066  -6.358  -2.274} {   2.263  -6.965  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   2.497  -4.900  -2.158} {   3.035  -5.957  -2.274} radius 0.03 filled yes resolution 20
draw cone {   3.035  -5.957  -2.274} {   3.325  -6.526  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   3.233  -4.449  -2.158} {   3.930  -5.409  -2.274} radius 0.03 filled yes resolution 20
draw cone {   3.930  -5.409  -2.274} {   4.305  -5.925  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   3.889  -3.889  -2.158} {   4.727  -4.727  -2.274} radius 0.03 filled yes resolution 20
draw cone {   4.727  -4.727  -2.274} {   5.179  -5.179  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   4.449  -3.233  -2.158} {   5.409  -3.930  -2.274} radius 0.03 filled yes resolution 20
draw cone {   5.409  -3.930  -2.274} {   5.925  -4.305  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   4.900  -2.497  -2.158} {   5.957  -3.035  -2.274} radius 0.03 filled yes resolution 20
draw cone {   5.957  -3.035  -2.274} {   6.526  -3.325  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   5.230  -1.699  -2.158} {   6.358  -2.066  -2.274} radius 0.03 filled yes resolution 20
draw cone {   6.358  -2.066  -2.274} {   6.965  -2.263  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   5.432  -0.860  -2.158} {   6.603  -1.046  -2.274} radius 0.03 filled yes resolution 20
draw cone {   6.603  -1.046  -2.274} {   7.234  -1.146  -2.337} radius 0.06 resolution 20
draw color  967
draw cylinder {   5.499  -0.000  -2.158} {   6.685  -0.000  -2.274} radius 0.03 filled yes resolution 20
draw cone {   6.685  -0.000  -2.274} {   7.324  -0.000  -2.337} radius 0.06 resolution 20
draw color  886
draw cylinder {   5.043   0.865  -2.954} {   6.130   1.051  -3.113} radius 0.03 filled yes resolution 20
draw cone {   6.130   1.051  -3.113} {   6.716   1.152  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   4.824   1.704  -2.954} {   5.864   2.072  -3.113} radius 0.03 filled yes resolution 20
draw cone {   5.864   2.072  -3.113} {   6.425   2.270  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   4.467   2.495  -2.954} {   5.430   3.033  -3.113} radius 0.03 filled yes resolution 20
draw cone {   5.430   3.033  -3.113} {   5.948   3.323  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   3.981   3.214  -2.954} {   4.839   3.907  -3.113} radius 0.03 filled yes resolution 20
draw cone {   4.839   3.907  -3.113} {   5.301   4.281  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   3.380   3.841  -2.954} {   4.109   4.669  -3.113} radius 0.03 filled yes resolution 20
draw cone {   4.109   4.669  -3.113} {   4.502   5.115  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   2.683   4.357  -2.954} {   3.261   5.296  -3.113} radius 0.03 filled yes resolution 20
draw cone {   3.261   5.296  -3.113} {   3.572   5.802  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   1.908   4.747  -2.954} {   2.319   5.771  -3.113} radius 0.03 filled yes resolution 20
draw cone {   2.319   5.771  -3.113} {   2.541   6.322  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   1.078   5.002  -2.954} {   1.310   6.080  -3.113} radius 0.03 filled yes resolution 20
draw cone {   1.310   6.080  -3.113} {   1.436   6.661  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   0.217   5.112  -2.954} {   0.264   6.214  -3.113} radius 0.03 filled yes resolution 20
draw cone {   0.264   6.214  -3.113} {   0.289   6.808  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -0.650   5.075  -2.954} {  -0.790   6.169  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -0.790   6.169  -3.113} {  -0.865   6.759  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -1.498   4.892  -2.954} {  -1.821   5.947  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -1.821   5.947  -3.113} {  -1.995   6.515  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -2.303   4.569  -2.954} {  -2.800   5.554  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -2.800   5.554  -3.113} {  -3.068   6.084  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -3.042   4.114  -2.954} {  -3.698   5.001  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -3.698   5.001  -3.113} {  -4.052   5.478  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -3.694   3.540  -2.954} {  -4.490   4.304  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -4.490   4.304  -3.113} {  -4.919   4.715  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -4.239   2.865  -2.954} {  -5.153   3.483  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -5.153   3.483  -3.113} {  -5.645   3.816  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -4.662   2.107  -2.954} {  -5.668   2.562  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -5.668   2.562  -3.113} {  -6.209   2.807  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -4.951   1.289  -2.954} {  -6.019   1.567  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -6.019   1.567  -3.113} {  -6.594   1.717  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -5.098   0.434  -2.954} {  -6.197   0.527  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -6.197   0.527  -3.113} {  -6.789   0.578  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -5.098  -0.434  -2.954} {  -6.197  -0.527  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -6.197  -0.527  -3.113} {  -6.789  -0.578  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -4.951  -1.289  -2.954} {  -6.019  -1.567  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -6.019  -1.567  -3.113} {  -6.594  -1.717  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -4.662  -2.107  -2.954} {  -5.668  -2.562  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -5.668  -2.562  -3.113} {  -6.209  -2.807  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -4.239  -2.865  -2.954} {  -5.153  -3.483  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -5.153  -3.483  -3.113} {  -5.645  -3.816  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -3.694  -3.540  -2.954} {  -4.490  -4.304  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -4.490  -4.304  -3.113} {  -4.919  -4.715  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -3.042  -4.114  -2.954} {  -3.698  -5.001  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -3.698  -5.001  -3.113} {  -4.052  -5.478  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -2.303  -4.569  -2.954} {  -2.800  -5.554  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -2.800  -5.554  -3.113} {  -3.068  -6.084  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -1.498  -4.892  -2.954} {  -1.821  -5.947  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -1.821  -5.947  -3.113} {  -1.995  -6.515  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {  -0.650  -5.075  -2.954} {  -0.790  -6.169  -3.113} radius 0.03 filled yes resolution 20
draw cone {  -0.790  -6.169  -3.113} {  -0.865  -6.759  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   0.217  -5.112  -2.954} {   0.264  -6.214  -3.113} radius 0.03 filled yes resolution 20
draw cone {   0.264  -6.214  -3.113} {   0.289  -6.808  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   1.078  -5.002  -2.954} {   1.310  -6.080  -3.113} radius 0.03 filled yes resolution 20
draw cone {   1.310  -6.080  -3.113} {   1.436  -6.661  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   1.908  -4.747  -2.954} {   2.319  -5.771  -3.113} radius 0.03 filled yes resolution 20
draw cone {   2.319  -5.771  -3.113} {   2.541  -6.322  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   2.683  -4.357  -2.954} {   3.261  -5.296  -3.113} radius 0.03 filled yes resolution 20
draw cone {   3.261  -5.296  -3.113} {   3.572  -5.802  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   3.380  -3.841  -2.954} {   4.109  -4.669  -3.113} radius 0.03 filled yes resolution 20
draw cone {   4.109  -4.669  -3.113} {   4.502  -5.115  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   3.981  -3.214  -2.954} {   4.839  -3.907  -3.113} radius 0.03 filled yes resolution 20
draw cone {   4.839  -3.907  -3.113} {   5.301  -4.281  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   4.467  -2.495  -2.954} {   5.430  -3.033  -3.113} radius 0.03 filled yes resolution 20
draw cone {   5.430  -3.033  -3.113} {   5.948  -3.323  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   4.824  -1.704  -2.954} {   5.864  -2.072  -3.113} radius 0.03 filled yes resolution 20
draw cone {   5.864  -2.072  -3.113} {   6.425  -2.270  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   5.043  -0.865  -2.954} {   6.130  -1.051  -3.113} radius 0.03 filled yes resolution 20
draw cone {   6.130  -1.051  -3.113} {   6.716  -1.152  -3.198} radius 0.06 resolution 20
draw color  886
draw cylinder {   5.116  -0.000  -2.954} {   6.220  -0.000  -3.113} radius 0.03 filled yes resolution 20
draw cone {   6.220  -0.000  -3.113} {   6.814  -0.000  -3.198} radius 0.06 resolution 20
draw color  782
draw cylinder {   4.535   0.874  -3.683} {   5.514   1.063  -3.881} radius 0.03 filled yes resolution 20
draw cone {   5.514   1.063  -3.881} {   6.040   1.164  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   4.288   1.717  -3.683} {   5.213   2.087  -3.881} radius 0.03 filled yes resolution 20
draw cone {   5.213   2.087  -3.881} {   5.711   2.286  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   3.886   2.497  -3.683} {   4.724   3.036  -3.881} radius 0.03 filled yes resolution 20
draw cone {   4.724   3.036  -3.881} {   5.175   3.326  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   3.343   3.187  -3.683} {   4.064   3.875  -3.881} radius 0.03 filled yes resolution 20
draw cone {   4.064   3.875  -3.881} {   4.452   4.245  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   2.679   3.762  -3.683} {   3.257   4.574  -3.881} radius 0.03 filled yes resolution 20
draw cone {   3.257   4.574  -3.881} {   3.568   5.011  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   1.919   4.202  -3.683} {   2.333   5.108  -3.881} radius 0.03 filled yes resolution 20
draw cone {   2.333   5.108  -3.881} {   2.555   5.595  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   1.089   4.489  -3.683} {   1.324   5.457  -3.881} radius 0.03 filled yes resolution 20
draw cone {   1.324   5.457  -3.881} {   1.450   5.978  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   0.220   4.614  -3.683} {   0.267   5.609  -3.881} radius 0.03 filled yes resolution 20
draw cone {   0.267   5.609  -3.881} {   0.293   6.144  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -0.657   4.572  -3.683} {  -0.799   5.558  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -0.799   5.558  -3.881} {  -0.875   6.089  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -1.511   4.365  -3.683} {  -1.836   5.306  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -1.836   5.306  -3.881} {  -2.012   5.813  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -2.309   4.000  -3.683} {  -2.807   4.863  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -2.807   4.863  -3.881} {  -3.076   5.327  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -3.025   3.491  -3.683} {  -3.677   4.244  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -3.677   4.244  -3.881} {  -4.028   4.649  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -3.631   2.855  -3.683} {  -4.414   3.471  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -4.414   3.471  -3.881} {  -4.835   3.802  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -4.105   2.117  -3.683} {  -4.991   2.573  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -4.991   2.573  -3.881} {  -5.468   2.819  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -4.432   1.301  -3.683} {  -5.388   1.582  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -5.388   1.582  -3.881} {  -5.902   1.733  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -4.598   0.439  -3.683} {  -5.590   0.534  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -5.590   0.534  -3.881} {  -6.123   0.585  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -4.598  -0.439  -3.683} {  -5.590  -0.534  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -5.590  -0.534  -3.881} {  -6.123  -0.585  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -4.432  -1.301  -3.683} {  -5.388  -1.582  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -5.388  -1.582  -3.881} {  -5.902  -1.733  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -4.105  -2.117  -3.683} {  -4.991  -2.573  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -4.991  -2.573  -3.881} {  -5.468  -2.819  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -3.631  -2.855  -3.683} {  -4.414  -3.471  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -4.414  -3.471  -3.881} {  -4.835  -3.802  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -3.025  -3.491  -3.683} {  -3.677  -4.244  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -3.677  -4.244  -3.881} {  -4.028  -4.649  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -2.309  -4.000  -3.683} {  -2.807  -4.863  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -2.807  -4.863  -3.881} {  -3.076  -5.327  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -1.511  -4.365  -3.683} {  -1.836  -5.306  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -1.836  -5.306  -3.881} {  -2.012  -5.813  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {  -0.657  -4.572  -3.683} {  -0.799  -5.558  -3.881} radius 0.03 filled yes resolution 20
draw cone {  -0.799  -5.558  -3.881} {  -0.875  -6.089  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   0.220  -4.614  -3.683} {   0.267  -5.609  -3.881} radius 0.03 filled yes resolution 20
draw cone {   0.267  -5.609  -3.881} {   0.293  -6.144  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   1.089  -4.489  -3.683} {   1.324  -5.457  -3.881} radius 0.03 filled yes resolution 20
draw cone {   1.324  -5.457  -3.881} {   1.450  -5.978  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   1.919  -4.202  -3.683} {   2.333  -5.108  -3.881} radius 0.03 filled yes resolution 20
draw cone {   2.333  -5.108  -3.881} {   2.555  -5.595  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   2.679  -3.762  -3.683} {   3.257  -4.574  -3.881} radius 0.03 filled yes resolution 20
draw cone {   3.257  -4.574  -3.881} {   3.568  -5.011  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   3.343  -3.187  -3.683} {   4.064  -3.875  -3.881} radius 0.03 filled yes resolution 20
draw cone {   4.064  -3.875  -3.881} {   4.452  -4.245  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   3.886  -2.497  -3.683} {   4.724  -3.036  -3.881} radius 0.03 filled yes resolution 20
draw cone {   4.724  -3.036  -3.881} {   5.175  -3.326  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   4.288  -1.717  -3.683} {   5.213  -2.087  -3.881} radius 0.03 filled yes resolution 20
draw cone {   5.213  -2.087  -3.881} {   5.711  -2.286  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   4.535  -0.874  -3.683} {   5.514  -1.063  -3.881} radius 0.03 filled yes resolution 20
draw cone {   5.514  -1.063  -3.881} {   6.040  -1.164  -3.988} radius 0.06 resolution 20
draw color  782
draw cylinder {   4.619  -0.000  -3.683} {   5.615  -0.000  -3.881} radius 0.03 filled yes resolution 20
draw cone {   5.615  -0.000  -3.881} {   6.151  -0.000  -3.988} radius 0.06 resolution 20
draw color  658
draw cylinder {   3.924   0.864  -4.331} {   4.771   1.050  -4.564} radius 0.03 filled yes resolution 20
draw cone {   4.771   1.050  -4.564} {   5.226   1.150  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   3.647   1.687  -4.331} {   4.433   2.051  -4.564} radius 0.03 filled yes resolution 20
draw cone {   4.433   2.051  -4.564} {   4.857   2.247  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   3.199   2.432  -4.331} {   3.889   2.956  -4.564} radius 0.03 filled yes resolution 20
draw cone {   3.889   2.956  -4.564} {   4.260   3.239  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   2.601   3.063  -4.331} {   3.162   3.723  -4.564} radius 0.03 filled yes resolution 20
draw cone {   3.162   3.723  -4.564} {   3.464   4.079  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   1.882   3.550  -4.331} {   2.288   4.316  -4.564} radius 0.03 filled yes resolution 20
draw cone {   2.288   4.316  -4.564} {   2.507   4.728  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   1.075   3.872  -4.331} {   1.307   4.707  -4.564} radius 0.03 filled yes resolution 20
draw cone {   1.307   4.707  -4.564} {   1.432   5.156  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   0.218   4.012  -4.331} {   0.264   4.878  -4.564} radius 0.03 filled yes resolution 20
draw cone {   0.264   4.878  -4.564} {   0.290   5.344  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -0.650   3.965  -4.331} {  -0.790   4.821  -4.564} radius 0.03 filled yes resolution 20
draw cone {  -0.790   4.821  -4.564} {  -0.866   5.281  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -1.487   3.733  -4.331} {  -1.808   4.538  -4.564} radius 0.03 filled yes resolution 20
draw cone {  -1.808   4.538  -4.564} {  -1.981   4.971  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -2.255   3.326  -4.331} {  -2.741   4.043  -4.564} radius 0.03 filled yes resolution 20
draw cone {  -2.741   4.043  -4.564} {  -3.003   4.429  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -2.917   2.763  -4.331} {  -3.546   3.359  -4.564} radius 0.03 filled yes resolution 20
draw cone {  -3.546   3.359  -4.564} {  -3.885   3.680  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -3.443   2.072  -4.331} {  -4.186   2.518  -4.564} radius 0.03 filled yes resolution 20
draw cone {  -4.186   2.518  -4.564} {  -4.585   2.759  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -3.808   1.283  -4.331} {  -4.629   1.560  -4.564} radius 0.03 filled yes resolution 20
draw cone {  -4.629   1.560  -4.564} {  -5.071   1.709  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -3.995   0.434  -4.331} {  -4.856   0.528  -4.564} radius 0.03 filled yes resolution 20
draw cone {  -4.856   0.528  -4.564} {  -5.320   0.579  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -3.995  -0.434  -4.331} {  -4.856  -0.528  -4.564} radius 0.03 filled yes resolution 20
draw cone {  -4.856  -0.528  -4.564} {  -5.320  -0.579  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -3.808  -1.283  -4.331} {  -4.629  -1.560  -4.564} radius 0.03 filled yes resolution 20
draw cone {  -4.629  -1.560  -4.564} {  -5.071  -1.709  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -3.443  -2.072  -4.331} {  -4.186  -2.518  -4.564} radius 0.03 filled yes resolution 20
draw cone {  -4.186  -2.518  -4.564} {  -4.585  -2.759  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -2.917  -2.763  -4.331} {  -3.546  -3.359  -4.564} radius 0.03 filled yes resolution 20
draw cone {  -3.546  -3.359  -4.564} {  -3.885  -3.680  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -2.255  -3.326  -4.331} {  -2.741  -4.043  -4.564} radius 0.03 filled yes resolution 20
draw cone {  -2.741  -4.043  -4.564} {  -3.003  -4.429  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -1.487  -3.733  -4.331} {  -1.808  -4.538  -4.564} radius 0.03 filled yes resolution 20
draw cone {  -1.808  -4.538  -4.564} {  -1.981  -4.971  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {  -0.650  -3.965  -4.331} {  -0.790  -4.821  -4.564} radius 0.03 filled yes resolution 20
draw cone {  -0.790  -4.821  -4.564} {  -0.866  -5.281  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   0.218  -4.012  -4.331} {   0.264  -4.878  -4.564} radius 0.03 filled yes resolution 20
draw cone {   0.264  -4.878  -4.564} {   0.290  -5.344  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   1.075  -3.872  -4.331} {   1.307  -4.707  -4.564} radius 0.03 filled yes resolution 20
draw cone {   1.307  -4.707  -4.564} {   1.432  -5.156  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   1.882  -3.550  -4.331} {   2.288  -4.316  -4.564} radius 0.03 filled yes resolution 20
draw cone {   2.288  -4.316  -4.564} {   2.507  -4.728  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   2.601  -3.063  -4.331} {   3.162  -3.723  -4.564} radius 0.03 filled yes resolution 20
draw cone {   3.162  -3.723  -4.564} {   3.464  -4.079  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   3.199  -2.432  -4.331} {   3.889  -2.956  -4.564} radius 0.03 filled yes resolution 20
draw cone {   3.889  -2.956  -4.564} {   4.260  -3.239  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   3.647  -1.687  -4.331} {   4.433  -2.051  -4.564} radius 0.03 filled yes resolution 20
draw cone {   4.433  -2.051  -4.564} {   4.857  -2.247  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   3.924  -0.864  -4.331} {   4.771  -1.050  -4.564} radius 0.03 filled yes resolution 20
draw cone {   4.771  -1.050  -4.564} {   5.226  -1.150  -4.689} radius 0.06 resolution 20
draw color  658
draw cylinder {   4.018  -0.000  -4.331} {   4.885  -0.000  -4.564} radius 0.03 filled yes resolution 20
draw cone {   4.885  -0.000  -4.564} {   5.351  -0.000  -4.689} radius 0.06 resolution 20
draw color  519
draw cylinder {   3.215   0.861  -4.881} {   3.908   1.047  -5.144} radius 0.03 filled yes resolution 20
draw cone {   3.908   1.047  -5.144} {   4.281   1.147  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   2.882   1.664  -4.881} {   3.504   2.023  -5.144} radius 0.03 filled yes resolution 20
draw cone {   3.504   2.023  -5.144} {   3.838   2.216  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   2.353   2.353  -4.881} {   2.861   2.861  -5.144} radius 0.03 filled yes resolution 20
draw cone {   2.861   2.861  -5.144} {   3.134   3.134  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   1.664   2.882  -4.881} {   2.023   3.504  -5.144} radius 0.03 filled yes resolution 20
draw cone {   2.023   3.504  -5.144} {   2.216   3.838  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   0.861   3.215  -4.881} {   1.047   3.908  -5.144} radius 0.03 filled yes resolution 20
draw cone {   1.047   3.908  -5.144} {   1.147   4.281  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   0.000   3.328  -4.881} {   0.000   4.046  -5.144} radius 0.03 filled yes resolution 20
draw cone {   0.000   4.046  -5.144} {   0.000   4.432  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -0.861   3.215  -4.881} {  -1.047   3.908  -5.144} radius 0.03 filled yes resolution 20
draw cone {  -1.047   3.908  -5.144} {  -1.147   4.281  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -1.664   2.882  -4.881} {  -2.023   3.504  -5.144} radius 0.03 filled yes resolution 20
draw cone {  -2.023   3.504  -5.144} {  -2.216   3.838  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -2.353   2.353  -4.881} {  -2.861   2.861  -5.144} radius 0.03 filled yes resolution 20
draw cone {  -2.861   2.861  -5.144} {  -3.134   3.134  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -2.882   1.664  -4.881} {  -3.504   2.023  -5.144} radius 0.03 filled yes resolution 20
draw cone {  -3.504   2.023  -5.144} {  -3.838   2.216  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -3.215   0.861  -4.881} {  -3.908   1.047  -5.144} radius 0.03 filled yes resolution 20
draw cone {  -3.908   1.047  -5.144} {  -4.281   1.147  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -3.328   0.000  -4.881} {  -4.046   0.000  -5.144} radius 0.03 filled yes resolution 20
draw cone {  -4.046   0.000  -5.144} {  -4.432   0.000  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -3.215  -0.861  -4.881} {  -3.908  -1.047  -5.144} radius 0.03 filled yes resolution 20
draw cone {  -3.908  -1.047  -5.144} {  -4.281  -1.147  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -2.882  -1.664  -4.881} {  -3.504  -2.023  -5.144} radius 0.03 filled yes resolution 20
draw cone {  -3.504  -2.023  -5.144} {  -3.838  -2.216  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -2.353  -2.353  -4.881} {  -2.861  -2.861  -5.144} radius 0.03 filled yes resolution 20
draw cone {  -2.861  -2.861  -5.144} {  -3.134  -3.134  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -1.664  -2.882  -4.881} {  -2.023  -3.504  -5.144} radius 0.03 filled yes resolution 20
draw cone {  -2.023  -3.504  -5.144} {  -2.216  -3.838  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -0.861  -3.215  -4.881} {  -1.047  -3.908  -5.144} radius 0.03 filled yes resolution 20
draw cone {  -1.047  -3.908  -5.144} {  -1.147  -4.281  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {  -0.000  -3.328  -4.881} {  -0.000  -4.046  -5.144} radius 0.03 filled yes resolution 20
draw cone {  -0.000  -4.046  -5.144} {  -0.000  -4.432  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   0.861  -3.215  -4.881} {   1.047  -3.908  -5.144} radius 0.03 filled yes resolution 20
draw cone {   1.047  -3.908  -5.144} {   1.147  -4.281  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   1.664  -2.882  -4.881} {   2.023  -3.504  -5.144} radius 0.03 filled yes resolution 20
draw cone {   2.023  -3.504  -5.144} {   2.216  -3.838  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   2.353  -2.353  -4.881} {   2.861  -2.861  -5.144} radius 0.03 filled yes resolution 20
draw cone {   2.861  -2.861  -5.144} {   3.134  -3.134  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   2.882  -1.664  -4.881} {   3.504  -2.023  -5.144} radius 0.03 filled yes resolution 20
draw cone {   3.504  -2.023  -5.144} {   3.838  -2.216  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   3.215  -0.861  -4.881} {   3.908  -1.047  -5.144} radius 0.03 filled yes resolution 20
draw cone {   3.908  -1.047  -5.144} {   4.281  -1.147  -5.285} radius 0.06 resolution 20
draw color  519
draw cylinder {   3.328  -0.000  -4.881} {   4.046  -0.000  -5.144} radius 0.03 filled yes resolution 20
draw cone {   4.046  -0.000  -5.144} {   4.432  -0.000  -5.285} radius 0.06 resolution 20
draw color  370
draw cylinder {   2.409   0.877  -5.323} {   2.928   1.066  -5.609} radius 0.03 filled yes resolution 20
draw cone {   2.928   1.066  -5.609} {   3.208   1.168  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   1.964   1.648  -5.323} {   2.387   2.003  -5.609} radius 0.03 filled yes resolution 20
draw cone {   2.387   2.003  -5.609} {   2.615   2.194  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   1.282   2.220  -5.323} {   1.558   2.699  -5.609} radius 0.03 filled yes resolution 20
draw cone {   1.558   2.699  -5.609} {   1.707   2.956  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   0.445   2.524  -5.323} {   0.541   3.069  -5.609} radius 0.03 filled yes resolution 20
draw cone {   0.541   3.069  -5.609} {   0.593   3.362  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -0.445   2.524  -5.323} {  -0.541   3.069  -5.609} radius 0.03 filled yes resolution 20
draw cone {  -0.541   3.069  -5.609} {  -0.593   3.362  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -1.282   2.220  -5.323} {  -1.558   2.699  -5.609} radius 0.03 filled yes resolution 20
draw cone {  -1.558   2.699  -5.609} {  -1.707   2.956  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -1.964   1.648  -5.323} {  -2.387   2.003  -5.609} radius 0.03 filled yes resolution 20
draw cone {  -2.387   2.003  -5.609} {  -2.615   2.194  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -2.409   0.877  -5.323} {  -2.928   1.066  -5.609} radius 0.03 filled yes resolution 20
draw cone {  -2.928   1.066  -5.609} {  -3.208   1.168  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -2.563   0.000  -5.323} {  -3.116   0.000  -5.609} radius 0.03 filled yes resolution 20
draw cone {  -3.116   0.000  -5.609} {  -3.414   0.000  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -2.409  -0.877  -5.323} {  -2.928  -1.066  -5.609} radius 0.03 filled yes resolution 20
draw cone {  -2.928  -1.066  -5.609} {  -3.208  -1.168  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -1.964  -1.648  -5.323} {  -2.387  -2.003  -5.609} radius 0.03 filled yes resolution 20
draw cone {  -2.387  -2.003  -5.609} {  -2.615  -2.194  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -1.282  -2.220  -5.323} {  -1.558  -2.699  -5.609} radius 0.03 filled yes resolution 20
draw cone {  -1.558  -2.699  -5.609} {  -1.707  -2.956  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {  -0.445  -2.524  -5.323} {  -0.541  -3.069  -5.609} radius 0.03 filled yes resolution 20
draw cone {  -0.541  -3.069  -5.609} {  -0.593  -3.362  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   0.445  -2.524  -5.323} {   0.541  -3.069  -5.609} radius 0.03 filled yes resolution 20
draw cone {   0.541  -3.069  -5.609} {   0.593  -3.362  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   1.282  -2.220  -5.323} {   1.558  -2.699  -5.609} radius 0.03 filled yes resolution 20
draw cone {   1.558  -2.699  -5.609} {   1.707  -2.956  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   1.964  -1.648  -5.323} {   2.387  -2.003  -5.609} radius 0.03 filled yes resolution 20
draw cone {   2.387  -2.003  -5.609} {   2.615  -2.194  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   2.409  -0.877  -5.323} {   2.928  -1.066  -5.609} radius 0.03 filled yes resolution 20
draw cone {   2.928  -1.066  -5.609} {   3.208  -1.168  -5.763} radius 0.06 resolution 20
draw color  370
draw cylinder {   2.563  -0.000  -5.323} {   3.116  -0.000  -5.609} radius 0.03 filled yes resolution 20
draw cone {   3.116  -0.000  -5.609} {   3.414  -0.000  -5.763} radius 0.06 resolution 20
draw color  223
draw cylinder {   1.508   0.871  -5.645} {   1.833   1.058  -5.949} radius 0.03 filled yes resolution 20
draw cone {   1.833   1.058  -5.949} {   2.008   1.160  -6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {   0.871   1.508  -5.645} {   1.058   1.833  -5.949} radius 0.03 filled yes resolution 20
draw cone {   1.058   1.833  -5.949} {   1.160   2.008  -6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {   0.000   1.741  -5.645} {   0.000   2.117  -5.949} radius 0.03 filled yes resolution 20
draw cone {   0.000   2.117  -5.949} {   0.000   2.319  -6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {  -0.871   1.508  -5.645} {  -1.058   1.833  -5.949} radius 0.03 filled yes resolution 20
draw cone {  -1.058   1.833  -5.949} {  -1.160   2.008  -6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {  -1.508   0.871  -5.645} {  -1.833   1.058  -5.949} radius 0.03 filled yes resolution 20
draw cone {  -1.833   1.058  -5.949} {  -2.008   1.160  -6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {  -1.741   0.000  -5.645} {  -2.117   0.000  -5.949} radius 0.03 filled yes resolution 20
draw cone {  -2.117   0.000  -5.949} {  -2.319   0.000  -6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {  -1.508  -0.871  -5.645} {  -1.833  -1.058  -5.949} radius 0.03 filled yes resolution 20
draw cone {  -1.833  -1.058  -5.949} {  -2.008  -1.160  -6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {  -0.871  -1.508  -5.645} {  -1.058  -1.833  -5.949} radius 0.03 filled yes resolution 20
draw cone {  -1.058  -1.833  -5.949} {  -1.160  -2.008  -6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {  -0.000  -1.741  -5.645} {  -0.000  -2.117  -5.949} radius 0.03 filled yes resolution 20
draw cone {  -0.000  -2.117  -5.949} {  -0.000  -2.319  -6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {   0.871  -1.508  -5.645} {   1.058  -1.833  -5.949} radius 0.03 filled yes resolution 20
draw cone {   1.058  -1.833  -5.949} {   1.160  -2.008  -6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {   1.508  -0.871  -5.645} {   1.833  -1.058  -5.949} radius 0.03 filled yes resolution 20
draw cone {   1.833  -1.058  -5.949} {   2.008  -1.160  -6.112} radius 0.06 resolution 20
draw color  223
draw cylinder {   1.741  -0.000  -5.645} {   2.117  -0.000  -5.949} radius 0.03 filled yes resolution 20
draw cone {   2.117  -0.000  -5.949} {   2.319  -0.000  -6.112} radius 0.06 resolution 20
draw color  102
draw cylinder {   0.440   0.763  -5.842} {   0.535   0.927  -6.156} radius 0.03 filled yes resolution 20
draw cone {   0.535   0.927  -6.156} {   0.586   1.016  -6.325} radius 0.06 resolution 20
draw color  102
draw cylinder {  -0.440   0.763  -5.842} {  -0.535   0.927  -6.156} radius 0.03 filled yes resolution 20
draw cone {  -0.535   0.927  -6.156} {  -0.586   1.016  -6.325} radius 0.06 resolution 20
draw color  102
draw cylinder {  -0.881   0.000  -5.842} {  -1.070   0.000  -6.156} radius 0.03 filled yes resolution 20
draw cone {  -1.070   0.000  -6.156} {  -1.173   0.000  -6.325} radius 0.06 resolution 20
draw color  102
draw cylinder {  -0.440  -0.763  -5.842} {  -0.535  -0.927  -6.156} radius 0.03 filled yes resolution 20
draw cone {  -0.535  -0.927  -6.156} {  -0.586  -1.016  -6.325} radius 0.06 resolution 20
draw color  102
draw cylinder {   0.440  -0.763  -5.842} {   0.535  -0.927  -6.156} radius 0.03 filled yes resolution 20
draw cone {   0.535  -0.927  -6.156} {   0.586  -1.016  -6.325} radius 0.06 resolution 20
draw color  102
draw cylinder {   0.881  -0.000  -5.842} {   1.070  -0.000  -6.156} radius 0.03 filled yes resolution 20
draw cone {   1.070  -0.000  -6.156} {   1.173  -0.000  -6.325} radius 0.06 resolution 20
draw color   51
draw cylinder {   0.000  -0.000  -5.908} {   0.000  -0.000  -6.225} radius 0.03 filled yes resolution 20
draw cone {   0.000  -0.000  -6.225} {   0.000  -0.000  -6.396} radius 0.06 resolution 20
