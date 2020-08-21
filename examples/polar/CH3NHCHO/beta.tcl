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
draw color  269
draw cylinder {   0.000   0.000   3.308} {   0.176   0.033   3.308} radius 0.03 filled yes resolution 20
draw cone {   0.176   0.033   3.308} {   0.271   0.052   3.308} radius 0.06 resolution 20
draw color  251
draw cylinder {   0.272   0.472   3.263} {   0.436   0.489   3.300} radius 0.03 filled yes resolution 20
draw cone {   0.436   0.489   3.300} {   0.524   0.498   3.321} radius 0.06 resolution 20
draw color  243
draw cylinder {  -0.272   0.472   3.263} {  -0.112   0.505   3.244} radius 0.03 filled yes resolution 20
draw cone {  -0.112   0.505   3.244} {  -0.026   0.523   3.234} radius 0.06 resolution 20
draw color  259
draw cylinder {  -0.544   0.000   3.263} {  -0.384   0.035   3.207} radius 0.03 filled yes resolution 20
draw cone {  -0.384   0.035   3.207} {  -0.298   0.054   3.177} radius 0.06 resolution 20
draw color  251
draw cylinder {  -0.272  -0.472   3.263} {  -0.108  -0.454   3.225} radius 0.03 filled yes resolution 20
draw cone {  -0.108  -0.454   3.225} {  -0.020  -0.445   3.205} radius 0.06 resolution 20
draw color  243
draw cylinder {   0.272  -0.472   3.263} {   0.432  -0.438   3.282} radius 0.03 filled yes resolution 20
draw cone {   0.432  -0.438   3.282} {   0.518  -0.421   3.292} radius 0.06 resolution 20
draw color  259
draw cylinder {   0.544  -0.000   3.263} {   0.705   0.035   3.319} radius 0.03 filled yes resolution 20
draw cone {   0.705   0.035   3.319} {   0.791   0.054   3.349} radius 0.06 resolution 20
draw color  238
draw cylinder {   0.930   0.537   3.129} {   1.055   0.533   3.231} radius 0.03 filled yes resolution 20
draw cone {   1.055   0.533   3.231} {   1.122   0.530   3.286} radius 0.06 resolution 20
draw color  218
draw cylinder {   0.537   0.930   3.129} {   0.665   0.900   3.200} radius 0.03 filled yes resolution 20
draw cone {   0.665   0.900   3.200} {   0.735   0.884   3.238} radius 0.06 resolution 20
draw color  175
draw cylinder {   0.000   1.074   3.129} {   0.123   1.062   3.149} radius 0.03 filled yes resolution 20
draw cone {   0.123   1.062   3.149} {   0.189   1.056   3.161} radius 0.06 resolution 20
draw color  170
draw cylinder {  -0.537   0.930   3.129} {  -0.424   0.962   3.094} radius 0.03 filled yes resolution 20
draw cone {  -0.424   0.962   3.094} {  -0.363   0.980   3.075} radius 0.06 resolution 20
draw color  214
draw cylinder {  -0.930   0.537   3.129} {  -0.821   0.595   3.047} radius 0.03 filled yes resolution 20
draw cone {  -0.821   0.595   3.047} {  -0.763   0.626   3.003} radius 0.06 resolution 20
draw color  238
draw cylinder {  -1.074   0.000   3.129} {  -0.959   0.039   3.023} radius 0.03 filled yes resolution 20
draw cone {  -0.959   0.039   3.023} {  -0.898   0.061   2.965} radius 0.06 resolution 20
draw color  238
draw cylinder {  -0.930  -0.537   3.129} {  -0.806  -0.541   3.027} radius 0.03 filled yes resolution 20
draw cone {  -0.806  -0.541   3.027} {  -0.739  -0.544   2.971} radius 0.06 resolution 20
draw color  218
draw cylinder {  -0.537  -0.930   3.129} {  -0.409  -0.960   3.058} radius 0.03 filled yes resolution 20
draw cone {  -0.409  -0.960   3.058} {  -0.340  -0.976   3.020} radius 0.06 resolution 20
draw color  175
draw cylinder {  -0.000  -1.074   3.129} {   0.123  -1.086   3.108} radius 0.03 filled yes resolution 20
draw cone {   0.123  -1.086   3.108} {   0.189  -1.092   3.097} radius 0.06 resolution 20
draw color  170
draw cylinder {   0.537  -0.930   3.129} {   0.650  -0.898   3.164} radius 0.03 filled yes resolution 20
draw cone {   0.650  -0.898   3.164} {   0.711  -0.881   3.183} radius 0.06 resolution 20
draw color  214
draw cylinder {   0.930  -0.537   3.129} {   1.039  -0.479   3.210} radius 0.03 filled yes resolution 20
draw cone {   1.039  -0.479   3.210} {   1.098  -0.448   3.254} radius 0.06 resolution 20
draw color  238
draw cylinder {   1.074  -0.000   3.129} {   1.189   0.039   3.235} radius 0.03 filled yes resolution 20
draw cone {   1.189   0.039   3.235} {   1.250   0.061   3.292} radius 0.06 resolution 20
draw color  232
draw cylinder {   1.480   0.538   2.909} {   1.538   0.522   3.055} radius 0.03 filled yes resolution 20
draw cone {   1.538   0.522   3.055} {   1.569   0.514   3.133} radius 0.06 resolution 20
draw color  244
draw cylinder {   1.206   1.012   2.909} {   1.276   0.937   3.038} radius 0.03 filled yes resolution 20
draw cone {   1.276   0.937   3.038} {   1.314   0.897   3.108} radius 0.06 resolution 20
draw color  234
draw cylinder {   0.787   1.364   2.909} {   0.861   1.261   3.006} radius 0.03 filled yes resolution 20
draw cone {   0.861   1.261   3.006} {   0.900   1.205   3.058} radius 0.06 resolution 20
draw color  169
draw cylinder {   0.273   1.551   2.909} {   0.341   1.464   2.962} radius 0.03 filled yes resolution 20
draw cone {   0.341   1.464   2.962} {   0.377   1.417   2.991} radius 0.06 resolution 20
draw color   68
draw cylinder {  -0.273   1.551   2.909} {  -0.219   1.517   2.912} radius 0.03 filled yes resolution 20
draw cone {  -0.219   1.517   2.912} {  -0.190   1.499   2.913} radius 0.06 resolution 20
draw color   79
draw cylinder {  -0.787   1.364   2.909} {  -0.747   1.394   2.861} radius 0.03 filled yes resolution 20
draw cone {  -0.747   1.394   2.861} {  -0.725   1.411   2.836} radius 0.06 resolution 20
draw color  174
draw cylinder {  -1.206   1.012   2.909} {  -1.174   1.089   2.817} radius 0.03 filled yes resolution 20
draw cone {  -1.174   1.089   2.817} {  -1.157   1.130   2.767} radius 0.06 resolution 20
draw color  227
draw cylinder {  -1.480   0.538   2.909} {  -1.446   0.621   2.783} radius 0.03 filled yes resolution 20
draw cone {  -1.446   0.621   2.783} {  -1.428   0.666   2.715} radius 0.06 resolution 20
draw color  233
draw cylinder {  -1.574   0.000   2.909} {  -1.530   0.046   2.765} radius 0.03 filled yes resolution 20
draw cone {  -1.530   0.046   2.765} {  -1.507   0.071   2.687} radius 0.06 resolution 20
draw color  232
draw cylinder {  -1.480  -0.538   2.909} {  -1.421  -0.555   2.764} radius 0.03 filled yes resolution 20
draw cone {  -1.421  -0.555   2.764} {  -1.390  -0.563   2.685} radius 0.06 resolution 20
draw color  244
draw cylinder {  -1.206  -1.012   2.909} {  -1.136  -1.087   2.780} radius 0.03 filled yes resolution 20
draw cone {  -1.136  -1.087   2.780} {  -1.098  -1.127   2.711} radius 0.06 resolution 20
draw color  234
draw cylinder {  -0.787  -1.364   2.909} {  -0.714  -1.466   2.813} radius 0.03 filled yes resolution 20
draw cone {  -0.714  -1.466   2.813} {  -0.674  -1.522   2.760} radius 0.06 resolution 20
draw color  169
draw cylinder {  -0.273  -1.551   2.909} {  -0.206  -1.637   2.856} radius 0.03 filled yes resolution 20
draw cone {  -0.206  -1.637   2.856} {  -0.170  -1.684   2.828} radius 0.06 resolution 20
draw color   68
draw cylinder {   0.273  -1.551   2.909} {   0.328  -1.584   2.907} radius 0.03 filled yes resolution 20
draw cone {   0.328  -1.584   2.907} {   0.357  -1.603   2.905} radius 0.06 resolution 20
draw color   79
draw cylinder {   0.787  -1.364   2.909} {   0.827  -1.333   2.957} radius 0.03 filled yes resolution 20
draw cone {   0.827  -1.333   2.957} {   0.849  -1.316   2.983} radius 0.06 resolution 20
draw color  174
draw cylinder {   1.206  -1.012   2.909} {   1.238  -0.935   3.002} radius 0.03 filled yes resolution 20
draw cone {   1.238  -0.935   3.002} {   1.256  -0.894   3.052} radius 0.06 resolution 20
draw color  227
draw cylinder {   1.480  -0.538   2.909} {   1.513  -0.456   3.036} radius 0.03 filled yes resolution 20
draw cone {   1.513  -0.456   3.036} {   1.531  -0.411   3.104} radius 0.06 resolution 20
draw color  233
draw cylinder {   1.574  -0.000   2.909} {   1.618   0.046   3.054} radius 0.03 filled yes resolution 20
draw cone {   1.618   0.046   3.054} {   1.642   0.071   3.132} radius 0.06 resolution 20
draw color  260
draw cylinder {   1.957   0.548   2.611} {   1.931   0.523   2.781} radius 0.03 filled yes resolution 20
draw cone {   1.931   0.523   2.781} {   1.918   0.509   2.872} radius 0.06 resolution 20
draw color  294
draw cylinder {   1.736   1.056   2.611} {   1.728   0.947   2.771} radius 0.03 filled yes resolution 20
draw cone {   1.728   0.947   2.771} {   1.724   0.889   2.857} radius 0.06 resolution 20
draw color  340
draw cylinder {   1.387   1.485   2.611} {   1.390   1.314   2.749} radius 0.03 filled yes resolution 20
draw cone {   1.390   1.314   2.749} {   1.392   1.222   2.823} radius 0.06 resolution 20
draw color  342
draw cylinder {   0.935   1.804   2.611} {   0.940   1.610   2.717} radius 0.03 filled yes resolution 20
draw cone {   0.940   1.610   2.717} {   0.942   1.505   2.774} radius 0.06 resolution 20
draw color  277
draw cylinder {   0.413   1.989   2.611} {   0.410   1.818   2.677} radius 0.03 filled yes resolution 20
draw cone {   0.410   1.818   2.677} {   0.408   1.725   2.712} radius 0.06 resolution 20
draw color  155
draw cylinder {  -0.139   2.027   2.611} {  -0.158   1.917   2.632} radius 0.03 filled yes resolution 20
draw cone {  -0.158   1.917   2.632} {  -0.169   1.858   2.643} radius 0.06 resolution 20
draw color   51
draw cylinder {  -0.680   1.915   2.611} {  -0.719   1.888   2.585} radius 0.03 filled yes resolution 20
draw cone {  -0.719   1.888   2.585} {  -0.740   1.874   2.571} radius 0.06 resolution 20
draw color  138
draw cylinder {  -1.172   1.660   2.611} {  -1.227   1.714   2.541} radius 0.03 filled yes resolution 20
draw cone {  -1.227   1.714   2.541} {  -1.256   1.743   2.503} radius 0.06 resolution 20
draw color  247
draw cylinder {  -1.576   1.282   2.611} {  -1.640   1.390   2.501} radius 0.03 filled yes resolution 20
draw cone {  -1.640   1.390   2.501} {  -1.674   1.448   2.442} radius 0.06 resolution 20
draw color  297
draw cylinder {  -1.864   0.809   2.611} {  -1.926   0.929   2.470} radius 0.03 filled yes resolution 20
draw cone {  -1.926   0.929   2.470} {  -1.960   0.993   2.394} radius 0.06 resolution 20
draw color  288
draw cylinder {  -2.013   0.277   2.611} {  -2.065   0.363   2.449} radius 0.03 filled yes resolution 20
draw cone {  -2.065   0.363   2.449} {  -2.093   0.409   2.362} radius 0.06 resolution 20
draw color  261
draw cylinder {  -2.013  -0.277   2.611} {  -2.048  -0.260   2.440} radius 0.03 filled yes resolution 20
draw cone {  -2.048  -0.260   2.440} {  -2.066  -0.251   2.348} radius 0.06 resolution 20
draw color  272
draw cylinder {  -1.864  -0.809   2.611} {  -1.880  -0.878   2.444} radius 0.03 filled yes resolution 20
draw cone {  -1.880  -0.878   2.444} {  -1.888  -0.914   2.354} radius 0.06 resolution 20
draw color  320
draw cylinder {  -1.576  -1.282   2.611} {  -1.577  -1.426   2.460} radius 0.03 filled yes resolution 20
draw cone {  -1.577  -1.426   2.460} {  -1.578  -1.503   2.379} radius 0.06 resolution 20
draw color  349
draw cylinder {  -1.172  -1.660   2.611} {  -1.166  -1.848   2.487} radius 0.03 filled yes resolution 20
draw cone {  -1.166  -1.848   2.487} {  -1.163  -1.949   2.421} radius 0.06 resolution 20
draw color  319
draw cylinder {  -0.680  -1.915   2.611} {  -0.679  -2.103   2.524} radius 0.03 filled yes resolution 20
draw cone {  -0.679  -2.103   2.524} {  -0.678  -2.205   2.477} radius 0.06 resolution 20
draw color  221
draw cylinder {  -0.139  -2.027   2.611} {  -0.150  -2.172   2.567} radius 0.03 filled yes resolution 20
draw cone {  -0.150  -2.172   2.567} {  -0.156  -2.250   2.543} radius 0.06 resolution 20
draw color   88
draw cylinder {   0.413  -1.989   2.611} {   0.384  -2.059   2.613} radius 0.03 filled yes resolution 20
draw cone {   0.384  -2.059   2.613} {   0.368  -2.096   2.614} radius 0.06 resolution 20
draw color   78
draw cylinder {   0.935  -1.804   2.611} {   0.887  -1.789   2.659} radius 0.03 filled yes resolution 20
draw cone {   0.887  -1.789   2.659} {   0.862  -1.780   2.685} radius 0.06 resolution 20
draw color  198
draw cylinder {   1.387  -1.485   2.611} {   1.326  -1.400   2.701} radius 0.03 filled yes resolution 20
draw cone {   1.326  -1.400   2.701} {   1.294  -1.354   2.750} radius 0.06 resolution 20
draw color  280
draw cylinder {   1.736  -1.056   2.611} {   1.671  -0.936   2.737} radius 0.03 filled yes resolution 20
draw cone {   1.671  -0.936   2.737} {   1.637  -0.872   2.805} radius 0.06 resolution 20
draw color  298
draw cylinder {   1.957  -0.548   2.611} {   1.898  -0.440   2.763} radius 0.03 filled yes resolution 20
draw cone {   1.898  -0.440   2.763} {   1.867  -0.382   2.845} radius 0.06 resolution 20
draw color  273
draw cylinder {   2.032  -0.000   2.611} {   1.988   0.055   2.778} radius 0.03 filled yes resolution 20
draw cone {   1.988   0.055   2.778} {   1.964   0.084   2.868} radius 0.06 resolution 20
draw color  328
draw cylinder {   2.373   0.542   2.240} {   2.255   0.513   2.416} radius 0.03 filled yes resolution 20
draw cone {   2.255   0.513   2.416} {   2.192   0.497   2.510} radius 0.06 resolution 20
draw color  365
draw cylinder {   2.193   1.056   2.240} {   2.097   0.927   2.410} radius 0.03 filled yes resolution 20
draw cone {   2.097   0.927   2.410} {   2.045   0.857   2.502} radius 0.06 resolution 20
draw color  443
draw cylinder {   1.903   1.517   2.240} {   1.824   1.300   2.396} radius 0.03 filled yes resolution 20
draw cone {   1.824   1.300   2.396} {   1.782   1.183   2.480} radius 0.06 resolution 20
draw color  504
draw cylinder {   1.517   1.903   2.240} {   1.448   1.627   2.374} radius 0.03 filled yes resolution 20
draw cone {   1.448   1.627   2.374} {   1.410   1.478   2.446} radius 0.06 resolution 20
draw color  512
draw cylinder {   1.056   2.193   2.240} {   0.986   1.900   2.345} radius 0.03 filled yes resolution 20
draw cone {   0.986   1.900   2.345} {   0.948   1.742   2.402} radius 0.06 resolution 20
draw color  455
draw cylinder {   0.542   2.373   2.240} {   0.461   2.108   2.312} radius 0.03 filled yes resolution 20
draw cone {   0.461   2.108   2.312} {   0.418   1.965   2.350} radius 0.06 resolution 20
draw color  347
draw cylinder {   0.000   2.434   2.240} {  -0.098   2.236   2.274} radius 0.03 filled yes resolution 20
draw cone {  -0.098   2.236   2.274} {  -0.152   2.129   2.292} radius 0.06 resolution 20
draw color  236
draw cylinder {  -0.542   2.373   2.240} {  -0.662   2.267   2.235} radius 0.03 filled yes resolution 20
draw cone {  -0.662   2.267   2.235} {  -0.727   2.211   2.232} radius 0.06 resolution 20
draw color  217
draw cylinder {  -1.056   2.193   2.240} {  -1.198   2.188   2.196} radius 0.03 filled yes resolution 20
draw cone {  -1.198   2.188   2.196} {  -1.275   2.185   2.172} radius 0.06 resolution 20
draw color  301
draw cylinder {  -1.517   1.903   2.240} {  -1.677   1.986   2.159} radius 0.03 filled yes resolution 20
draw cone {  -1.677   1.986   2.159} {  -1.763   2.031   2.116} radius 0.06 resolution 20
draw color  389
draw cylinder {  -1.903   1.517   2.240} {  -2.071   1.659   2.127} radius 0.03 filled yes resolution 20
draw cone {  -2.071   1.659   2.127} {  -2.162   1.735   2.065} radius 0.06 resolution 20
draw color  428
draw cylinder {  -2.193   1.056   2.240} {  -2.361   1.215   2.100} radius 0.03 filled yes resolution 20
draw cone {  -2.361   1.215   2.100} {  -2.451   1.300   2.024} radius 0.06 resolution 20
draw color  410
draw cylinder {  -2.373   0.542   2.240} {  -2.530   0.672   2.080} radius 0.03 filled yes resolution 20
draw cone {  -2.530   0.672   2.080} {  -2.615   0.743   1.994} radius 0.06 resolution 20
draw color  359
draw cylinder {  -2.434   0.000   2.240} {  -2.573   0.064   2.068} radius 0.03 filled yes resolution 20
draw cone {  -2.573   0.064   2.068} {  -2.649   0.098   1.975} radius 0.06 resolution 20
draw color  328
draw cylinder {  -2.373  -0.542   2.240} {  -2.490  -0.571   2.065} radius 0.03 filled yes resolution 20
draw cone {  -2.490  -0.571   2.065} {  -2.554  -0.586   1.971} radius 0.06 resolution 20
draw color  365
draw cylinder {  -2.193  -1.056   2.240} {  -2.289  -1.185   2.071} radius 0.03 filled yes resolution 20
draw cone {  -2.289  -1.185   2.071} {  -2.340  -1.255   1.979} radius 0.06 resolution 20
draw color  443
draw cylinder {  -1.903  -1.517   2.240} {  -1.982  -1.735   2.085} radius 0.03 filled yes resolution 20
draw cone {  -1.982  -1.735   2.085} {  -2.024  -1.852   2.001} radius 0.06 resolution 20
draw color  504
draw cylinder {  -1.517  -1.903   2.240} {  -1.587  -2.179   2.107} radius 0.03 filled yes resolution 20
draw cone {  -1.587  -2.179   2.107} {  -1.625  -2.327   2.035} radius 0.06 resolution 20
draw color  512
draw cylinder {  -1.056  -2.193   2.240} {  -1.126  -2.486   2.136} radius 0.03 filled yes resolution 20
draw cone {  -1.126  -2.486   2.136} {  -1.164  -2.643   2.079} radius 0.06 resolution 20
draw color  455
draw cylinder {  -0.542  -2.373   2.240} {  -0.622  -2.638   2.169} radius 0.03 filled yes resolution 20
draw cone {  -0.622  -2.638   2.169} {  -0.665  -2.781   2.131} radius 0.06 resolution 20
draw color  347
draw cylinder {  -0.000  -2.434   2.240} {  -0.098  -2.632   2.207} radius 0.03 filled yes resolution 20
draw cone {  -0.098  -2.632   2.207} {  -0.152  -2.739   2.189} radius 0.06 resolution 20
draw color  236
draw cylinder {   0.542  -2.373   2.240} {   0.421  -2.478   2.246} radius 0.03 filled yes resolution 20
draw cone {   0.421  -2.478   2.246} {   0.356  -2.535   2.249} radius 0.06 resolution 20
draw color  217
draw cylinder {   1.056  -2.193   2.240} {   0.914  -2.198   2.285} radius 0.03 filled yes resolution 20
draw cone {   0.914  -2.198   2.285} {   0.837  -2.200   2.309} radius 0.06 resolution 20
draw color  301
draw cylinder {   1.517  -1.903   2.240} {   1.358  -1.820   2.322} radius 0.03 filled yes resolution 20
draw cone {   1.358  -1.820   2.322} {   1.272  -1.775   2.365} radius 0.06 resolution 20
draw color  389
draw cylinder {   1.903  -1.517   2.240} {   1.734  -1.376   2.354} radius 0.03 filled yes resolution 20
draw cone {   1.734  -1.376   2.354} {   1.644  -1.300   2.416} radius 0.06 resolution 20
draw color  428
draw cylinder {   2.193  -1.056   2.240} {   2.025  -0.897   2.381} radius 0.03 filled yes resolution 20
draw cone {   2.025  -0.897   2.381} {   1.935  -0.812   2.457} radius 0.06 resolution 20
draw color  410
draw cylinder {   2.373  -0.542   2.240} {   2.215  -0.411   2.401} radius 0.03 filled yes resolution 20
draw cone {   2.215  -0.411   2.401} {   2.130  -0.340   2.487} radius 0.06 resolution 20
draw color  359
draw cylinder {   2.434  -0.000   2.240} {   2.294   0.064   2.413} radius 0.03 filled yes resolution 20
draw cone {   2.294   0.064   2.413} {   2.219   0.098   2.506} radius 0.06 resolution 20
draw color  418
draw cylinder {   2.716   0.540   1.809} {   2.508   0.509   1.971} radius 0.03 filled yes resolution 20
draw cone {   2.508   0.509   1.971} {   2.396   0.492   2.057} radius 0.06 resolution 20
draw color  448
draw cylinder {   2.559   1.060   1.809} {   2.376   0.914   1.967} radius 0.03 filled yes resolution 20
draw cone {   2.376   0.914   1.967} {   2.277   0.836   2.053} radius 0.06 resolution 20
draw color  539
draw cylinder {   2.303   1.539   1.809} {   2.142   1.286   1.958} radius 0.03 filled yes resolution 20
draw cone {   2.142   1.286   1.958} {   2.055   1.151   2.038} radius 0.06 resolution 20
draw color  635
draw cylinder {   1.958   1.958   1.809} {   1.812   1.623   1.943} radius 0.03 filled yes resolution 20
draw cone {   1.812   1.623   1.943} {   1.733   1.443   2.015} radius 0.06 resolution 20
draw color  693
draw cylinder {   1.539   2.303   1.809} {   1.398   1.921   1.923} radius 0.03 filled yes resolution 20
draw cone {   1.398   1.921   1.923} {   1.322   1.715   1.984} radius 0.06 resolution 20
draw color  691
draw cylinder {   1.060   2.559   1.809} {   0.915   2.173   1.898} radius 0.03 filled yes resolution 20
draw cone {   0.915   2.173   1.898} {   0.837   1.966   1.946} radius 0.06 resolution 20
draw color  627
draw cylinder {   0.540   2.716   1.809} {   0.382   2.371   1.870} radius 0.03 filled yes resolution 20
draw cone {   0.382   2.371   1.870} {   0.296   2.186   1.903} radius 0.06 resolution 20
draw color  519
draw cylinder {   0.000   2.769   1.809} {  -0.179   2.503   1.840} radius 0.03 filled yes resolution 20
draw cone {  -0.179   2.503   1.840} {  -0.276   2.359   1.857} radius 0.06 resolution 20
draw color  411
draw cylinder {  -0.540   2.716   1.809} {  -0.745   2.554   1.809} radius 0.03 filled yes resolution 20
draw cone {  -0.745   2.554   1.809} {  -0.855   2.466   1.808} radius 0.06 resolution 20
draw color  369
draw cylinder {  -1.060   2.559   1.809} {  -1.289   2.510   1.777} radius 0.03 filled yes resolution 20
draw cone {  -1.289   2.510   1.777} {  -1.413   2.484   1.760} radius 0.06 resolution 20
draw color  419
draw cylinder {  -1.539   2.303   1.809} {  -1.790   2.361   1.747} radius 0.03 filled yes resolution 20
draw cone {  -1.790   2.361   1.747} {  -1.925   2.393   1.713} radius 0.06 resolution 20
draw color  504
draw cylinder {  -1.958   1.958   1.809} {  -2.224   2.100   1.719} radius 0.03 filled yes resolution 20
draw cone {  -2.224   2.100   1.719} {  -2.367   2.176   1.671} radius 0.06 resolution 20
draw color  566
draw cylinder {  -2.303   1.539   1.809} {  -2.574   1.727   1.695} radius 0.03 filled yes resolution 20
draw cone {  -2.574   1.727   1.695} {  -2.720   1.829   1.633} radius 0.06 resolution 20
draw color  576
draw cylinder {  -2.559   1.060   1.809} {  -2.826   1.252   1.675} radius 0.03 filled yes resolution 20
draw cone {  -2.826   1.252   1.675} {  -2.969   1.355   1.602} radius 0.06 resolution 20
draw color  533
draw cylinder {  -2.716   0.540   1.809} {  -2.970   0.692   1.660} radius 0.03 filled yes resolution 20
draw cone {  -2.970   0.692   1.660} {  -3.106   0.773   1.580} radius 0.06 resolution 20
draw color  463
draw cylinder {  -2.769   0.000   1.809} {  -3.002   0.073   1.651} radius 0.03 filled yes resolution 20
draw cone {  -3.002   0.073   1.651} {  -3.127   0.112   1.566} radius 0.06 resolution 20
draw color  418
draw cylinder {  -2.716  -0.540   1.809} {  -2.924  -0.572   1.648} radius 0.03 filled yes resolution 20
draw cone {  -2.924  -0.572   1.648} {  -3.036  -0.588   1.561} radius 0.06 resolution 20
draw color  448
draw cylinder {  -2.559  -1.060   1.809} {  -2.741  -1.205   1.651} radius 0.03 filled yes resolution 20
draw cone {  -2.741  -1.205   1.651} {  -2.840  -1.283   1.566} radius 0.06 resolution 20
draw color  539
draw cylinder {  -2.303  -1.539   1.809} {  -2.464  -1.791   1.661} radius 0.03 filled yes resolution 20
draw cone {  -2.464  -1.791   1.661} {  -2.551  -1.926   1.580} radius 0.06 resolution 20
draw color  635
draw cylinder {  -1.958  -1.958   1.809} {  -2.105  -2.293   1.676} radius 0.03 filled yes resolution 20
draw cone {  -2.105  -2.293   1.676} {  -2.184  -2.474   1.604} radius 0.06 resolution 20
draw color  693
draw cylinder {  -1.539  -2.303   1.809} {  -1.679  -2.685   1.696} radius 0.03 filled yes resolution 20
draw cone {  -1.679  -2.685   1.696} {  -1.755  -2.890   1.635} radius 0.06 resolution 20
draw color  691
draw cylinder {  -1.060  -2.559   1.809} {  -1.205  -2.944   1.720} radius 0.03 filled yes resolution 20
draw cone {  -1.205  -2.944   1.720} {  -1.283  -3.151   1.672} radius 0.06 resolution 20
draw color  627
draw cylinder {  -0.540  -2.716   1.809} {  -0.699  -3.061   1.748} radius 0.03 filled yes resolution 20
draw cone {  -0.699  -3.061   1.748} {  -0.784  -3.247   1.715} radius 0.06 resolution 20
draw color  519
draw cylinder {  -0.000  -2.769   1.809} {  -0.179  -3.036   1.779} radius 0.03 filled yes resolution 20
draw cone {  -0.179  -3.036   1.779} {  -0.276  -3.179   1.762} radius 0.06 resolution 20
draw color  411
draw cylinder {   0.540  -2.716   1.809} {   0.336  -2.878   1.810} radius 0.03 filled yes resolution 20
draw cone {   0.336  -2.878   1.810} {   0.226  -2.966   1.810} radius 0.06 resolution 20
draw color  369
draw cylinder {   1.060  -2.559   1.809} {   0.830  -2.607   1.841} radius 0.03 filled yes resolution 20
draw cone {   0.830  -2.607   1.841} {   0.707  -2.633   1.859} radius 0.06 resolution 20
draw color  419
draw cylinder {   1.539  -2.303   1.809} {   1.288  -2.244   1.872} radius 0.03 filled yes resolution 20
draw cone {   1.288  -2.244   1.872} {   1.153  -2.213   1.905} radius 0.06 resolution 20
draw color  504
draw cylinder {   1.958  -1.958   1.809} {   1.693  -1.817   1.900} radius 0.03 filled yes resolution 20
draw cone {   1.693  -1.817   1.900} {   1.550  -1.740   1.948} radius 0.06 resolution 20
draw color  566
draw cylinder {   2.303  -1.539   1.809} {   2.031  -1.350   1.924} radius 0.03 filled yes resolution 20
draw cone {   2.031  -1.350   1.924} {   1.885  -1.249   1.986} radius 0.06 resolution 20
draw color  576
draw cylinder {   2.559  -1.060   1.809} {   2.292  -0.868   1.944} radius 0.03 filled yes resolution 20
draw cone {   2.292  -0.868   1.944} {   2.148  -0.765   2.016} radius 0.06 resolution 20
draw color  533
draw cylinder {   2.716  -0.540   1.809} {   2.463  -0.389   1.959} radius 0.03 filled yes resolution 20
draw cone {   2.463  -0.389   1.959} {   2.326  -0.308   2.039} radius 0.06 resolution 20
draw color  463
draw cylinder {   2.769  -0.000   1.809} {   2.537   0.073   1.968} radius 0.03 filled yes resolution 20
draw cone {   2.537   0.073   1.968} {   2.411   0.112   2.053} radius 0.06 resolution 20
draw color  507
draw cylinder {   2.981   0.541   1.329} {   2.695   0.508   1.458} radius 0.03 filled yes resolution 20
draw cone {   2.695   0.508   1.458} {   2.541   0.491   1.528} radius 0.06 resolution 20
draw color  529
draw cylinder {   2.836   1.064   1.329} {   2.578   0.907   1.457} radius 0.03 filled yes resolution 20
draw cone {   2.578   0.907   1.457} {   2.439   0.823   1.525} radius 0.06 resolution 20
draw color  623
draw cylinder {   2.601   1.554   1.329} {   2.367   1.276   1.451} radius 0.03 filled yes resolution 20
draw cone {   2.367   1.276   1.451} {   2.241   1.127   1.516} radius 0.06 resolution 20
draw color  739
draw cylinder {   2.281   1.993   1.329} {   2.067   1.615   1.441} radius 0.03 filled yes resolution 20
draw cone {   2.067   1.615   1.441} {   1.951   1.412   1.501} radius 0.06 resolution 20
draw color  828
draw cylinder {   1.889   2.369   1.329} {   1.684   1.923   1.428} radius 0.03 filled yes resolution 20
draw cone {   1.684   1.923   1.428} {   1.574   1.683   1.481} radius 0.06 resolution 20
draw color  864
draw cylinder {   1.436   2.668   1.329} {   1.231   2.195   1.411} radius 0.03 filled yes resolution 20
draw cone {   1.231   2.195   1.411} {   1.122   1.941   1.455} radius 0.06 resolution 20
draw color  838
draw cylinder {   0.936   2.881   1.329} {   0.723   2.427   1.392} radius 0.03 filled yes resolution 20
draw cone {   0.723   2.427   1.392} {   0.608   2.182   1.426} radius 0.06 resolution 20
draw color  756
draw cylinder {   0.407   3.002   1.329} {   0.175   2.608   1.370} radius 0.03 filled yes resolution 20
draw cone {   0.175   2.608   1.370} {   0.050   2.396   1.393} radius 0.06 resolution 20
draw color  643
draw cylinder {  -0.136   3.026   1.329} {  -0.392   2.727   1.348} radius 0.03 filled yes resolution 20
draw cone {  -0.392   2.727   1.348} {  -0.530   2.566   1.358} radius 0.06 resolution 20
draw color  543
draw cylinder {  -0.674   2.953   1.329} {  -0.958   2.772   1.325} radius 0.03 filled yes resolution 20
draw cone {  -0.958   2.772   1.325} {  -1.110   2.674   1.322} radius 0.06 resolution 20
draw color  509
draw cylinder {  -1.191   2.786   1.329} {  -1.501   2.729   1.302} radius 0.03 filled yes resolution 20
draw cone {  -1.501   2.729   1.302} {  -1.669   2.699   1.287} radius 0.06 resolution 20
draw color  555
draw cylinder {  -1.669   2.528   1.329} {  -2.003   2.588   1.279} radius 0.03 filled yes resolution 20
draw cone {  -2.003   2.588   1.279} {  -2.183   2.621   1.253} radius 0.06 resolution 20
draw color  635
draw cylinder {  -2.094   2.190   1.329} {  -2.445   2.343   1.259} radius 0.03 filled yes resolution 20
draw cone {  -2.445   2.343   1.259} {  -2.634   2.426   1.221} radius 0.06 resolution 20
draw color  699
draw cylinder {  -2.451   1.781   1.329} {  -2.810   1.992   1.240} radius 0.03 filled yes resolution 20
draw cone {  -2.810   1.992   1.240} {  -3.003   2.106   1.193} radius 0.06 resolution 20
draw color  716
draw cylinder {  -2.729   1.314   1.329} {  -3.086   1.542   1.225} radius 0.03 filled yes resolution 20
draw cone {  -3.086   1.542   1.225} {  -3.278   1.664   1.169} radius 0.06 resolution 20
draw color  679
draw cylinder {  -2.920   0.806   1.329} {  -3.265   1.004   1.213} radius 0.03 filled yes resolution 20
draw cone {  -3.265   1.004   1.213} {  -3.451   1.111   1.150} radius 0.06 resolution 20
draw color  603
draw cylinder {  -3.017   0.272   1.329} {  -3.342   0.400   1.204} radius 0.03 filled yes resolution 20
draw cone {  -3.342   0.400   1.204} {  -3.518   0.469   1.137} radius 0.06 resolution 20
draw color  527
draw cylinder {  -3.017  -0.272   1.329} {  -3.317  -0.245   1.200} radius 0.03 filled yes resolution 20
draw cone {  -3.317  -0.245   1.200} {  -3.478  -0.231   1.131} radius 0.06 resolution 20
draw color  507
draw cylinder {  -2.920  -0.806   1.329} {  -3.192  -0.900   1.200} radius 0.03 filled yes resolution 20
draw cone {  -3.192  -0.900   1.200} {  -3.339  -0.951   1.130} radius 0.06 resolution 20
draw color  570
draw cylinder {  -2.729  -1.314   1.329} {  -2.975  -1.533   1.203} radius 0.03 filled yes resolution 20
draw cone {  -2.975  -1.533   1.203} {  -3.107  -1.651   1.136} radius 0.06 resolution 20
draw color  682
draw cylinder {  -2.451  -1.781   1.329} {  -2.674  -2.112   1.211} radius 0.03 filled yes resolution 20
draw cone {  -2.674  -2.112   1.211} {  -2.794  -2.290   1.148} radius 0.06 resolution 20
draw color  789
draw cylinder {  -2.094  -2.190   1.329} {  -2.302  -2.606   1.223} radius 0.03 filled yes resolution 20
draw cone {  -2.302  -2.606   1.223} {  -2.414  -2.830   1.166} radius 0.06 resolution 20
draw color  854
draw cylinder {  -1.669  -2.528   1.329} {  -1.872  -2.993   1.238} radius 0.03 filled yes resolution 20
draw cone {  -1.872  -2.993   1.238} {  -1.981  -3.243   1.189} radius 0.06 resolution 20
draw color  859
draw cylinder {  -1.191  -2.786   1.329} {  -1.398  -3.255   1.256} radius 0.03 filled yes resolution 20
draw cone {  -1.398  -3.255   1.256} {  -1.510  -3.507   1.217} radius 0.06 resolution 20
draw color  803
draw cylinder {  -0.674  -2.953   1.329} {  -0.896  -3.383   1.276} radius 0.03 filled yes resolution 20
draw cone {  -0.896  -3.383   1.276} {  -1.015  -3.614   1.248} radius 0.06 resolution 20
draw color  701
draw cylinder {  -0.136  -3.026   1.329} {  -0.379  -3.377   1.298} radius 0.03 filled yes resolution 20
draw cone {  -0.379  -3.377   1.298} {  -0.510  -3.565   1.282} radius 0.06 resolution 20
draw color  588
draw cylinder {   0.407  -3.002   1.329} {   0.137  -3.244   1.321} radius 0.03 filled yes resolution 20
draw cone {   0.137  -3.244   1.321} {  -0.008  -3.375   1.317} radius 0.06 resolution 20
draw color  515
draw cylinder {   0.936  -2.881   1.329} {   0.639  -3.000   1.345} radius 0.03 filled yes resolution 20
draw cone {   0.639  -3.000   1.345} {   0.479  -3.064   1.353} radius 0.06 resolution 20
draw color  524
draw cylinder {   1.436  -2.668   1.329} {   1.112  -2.664   1.367} radius 0.03 filled yes resolution 20
draw cone {   1.112  -2.664   1.367} {   0.938  -2.662   1.388} radius 0.06 resolution 20
draw color  594
draw cylinder {   1.889  -2.369   1.329} {   1.545  -2.258   1.389} radius 0.03 filled yes resolution 20
draw cone {   1.545  -2.258   1.389} {   1.360  -2.198   1.421} radius 0.06 resolution 20
draw color  672
draw cylinder {   2.281  -1.993   1.329} {   1.925  -1.806   1.408} radius 0.03 filled yes resolution 20
draw cone {   1.925  -1.806   1.408} {   1.734  -1.705   1.451} radius 0.06 resolution 20
draw color  714
draw cylinder {   2.601  -1.554   1.329} {   2.242  -1.329   1.425} radius 0.03 filled yes resolution 20
draw cone {   2.242  -1.329   1.425} {   2.048  -1.208   1.477} radius 0.06 resolution 20
draw color  704
draw cylinder {   2.836  -1.064   1.329} {   2.484  -0.846   1.439} radius 0.03 filled yes resolution 20
draw cone {   2.484  -0.846   1.439} {   2.295  -0.729   1.499} radius 0.06 resolution 20
draw color  644
draw cylinder {   2.981  -0.541   1.329} {   2.645  -0.373   1.450} radius 0.03 filled yes resolution 20
draw cone {   2.645  -0.373   1.450} {   2.464  -0.282   1.515} radius 0.06 resolution 20
draw color  562
draw cylinder {   3.029  -0.000   1.329} {   2.716   0.081   1.456} radius 0.03 filled yes resolution 20
draw cone {   2.716   0.081   1.456} {   2.548   0.124   1.525} radius 0.06 resolution 20
draw color  575
draw cylinder {   3.161   0.542   0.812} {   2.817   0.509   0.896} radius 0.03 filled yes resolution 20
draw cone {   2.817   0.509   0.896} {   2.633   0.491   0.941} radius 0.06 resolution 20
draw color  591
draw cylinder {   3.024   1.068   0.812} {   2.710   0.903   0.895} radius 0.03 filled yes resolution 20
draw cone {   2.710   0.903   0.895} {   2.541   0.814   0.940} radius 0.06 resolution 20
draw color  686
draw cylinder {   2.800   1.564   0.812} {   2.513   1.270   0.892} radius 0.03 filled yes resolution 20
draw cone {   2.513   1.270   0.892} {   2.358   1.111   0.935} radius 0.06 resolution 20
draw color  811
draw cylinder {   2.495   2.015   0.812} {   2.229   1.609   0.886} radius 0.03 filled yes resolution 20
draw cone {   2.229   1.609   0.886} {   2.086   1.390   0.926} radius 0.06 resolution 20
draw color  920
draw cylinder {   2.119   2.407   0.812} {   1.866   1.920   0.878} radius 0.03 filled yes resolution 20
draw cone {   1.866   1.920   0.878} {   1.730   1.658   0.914} radius 0.06 resolution 20
draw color  980
draw cylinder {   1.681   2.731   0.812} {   1.432   2.202   0.869} radius 0.03 filled yes resolution 20
draw cone {   1.432   2.202   0.869} {   1.298   1.917   0.899} radius 0.06 resolution 20
draw color  979
draw cylinder {   1.196   2.976   0.812} {   0.940   2.449   0.858} radius 0.03 filled yes resolution 20
draw cone {   0.940   2.449   0.858} {   0.803   2.165   0.882} radius 0.06 resolution 20
draw color  919
draw cylinder {   0.676   3.135   0.812} {   0.405   2.655   0.845} radius 0.03 filled yes resolution 20
draw cone {   0.405   2.655   0.845} {   0.259   2.396   0.863} radius 0.06 resolution 20
draw color  815
draw cylinder {   0.136   3.204   0.812} {  -0.158   2.809   0.832} radius 0.03 filled yes resolution 20
draw cone {  -0.158   2.809   0.832} {  -0.316   2.596   0.842} radius 0.06 resolution 20
draw color  701
draw cylinder {  -0.407   3.181   0.812} {  -0.729   2.900   0.818} radius 0.03 filled yes resolution 20
draw cone {  -0.729   2.900   0.818} {  -0.903   2.749   0.820} radius 0.06 resolution 20
draw color  623
draw cylinder {  -0.939   3.066   0.812} {  -1.291   2.916   0.803} radius 0.03 filled yes resolution 20
draw cone {  -1.291   2.916   0.803} {  -1.480   2.834   0.799} radius 0.06 resolution 20
draw color  620
draw cylinder {  -1.444   2.863   0.812} {  -1.823   2.844   0.789} radius 0.03 filled yes resolution 20
draw cone {  -1.823   2.844   0.789} {  -2.027   2.834   0.777} radius 0.06 resolution 20
draw color  681
draw cylinder {  -1.907   2.578   0.812} {  -2.309   2.676   0.776} radius 0.03 filled yes resolution 20
draw cone {  -2.309   2.676   0.776} {  -2.526   2.729   0.757} radius 0.06 resolution 20
draw color  759
draw cylinder {  -2.315   2.219   0.812} {  -2.733   2.407   0.764} radius 0.03 filled yes resolution 20
draw cone {  -2.733   2.407   0.764} {  -2.958   2.508   0.738} radius 0.06 resolution 20
draw color  812
draw cylinder {  -2.657   1.796   0.812} {  -3.081   2.036   0.753} radius 0.03 filled yes resolution 20
draw cone {  -3.081   2.036   0.753} {  -3.309   2.165   0.721} radius 0.06 resolution 20
draw color  816
draw cylinder {  -2.922   1.321   0.812} {  -3.342   1.570   0.744} radius 0.03 filled yes resolution 20
draw cone {  -3.342   1.570   0.744} {  -3.568   1.704   0.707} radius 0.06 resolution 20
draw color  767
draw cylinder {  -3.103   0.808   0.812} {  -3.510   1.021   0.736} radius 0.03 filled yes resolution 20
draw cone {  -3.510   1.021   0.736} {  -3.729   1.136   0.696} radius 0.06 resolution 20
draw color  682
draw cylinder {  -3.195   0.272   0.812} {  -3.580   0.409   0.731} radius 0.03 filled yes resolution 20
draw cone {  -3.580   0.409   0.731} {  -3.788   0.483   0.688} radius 0.06 resolution 20
draw color  599
draw cylinder {  -3.195  -0.272   0.812} {  -3.553  -0.243   0.729} radius 0.03 filled yes resolution 20
draw cone {  -3.553  -0.243   0.729} {  -3.746  -0.227   0.684} radius 0.06 resolution 20
draw color  572
draw cylinder {  -3.103  -0.808   0.812} {  -3.432  -0.907   0.728} radius 0.03 filled yes resolution 20
draw cone {  -3.432  -0.907   0.728} {  -3.609  -0.960   0.683} radius 0.06 resolution 20
draw color  631
draw cylinder {  -2.922  -1.321   0.812} {  -3.222  -1.552   0.730} radius 0.03 filled yes resolution 20
draw cone {  -3.222  -1.552   0.730} {  -3.384  -1.676   0.687} radius 0.06 resolution 20
draw color  748
draw cylinder {  -2.657  -1.796   0.812} {  -2.932  -2.149   0.735} radius 0.03 filled yes resolution 20
draw cone {  -2.932  -2.149   0.735} {  -3.081  -2.339   0.693} radius 0.06 resolution 20
draw color  870
draw cylinder {  -2.315  -2.219   0.812} {  -2.573  -2.670   0.742} radius 0.03 filled yes resolution 20
draw cone {  -2.573  -2.670   0.742} {  -2.712  -2.913   0.704} radius 0.06 resolution 20
draw color  957
draw cylinder {  -1.907  -2.578   0.812} {  -2.156  -3.092   0.750} radius 0.03 filled yes resolution 20
draw cone {  -2.156  -3.092   0.750} {  -2.291  -3.368   0.717} radius 0.06 resolution 20
draw color  988
draw cylinder {  -1.444  -2.863   0.812} {  -1.695  -3.397   0.761} radius 0.03 filled yes resolution 20
draw cone {  -1.695  -3.397   0.761} {  -1.830  -3.684   0.733} radius 0.06 resolution 20
draw color  956
draw cylinder {  -0.939  -3.066   0.812} {  -1.201  -3.575   0.773} radius 0.03 filled yes resolution 20
draw cone {  -1.201  -3.575   0.773} {  -1.342  -3.849   0.751} radius 0.06 resolution 20
draw color  871
draw cylinder {  -0.407  -3.181   0.812} {  -0.689  -3.623   0.786} radius 0.03 filled yes resolution 20
draw cone {  -0.689  -3.623   0.786} {  -0.841  -3.861   0.772} radius 0.06 resolution 20
draw color  757
draw cylinder {   0.136  -3.204   0.812} {  -0.171  -3.545   0.800} radius 0.03 filled yes resolution 20
draw cone {  -0.171  -3.545   0.800} {  -0.337  -3.728   0.793} radius 0.06 resolution 20
draw color  654
draw cylinder {   0.676  -3.135   0.812} {   0.339  -3.352   0.814} radius 0.03 filled yes resolution 20
draw cone {   0.339  -3.352   0.814} {   0.158  -3.468   0.815} radius 0.06 resolution 20
draw color  611
draw cylinder {   1.196  -2.976   0.812} {   0.830  -3.060   0.828} radius 0.03 filled yes resolution 20
draw cone {   0.830  -3.060   0.828} {   0.633  -3.105   0.836} radius 0.06 resolution 20
draw color  645
draw cylinder {   1.681  -2.731   0.812} {   1.290  -2.689   0.842} radius 0.03 filled yes resolution 20
draw cone {   1.290  -2.689   0.842} {   1.079  -2.666   0.857} radius 0.06 resolution 20
draw color  721
draw cylinder {   2.119  -2.407   0.812} {   1.708  -2.260   0.854} radius 0.03 filled yes resolution 20
draw cone {   1.708  -2.260   0.854} {   1.486  -2.181   0.877} radius 0.06 resolution 20
draw color  790
draw cylinder {   2.495  -2.015   0.812} {   2.073  -1.795   0.866} radius 0.03 filled yes resolution 20
draw cone {   2.073  -1.795   0.866} {   1.846  -1.677   0.895} radius 0.06 resolution 20
draw color  820
draw cylinder {   2.800  -1.564   0.812} {   2.377  -1.314   0.876} radius 0.03 filled yes resolution 20
draw cone {   2.377  -1.314   0.876} {   2.149  -1.179   0.911} radius 0.06 resolution 20
draw color  797
draw cylinder {   3.024  -1.068   0.812} {   2.609  -0.832   0.884} radius 0.03 filled yes resolution 20
draw cone {   2.609  -0.832   0.884} {   2.386  -0.704   0.923} radius 0.06 resolution 20
draw color  728
draw cylinder {   3.161  -0.542   0.812} {   2.764  -0.362   0.890} radius 0.03 filled yes resolution 20
draw cone {   2.764  -0.362   0.890} {   2.551  -0.265   0.933} radius 0.06 resolution 20
draw color  637
draw cylinder {   3.207  -0.000   0.812} {   2.835   0.086   0.894} radius 0.03 filled yes resolution 20
draw cone {   2.835   0.086   0.894} {   2.635   0.133   0.939} radius 0.06 resolution 20
draw color  612
draw cylinder {   3.252   0.543   0.273} {   2.878   0.509   0.302} radius 0.03 filled yes resolution 20
draw cone {   2.878   0.509   0.302} {   2.677   0.491   0.318} radius 0.06 resolution 20
draw color  625
draw cylinder {   3.118   1.070   0.273} {   2.775   0.902   0.302} radius 0.03 filled yes resolution 20
draw cone {   2.775   0.902   0.302} {   2.590   0.811   0.317} radius 0.06 resolution 20
draw color  720
draw cylinder {   2.899   1.569   0.273} {   2.584   1.267   0.301} radius 0.03 filled yes resolution 20
draw cone {   2.584   1.267   0.301} {   2.414   1.104   0.316} radius 0.06 resolution 20
draw color  849
draw cylinder {   2.602   2.025   0.273} {   2.309   1.605   0.299} radius 0.03 filled yes resolution 20
draw cone {   2.309   1.605   0.299} {   2.151   1.379   0.313} radius 0.06 resolution 20
draw color  966
draw cylinder {   2.233   2.426   0.273} {   1.955   1.918   0.297} radius 0.03 filled yes resolution 20
draw cone {   1.955   1.918   0.297} {   1.805   1.644   0.309} radius 0.06 resolution 20
draw color 1038
draw cylinder {   1.803   2.760   0.273} {   1.530   2.203   0.293} radius 0.03 filled yes resolution 20
draw cone {   1.530   2.203   0.293} {   1.383   1.903   0.304} radius 0.06 resolution 20
draw color 1050
draw cylinder {   1.324   3.019   0.273} {   1.047   2.456   0.290} radius 0.03 filled yes resolution 20
draw cone {   1.047   2.456   0.290} {   0.897   2.154   0.299} radius 0.06 resolution 20
draw color 1001
draw cylinder {   0.809   3.196   0.273} {   0.517   2.672   0.286} radius 0.03 filled yes resolution 20
draw cone {   0.517   2.672   0.286} {   0.360   2.391   0.292} radius 0.06 resolution 20
draw color  904
draw cylinder {   0.272   3.285   0.273} {  -0.042   2.842   0.281} radius 0.03 filled yes resolution 20
draw cone {  -0.042   2.842   0.281} {  -0.211   2.603   0.285} radius 0.06 resolution 20
draw color  788
draw cylinder {  -0.272   3.285   0.273} {  -0.614   2.953   0.276} radius 0.03 filled yes resolution 20
draw cone {  -0.614   2.953   0.276} {  -0.799   2.774   0.278} radius 0.06 resolution 20
draw color  695
draw cylinder {  -0.809   3.196   0.273} {  -1.182   2.994   0.272} radius 0.03 filled yes resolution 20
draw cone {  -1.182   2.994   0.272} {  -1.382   2.885   0.271} radius 0.06 resolution 20
draw color  666
draw cylinder {  -1.324   3.019   0.273} {  -1.726   2.954   0.267} radius 0.03 filled yes resolution 20
draw cone {  -1.726   2.954   0.267} {  -1.943   2.918   0.263} radius 0.06 resolution 20
draw color  710
draw cylinder {  -1.803   2.760   0.273} {  -2.231   2.822   0.262} radius 0.03 filled yes resolution 20
draw cone {  -2.231   2.822   0.262} {  -2.461   2.855   0.256} radius 0.06 resolution 20
draw color  787
draw cylinder {  -2.233   2.426   0.273} {  -2.679   2.591   0.258} radius 0.03 filled yes resolution 20
draw cone {  -2.679   2.591   0.258} {  -2.920   2.681   0.250} radius 0.06 resolution 20
draw color  852
draw cylinder {  -2.602   2.025   0.273} {  -3.058   2.260   0.254} radius 0.03 filled yes resolution 20
draw cone {  -3.058   2.260   0.254} {  -3.304   2.387   0.244} radius 0.06 resolution 20
draw color  876
draw cylinder {  -2.899   1.569   0.273} {  -3.356   1.832   0.251} radius 0.03 filled yes resolution 20
draw cone {  -3.356   1.832   0.251} {  -3.602   1.974   0.239} radius 0.06 resolution 20
draw color  847
draw cylinder {  -3.118   1.070   0.273} {  -3.565   1.317   0.248} radius 0.03 filled yes resolution 20
draw cone {  -3.565   1.317   0.248} {  -3.806   1.449   0.235} radius 0.06 resolution 20
draw color  772
draw cylinder {  -3.252   0.543   0.273} {  -3.680   0.729   0.246} radius 0.03 filled yes resolution 20
draw cone {  -3.680   0.729   0.246} {  -3.911   0.829   0.231} radius 0.06 resolution 20
draw color  678
draw cylinder {  -3.297   0.000   0.273} {  -3.700   0.089   0.245} radius 0.03 filled yes resolution 20
draw cone {  -3.700   0.089   0.245} {  -3.917   0.137   0.229} radius 0.06 resolution 20
draw color  612
draw cylinder {  -3.252  -0.543   0.273} {  -3.625  -0.576   0.244} radius 0.03 filled yes resolution 20
draw cone {  -3.625  -0.576   0.244} {  -3.827  -0.594   0.229} radius 0.06 resolution 20
draw color  625
draw cylinder {  -3.118  -1.070   0.273} {  -3.461  -1.239   0.244} radius 0.03 filled yes resolution 20
draw cone {  -3.461  -1.239   0.244} {  -3.646  -1.330   0.229} radius 0.06 resolution 20
draw color  720
draw cylinder {  -2.899  -1.569   0.273} {  -3.215  -1.872   0.246} radius 0.03 filled yes resolution 20
draw cone {  -3.215  -1.872   0.246} {  -3.385  -2.034   0.231} radius 0.06 resolution 20
draw color  849
draw cylinder {  -2.602  -2.025   0.273} {  -2.894  -2.445   0.247} radius 0.03 filled yes resolution 20
draw cone {  -2.894  -2.445   0.247} {  -3.052  -2.671   0.233} radius 0.06 resolution 20
draw color  966
draw cylinder {  -2.233  -2.426   0.273} {  -2.511  -2.933   0.250} radius 0.03 filled yes resolution 20
draw cone {  -2.511  -2.933   0.250} {  -2.661  -3.207   0.237} radius 0.06 resolution 20
draw color 1038
draw cylinder {  -1.803  -2.760   0.273} {  -2.076  -3.317   0.253} radius 0.03 filled yes resolution 20
draw cone {  -2.076  -3.317   0.253} {  -2.223  -3.617   0.242} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -1.324  -3.019   0.273} {  -1.602  -3.582   0.257} radius 0.03 filled yes resolution 20
draw cone {  -1.602  -3.582   0.257} {  -1.752  -3.885   0.248} radius 0.06 resolution 20
draw color 1001
draw cylinder {  -0.809  -3.196   0.273} {  -1.101  -3.719   0.261} radius 0.03 filled yes resolution 20
draw cone {  -1.101  -3.719   0.261} {  -1.259  -4.001   0.254} radius 0.06 resolution 20
draw color  904
draw cylinder {  -0.272  -3.285   0.273} {  -0.587  -3.729   0.265} radius 0.03 filled yes resolution 20
draw cone {  -0.587  -3.729   0.265} {  -0.756  -3.968   0.261} radius 0.06 resolution 20
draw color  788
draw cylinder {   0.272  -3.285   0.273} {  -0.070  -3.618   0.270} radius 0.03 filled yes resolution 20
draw cone {  -0.070  -3.618   0.270} {  -0.254  -3.797   0.268} radius 0.06 resolution 20
draw color  695
draw cylinder {   0.809  -3.196   0.273} {   0.437  -3.398   0.275} radius 0.03 filled yes resolution 20
draw cone {   0.437  -3.398   0.275} {   0.236  -3.506   0.276} radius 0.06 resolution 20
draw color  666
draw cylinder {   1.324  -3.019   0.273} {   0.922  -3.084   0.280} radius 0.03 filled yes resolution 20
draw cone {   0.922  -3.084   0.280} {   0.706  -3.120   0.283} radius 0.06 resolution 20
draw color  710
draw cylinder {   1.803  -2.760   0.273} {   1.376  -2.698   0.284} radius 0.03 filled yes resolution 20
draw cone {   1.376  -2.698   0.284} {   1.145  -2.665   0.290} radius 0.06 resolution 20
draw color  787
draw cylinder {   2.233  -2.426   0.273} {   1.786  -2.260   0.288} radius 0.03 filled yes resolution 20
draw cone {   1.786  -2.260   0.288} {   1.546  -2.170   0.297} radius 0.06 resolution 20
draw color  852
draw cylinder {   2.602  -2.025   0.273} {   2.145  -1.789   0.292} radius 0.03 filled yes resolution 20
draw cone {   2.145  -1.789   0.292} {   1.899  -1.663   0.302} radius 0.06 resolution 20
draw color  876
draw cylinder {   2.899  -1.569   0.273} {   2.443  -1.306   0.296} radius 0.03 filled yes resolution 20
draw cone {   2.443  -1.306   0.296} {   2.197  -1.164   0.308} radius 0.06 resolution 20
draw color  847
draw cylinder {   3.118  -1.070   0.273} {   2.671  -0.824   0.298} radius 0.03 filled yes resolution 20
draw cone {   2.671  -0.824   0.298} {   2.430  -0.692   0.312} radius 0.06 resolution 20
draw color  772
draw cylinder {   3.252  -0.543   0.273} {   2.823  -0.357   0.300} radius 0.03 filled yes resolution 20
draw cone {   2.823  -0.357   0.300} {   2.593  -0.256   0.315} radius 0.06 resolution 20
draw color  678
draw cylinder {   3.297  -0.000   0.273} {   2.894   0.089   0.302} radius 0.03 filled yes resolution 20
draw cone {   2.894   0.089   0.302} {   2.677   0.137   0.317} radius 0.06 resolution 20
draw color  612
draw cylinder {   3.252   0.543  -0.273} {   2.878   0.509  -0.302} radius 0.03 filled yes resolution 20
draw cone {   2.878   0.509  -0.302} {   2.677   0.491  -0.318} radius 0.06 resolution 20
draw color  625
draw cylinder {   3.118   1.070  -0.273} {   2.775   0.902  -0.302} radius 0.03 filled yes resolution 20
draw cone {   2.775   0.902  -0.302} {   2.590   0.811  -0.317} radius 0.06 resolution 20
draw color  720
draw cylinder {   2.899   1.569  -0.273} {   2.584   1.267  -0.301} radius 0.03 filled yes resolution 20
draw cone {   2.584   1.267  -0.301} {   2.414   1.104  -0.316} radius 0.06 resolution 20
draw color  849
draw cylinder {   2.602   2.025  -0.273} {   2.309   1.605  -0.299} radius 0.03 filled yes resolution 20
draw cone {   2.309   1.605  -0.299} {   2.151   1.379  -0.313} radius 0.06 resolution 20
draw color  966
draw cylinder {   2.233   2.426  -0.273} {   1.955   1.918  -0.297} radius 0.03 filled yes resolution 20
draw cone {   1.955   1.918  -0.297} {   1.805   1.644  -0.309} radius 0.06 resolution 20
draw color 1038
draw cylinder {   1.803   2.760  -0.273} {   1.530   2.203  -0.293} radius 0.03 filled yes resolution 20
draw cone {   1.530   2.203  -0.293} {   1.383   1.903  -0.304} radius 0.06 resolution 20
draw color 1050
draw cylinder {   1.324   3.019  -0.273} {   1.047   2.456  -0.290} radius 0.03 filled yes resolution 20
draw cone {   1.047   2.456  -0.290} {   0.897   2.154  -0.299} radius 0.06 resolution 20
draw color 1001
draw cylinder {   0.809   3.196  -0.273} {   0.517   2.672  -0.286} radius 0.03 filled yes resolution 20
draw cone {   0.517   2.672  -0.286} {   0.360   2.391  -0.292} radius 0.06 resolution 20
draw color  904
draw cylinder {   0.272   3.285  -0.273} {  -0.042   2.842  -0.281} radius 0.03 filled yes resolution 20
draw cone {  -0.042   2.842  -0.281} {  -0.211   2.603  -0.285} radius 0.06 resolution 20
draw color  788
draw cylinder {  -0.272   3.285  -0.273} {  -0.614   2.953  -0.276} radius 0.03 filled yes resolution 20
draw cone {  -0.614   2.953  -0.276} {  -0.799   2.774  -0.278} radius 0.06 resolution 20
draw color  695
draw cylinder {  -0.809   3.196  -0.273} {  -1.182   2.994  -0.272} radius 0.03 filled yes resolution 20
draw cone {  -1.182   2.994  -0.272} {  -1.382   2.885  -0.271} radius 0.06 resolution 20
draw color  666
draw cylinder {  -1.324   3.019  -0.273} {  -1.726   2.954  -0.267} radius 0.03 filled yes resolution 20
draw cone {  -1.726   2.954  -0.267} {  -1.943   2.918  -0.263} radius 0.06 resolution 20
draw color  710
draw cylinder {  -1.803   2.760  -0.273} {  -2.231   2.822  -0.262} radius 0.03 filled yes resolution 20
draw cone {  -2.231   2.822  -0.262} {  -2.461   2.855  -0.256} radius 0.06 resolution 20
draw color  787
draw cylinder {  -2.233   2.426  -0.273} {  -2.679   2.591  -0.258} radius 0.03 filled yes resolution 20
draw cone {  -2.679   2.591  -0.258} {  -2.920   2.681  -0.250} radius 0.06 resolution 20
draw color  852
draw cylinder {  -2.602   2.025  -0.273} {  -3.058   2.260  -0.254} radius 0.03 filled yes resolution 20
draw cone {  -3.058   2.260  -0.254} {  -3.304   2.387  -0.244} radius 0.06 resolution 20
draw color  876
draw cylinder {  -2.899   1.569  -0.273} {  -3.356   1.832  -0.251} radius 0.03 filled yes resolution 20
draw cone {  -3.356   1.832  -0.251} {  -3.602   1.974  -0.239} radius 0.06 resolution 20
draw color  847
draw cylinder {  -3.118   1.070  -0.273} {  -3.565   1.317  -0.248} radius 0.03 filled yes resolution 20
draw cone {  -3.565   1.317  -0.248} {  -3.806   1.449  -0.235} radius 0.06 resolution 20
draw color  772
draw cylinder {  -3.252   0.543  -0.273} {  -3.680   0.729  -0.246} radius 0.03 filled yes resolution 20
draw cone {  -3.680   0.729  -0.246} {  -3.911   0.829  -0.231} radius 0.06 resolution 20
draw color  678
draw cylinder {  -3.297   0.000  -0.273} {  -3.700   0.089  -0.245} radius 0.03 filled yes resolution 20
draw cone {  -3.700   0.089  -0.245} {  -3.917   0.137  -0.229} radius 0.06 resolution 20
draw color  612
draw cylinder {  -3.252  -0.543  -0.273} {  -3.625  -0.576  -0.244} radius 0.03 filled yes resolution 20
draw cone {  -3.625  -0.576  -0.244} {  -3.827  -0.594  -0.229} radius 0.06 resolution 20
draw color  625
draw cylinder {  -3.118  -1.070  -0.273} {  -3.461  -1.239  -0.244} radius 0.03 filled yes resolution 20
draw cone {  -3.461  -1.239  -0.244} {  -3.646  -1.330  -0.229} radius 0.06 resolution 20
draw color  720
draw cylinder {  -2.899  -1.569  -0.273} {  -3.215  -1.872  -0.246} radius 0.03 filled yes resolution 20
draw cone {  -3.215  -1.872  -0.246} {  -3.385  -2.034  -0.231} radius 0.06 resolution 20
draw color  849
draw cylinder {  -2.602  -2.025  -0.273} {  -2.894  -2.445  -0.247} radius 0.03 filled yes resolution 20
draw cone {  -2.894  -2.445  -0.247} {  -3.052  -2.671  -0.233} radius 0.06 resolution 20
draw color  966
draw cylinder {  -2.233  -2.426  -0.273} {  -2.511  -2.933  -0.250} radius 0.03 filled yes resolution 20
draw cone {  -2.511  -2.933  -0.250} {  -2.661  -3.207  -0.237} radius 0.06 resolution 20
draw color 1038
draw cylinder {  -1.803  -2.760  -0.273} {  -2.076  -3.317  -0.253} radius 0.03 filled yes resolution 20
draw cone {  -2.076  -3.317  -0.253} {  -2.223  -3.617  -0.242} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -1.324  -3.019  -0.273} {  -1.602  -3.582  -0.257} radius 0.03 filled yes resolution 20
draw cone {  -1.602  -3.582  -0.257} {  -1.752  -3.885  -0.248} radius 0.06 resolution 20
draw color 1001
draw cylinder {  -0.809  -3.196  -0.273} {  -1.101  -3.719  -0.261} radius 0.03 filled yes resolution 20
draw cone {  -1.101  -3.719  -0.261} {  -1.259  -4.001  -0.254} radius 0.06 resolution 20
draw color  904
draw cylinder {  -0.272  -3.285  -0.273} {  -0.587  -3.729  -0.265} radius 0.03 filled yes resolution 20
draw cone {  -0.587  -3.729  -0.265} {  -0.756  -3.968  -0.261} radius 0.06 resolution 20
draw color  788
draw cylinder {   0.272  -3.285  -0.273} {  -0.070  -3.618  -0.270} radius 0.03 filled yes resolution 20
draw cone {  -0.070  -3.618  -0.270} {  -0.254  -3.797  -0.268} radius 0.06 resolution 20
draw color  695
draw cylinder {   0.809  -3.196  -0.273} {   0.437  -3.398  -0.275} radius 0.03 filled yes resolution 20
draw cone {   0.437  -3.398  -0.275} {   0.236  -3.506  -0.276} radius 0.06 resolution 20
draw color  666
draw cylinder {   1.324  -3.019  -0.273} {   0.922  -3.084  -0.280} radius 0.03 filled yes resolution 20
draw cone {   0.922  -3.084  -0.280} {   0.706  -3.120  -0.283} radius 0.06 resolution 20
draw color  710
draw cylinder {   1.803  -2.760  -0.273} {   1.376  -2.698  -0.284} radius 0.03 filled yes resolution 20
draw cone {   1.376  -2.698  -0.284} {   1.145  -2.665  -0.290} radius 0.06 resolution 20
draw color  787
draw cylinder {   2.233  -2.426  -0.273} {   1.786  -2.260  -0.288} radius 0.03 filled yes resolution 20
draw cone {   1.786  -2.260  -0.288} {   1.546  -2.170  -0.297} radius 0.06 resolution 20
draw color  852
draw cylinder {   2.602  -2.025  -0.273} {   2.145  -1.789  -0.292} radius 0.03 filled yes resolution 20
draw cone {   2.145  -1.789  -0.292} {   1.899  -1.663  -0.302} radius 0.06 resolution 20
draw color  876
draw cylinder {   2.899  -1.569  -0.273} {   2.443  -1.306  -0.296} radius 0.03 filled yes resolution 20
draw cone {   2.443  -1.306  -0.296} {   2.197  -1.164  -0.308} radius 0.06 resolution 20
draw color  847
draw cylinder {   3.118  -1.070  -0.273} {   2.671  -0.824  -0.298} radius 0.03 filled yes resolution 20
draw cone {   2.671  -0.824  -0.298} {   2.430  -0.692  -0.312} radius 0.06 resolution 20
draw color  772
draw cylinder {   3.252  -0.543  -0.273} {   2.823  -0.357  -0.300} radius 0.03 filled yes resolution 20
draw cone {   2.823  -0.357  -0.300} {   2.593  -0.256  -0.315} radius 0.06 resolution 20
draw color  678
draw cylinder {   3.297  -0.000  -0.273} {   2.894   0.089  -0.302} radius 0.03 filled yes resolution 20
draw cone {   2.894   0.089  -0.302} {   2.677   0.137  -0.317} radius 0.06 resolution 20
draw color  575
draw cylinder {   3.161   0.542  -0.812} {   2.817   0.509  -0.896} radius 0.03 filled yes resolution 20
draw cone {   2.817   0.509  -0.896} {   2.633   0.491  -0.941} radius 0.06 resolution 20
draw color  591
draw cylinder {   3.024   1.068  -0.812} {   2.710   0.903  -0.895} radius 0.03 filled yes resolution 20
draw cone {   2.710   0.903  -0.895} {   2.541   0.814  -0.940} radius 0.06 resolution 20
draw color  686
draw cylinder {   2.800   1.564  -0.812} {   2.513   1.270  -0.892} radius 0.03 filled yes resolution 20
draw cone {   2.513   1.270  -0.892} {   2.358   1.111  -0.935} radius 0.06 resolution 20
draw color  811
draw cylinder {   2.495   2.015  -0.812} {   2.229   1.609  -0.886} radius 0.03 filled yes resolution 20
draw cone {   2.229   1.609  -0.886} {   2.086   1.390  -0.926} radius 0.06 resolution 20
draw color  920
draw cylinder {   2.119   2.407  -0.812} {   1.866   1.920  -0.878} radius 0.03 filled yes resolution 20
draw cone {   1.866   1.920  -0.878} {   1.730   1.658  -0.914} radius 0.06 resolution 20
draw color  980
draw cylinder {   1.681   2.731  -0.812} {   1.432   2.202  -0.869} radius 0.03 filled yes resolution 20
draw cone {   1.432   2.202  -0.869} {   1.298   1.917  -0.899} radius 0.06 resolution 20
draw color  979
draw cylinder {   1.196   2.976  -0.812} {   0.940   2.449  -0.858} radius 0.03 filled yes resolution 20
draw cone {   0.940   2.449  -0.858} {   0.803   2.165  -0.882} radius 0.06 resolution 20
draw color  919
draw cylinder {   0.676   3.135  -0.812} {   0.405   2.655  -0.845} radius 0.03 filled yes resolution 20
draw cone {   0.405   2.655  -0.845} {   0.259   2.396  -0.863} radius 0.06 resolution 20
draw color  815
draw cylinder {   0.136   3.204  -0.812} {  -0.158   2.809  -0.832} radius 0.03 filled yes resolution 20
draw cone {  -0.158   2.809  -0.832} {  -0.316   2.596  -0.842} radius 0.06 resolution 20
draw color  701
draw cylinder {  -0.407   3.181  -0.812} {  -0.729   2.900  -0.818} radius 0.03 filled yes resolution 20
draw cone {  -0.729   2.900  -0.818} {  -0.903   2.749  -0.820} radius 0.06 resolution 20
draw color  623
draw cylinder {  -0.939   3.066  -0.812} {  -1.291   2.916  -0.803} radius 0.03 filled yes resolution 20
draw cone {  -1.291   2.916  -0.803} {  -1.480   2.834  -0.799} radius 0.06 resolution 20
draw color  620
draw cylinder {  -1.444   2.863  -0.812} {  -1.823   2.844  -0.789} radius 0.03 filled yes resolution 20
draw cone {  -1.823   2.844  -0.789} {  -2.027   2.834  -0.777} radius 0.06 resolution 20
draw color  681
draw cylinder {  -1.907   2.578  -0.812} {  -2.309   2.676  -0.776} radius 0.03 filled yes resolution 20
draw cone {  -2.309   2.676  -0.776} {  -2.526   2.729  -0.757} radius 0.06 resolution 20
draw color  759
draw cylinder {  -2.315   2.219  -0.812} {  -2.733   2.407  -0.764} radius 0.03 filled yes resolution 20
draw cone {  -2.733   2.407  -0.764} {  -2.958   2.508  -0.738} radius 0.06 resolution 20
draw color  812
draw cylinder {  -2.657   1.796  -0.812} {  -3.081   2.036  -0.753} radius 0.03 filled yes resolution 20
draw cone {  -3.081   2.036  -0.753} {  -3.309   2.165  -0.721} radius 0.06 resolution 20
draw color  816
draw cylinder {  -2.922   1.321  -0.812} {  -3.342   1.570  -0.744} radius 0.03 filled yes resolution 20
draw cone {  -3.342   1.570  -0.744} {  -3.568   1.704  -0.707} radius 0.06 resolution 20
draw color  767
draw cylinder {  -3.103   0.808  -0.812} {  -3.510   1.021  -0.736} radius 0.03 filled yes resolution 20
draw cone {  -3.510   1.021  -0.736} {  -3.729   1.136  -0.696} radius 0.06 resolution 20
draw color  682
draw cylinder {  -3.195   0.272  -0.812} {  -3.580   0.409  -0.731} radius 0.03 filled yes resolution 20
draw cone {  -3.580   0.409  -0.731} {  -3.788   0.483  -0.688} radius 0.06 resolution 20
draw color  599
draw cylinder {  -3.195  -0.272  -0.812} {  -3.553  -0.243  -0.729} radius 0.03 filled yes resolution 20
draw cone {  -3.553  -0.243  -0.729} {  -3.746  -0.227  -0.684} radius 0.06 resolution 20
draw color  572
draw cylinder {  -3.103  -0.808  -0.812} {  -3.432  -0.907  -0.728} radius 0.03 filled yes resolution 20
draw cone {  -3.432  -0.907  -0.728} {  -3.609  -0.960  -0.683} radius 0.06 resolution 20
draw color  631
draw cylinder {  -2.922  -1.321  -0.812} {  -3.222  -1.552  -0.730} radius 0.03 filled yes resolution 20
draw cone {  -3.222  -1.552  -0.730} {  -3.384  -1.676  -0.687} radius 0.06 resolution 20
draw color  748
draw cylinder {  -2.657  -1.796  -0.812} {  -2.932  -2.149  -0.735} radius 0.03 filled yes resolution 20
draw cone {  -2.932  -2.149  -0.735} {  -3.081  -2.339  -0.693} radius 0.06 resolution 20
draw color  870
draw cylinder {  -2.315  -2.219  -0.812} {  -2.573  -2.670  -0.742} radius 0.03 filled yes resolution 20
draw cone {  -2.573  -2.670  -0.742} {  -2.712  -2.913  -0.704} radius 0.06 resolution 20
draw color  957
draw cylinder {  -1.907  -2.578  -0.812} {  -2.156  -3.092  -0.750} radius 0.03 filled yes resolution 20
draw cone {  -2.156  -3.092  -0.750} {  -2.291  -3.368  -0.717} radius 0.06 resolution 20
draw color  988
draw cylinder {  -1.444  -2.863  -0.812} {  -1.695  -3.397  -0.761} radius 0.03 filled yes resolution 20
draw cone {  -1.695  -3.397  -0.761} {  -1.830  -3.684  -0.733} radius 0.06 resolution 20
draw color  956
draw cylinder {  -0.939  -3.066  -0.812} {  -1.201  -3.575  -0.773} radius 0.03 filled yes resolution 20
draw cone {  -1.201  -3.575  -0.773} {  -1.342  -3.849  -0.751} radius 0.06 resolution 20
draw color  871
draw cylinder {  -0.407  -3.181  -0.812} {  -0.689  -3.623  -0.786} radius 0.03 filled yes resolution 20
draw cone {  -0.689  -3.623  -0.786} {  -0.841  -3.861  -0.772} radius 0.06 resolution 20
draw color  757
draw cylinder {   0.136  -3.204  -0.812} {  -0.171  -3.545  -0.800} radius 0.03 filled yes resolution 20
draw cone {  -0.171  -3.545  -0.800} {  -0.337  -3.728  -0.793} radius 0.06 resolution 20
draw color  654
draw cylinder {   0.676  -3.135  -0.812} {   0.339  -3.352  -0.814} radius 0.03 filled yes resolution 20
draw cone {   0.339  -3.352  -0.814} {   0.158  -3.468  -0.815} radius 0.06 resolution 20
draw color  611
draw cylinder {   1.196  -2.976  -0.812} {   0.830  -3.060  -0.828} radius 0.03 filled yes resolution 20
draw cone {   0.830  -3.060  -0.828} {   0.633  -3.105  -0.836} radius 0.06 resolution 20
draw color  645
draw cylinder {   1.681  -2.731  -0.812} {   1.290  -2.689  -0.842} radius 0.03 filled yes resolution 20
draw cone {   1.290  -2.689  -0.842} {   1.079  -2.666  -0.857} radius 0.06 resolution 20
draw color  721
draw cylinder {   2.119  -2.407  -0.812} {   1.708  -2.260  -0.854} radius 0.03 filled yes resolution 20
draw cone {   1.708  -2.260  -0.854} {   1.486  -2.181  -0.877} radius 0.06 resolution 20
draw color  790
draw cylinder {   2.495  -2.015  -0.812} {   2.073  -1.795  -0.866} radius 0.03 filled yes resolution 20
draw cone {   2.073  -1.795  -0.866} {   1.846  -1.677  -0.895} radius 0.06 resolution 20
draw color  820
draw cylinder {   2.800  -1.564  -0.812} {   2.377  -1.314  -0.876} radius 0.03 filled yes resolution 20
draw cone {   2.377  -1.314  -0.876} {   2.149  -1.179  -0.911} radius 0.06 resolution 20
draw color  797
draw cylinder {   3.024  -1.068  -0.812} {   2.609  -0.832  -0.884} radius 0.03 filled yes resolution 20
draw cone {   2.609  -0.832  -0.884} {   2.386  -0.704  -0.923} radius 0.06 resolution 20
draw color  728
draw cylinder {   3.161  -0.542  -0.812} {   2.764  -0.362  -0.890} radius 0.03 filled yes resolution 20
draw cone {   2.764  -0.362  -0.890} {   2.551  -0.265  -0.933} radius 0.06 resolution 20
draw color  637
draw cylinder {   3.207  -0.000  -0.812} {   2.835   0.086  -0.894} radius 0.03 filled yes resolution 20
draw cone {   2.835   0.086  -0.894} {   2.635   0.133  -0.939} radius 0.06 resolution 20
draw color  507
draw cylinder {   2.981   0.541  -1.329} {   2.695   0.508  -1.458} radius 0.03 filled yes resolution 20
draw cone {   2.695   0.508  -1.458} {   2.541   0.491  -1.528} radius 0.06 resolution 20
draw color  529
draw cylinder {   2.836   1.064  -1.329} {   2.578   0.907  -1.457} radius 0.03 filled yes resolution 20
draw cone {   2.578   0.907  -1.457} {   2.439   0.823  -1.525} radius 0.06 resolution 20
draw color  623
draw cylinder {   2.601   1.554  -1.329} {   2.367   1.276  -1.451} radius 0.03 filled yes resolution 20
draw cone {   2.367   1.276  -1.451} {   2.241   1.127  -1.516} radius 0.06 resolution 20
draw color  739
draw cylinder {   2.281   1.993  -1.329} {   2.067   1.615  -1.441} radius 0.03 filled yes resolution 20
draw cone {   2.067   1.615  -1.441} {   1.951   1.412  -1.501} radius 0.06 resolution 20
draw color  828
draw cylinder {   1.889   2.369  -1.329} {   1.684   1.923  -1.428} radius 0.03 filled yes resolution 20
draw cone {   1.684   1.923  -1.428} {   1.574   1.683  -1.481} radius 0.06 resolution 20
draw color  864
draw cylinder {   1.436   2.668  -1.329} {   1.231   2.195  -1.411} radius 0.03 filled yes resolution 20
draw cone {   1.231   2.195  -1.411} {   1.122   1.941  -1.455} radius 0.06 resolution 20
draw color  838
draw cylinder {   0.936   2.881  -1.329} {   0.723   2.427  -1.392} radius 0.03 filled yes resolution 20
draw cone {   0.723   2.427  -1.392} {   0.608   2.182  -1.426} radius 0.06 resolution 20
draw color  756
draw cylinder {   0.407   3.002  -1.329} {   0.175   2.608  -1.370} radius 0.03 filled yes resolution 20
draw cone {   0.175   2.608  -1.370} {   0.050   2.396  -1.393} radius 0.06 resolution 20
draw color  643
draw cylinder {  -0.136   3.026  -1.329} {  -0.392   2.727  -1.348} radius 0.03 filled yes resolution 20
draw cone {  -0.392   2.727  -1.348} {  -0.530   2.566  -1.358} radius 0.06 resolution 20
draw color  543
draw cylinder {  -0.674   2.953  -1.329} {  -0.958   2.772  -1.325} radius 0.03 filled yes resolution 20
draw cone {  -0.958   2.772  -1.325} {  -1.110   2.674  -1.322} radius 0.06 resolution 20
draw color  509
draw cylinder {  -1.191   2.786  -1.329} {  -1.501   2.729  -1.302} radius 0.03 filled yes resolution 20
draw cone {  -1.501   2.729  -1.302} {  -1.669   2.699  -1.287} radius 0.06 resolution 20
draw color  555
draw cylinder {  -1.669   2.528  -1.329} {  -2.003   2.588  -1.279} radius 0.03 filled yes resolution 20
draw cone {  -2.003   2.588  -1.279} {  -2.183   2.621  -1.253} radius 0.06 resolution 20
draw color  635
draw cylinder {  -2.094   2.190  -1.329} {  -2.445   2.343  -1.259} radius 0.03 filled yes resolution 20
draw cone {  -2.445   2.343  -1.259} {  -2.634   2.426  -1.221} radius 0.06 resolution 20
draw color  699
draw cylinder {  -2.451   1.781  -1.329} {  -2.810   1.992  -1.240} radius 0.03 filled yes resolution 20
draw cone {  -2.810   1.992  -1.240} {  -3.003   2.106  -1.193} radius 0.06 resolution 20
draw color  716
draw cylinder {  -2.729   1.314  -1.329} {  -3.086   1.542  -1.225} radius 0.03 filled yes resolution 20
draw cone {  -3.086   1.542  -1.225} {  -3.278   1.664  -1.169} radius 0.06 resolution 20
draw color  679
draw cylinder {  -2.920   0.806  -1.329} {  -3.265   1.004  -1.213} radius 0.03 filled yes resolution 20
draw cone {  -3.265   1.004  -1.213} {  -3.451   1.111  -1.150} radius 0.06 resolution 20
draw color  603
draw cylinder {  -3.017   0.272  -1.329} {  -3.342   0.400  -1.204} radius 0.03 filled yes resolution 20
draw cone {  -3.342   0.400  -1.204} {  -3.518   0.469  -1.137} radius 0.06 resolution 20
draw color  527
draw cylinder {  -3.017  -0.272  -1.329} {  -3.317  -0.245  -1.200} radius 0.03 filled yes resolution 20
draw cone {  -3.317  -0.245  -1.200} {  -3.478  -0.231  -1.131} radius 0.06 resolution 20
draw color  507
draw cylinder {  -2.920  -0.806  -1.329} {  -3.192  -0.900  -1.200} radius 0.03 filled yes resolution 20
draw cone {  -3.192  -0.900  -1.200} {  -3.339  -0.951  -1.130} radius 0.06 resolution 20
draw color  570
draw cylinder {  -2.729  -1.314  -1.329} {  -2.975  -1.533  -1.203} radius 0.03 filled yes resolution 20
draw cone {  -2.975  -1.533  -1.203} {  -3.107  -1.651  -1.136} radius 0.06 resolution 20
draw color  682
draw cylinder {  -2.451  -1.781  -1.329} {  -2.674  -2.112  -1.211} radius 0.03 filled yes resolution 20
draw cone {  -2.674  -2.112  -1.211} {  -2.794  -2.290  -1.148} radius 0.06 resolution 20
draw color  789
draw cylinder {  -2.094  -2.190  -1.329} {  -2.302  -2.606  -1.223} radius 0.03 filled yes resolution 20
draw cone {  -2.302  -2.606  -1.223} {  -2.414  -2.830  -1.166} radius 0.06 resolution 20
draw color  854
draw cylinder {  -1.669  -2.528  -1.329} {  -1.872  -2.993  -1.238} radius 0.03 filled yes resolution 20
draw cone {  -1.872  -2.993  -1.238} {  -1.981  -3.243  -1.189} radius 0.06 resolution 20
draw color  859
draw cylinder {  -1.191  -2.786  -1.329} {  -1.398  -3.255  -1.256} radius 0.03 filled yes resolution 20
draw cone {  -1.398  -3.255  -1.256} {  -1.510  -3.507  -1.217} radius 0.06 resolution 20
draw color  803
draw cylinder {  -0.674  -2.953  -1.329} {  -0.896  -3.383  -1.276} radius 0.03 filled yes resolution 20
draw cone {  -0.896  -3.383  -1.276} {  -1.015  -3.614  -1.248} radius 0.06 resolution 20
draw color  701
draw cylinder {  -0.136  -3.026  -1.329} {  -0.379  -3.377  -1.298} radius 0.03 filled yes resolution 20
draw cone {  -0.379  -3.377  -1.298} {  -0.510  -3.565  -1.282} radius 0.06 resolution 20
draw color  588
draw cylinder {   0.407  -3.002  -1.329} {   0.137  -3.244  -1.321} radius 0.03 filled yes resolution 20
draw cone {   0.137  -3.244  -1.321} {  -0.008  -3.375  -1.317} radius 0.06 resolution 20
draw color  515
draw cylinder {   0.936  -2.881  -1.329} {   0.639  -3.000  -1.345} radius 0.03 filled yes resolution 20
draw cone {   0.639  -3.000  -1.345} {   0.479  -3.064  -1.353} radius 0.06 resolution 20
draw color  524
draw cylinder {   1.436  -2.668  -1.329} {   1.112  -2.664  -1.367} radius 0.03 filled yes resolution 20
draw cone {   1.112  -2.664  -1.367} {   0.938  -2.662  -1.388} radius 0.06 resolution 20
draw color  594
draw cylinder {   1.889  -2.369  -1.329} {   1.545  -2.258  -1.389} radius 0.03 filled yes resolution 20
draw cone {   1.545  -2.258  -1.389} {   1.360  -2.198  -1.421} radius 0.06 resolution 20
draw color  672
draw cylinder {   2.281  -1.993  -1.329} {   1.925  -1.806  -1.408} radius 0.03 filled yes resolution 20
draw cone {   1.925  -1.806  -1.408} {   1.734  -1.705  -1.451} radius 0.06 resolution 20
draw color  714
draw cylinder {   2.601  -1.554  -1.329} {   2.242  -1.329  -1.425} radius 0.03 filled yes resolution 20
draw cone {   2.242  -1.329  -1.425} {   2.048  -1.208  -1.477} radius 0.06 resolution 20
draw color  704
draw cylinder {   2.836  -1.064  -1.329} {   2.484  -0.846  -1.439} radius 0.03 filled yes resolution 20
draw cone {   2.484  -0.846  -1.439} {   2.295  -0.729  -1.499} radius 0.06 resolution 20
draw color  644
draw cylinder {   2.981  -0.541  -1.329} {   2.645  -0.373  -1.450} radius 0.03 filled yes resolution 20
draw cone {   2.645  -0.373  -1.450} {   2.464  -0.282  -1.515} radius 0.06 resolution 20
draw color  562
draw cylinder {   3.029  -0.000  -1.329} {   2.716   0.081  -1.456} radius 0.03 filled yes resolution 20
draw cone {   2.716   0.081  -1.456} {   2.548   0.124  -1.525} radius 0.06 resolution 20
draw color  418
draw cylinder {   2.716   0.540  -1.809} {   2.508   0.509  -1.971} radius 0.03 filled yes resolution 20
draw cone {   2.508   0.509  -1.971} {   2.396   0.492  -2.057} radius 0.06 resolution 20
draw color  448
draw cylinder {   2.559   1.060  -1.809} {   2.376   0.914  -1.967} radius 0.03 filled yes resolution 20
draw cone {   2.376   0.914  -1.967} {   2.277   0.836  -2.053} radius 0.06 resolution 20
draw color  539
draw cylinder {   2.303   1.539  -1.809} {   2.142   1.286  -1.958} radius 0.03 filled yes resolution 20
draw cone {   2.142   1.286  -1.958} {   2.055   1.151  -2.038} radius 0.06 resolution 20
draw color  635
draw cylinder {   1.958   1.958  -1.809} {   1.812   1.623  -1.943} radius 0.03 filled yes resolution 20
draw cone {   1.812   1.623  -1.943} {   1.733   1.443  -2.015} radius 0.06 resolution 20
draw color  693
draw cylinder {   1.539   2.303  -1.809} {   1.398   1.921  -1.923} radius 0.03 filled yes resolution 20
draw cone {   1.398   1.921  -1.923} {   1.322   1.715  -1.984} radius 0.06 resolution 20
draw color  691
draw cylinder {   1.060   2.559  -1.809} {   0.915   2.173  -1.898} radius 0.03 filled yes resolution 20
draw cone {   0.915   2.173  -1.898} {   0.837   1.966  -1.946} radius 0.06 resolution 20
draw color  627
draw cylinder {   0.540   2.716  -1.809} {   0.382   2.371  -1.870} radius 0.03 filled yes resolution 20
draw cone {   0.382   2.371  -1.870} {   0.296   2.186  -1.903} radius 0.06 resolution 20
draw color  519
draw cylinder {   0.000   2.769  -1.809} {  -0.179   2.503  -1.840} radius 0.03 filled yes resolution 20
draw cone {  -0.179   2.503  -1.840} {  -0.276   2.359  -1.857} radius 0.06 resolution 20
draw color  411
draw cylinder {  -0.540   2.716  -1.809} {  -0.745   2.554  -1.809} radius 0.03 filled yes resolution 20
draw cone {  -0.745   2.554  -1.809} {  -0.855   2.466  -1.808} radius 0.06 resolution 20
draw color  369
draw cylinder {  -1.060   2.559  -1.809} {  -1.289   2.510  -1.777} radius 0.03 filled yes resolution 20
draw cone {  -1.289   2.510  -1.777} {  -1.413   2.484  -1.760} radius 0.06 resolution 20
draw color  419
draw cylinder {  -1.539   2.303  -1.809} {  -1.790   2.361  -1.747} radius 0.03 filled yes resolution 20
draw cone {  -1.790   2.361  -1.747} {  -1.925   2.393  -1.713} radius 0.06 resolution 20
draw color  504
draw cylinder {  -1.958   1.958  -1.809} {  -2.224   2.100  -1.719} radius 0.03 filled yes resolution 20
draw cone {  -2.224   2.100  -1.719} {  -2.367   2.176  -1.671} radius 0.06 resolution 20
draw color  566
draw cylinder {  -2.303   1.539  -1.809} {  -2.574   1.727  -1.695} radius 0.03 filled yes resolution 20
draw cone {  -2.574   1.727  -1.695} {  -2.720   1.829  -1.633} radius 0.06 resolution 20
draw color  576
draw cylinder {  -2.559   1.060  -1.809} {  -2.826   1.252  -1.675} radius 0.03 filled yes resolution 20
draw cone {  -2.826   1.252  -1.675} {  -2.969   1.355  -1.602} radius 0.06 resolution 20
draw color  533
draw cylinder {  -2.716   0.540  -1.809} {  -2.970   0.692  -1.660} radius 0.03 filled yes resolution 20
draw cone {  -2.970   0.692  -1.660} {  -3.106   0.773  -1.580} radius 0.06 resolution 20
draw color  463
draw cylinder {  -2.769   0.000  -1.809} {  -3.002   0.073  -1.651} radius 0.03 filled yes resolution 20
draw cone {  -3.002   0.073  -1.651} {  -3.127   0.112  -1.566} radius 0.06 resolution 20
draw color  418
draw cylinder {  -2.716  -0.540  -1.809} {  -2.924  -0.572  -1.648} radius 0.03 filled yes resolution 20
draw cone {  -2.924  -0.572  -1.648} {  -3.036  -0.588  -1.561} radius 0.06 resolution 20
draw color  448
draw cylinder {  -2.559  -1.060  -1.809} {  -2.741  -1.205  -1.651} radius 0.03 filled yes resolution 20
draw cone {  -2.741  -1.205  -1.651} {  -2.840  -1.283  -1.566} radius 0.06 resolution 20
draw color  539
draw cylinder {  -2.303  -1.539  -1.809} {  -2.464  -1.791  -1.661} radius 0.03 filled yes resolution 20
draw cone {  -2.464  -1.791  -1.661} {  -2.551  -1.926  -1.580} radius 0.06 resolution 20
draw color  635
draw cylinder {  -1.958  -1.958  -1.809} {  -2.105  -2.293  -1.676} radius 0.03 filled yes resolution 20
draw cone {  -2.105  -2.293  -1.676} {  -2.184  -2.474  -1.604} radius 0.06 resolution 20
draw color  693
draw cylinder {  -1.539  -2.303  -1.809} {  -1.679  -2.685  -1.696} radius 0.03 filled yes resolution 20
draw cone {  -1.679  -2.685  -1.696} {  -1.755  -2.890  -1.635} radius 0.06 resolution 20
draw color  691
draw cylinder {  -1.060  -2.559  -1.809} {  -1.205  -2.944  -1.720} radius 0.03 filled yes resolution 20
draw cone {  -1.205  -2.944  -1.720} {  -1.283  -3.151  -1.672} radius 0.06 resolution 20
draw color  627
draw cylinder {  -0.540  -2.716  -1.809} {  -0.699  -3.061  -1.748} radius 0.03 filled yes resolution 20
draw cone {  -0.699  -3.061  -1.748} {  -0.784  -3.247  -1.715} radius 0.06 resolution 20
draw color  519
draw cylinder {  -0.000  -2.769  -1.809} {  -0.179  -3.036  -1.779} radius 0.03 filled yes resolution 20
draw cone {  -0.179  -3.036  -1.779} {  -0.276  -3.179  -1.762} radius 0.06 resolution 20
draw color  411
draw cylinder {   0.540  -2.716  -1.809} {   0.336  -2.878  -1.810} radius 0.03 filled yes resolution 20
draw cone {   0.336  -2.878  -1.810} {   0.226  -2.966  -1.810} radius 0.06 resolution 20
draw color  369
draw cylinder {   1.060  -2.559  -1.809} {   0.830  -2.607  -1.841} radius 0.03 filled yes resolution 20
draw cone {   0.830  -2.607  -1.841} {   0.707  -2.633  -1.859} radius 0.06 resolution 20
draw color  419
draw cylinder {   1.539  -2.303  -1.809} {   1.288  -2.244  -1.872} radius 0.03 filled yes resolution 20
draw cone {   1.288  -2.244  -1.872} {   1.153  -2.213  -1.905} radius 0.06 resolution 20
draw color  504
draw cylinder {   1.958  -1.958  -1.809} {   1.693  -1.817  -1.900} radius 0.03 filled yes resolution 20
draw cone {   1.693  -1.817  -1.900} {   1.550  -1.740  -1.948} radius 0.06 resolution 20
draw color  566
draw cylinder {   2.303  -1.539  -1.809} {   2.031  -1.350  -1.924} radius 0.03 filled yes resolution 20
draw cone {   2.031  -1.350  -1.924} {   1.885  -1.249  -1.986} radius 0.06 resolution 20
draw color  576
draw cylinder {   2.559  -1.060  -1.809} {   2.292  -0.868  -1.944} radius 0.03 filled yes resolution 20
draw cone {   2.292  -0.868  -1.944} {   2.148  -0.765  -2.016} radius 0.06 resolution 20
draw color  533
draw cylinder {   2.716  -0.540  -1.809} {   2.463  -0.389  -1.959} radius 0.03 filled yes resolution 20
draw cone {   2.463  -0.389  -1.959} {   2.326  -0.308  -2.039} radius 0.06 resolution 20
draw color  463
draw cylinder {   2.769  -0.000  -1.809} {   2.537   0.073  -1.968} radius 0.03 filled yes resolution 20
draw cone {   2.537   0.073  -1.968} {   2.411   0.112  -2.053} radius 0.06 resolution 20
draw color  328
draw cylinder {   2.373   0.542  -2.240} {   2.255   0.513  -2.416} radius 0.03 filled yes resolution 20
draw cone {   2.255   0.513  -2.416} {   2.192   0.497  -2.510} radius 0.06 resolution 20
draw color  365
draw cylinder {   2.193   1.056  -2.240} {   2.097   0.927  -2.410} radius 0.03 filled yes resolution 20
draw cone {   2.097   0.927  -2.410} {   2.045   0.857  -2.502} radius 0.06 resolution 20
draw color  443
draw cylinder {   1.903   1.517  -2.240} {   1.824   1.300  -2.396} radius 0.03 filled yes resolution 20
draw cone {   1.824   1.300  -2.396} {   1.782   1.183  -2.480} radius 0.06 resolution 20
draw color  504
draw cylinder {   1.517   1.903  -2.240} {   1.448   1.627  -2.374} radius 0.03 filled yes resolution 20
draw cone {   1.448   1.627  -2.374} {   1.410   1.478  -2.446} radius 0.06 resolution 20
draw color  512
draw cylinder {   1.056   2.193  -2.240} {   0.986   1.900  -2.345} radius 0.03 filled yes resolution 20
draw cone {   0.986   1.900  -2.345} {   0.948   1.742  -2.402} radius 0.06 resolution 20
draw color  455
draw cylinder {   0.542   2.373  -2.240} {   0.461   2.108  -2.312} radius 0.03 filled yes resolution 20
draw cone {   0.461   2.108  -2.312} {   0.418   1.965  -2.350} radius 0.06 resolution 20
draw color  347
draw cylinder {   0.000   2.434  -2.240} {  -0.098   2.236  -2.274} radius 0.03 filled yes resolution 20
draw cone {  -0.098   2.236  -2.274} {  -0.152   2.129  -2.292} radius 0.06 resolution 20
draw color  236
draw cylinder {  -0.542   2.373  -2.240} {  -0.662   2.267  -2.235} radius 0.03 filled yes resolution 20
draw cone {  -0.662   2.267  -2.235} {  -0.727   2.211  -2.232} radius 0.06 resolution 20
draw color  217
draw cylinder {  -1.056   2.193  -2.240} {  -1.198   2.188  -2.196} radius 0.03 filled yes resolution 20
draw cone {  -1.198   2.188  -2.196} {  -1.275   2.185  -2.172} radius 0.06 resolution 20
draw color  301
draw cylinder {  -1.517   1.903  -2.240} {  -1.677   1.986  -2.159} radius 0.03 filled yes resolution 20
draw cone {  -1.677   1.986  -2.159} {  -1.763   2.031  -2.116} radius 0.06 resolution 20
draw color  389
draw cylinder {  -1.903   1.517  -2.240} {  -2.071   1.659  -2.127} radius 0.03 filled yes resolution 20
draw cone {  -2.071   1.659  -2.127} {  -2.162   1.735  -2.065} radius 0.06 resolution 20
draw color  428
draw cylinder {  -2.193   1.056  -2.240} {  -2.361   1.215  -2.100} radius 0.03 filled yes resolution 20
draw cone {  -2.361   1.215  -2.100} {  -2.451   1.300  -2.024} radius 0.06 resolution 20
draw color  410
draw cylinder {  -2.373   0.542  -2.240} {  -2.530   0.672  -2.080} radius 0.03 filled yes resolution 20
draw cone {  -2.530   0.672  -2.080} {  -2.615   0.743  -1.994} radius 0.06 resolution 20
draw color  359
draw cylinder {  -2.434   0.000  -2.240} {  -2.573   0.064  -2.068} radius 0.03 filled yes resolution 20
draw cone {  -2.573   0.064  -2.068} {  -2.649   0.098  -1.975} radius 0.06 resolution 20
draw color  328
draw cylinder {  -2.373  -0.542  -2.240} {  -2.490  -0.571  -2.065} radius 0.03 filled yes resolution 20
draw cone {  -2.490  -0.571  -2.065} {  -2.554  -0.586  -1.971} radius 0.06 resolution 20
draw color  365
draw cylinder {  -2.193  -1.056  -2.240} {  -2.289  -1.185  -2.071} radius 0.03 filled yes resolution 20
draw cone {  -2.289  -1.185  -2.071} {  -2.340  -1.255  -1.979} radius 0.06 resolution 20
draw color  443
draw cylinder {  -1.903  -1.517  -2.240} {  -1.982  -1.735  -2.085} radius 0.03 filled yes resolution 20
draw cone {  -1.982  -1.735  -2.085} {  -2.024  -1.852  -2.001} radius 0.06 resolution 20
draw color  504
draw cylinder {  -1.517  -1.903  -2.240} {  -1.587  -2.179  -2.107} radius 0.03 filled yes resolution 20
draw cone {  -1.587  -2.179  -2.107} {  -1.625  -2.327  -2.035} radius 0.06 resolution 20
draw color  512
draw cylinder {  -1.056  -2.193  -2.240} {  -1.126  -2.486  -2.136} radius 0.03 filled yes resolution 20
draw cone {  -1.126  -2.486  -2.136} {  -1.164  -2.643  -2.079} radius 0.06 resolution 20
draw color  455
draw cylinder {  -0.542  -2.373  -2.240} {  -0.622  -2.638  -2.169} radius 0.03 filled yes resolution 20
draw cone {  -0.622  -2.638  -2.169} {  -0.665  -2.781  -2.131} radius 0.06 resolution 20
draw color  347
draw cylinder {  -0.000  -2.434  -2.240} {  -0.098  -2.632  -2.207} radius 0.03 filled yes resolution 20
draw cone {  -0.098  -2.632  -2.207} {  -0.152  -2.739  -2.189} radius 0.06 resolution 20
draw color  236
draw cylinder {   0.542  -2.373  -2.240} {   0.421  -2.478  -2.246} radius 0.03 filled yes resolution 20
draw cone {   0.421  -2.478  -2.246} {   0.356  -2.535  -2.249} radius 0.06 resolution 20
draw color  217
draw cylinder {   1.056  -2.193  -2.240} {   0.914  -2.198  -2.285} radius 0.03 filled yes resolution 20
draw cone {   0.914  -2.198  -2.285} {   0.837  -2.200  -2.309} radius 0.06 resolution 20
draw color  301
draw cylinder {   1.517  -1.903  -2.240} {   1.358  -1.820  -2.322} radius 0.03 filled yes resolution 20
draw cone {   1.358  -1.820  -2.322} {   1.272  -1.775  -2.365} radius 0.06 resolution 20
draw color  389
draw cylinder {   1.903  -1.517  -2.240} {   1.734  -1.376  -2.354} radius 0.03 filled yes resolution 20
draw cone {   1.734  -1.376  -2.354} {   1.644  -1.300  -2.416} radius 0.06 resolution 20
draw color  428
draw cylinder {   2.193  -1.056  -2.240} {   2.025  -0.897  -2.381} radius 0.03 filled yes resolution 20
draw cone {   2.025  -0.897  -2.381} {   1.935  -0.812  -2.457} radius 0.06 resolution 20
draw color  410
draw cylinder {   2.373  -0.542  -2.240} {   2.215  -0.411  -2.401} radius 0.03 filled yes resolution 20
draw cone {   2.215  -0.411  -2.401} {   2.130  -0.340  -2.487} radius 0.06 resolution 20
draw color  359
draw cylinder {   2.434  -0.000  -2.240} {   2.294   0.064  -2.413} radius 0.03 filled yes resolution 20
draw cone {   2.294   0.064  -2.413} {   2.219   0.098  -2.506} radius 0.06 resolution 20
draw color  260
draw cylinder {   1.957   0.548  -2.611} {   1.931   0.523  -2.781} radius 0.03 filled yes resolution 20
draw cone {   1.931   0.523  -2.781} {   1.918   0.509  -2.872} radius 0.06 resolution 20
draw color  294
draw cylinder {   1.736   1.056  -2.611} {   1.728   0.947  -2.771} radius 0.03 filled yes resolution 20
draw cone {   1.728   0.947  -2.771} {   1.724   0.889  -2.857} radius 0.06 resolution 20
draw color  340
draw cylinder {   1.387   1.485  -2.611} {   1.390   1.314  -2.749} radius 0.03 filled yes resolution 20
draw cone {   1.390   1.314  -2.749} {   1.392   1.222  -2.823} radius 0.06 resolution 20
draw color  342
draw cylinder {   0.935   1.804  -2.611} {   0.940   1.610  -2.717} radius 0.03 filled yes resolution 20
draw cone {   0.940   1.610  -2.717} {   0.942   1.505  -2.774} radius 0.06 resolution 20
draw color  277
draw cylinder {   0.413   1.989  -2.611} {   0.410   1.818  -2.677} radius 0.03 filled yes resolution 20
draw cone {   0.410   1.818  -2.677} {   0.408   1.725  -2.712} radius 0.06 resolution 20
draw color  155
draw cylinder {  -0.139   2.027  -2.611} {  -0.158   1.917  -2.632} radius 0.03 filled yes resolution 20
draw cone {  -0.158   1.917  -2.632} {  -0.169   1.858  -2.643} radius 0.06 resolution 20
draw color   51
draw cylinder {  -0.680   1.915  -2.611} {  -0.719   1.888  -2.585} radius 0.03 filled yes resolution 20
draw cone {  -0.719   1.888  -2.585} {  -0.740   1.874  -2.571} radius 0.06 resolution 20
draw color  138
draw cylinder {  -1.172   1.660  -2.611} {  -1.227   1.714  -2.541} radius 0.03 filled yes resolution 20
draw cone {  -1.227   1.714  -2.541} {  -1.256   1.743  -2.503} radius 0.06 resolution 20
draw color  247
draw cylinder {  -1.576   1.282  -2.611} {  -1.640   1.390  -2.501} radius 0.03 filled yes resolution 20
draw cone {  -1.640   1.390  -2.501} {  -1.674   1.448  -2.442} radius 0.06 resolution 20
draw color  297
draw cylinder {  -1.864   0.809  -2.611} {  -1.926   0.929  -2.470} radius 0.03 filled yes resolution 20
draw cone {  -1.926   0.929  -2.470} {  -1.960   0.993  -2.394} radius 0.06 resolution 20
draw color  288
draw cylinder {  -2.013   0.277  -2.611} {  -2.065   0.363  -2.449} radius 0.03 filled yes resolution 20
draw cone {  -2.065   0.363  -2.449} {  -2.093   0.409  -2.362} radius 0.06 resolution 20
draw color  261
draw cylinder {  -2.013  -0.277  -2.611} {  -2.048  -0.260  -2.440} radius 0.03 filled yes resolution 20
draw cone {  -2.048  -0.260  -2.440} {  -2.066  -0.251  -2.348} radius 0.06 resolution 20
draw color  272
draw cylinder {  -1.864  -0.809  -2.611} {  -1.880  -0.878  -2.444} radius 0.03 filled yes resolution 20
draw cone {  -1.880  -0.878  -2.444} {  -1.888  -0.914  -2.354} radius 0.06 resolution 20
draw color  320
draw cylinder {  -1.576  -1.282  -2.611} {  -1.577  -1.426  -2.460} radius 0.03 filled yes resolution 20
draw cone {  -1.577  -1.426  -2.460} {  -1.578  -1.503  -2.379} radius 0.06 resolution 20
draw color  349
draw cylinder {  -1.172  -1.660  -2.611} {  -1.166  -1.848  -2.487} radius 0.03 filled yes resolution 20
draw cone {  -1.166  -1.848  -2.487} {  -1.163  -1.949  -2.421} radius 0.06 resolution 20
draw color  319
draw cylinder {  -0.680  -1.915  -2.611} {  -0.679  -2.103  -2.524} radius 0.03 filled yes resolution 20
draw cone {  -0.679  -2.103  -2.524} {  -0.678  -2.205  -2.477} radius 0.06 resolution 20
draw color  221
draw cylinder {  -0.139  -2.027  -2.611} {  -0.150  -2.172  -2.567} radius 0.03 filled yes resolution 20
draw cone {  -0.150  -2.172  -2.567} {  -0.156  -2.250  -2.543} radius 0.06 resolution 20
draw color   88
draw cylinder {   0.413  -1.989  -2.611} {   0.384  -2.059  -2.613} radius 0.03 filled yes resolution 20
draw cone {   0.384  -2.059  -2.613} {   0.368  -2.096  -2.614} radius 0.06 resolution 20
draw color   78
draw cylinder {   0.935  -1.804  -2.611} {   0.887  -1.789  -2.659} radius 0.03 filled yes resolution 20
draw cone {   0.887  -1.789  -2.659} {   0.862  -1.780  -2.685} radius 0.06 resolution 20
draw color  198
draw cylinder {   1.387  -1.485  -2.611} {   1.326  -1.400  -2.701} radius 0.03 filled yes resolution 20
draw cone {   1.326  -1.400  -2.701} {   1.294  -1.354  -2.750} radius 0.06 resolution 20
draw color  280
draw cylinder {   1.736  -1.056  -2.611} {   1.671  -0.936  -2.737} radius 0.03 filled yes resolution 20
draw cone {   1.671  -0.936  -2.737} {   1.637  -0.872  -2.805} radius 0.06 resolution 20
draw color  298
draw cylinder {   1.957  -0.548  -2.611} {   1.898  -0.440  -2.763} radius 0.03 filled yes resolution 20
draw cone {   1.898  -0.440  -2.763} {   1.867  -0.382  -2.845} radius 0.06 resolution 20
draw color  273
draw cylinder {   2.032  -0.000  -2.611} {   1.988   0.055  -2.778} radius 0.03 filled yes resolution 20
draw cone {   1.988   0.055  -2.778} {   1.964   0.084  -2.868} radius 0.06 resolution 20
draw color  232
draw cylinder {   1.480   0.538  -2.909} {   1.538   0.522  -3.055} radius 0.03 filled yes resolution 20
draw cone {   1.538   0.522  -3.055} {   1.569   0.514  -3.133} radius 0.06 resolution 20
draw color  244
draw cylinder {   1.206   1.012  -2.909} {   1.276   0.937  -3.038} radius 0.03 filled yes resolution 20
draw cone {   1.276   0.937  -3.038} {   1.314   0.897  -3.108} radius 0.06 resolution 20
draw color  234
draw cylinder {   0.787   1.364  -2.909} {   0.861   1.261  -3.006} radius 0.03 filled yes resolution 20
draw cone {   0.861   1.261  -3.006} {   0.900   1.205  -3.058} radius 0.06 resolution 20
draw color  169
draw cylinder {   0.273   1.551  -2.909} {   0.341   1.464  -2.962} radius 0.03 filled yes resolution 20
draw cone {   0.341   1.464  -2.962} {   0.377   1.417  -2.991} radius 0.06 resolution 20
draw color   68
draw cylinder {  -0.273   1.551  -2.909} {  -0.219   1.517  -2.912} radius 0.03 filled yes resolution 20
draw cone {  -0.219   1.517  -2.912} {  -0.190   1.499  -2.913} radius 0.06 resolution 20
draw color   79
draw cylinder {  -0.787   1.364  -2.909} {  -0.747   1.394  -2.861} radius 0.03 filled yes resolution 20
draw cone {  -0.747   1.394  -2.861} {  -0.725   1.411  -2.836} radius 0.06 resolution 20
draw color  174
draw cylinder {  -1.206   1.012  -2.909} {  -1.174   1.089  -2.817} radius 0.03 filled yes resolution 20
draw cone {  -1.174   1.089  -2.817} {  -1.157   1.130  -2.767} radius 0.06 resolution 20
draw color  227
draw cylinder {  -1.480   0.538  -2.909} {  -1.446   0.621  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -1.446   0.621  -2.783} {  -1.428   0.666  -2.715} radius 0.06 resolution 20
draw color  233
draw cylinder {  -1.574   0.000  -2.909} {  -1.530   0.046  -2.765} radius 0.03 filled yes resolution 20
draw cone {  -1.530   0.046  -2.765} {  -1.507   0.071  -2.687} radius 0.06 resolution 20
draw color  232
draw cylinder {  -1.480  -0.538  -2.909} {  -1.421  -0.555  -2.764} radius 0.03 filled yes resolution 20
draw cone {  -1.421  -0.555  -2.764} {  -1.390  -0.563  -2.685} radius 0.06 resolution 20
draw color  244
draw cylinder {  -1.206  -1.012  -2.909} {  -1.136  -1.087  -2.780} radius 0.03 filled yes resolution 20
draw cone {  -1.136  -1.087  -2.780} {  -1.098  -1.127  -2.711} radius 0.06 resolution 20
draw color  234
draw cylinder {  -0.787  -1.364  -2.909} {  -0.714  -1.466  -2.813} radius 0.03 filled yes resolution 20
draw cone {  -0.714  -1.466  -2.813} {  -0.674  -1.522  -2.760} radius 0.06 resolution 20
draw color  169
draw cylinder {  -0.273  -1.551  -2.909} {  -0.206  -1.637  -2.856} radius 0.03 filled yes resolution 20
draw cone {  -0.206  -1.637  -2.856} {  -0.170  -1.684  -2.828} radius 0.06 resolution 20
draw color   68
draw cylinder {   0.273  -1.551  -2.909} {   0.328  -1.584  -2.907} radius 0.03 filled yes resolution 20
draw cone {   0.328  -1.584  -2.907} {   0.357  -1.603  -2.905} radius 0.06 resolution 20
draw color   79
draw cylinder {   0.787  -1.364  -2.909} {   0.827  -1.333  -2.957} radius 0.03 filled yes resolution 20
draw cone {   0.827  -1.333  -2.957} {   0.849  -1.316  -2.983} radius 0.06 resolution 20
draw color  174
draw cylinder {   1.206  -1.012  -2.909} {   1.238  -0.935  -3.002} radius 0.03 filled yes resolution 20
draw cone {   1.238  -0.935  -3.002} {   1.256  -0.894  -3.052} radius 0.06 resolution 20
draw color  227
draw cylinder {   1.480  -0.538  -2.909} {   1.513  -0.456  -3.036} radius 0.03 filled yes resolution 20
draw cone {   1.513  -0.456  -3.036} {   1.531  -0.411  -3.104} radius 0.06 resolution 20
draw color  233
draw cylinder {   1.574  -0.000  -2.909} {   1.618   0.046  -3.054} radius 0.03 filled yes resolution 20
draw cone {   1.618   0.046  -3.054} {   1.642   0.071  -3.132} radius 0.06 resolution 20
draw color  238
draw cylinder {   0.930   0.537  -3.129} {   1.055   0.533  -3.231} radius 0.03 filled yes resolution 20
draw cone {   1.055   0.533  -3.231} {   1.122   0.530  -3.286} radius 0.06 resolution 20
draw color  218
draw cylinder {   0.537   0.930  -3.129} {   0.665   0.900  -3.200} radius 0.03 filled yes resolution 20
draw cone {   0.665   0.900  -3.200} {   0.735   0.884  -3.238} radius 0.06 resolution 20
draw color  175
draw cylinder {   0.000   1.074  -3.129} {   0.123   1.062  -3.149} radius 0.03 filled yes resolution 20
draw cone {   0.123   1.062  -3.149} {   0.189   1.056  -3.161} radius 0.06 resolution 20
draw color  170
draw cylinder {  -0.537   0.930  -3.129} {  -0.424   0.962  -3.094} radius 0.03 filled yes resolution 20
draw cone {  -0.424   0.962  -3.094} {  -0.363   0.980  -3.075} radius 0.06 resolution 20
draw color  214
draw cylinder {  -0.930   0.537  -3.129} {  -0.821   0.595  -3.047} radius 0.03 filled yes resolution 20
draw cone {  -0.821   0.595  -3.047} {  -0.763   0.626  -3.003} radius 0.06 resolution 20
draw color  238
draw cylinder {  -1.074   0.000  -3.129} {  -0.959   0.039  -3.023} radius 0.03 filled yes resolution 20
draw cone {  -0.959   0.039  -3.023} {  -0.898   0.061  -2.965} radius 0.06 resolution 20
draw color  238
draw cylinder {  -0.930  -0.537  -3.129} {  -0.806  -0.541  -3.027} radius 0.03 filled yes resolution 20
draw cone {  -0.806  -0.541  -3.027} {  -0.739  -0.544  -2.971} radius 0.06 resolution 20
draw color  218
draw cylinder {  -0.537  -0.930  -3.129} {  -0.409  -0.960  -3.058} radius 0.03 filled yes resolution 20
draw cone {  -0.409  -0.960  -3.058} {  -0.340  -0.976  -3.020} radius 0.06 resolution 20
draw color  175
draw cylinder {  -0.000  -1.074  -3.129} {   0.123  -1.086  -3.108} radius 0.03 filled yes resolution 20
draw cone {   0.123  -1.086  -3.108} {   0.189  -1.092  -3.097} radius 0.06 resolution 20
draw color  170
draw cylinder {   0.537  -0.930  -3.129} {   0.650  -0.898  -3.164} radius 0.03 filled yes resolution 20
draw cone {   0.650  -0.898  -3.164} {   0.711  -0.881  -3.183} radius 0.06 resolution 20
draw color  214
draw cylinder {   0.930  -0.537  -3.129} {   1.039  -0.479  -3.210} radius 0.03 filled yes resolution 20
draw cone {   1.039  -0.479  -3.210} {   1.098  -0.448  -3.254} radius 0.06 resolution 20
draw color  238
draw cylinder {   1.074  -0.000  -3.129} {   1.189   0.039  -3.235} radius 0.03 filled yes resolution 20
draw cone {   1.189   0.039  -3.235} {   1.250   0.061  -3.292} radius 0.06 resolution 20
draw color  251
draw cylinder {   0.272   0.472  -3.263} {   0.436   0.489  -3.300} radius 0.03 filled yes resolution 20
draw cone {   0.436   0.489  -3.300} {   0.524   0.498  -3.321} radius 0.06 resolution 20
draw color  243
draw cylinder {  -0.272   0.472  -3.263} {  -0.112   0.505  -3.244} radius 0.03 filled yes resolution 20
draw cone {  -0.112   0.505  -3.244} {  -0.026   0.523  -3.234} radius 0.06 resolution 20
draw color  259
draw cylinder {  -0.544   0.000  -3.263} {  -0.384   0.035  -3.207} radius 0.03 filled yes resolution 20
draw cone {  -0.384   0.035  -3.207} {  -0.298   0.054  -3.177} radius 0.06 resolution 20
draw color  251
draw cylinder {  -0.272  -0.472  -3.263} {  -0.108  -0.454  -3.225} radius 0.03 filled yes resolution 20
draw cone {  -0.108  -0.454  -3.225} {  -0.020  -0.445  -3.205} radius 0.06 resolution 20
draw color  243
draw cylinder {   0.272  -0.472  -3.263} {   0.432  -0.438  -3.282} radius 0.03 filled yes resolution 20
draw cone {   0.432  -0.438  -3.282} {   0.518  -0.421  -3.292} radius 0.06 resolution 20
draw color  259
draw cylinder {   0.544  -0.000  -3.263} {   0.705   0.035  -3.319} radius 0.03 filled yes resolution 20
draw cone {   0.705   0.035  -3.319} {   0.791   0.054  -3.349} radius 0.06 resolution 20
draw color  269
draw cylinder {   0.000  -0.000  -3.308} {   0.176   0.033  -3.308} radius 0.03 filled yes resolution 20
draw cone {   0.176   0.033  -3.308} {   0.271   0.052  -3.308} radius 0.06 resolution 20
