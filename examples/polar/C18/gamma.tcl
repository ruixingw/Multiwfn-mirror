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
draw cylinder {   0.000   0.000   5.908} {   0.000   0.000   6.001} radius 0.03 filled yes resolution 20
draw cone {   0.000   0.000   6.001} {   0.000   0.000   6.051} radius 0.06 resolution 20
draw color   59
draw cylinder {   0.462   0.800   5.835} {   0.485   0.839   5.931} radius 0.03 filled yes resolution 20
draw cone {   0.485   0.839   5.931} {   0.497   0.860   5.983} radius 0.06 resolution 20
draw color   59
draw cylinder {  -0.462   0.800   5.835} {  -0.485   0.839   5.931} radius 0.03 filled yes resolution 20
draw cone {  -0.485   0.839   5.931} {  -0.497   0.860   5.983} radius 0.06 resolution 20
draw color   59
draw cylinder {  -0.924   0.000   5.835} {  -0.969   0.000   5.931} radius 0.03 filled yes resolution 20
draw cone {  -0.969   0.000   5.931} {  -0.993   0.000   5.983} radius 0.06 resolution 20
draw color   59
draw cylinder {  -0.462  -0.800   5.835} {  -0.485  -0.839   5.931} radius 0.03 filled yes resolution 20
draw cone {  -0.485  -0.839   5.931} {  -0.497  -0.860   5.983} radius 0.06 resolution 20
draw color   59
draw cylinder {   0.462  -0.800   5.835} {   0.485  -0.839   5.931} radius 0.03 filled yes resolution 20
draw cone {   0.485  -0.839   5.931} {   0.497  -0.860   5.983} radius 0.06 resolution 20
draw color   59
draw cylinder {   0.924  -0.000   5.835} {   0.969  -0.000   5.931} radius 0.03 filled yes resolution 20
draw cone {   0.969  -0.000   5.931} {   0.993  -0.000   5.983} radius 0.06 resolution 20
draw color   93
draw cylinder {   1.581   0.913   5.619} {   1.683   0.972   5.722} radius 0.03 filled yes resolution 20
draw cone {   1.683   0.972   5.722} {   1.737   1.003   5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {   0.913   1.581   5.619} {   0.972   1.683   5.722} radius 0.03 filled yes resolution 20
draw cone {   0.972   1.683   5.722} {   1.003   1.738   5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {   0.000   1.826   5.619} {   0.000   1.943   5.722} radius 0.03 filled yes resolution 20
draw cone {   0.000   1.943   5.722} {   0.000   2.006   5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {  -0.913   1.581   5.619} {  -0.972   1.683   5.722} radius 0.03 filled yes resolution 20
draw cone {  -0.972   1.683   5.722} {  -1.003   1.738   5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {  -1.581   0.913   5.619} {  -1.683   0.972   5.722} radius 0.03 filled yes resolution 20
draw cone {  -1.683   0.972   5.722} {  -1.737   1.003   5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {  -1.826   0.000   5.619} {  -1.943   0.000   5.722} radius 0.03 filled yes resolution 20
draw cone {  -1.943   0.000   5.722} {  -2.006   0.000   5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {  -1.581  -0.913   5.619} {  -1.683  -0.972   5.722} radius 0.03 filled yes resolution 20
draw cone {  -1.683  -0.972   5.722} {  -1.737  -1.003   5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {  -0.913  -1.581   5.619} {  -0.972  -1.683   5.722} radius 0.03 filled yes resolution 20
draw cone {  -0.972  -1.683   5.722} {  -1.003  -1.738   5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {  -0.000  -1.826   5.619} {  -0.000  -1.943   5.722} radius 0.03 filled yes resolution 20
draw cone {  -0.000  -1.943   5.722} {  -0.000  -2.006   5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {   0.913  -1.581   5.619} {   0.972  -1.683   5.722} radius 0.03 filled yes resolution 20
draw cone {   0.972  -1.683   5.722} {   1.003  -1.738   5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {   1.581  -0.913   5.619} {   1.683  -0.972   5.722} radius 0.03 filled yes resolution 20
draw cone {   1.683  -0.972   5.722} {   1.737  -1.003   5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {   1.826  -0.000   5.619} {   1.943  -0.000   5.722} radius 0.03 filled yes resolution 20
draw cone {   1.943  -0.000   5.722} {   2.006  -0.000   5.778} radius 0.06 resolution 20
draw color  166
draw cylinder {   2.520   0.917   5.264} {   2.744   0.999   5.377} radius 0.03 filled yes resolution 20
draw cone {   2.744   0.999   5.377} {   2.864   1.042   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   2.055   1.724   5.264} {   2.237   1.877   5.377} radius 0.03 filled yes resolution 20
draw cone {   2.237   1.877   5.377} {   2.335   1.959   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   1.341   2.323   5.264} {   1.460   2.529   5.377} radius 0.03 filled yes resolution 20
draw cone {   1.460   2.529   5.377} {   1.524   2.640   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   0.466   2.641   5.264} {   0.507   2.876   5.377} radius 0.03 filled yes resolution 20
draw cone {   0.507   2.876   5.377} {   0.529   3.002   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -0.466   2.641   5.264} {  -0.507   2.876   5.377} radius 0.03 filled yes resolution 20
draw cone {  -0.507   2.876   5.377} {  -0.529   3.002   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -1.341   2.323   5.264} {  -1.460   2.529   5.377} radius 0.03 filled yes resolution 20
draw cone {  -1.460   2.529   5.377} {  -1.524   2.640   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -2.055   1.724   5.264} {  -2.237   1.877   5.377} radius 0.03 filled yes resolution 20
draw cone {  -2.237   1.877   5.377} {  -2.335   1.959   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -2.520   0.917   5.264} {  -2.744   0.999   5.377} radius 0.03 filled yes resolution 20
draw cone {  -2.744   0.999   5.377} {  -2.864   1.042   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -2.682   0.000   5.264} {  -2.920   0.000   5.377} radius 0.03 filled yes resolution 20
draw cone {  -2.920   0.000   5.377} {  -3.048   0.000   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -2.520  -0.917   5.264} {  -2.744  -0.999   5.377} radius 0.03 filled yes resolution 20
draw cone {  -2.744  -0.999   5.377} {  -2.864  -1.042   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -2.055  -1.724   5.264} {  -2.237  -1.877   5.377} radius 0.03 filled yes resolution 20
draw cone {  -2.237  -1.877   5.377} {  -2.335  -1.959   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -1.341  -2.323   5.264} {  -1.460  -2.529   5.377} radius 0.03 filled yes resolution 20
draw cone {  -1.460  -2.529   5.377} {  -1.524  -2.640   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -0.466  -2.641   5.264} {  -0.507  -2.876   5.377} radius 0.03 filled yes resolution 20
draw cone {  -0.507  -2.876   5.377} {  -0.529  -3.002   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   0.466  -2.641   5.264} {   0.507  -2.876   5.377} radius 0.03 filled yes resolution 20
draw cone {   0.507  -2.876   5.377} {   0.529  -3.002   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   1.341  -2.323   5.264} {   1.460  -2.529   5.377} radius 0.03 filled yes resolution 20
draw cone {   1.460  -2.529   5.377} {   1.524  -2.640   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   2.055  -1.724   5.264} {   2.237  -1.877   5.377} radius 0.03 filled yes resolution 20
draw cone {   2.237  -1.877   5.377} {   2.335  -1.959   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   2.520  -0.917   5.264} {   2.744  -0.999   5.377} radius 0.03 filled yes resolution 20
draw cone {   2.744  -0.999   5.377} {   2.864  -1.042   5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   2.682  -0.000   5.264} {   2.920  -0.000   5.377} radius 0.03 filled yes resolution 20
draw cone {   2.920  -0.000   5.377} {   3.048  -0.000   5.438} radius 0.06 resolution 20
draw color  281
draw cylinder {   3.344   0.937   4.780} {   3.743   1.049   4.900} radius 0.03 filled yes resolution 20
draw cone {   3.743   1.049   4.900} {   3.957   1.109   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   2.967   1.804   4.780} {   3.321   2.019   4.900} radius 0.03 filled yes resolution 20
draw cone {   3.321   2.019   4.900} {   3.511   2.135   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   2.370   2.538   4.780} {   2.653   2.841   4.900} radius 0.03 filled yes resolution 20
draw cone {   2.653   2.841   4.900} {   2.805   3.004   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   1.598   3.083   4.780} {   1.788   3.451   4.900} radius 0.03 filled yes resolution 20
draw cone {   1.788   3.451   4.900} {   1.891   3.649   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   0.707   3.400   4.780} {   0.791   3.805   4.900} radius 0.03 filled yes resolution 20
draw cone {   0.791   3.805   4.900} {   0.836   4.024   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -0.237   3.464   4.780} {  -0.265   3.878   4.900} radius 0.03 filled yes resolution 20
draw cone {  -0.265   3.878   4.900} {  -0.280   4.100   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -1.163   3.272   4.780} {  -1.302   3.662   4.900} radius 0.03 filled yes resolution 20
draw cone {  -1.302   3.662   4.900} {  -1.376   3.872   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -2.003   2.837   4.780} {  -2.241   3.175   4.900} radius 0.03 filled yes resolution 20
draw cone {  -2.241   3.175   4.900} {  -2.370   3.358   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -2.694   2.191   4.780} {  -3.015   2.453   4.900} radius 0.03 filled yes resolution 20
draw cone {  -3.015   2.453   4.900} {  -3.188   2.594   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -3.185   1.383   4.780} {  -3.565   1.548   4.900} radius 0.03 filled yes resolution 20
draw cone {  -3.565   1.548   4.900} {  -3.769   1.637   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -3.440   0.473   4.780} {  -3.851   0.529   4.900} radius 0.03 filled yes resolution 20
draw cone {  -3.851   0.529   4.900} {  -4.071   0.560   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -3.440  -0.473   4.780} {  -3.851  -0.529   4.900} radius 0.03 filled yes resolution 20
draw cone {  -3.851  -0.529   4.900} {  -4.071  -0.560   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -3.185  -1.383   4.780} {  -3.565  -1.548   4.900} radius 0.03 filled yes resolution 20
draw cone {  -3.565  -1.548   4.900} {  -3.769  -1.637   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -2.694  -2.191   4.780} {  -3.015  -2.453   4.900} radius 0.03 filled yes resolution 20
draw cone {  -3.015  -2.453   4.900} {  -3.188  -2.594   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -2.003  -2.837   4.780} {  -2.241  -3.175   4.900} radius 0.03 filled yes resolution 20
draw cone {  -2.241  -3.175   4.900} {  -2.370  -3.358   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -1.163  -3.272   4.780} {  -1.302  -3.662   4.900} radius 0.03 filled yes resolution 20
draw cone {  -1.302  -3.662   4.900} {  -1.376  -3.872   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -0.237  -3.464   4.780} {  -0.265  -3.878   4.900} radius 0.03 filled yes resolution 20
draw cone {  -0.265  -3.878   4.900} {  -0.280  -4.100   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   0.707  -3.400   4.780} {   0.791  -3.805   4.900} radius 0.03 filled yes resolution 20
draw cone {   0.791  -3.805   4.900} {   0.836  -4.024   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   1.598  -3.083   4.780} {   1.788  -3.451   4.900} radius 0.03 filled yes resolution 20
draw cone {   1.788  -3.451   4.900} {   1.891  -3.649   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   2.370  -2.538   4.780} {   2.653  -2.841   4.900} radius 0.03 filled yes resolution 20
draw cone {   2.653  -2.841   4.900} {   2.805  -3.004   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   2.967  -1.804   4.780} {   3.321  -2.019   4.900} radius 0.03 filled yes resolution 20
draw cone {   3.321  -2.019   4.900} {   3.511  -2.135   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   3.344  -0.937   4.780} {   3.743  -1.049   4.900} radius 0.03 filled yes resolution 20
draw cone {   3.743  -1.049   4.900} {   3.957  -1.109   4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   3.473  -0.000   4.780} {   3.887  -0.000   4.900} radius 0.03 filled yes resolution 20
draw cone {   3.887  -0.000   4.900} {   4.110  -0.000   4.966} radius 0.06 resolution 20
draw color  432
draw cylinder {   4.073   0.930   4.177} {   4.697   1.072   4.301} radius 0.03 filled yes resolution 20
draw cone {   4.697   1.072   4.301} {   5.033   1.149   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   3.764   1.813   4.177} {   4.341   2.090   4.301} radius 0.03 filled yes resolution 20
draw cone {   4.341   2.090   4.301} {   4.651   2.240   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   3.266   2.605   4.177} {   3.767   3.004   4.301} radius 0.03 filled yes resolution 20
draw cone {   3.767   3.004   4.301} {   4.036   3.219   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   2.605   3.266   4.177} {   3.004   3.767   4.301} radius 0.03 filled yes resolution 20
draw cone {   3.004   3.767   4.301} {   3.219   4.036   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   1.813   3.764   4.177} {   2.090   4.341   4.301} radius 0.03 filled yes resolution 20
draw cone {   2.090   4.341   4.301} {   2.240   4.651   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   0.930   4.073   4.177} {   1.072   4.697   4.301} radius 0.03 filled yes resolution 20
draw cone {   1.072   4.697   4.301} {   1.149   5.033   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   0.000   4.177   4.177} {   0.000   4.818   4.301} radius 0.03 filled yes resolution 20
draw cone {   0.000   4.818   4.301} {   0.000   5.162   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -0.930   4.073   4.177} {  -1.072   4.697   4.301} radius 0.03 filled yes resolution 20
draw cone {  -1.072   4.697   4.301} {  -1.149   5.033   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -1.813   3.764   4.177} {  -2.090   4.341   4.301} radius 0.03 filled yes resolution 20
draw cone {  -2.090   4.341   4.301} {  -2.240   4.651   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -2.605   3.266   4.177} {  -3.004   3.767   4.301} radius 0.03 filled yes resolution 20
draw cone {  -3.004   3.767   4.301} {  -3.219   4.036   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -3.266   2.605   4.177} {  -3.767   3.004   4.301} radius 0.03 filled yes resolution 20
draw cone {  -3.767   3.004   4.301} {  -4.036   3.219   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -3.764   1.813   4.177} {  -4.341   2.090   4.301} radius 0.03 filled yes resolution 20
draw cone {  -4.341   2.090   4.301} {  -4.651   2.240   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -4.073   0.930   4.177} {  -4.697   1.072   4.301} radius 0.03 filled yes resolution 20
draw cone {  -4.697   1.072   4.301} {  -5.033   1.149   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -4.177   0.000   4.177} {  -4.818   0.000   4.301} radius 0.03 filled yes resolution 20
draw cone {  -4.818   0.000   4.301} {  -5.162   0.000   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -4.073  -0.930   4.177} {  -4.697  -1.072   4.301} radius 0.03 filled yes resolution 20
draw cone {  -4.697  -1.072   4.301} {  -5.033  -1.149   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -3.764  -1.813   4.177} {  -4.341  -2.090   4.301} radius 0.03 filled yes resolution 20
draw cone {  -4.341  -2.090   4.301} {  -4.651  -2.240   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -3.266  -2.605   4.177} {  -3.767  -3.004   4.301} radius 0.03 filled yes resolution 20
draw cone {  -3.767  -3.004   4.301} {  -4.036  -3.219   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -2.605  -3.266   4.177} {  -3.004  -3.767   4.301} radius 0.03 filled yes resolution 20
draw cone {  -3.004  -3.767   4.301} {  -3.219  -4.036   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -1.813  -3.764   4.177} {  -2.090  -4.341   4.301} radius 0.03 filled yes resolution 20
draw cone {  -2.090  -4.341   4.301} {  -2.240  -4.651   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -0.930  -4.073   4.177} {  -1.072  -4.697   4.301} radius 0.03 filled yes resolution 20
draw cone {  -1.072  -4.697   4.301} {  -1.149  -5.033   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -0.000  -4.177   4.177} {  -0.000  -4.818   4.301} radius 0.03 filled yes resolution 20
draw cone {  -0.000  -4.818   4.301} {  -0.000  -5.162   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   0.930  -4.073   4.177} {   1.072  -4.697   4.301} radius 0.03 filled yes resolution 20
draw cone {   1.072  -4.697   4.301} {   1.149  -5.033   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   1.813  -3.764   4.177} {   2.090  -4.341   4.301} radius 0.03 filled yes resolution 20
draw cone {   2.090  -4.341   4.301} {   2.240  -4.651   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   2.605  -3.266   4.177} {   3.004  -3.767   4.301} radius 0.03 filled yes resolution 20
draw cone {   3.004  -3.767   4.301} {   3.219  -4.036   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   3.266  -2.605   4.177} {   3.767  -3.004   4.301} radius 0.03 filled yes resolution 20
draw cone {   3.767  -3.004   4.301} {   4.036  -3.219   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   3.764  -1.813   4.177} {   4.341  -2.090   4.301} radius 0.03 filled yes resolution 20
draw cone {   4.341  -2.090   4.301} {   4.651  -2.240   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   4.073  -0.930   4.177} {   4.697  -1.072   4.301} radius 0.03 filled yes resolution 20
draw cone {   4.697  -1.072   4.301} {   5.033  -1.149   4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   4.177  -0.000   4.177} {   4.818  -0.000   4.301} radius 0.03 filled yes resolution 20
draw cone {   4.818  -0.000   4.301} {   5.162  -0.000   4.367} radius 0.06 resolution 20
draw color  604
draw cylinder {   4.688   0.932   3.473} {   5.565   1.107   3.590} radius 0.03 filled yes resolution 20
draw cone {   5.565   1.107   3.590} {   6.038   1.201   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   4.416   1.829   3.473} {   5.242   2.171   3.590} radius 0.03 filled yes resolution 20
draw cone {   5.242   2.171   3.590} {   5.687   2.356   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   3.974   2.655   3.473} {   4.718   3.152   3.590} radius 0.03 filled yes resolution 20
draw cone {   4.718   3.152   3.590} {   5.119   3.420   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   3.380   3.380   3.473} {   4.012   4.012   3.590} radius 0.03 filled yes resolution 20
draw cone {   4.012   4.012   3.590} {   4.353   4.353   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   2.655   3.974   3.473} {   3.152   4.718   3.590} radius 0.03 filled yes resolution 20
draw cone {   3.152   4.718   3.590} {   3.420   5.119   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   1.829   4.416   3.473} {   2.171   5.242   3.590} radius 0.03 filled yes resolution 20
draw cone {   2.171   5.242   3.590} {   2.356   5.687   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   0.932   4.688   3.473} {   1.107   5.565   3.590} radius 0.03 filled yes resolution 20
draw cone {   1.107   5.565   3.590} {   1.201   6.038   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   0.000   4.780   3.473} {   0.000   5.674   3.590} radius 0.03 filled yes resolution 20
draw cone {   0.000   5.674   3.590} {   0.000   6.156   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -0.932   4.688   3.473} {  -1.107   5.565   3.590} radius 0.03 filled yes resolution 20
draw cone {  -1.107   5.565   3.590} {  -1.201   6.038   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -1.829   4.416   3.473} {  -2.171   5.242   3.590} radius 0.03 filled yes resolution 20
draw cone {  -2.171   5.242   3.590} {  -2.356   5.687   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -2.655   3.974   3.473} {  -3.152   4.718   3.590} radius 0.03 filled yes resolution 20
draw cone {  -3.152   4.718   3.590} {  -3.420   5.119   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -3.380   3.380   3.473} {  -4.012   4.012   3.590} radius 0.03 filled yes resolution 20
draw cone {  -4.012   4.012   3.590} {  -4.353   4.353   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -3.974   2.655   3.473} {  -4.718   3.152   3.590} radius 0.03 filled yes resolution 20
draw cone {  -4.718   3.152   3.590} {  -5.119   3.420   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -4.416   1.829   3.473} {  -5.242   2.171   3.590} radius 0.03 filled yes resolution 20
draw cone {  -5.242   2.171   3.590} {  -5.687   2.356   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -4.688   0.932   3.473} {  -5.565   1.107   3.590} radius 0.03 filled yes resolution 20
draw cone {  -5.565   1.107   3.590} {  -6.038   1.201   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -4.780   0.000   3.473} {  -5.674   0.000   3.590} radius 0.03 filled yes resolution 20
draw cone {  -5.674   0.000   3.590} {  -6.156   0.000   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -4.688  -0.932   3.473} {  -5.565  -1.107   3.590} radius 0.03 filled yes resolution 20
draw cone {  -5.565  -1.107   3.590} {  -6.038  -1.201   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -4.416  -1.829   3.473} {  -5.242  -2.171   3.590} radius 0.03 filled yes resolution 20
draw cone {  -5.242  -2.171   3.590} {  -5.687  -2.356   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -3.974  -2.655   3.473} {  -4.718  -3.152   3.590} radius 0.03 filled yes resolution 20
draw cone {  -4.718  -3.152   3.590} {  -5.119  -3.420   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -3.380  -3.380   3.473} {  -4.012  -4.012   3.590} radius 0.03 filled yes resolution 20
draw cone {  -4.012  -4.012   3.590} {  -4.353  -4.353   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -2.655  -3.974   3.473} {  -3.152  -4.718   3.590} radius 0.03 filled yes resolution 20
draw cone {  -3.152  -4.718   3.590} {  -3.420  -5.119   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -1.829  -4.416   3.473} {  -2.171  -5.242   3.590} radius 0.03 filled yes resolution 20
draw cone {  -2.171  -5.242   3.590} {  -2.356  -5.687   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -0.932  -4.688   3.473} {  -1.107  -5.565   3.590} radius 0.03 filled yes resolution 20
draw cone {  -1.107  -5.565   3.590} {  -1.201  -6.038   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -0.000  -4.780   3.473} {  -0.000  -5.674   3.590} radius 0.03 filled yes resolution 20
draw cone {  -0.000  -5.674   3.590} {  -0.000  -6.156   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   0.932  -4.688   3.473} {   1.107  -5.565   3.590} radius 0.03 filled yes resolution 20
draw cone {   1.107  -5.565   3.590} {   1.201  -6.038   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   1.829  -4.416   3.473} {   2.171  -5.242   3.590} radius 0.03 filled yes resolution 20
draw cone {   2.171  -5.242   3.590} {   2.356  -5.687   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   2.655  -3.974   3.473} {   3.152  -4.718   3.590} radius 0.03 filled yes resolution 20
draw cone {   3.152  -4.718   3.590} {   3.420  -5.119   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   3.380  -3.380   3.473} {   4.012  -4.012   3.590} radius 0.03 filled yes resolution 20
draw cone {   4.012  -4.012   3.590} {   4.353  -4.353   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   3.974  -2.655   3.473} {   4.718  -3.152   3.590} radius 0.03 filled yes resolution 20
draw cone {   4.718  -3.152   3.590} {   5.119  -3.420   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   4.416  -1.829   3.473} {   5.242  -2.171   3.590} radius 0.03 filled yes resolution 20
draw cone {   5.242  -2.171   3.590} {   5.687  -2.356   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   4.688  -0.932   3.473} {   5.565  -1.107   3.590} radius 0.03 filled yes resolution 20
draw cone {   5.565  -1.107   3.590} {   6.038  -1.201   3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   4.780  -0.000   3.473} {   5.674  -0.000   3.590} radius 0.03 filled yes resolution 20
draw cone {   5.674  -0.000   3.590} {   6.156  -0.000   3.653} radius 0.06 resolution 20
draw color  774
draw cylinder {   5.179   0.940   2.682} {   6.308   1.145   2.783} radius 0.03 filled yes resolution 20
draw cone {   6.308   1.145   2.783} {   6.915   1.255   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   4.928   1.850   2.682} {   6.002   2.253   2.783} radius 0.03 filled yes resolution 20
draw cone {   6.002   2.253   2.783} {   6.580   2.469   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   4.519   2.700   2.682} {   5.503   3.288   2.783} radius 0.03 filled yes resolution 20
draw cone {   5.503   3.288   2.783} {   6.033   3.605   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   3.964   3.463   2.682} {   4.828   4.218   2.783} radius 0.03 filled yes resolution 20
draw cone {   4.828   4.218   2.783} {   5.293   4.624   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   3.282   4.116   2.682} {   3.997   5.012   2.783} radius 0.03 filled yes resolution 20
draw cone {   3.997   5.012   2.783} {   4.382   5.495   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   2.494   4.635   2.682} {   3.038   5.645   2.783} radius 0.03 filled yes resolution 20
draw cone {   3.038   5.645   2.783} {   3.330   6.189   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   1.627   5.006   2.682} {   1.981   6.097   2.783} radius 0.03 filled yes resolution 20
draw cone {   1.981   6.097   2.783} {   2.172   6.684   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   0.707   5.216   2.682} {   0.861   6.353   2.783} radius 0.03 filled yes resolution 20
draw cone {   0.861   6.353   2.783} {   0.943   6.964   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -0.236   5.259   2.682} {  -0.288   6.404   2.783} radius 0.03 filled yes resolution 20
draw cone {  -0.288   6.404   2.783} {  -0.315   7.021   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -1.171   5.132   2.682} {  -1.426   6.250   2.783} radius 0.03 filled yes resolution 20
draw cone {  -1.426   6.250   2.783} {  -1.564   6.852   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -2.069   4.840   2.682} {  -2.520   5.895   2.783} radius 0.03 filled yes resolution 20
draw cone {  -2.520   5.895   2.783} {  -2.762   6.463   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -2.900   4.393   2.682} {  -3.532   5.350   2.783} radius 0.03 filled yes resolution 20
draw cone {  -3.532   5.350   2.783} {  -3.872   5.865   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -3.638   3.805   2.682} {  -4.430   4.634   2.783} radius 0.03 filled yes resolution 20
draw cone {  -4.430   4.634   2.783} {  -4.857   5.080   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -4.259   3.094   2.682} {  -5.186   3.768   2.783} radius 0.03 filled yes resolution 20
draw cone {  -5.186   3.768   2.783} {  -5.686   4.131   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -4.743   2.284   2.682} {  -5.776   2.781   2.783} radius 0.03 filled yes resolution 20
draw cone {  -5.776   2.781   2.783} {  -6.332   3.049   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -5.074   1.400   2.682} {  -6.180   1.705   2.783} radius 0.03 filled yes resolution 20
draw cone {  -6.180   1.705   2.783} {  -6.775   1.870   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -5.243   0.472   2.682} {  -6.385   0.575   2.783} radius 0.03 filled yes resolution 20
draw cone {  -6.385   0.575   2.783} {  -7.000   0.630   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -5.243  -0.472   2.682} {  -6.385  -0.575   2.783} radius 0.03 filled yes resolution 20
draw cone {  -6.385  -0.575   2.783} {  -7.000  -0.630   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -5.074  -1.400   2.682} {  -6.180  -1.705   2.783} radius 0.03 filled yes resolution 20
draw cone {  -6.180  -1.705   2.783} {  -6.775  -1.870   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -4.743  -2.284   2.682} {  -5.776  -2.781   2.783} radius 0.03 filled yes resolution 20
draw cone {  -5.776  -2.781   2.783} {  -6.332  -3.049   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -4.259  -3.094   2.682} {  -5.186  -3.768   2.783} radius 0.03 filled yes resolution 20
draw cone {  -5.186  -3.768   2.783} {  -5.686  -4.131   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -3.638  -3.805   2.682} {  -4.430  -4.634   2.783} radius 0.03 filled yes resolution 20
draw cone {  -4.430  -4.634   2.783} {  -4.857  -5.080   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -2.900  -4.393   2.682} {  -3.532  -5.350   2.783} radius 0.03 filled yes resolution 20
draw cone {  -3.532  -5.350   2.783} {  -3.872  -5.865   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -2.069  -4.840   2.682} {  -2.520  -5.895   2.783} radius 0.03 filled yes resolution 20
draw cone {  -2.520  -5.895   2.783} {  -2.762  -6.463   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -1.171  -5.132   2.682} {  -1.426  -6.250   2.783} radius 0.03 filled yes resolution 20
draw cone {  -1.426  -6.250   2.783} {  -1.564  -6.852   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -0.236  -5.259   2.682} {  -0.288  -6.404   2.783} radius 0.03 filled yes resolution 20
draw cone {  -0.288  -6.404   2.783} {  -0.315  -7.021   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   0.707  -5.216   2.682} {   0.861  -6.353   2.783} radius 0.03 filled yes resolution 20
draw cone {   0.861  -6.353   2.783} {   0.943  -6.964   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   1.627  -5.006   2.682} {   1.981  -6.097   2.783} radius 0.03 filled yes resolution 20
draw cone {   1.981  -6.097   2.783} {   2.172  -6.684   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   2.494  -4.635   2.682} {   3.038  -5.645   2.783} radius 0.03 filled yes resolution 20
draw cone {   3.038  -5.645   2.783} {   3.330  -6.189   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   3.282  -4.116   2.682} {   3.997  -5.012   2.783} radius 0.03 filled yes resolution 20
draw cone {   3.997  -5.012   2.783} {   4.382  -5.495   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   3.964  -3.463   2.682} {   4.828  -4.218   2.783} radius 0.03 filled yes resolution 20
draw cone {   4.828  -4.218   2.783} {   5.293  -4.624   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   4.519  -2.700   2.682} {   5.503  -3.288   2.783} radius 0.03 filled yes resolution 20
draw cone {   5.503  -3.288   2.783} {   6.033  -3.605   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   4.928  -1.850   2.682} {   6.002  -2.253   2.783} radius 0.03 filled yes resolution 20
draw cone {   6.002  -2.253   2.783} {   6.580  -2.469   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   5.179  -0.940   2.682} {   6.308  -1.145   2.783} radius 0.03 filled yes resolution 20
draw cone {   6.308  -1.145   2.783} {   6.915  -1.255   2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   5.264  -0.000   2.682} {   6.411  -0.000   2.783} radius 0.03 filled yes resolution 20
draw cone {   6.411  -0.000   2.783} {   7.028  -0.000   2.837} radius 0.06 resolution 20
draw color  919
draw cylinder {   5.542   0.925   1.826} {   6.884   1.149   1.900} radius 0.03 filled yes resolution 20
draw cone {   6.884   1.149   1.900} {   7.607   1.269   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   5.314   1.824   1.826} {   6.601   2.266   1.900} radius 0.03 filled yes resolution 20
draw cone {   6.601   2.266   1.900} {   7.294   2.504   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   4.942   2.674   1.826} {   6.138   3.322   1.900} radius 0.03 filled yes resolution 20
draw cone {   6.138   3.322   1.900} {   6.782   3.670   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   4.434   3.451   1.826} {   5.508   4.287   1.900} radius 0.03 filled yes resolution 20
draw cone {   5.508   4.287   1.900} {   6.086   4.737   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   3.805   4.134   1.826} {   4.727   5.135   1.900} radius 0.03 filled yes resolution 20
draw cone {   4.727   5.135   1.900} {   5.223   5.674   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   3.073   4.704   1.826} {   3.817   5.843   1.900} radius 0.03 filled yes resolution 20
draw cone {   3.817   5.843   1.900} {   4.218   6.456   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   2.257   5.145   1.826} {   2.804   6.391   1.900} radius 0.03 filled yes resolution 20
draw cone {   2.804   6.391   1.900} {   3.098   7.062   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   1.379   5.447   1.826} {   1.713   6.766   1.900} radius 0.03 filled yes resolution 20
draw cone {   1.713   6.766   1.900} {   1.893   7.476   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   0.464   5.600   1.826} {   0.576   6.955   1.900} radius 0.03 filled yes resolution 20
draw cone {   0.576   6.955   1.900} {   0.637   7.686   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -0.464   5.600   1.826} {  -0.576   6.955   1.900} radius 0.03 filled yes resolution 20
draw cone {  -0.576   6.955   1.900} {  -0.637   7.686   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -1.379   5.447   1.826} {  -1.713   6.766   1.900} radius 0.03 filled yes resolution 20
draw cone {  -1.713   6.766   1.900} {  -1.893   7.476   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -2.257   5.145   1.826} {  -2.804   6.391   1.900} radius 0.03 filled yes resolution 20
draw cone {  -2.804   6.391   1.900} {  -3.098   7.062   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -3.073   4.704   1.826} {  -3.817   5.843   1.900} radius 0.03 filled yes resolution 20
draw cone {  -3.817   5.843   1.900} {  -4.218   6.456   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -3.805   4.134   1.826} {  -4.727   5.135   1.900} radius 0.03 filled yes resolution 20
draw cone {  -4.727   5.135   1.900} {  -5.223   5.674   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -4.434   3.451   1.826} {  -5.508   4.287   1.900} radius 0.03 filled yes resolution 20
draw cone {  -5.508   4.287   1.900} {  -6.086   4.737   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -4.942   2.674   1.826} {  -6.138   3.322   1.900} radius 0.03 filled yes resolution 20
draw cone {  -6.138   3.322   1.900} {  -6.782   3.670   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -5.314   1.824   1.826} {  -6.601   2.266   1.900} radius 0.03 filled yes resolution 20
draw cone {  -6.601   2.266   1.900} {  -7.294   2.504   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -5.542   0.925   1.826} {  -6.884   1.149   1.900} radius 0.03 filled yes resolution 20
draw cone {  -6.884   1.149   1.900} {  -7.607   1.269   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -5.619   0.000   1.826} {  -6.979   0.000   1.900} radius 0.03 filled yes resolution 20
draw cone {  -6.979   0.000   1.900} {  -7.712   0.000   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -5.542  -0.925   1.826} {  -6.884  -1.149   1.900} radius 0.03 filled yes resolution 20
draw cone {  -6.884  -1.149   1.900} {  -7.607  -1.269   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -5.314  -1.824   1.826} {  -6.601  -2.266   1.900} radius 0.03 filled yes resolution 20
draw cone {  -6.601  -2.266   1.900} {  -7.294  -2.504   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -4.942  -2.674   1.826} {  -6.138  -3.322   1.900} radius 0.03 filled yes resolution 20
draw cone {  -6.138  -3.322   1.900} {  -6.782  -3.670   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -4.434  -3.451   1.826} {  -5.508  -4.287   1.900} radius 0.03 filled yes resolution 20
draw cone {  -5.508  -4.287   1.900} {  -6.086  -4.737   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -3.805  -4.134   1.826} {  -4.727  -5.135   1.900} radius 0.03 filled yes resolution 20
draw cone {  -4.727  -5.135   1.900} {  -5.223  -5.674   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -3.073  -4.704   1.826} {  -3.817  -5.843   1.900} radius 0.03 filled yes resolution 20
draw cone {  -3.817  -5.843   1.900} {  -4.218  -6.456   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -2.257  -5.145   1.826} {  -2.804  -6.391   1.900} radius 0.03 filled yes resolution 20
draw cone {  -2.804  -6.391   1.900} {  -3.098  -7.062   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -1.379  -5.447   1.826} {  -1.713  -6.766   1.900} radius 0.03 filled yes resolution 20
draw cone {  -1.713  -6.766   1.900} {  -1.893  -7.476   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -0.464  -5.600   1.826} {  -0.576  -6.955   1.900} radius 0.03 filled yes resolution 20
draw cone {  -0.576  -6.955   1.900} {  -0.637  -7.686   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   0.464  -5.600   1.826} {   0.576  -6.955   1.900} radius 0.03 filled yes resolution 20
draw cone {   0.576  -6.955   1.900} {   0.637  -7.686   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   1.379  -5.447   1.826} {   1.713  -6.766   1.900} radius 0.03 filled yes resolution 20
draw cone {   1.713  -6.766   1.900} {   1.893  -7.476   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   2.257  -5.145   1.826} {   2.804  -6.391   1.900} radius 0.03 filled yes resolution 20
draw cone {   2.804  -6.391   1.900} {   3.098  -7.062   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   3.073  -4.704   1.826} {   3.817  -5.843   1.900} radius 0.03 filled yes resolution 20
draw cone {   3.817  -5.843   1.900} {   4.218  -6.456   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   3.805  -4.134   1.826} {   4.727  -5.135   1.900} radius 0.03 filled yes resolution 20
draw cone {   4.727  -5.135   1.900} {   5.223  -5.674   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   4.434  -3.451   1.826} {   5.508  -4.287   1.900} radius 0.03 filled yes resolution 20
draw cone {   5.508  -4.287   1.900} {   6.086  -4.737   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   4.942  -2.674   1.826} {   6.138  -3.322   1.900} radius 0.03 filled yes resolution 20
draw cone {   6.138  -3.322   1.900} {   6.782  -3.670   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   5.314  -1.824   1.826} {   6.601  -2.266   1.900} radius 0.03 filled yes resolution 20
draw cone {   6.601  -2.266   1.900} {   7.294  -2.504   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   5.542  -0.925   1.826} {   6.884  -1.149   1.900} radius 0.03 filled yes resolution 20
draw cone {   6.884  -1.149   1.900} {   7.607  -1.269   1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   5.619  -0.000   1.826} {   6.979  -0.000   1.900} radius 0.03 filled yes resolution 20
draw cone {   6.979  -0.000   1.900} {   7.712  -0.000   1.940} radius 0.06 resolution 20
draw color 1016
draw cylinder {   5.760   0.936   0.924} {   7.244   1.177   0.964} radius 0.03 filled yes resolution 20
draw cone {   7.244   1.177   0.964} {   8.043   1.307   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   5.535   1.848   0.924} {   6.961   2.324   0.964} radius 0.03 filled yes resolution 20
draw cone {   6.961   2.324   0.964} {   7.730   2.581   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   5.167   2.712   0.924} {   6.498   3.411   0.964} radius 0.03 filled yes resolution 20
draw cone {   6.498   3.411   0.964} {   7.216   3.787   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   4.665   3.505   0.924} {   5.867   4.409   0.964} radius 0.03 filled yes resolution 20
draw cone {   5.867   4.409   0.964} {   6.515   4.895   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   4.042   4.208   0.924} {   5.084   5.293   0.964} radius 0.03 filled yes resolution 20
draw cone {   5.084   5.293   0.964} {   5.645   5.877   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   3.315   4.802   0.924} {   4.169   6.040   0.964} radius 0.03 filled yes resolution 20
draw cone {   4.169   6.040   0.964} {   4.629   6.707   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   2.501   5.272   0.924} {   3.146   6.631   0.964} radius 0.03 filled yes resolution 20
draw cone {   3.146   6.631   0.964} {   3.493   7.362   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   1.623   5.605   0.924} {   2.042   7.049   0.964} radius 0.03 filled yes resolution 20
draw cone {   2.042   7.049   0.964} {   2.267   7.827   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   0.703   5.793   0.924} {   0.885   7.286   0.964} radius 0.03 filled yes resolution 20
draw cone {   0.885   7.286   0.964} {   0.982   8.090   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -0.235   5.830   0.924} {  -0.296   7.333   0.964} radius 0.03 filled yes resolution 20
draw cone {  -0.296   7.333   0.964} {  -0.328   8.142   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -1.167   5.717   0.924} {  -1.468   7.191   0.964} radius 0.03 filled yes resolution 20
draw cone {  -1.468   7.191   0.964} {  -1.630   7.984   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -2.069   5.456   0.924} {  -2.602   6.862   0.964} radius 0.03 filled yes resolution 20
draw cone {  -2.602   6.862   0.964} {  -2.890   7.620   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -2.918   5.053   0.924} {  -3.670   6.356   0.964} radius 0.03 filled yes resolution 20
draw cone {  -3.670   6.356   0.964} {  -4.074   7.057   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -3.690   4.520   0.924} {  -4.642   5.685   0.964} radius 0.03 filled yes resolution 20
draw cone {  -4.642   5.685   0.964} {  -5.154   6.312   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -4.368   3.869   0.924} {  -5.493   4.867   0.964} radius 0.03 filled yes resolution 20
draw cone {  -5.493   4.867   0.964} {  -6.100   5.404   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -4.932   3.119   0.924} {  -6.203   3.923   0.964} radius 0.03 filled yes resolution 20
draw cone {  -6.203   3.923   0.964} {  -6.887   4.355   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -5.368   2.287   0.924} {  -6.752   2.877   0.964} radius 0.03 filled yes resolution 20
draw cone {  -6.752   2.877   0.964} {  -7.497   3.194   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -5.666   1.396   0.924} {  -7.126   1.756   0.964} radius 0.03 filled yes resolution 20
draw cone {  -7.126   1.756   0.964} {  -7.912   1.950   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -5.816   0.470   0.924} {  -7.315   0.591   0.964} radius 0.03 filled yes resolution 20
draw cone {  -7.315   0.591   0.964} {  -8.122   0.656   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -5.816  -0.470   0.924} {  -7.315  -0.591   0.964} radius 0.03 filled yes resolution 20
draw cone {  -7.315  -0.591   0.964} {  -8.122  -0.656   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -5.666  -1.396   0.924} {  -7.126  -1.756   0.964} radius 0.03 filled yes resolution 20
draw cone {  -7.126  -1.756   0.964} {  -7.912  -1.950   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -5.368  -2.287   0.924} {  -6.752  -2.877   0.964} radius 0.03 filled yes resolution 20
draw cone {  -6.752  -2.877   0.964} {  -7.497  -3.194   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -4.932  -3.119   0.924} {  -6.203  -3.923   0.964} radius 0.03 filled yes resolution 20
draw cone {  -6.203  -3.923   0.964} {  -6.887  -4.355   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -4.368  -3.869   0.924} {  -5.493  -4.867   0.964} radius 0.03 filled yes resolution 20
draw cone {  -5.493  -4.867   0.964} {  -6.100  -5.404   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -3.690  -4.520   0.924} {  -4.642  -5.685   0.964} radius 0.03 filled yes resolution 20
draw cone {  -4.642  -5.685   0.964} {  -5.154  -6.312   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -2.918  -5.053   0.924} {  -3.670  -6.356   0.964} radius 0.03 filled yes resolution 20
draw cone {  -3.670  -6.356   0.964} {  -4.074  -7.057   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -2.069  -5.456   0.924} {  -2.602  -6.862   0.964} radius 0.03 filled yes resolution 20
draw cone {  -2.602  -6.862   0.964} {  -2.890  -7.620   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -1.167  -5.717   0.924} {  -1.468  -7.191   0.964} radius 0.03 filled yes resolution 20
draw cone {  -1.468  -7.191   0.964} {  -1.630  -7.984   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -0.235  -5.830   0.924} {  -0.296  -7.333   0.964} radius 0.03 filled yes resolution 20
draw cone {  -0.296  -7.333   0.964} {  -0.328  -8.142   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   0.703  -5.793   0.924} {   0.885  -7.286   0.964} radius 0.03 filled yes resolution 20
draw cone {   0.885  -7.286   0.964} {   0.982  -8.090   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   1.623  -5.605   0.924} {   2.042  -7.049   0.964} radius 0.03 filled yes resolution 20
draw cone {   2.042  -7.049   0.964} {   2.267  -7.827   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   2.501  -5.272   0.924} {   3.146  -6.631   0.964} radius 0.03 filled yes resolution 20
draw cone {   3.146  -6.631   0.964} {   3.493  -7.362   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   3.315  -4.802   0.924} {   4.169  -6.040   0.964} radius 0.03 filled yes resolution 20
draw cone {   4.169  -6.040   0.964} {   4.629  -6.707   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   4.042  -4.208   0.924} {   5.084  -5.293   0.964} radius 0.03 filled yes resolution 20
draw cone {   5.084  -5.293   0.964} {   5.645  -5.877   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   4.665  -3.505   0.924} {   5.867  -4.409   0.964} radius 0.03 filled yes resolution 20
draw cone {   5.867  -4.409   0.964} {   6.515  -4.895   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   5.167  -2.712   0.924} {   6.498  -3.411   0.964} radius 0.03 filled yes resolution 20
draw cone {   6.498  -3.411   0.964} {   7.216  -3.787   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   5.535  -1.848   0.924} {   6.961  -2.324   0.964} radius 0.03 filled yes resolution 20
draw cone {   6.961  -2.324   0.964} {   7.730  -2.581   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   5.760  -0.936   0.924} {   7.244  -1.177   0.964} radius 0.03 filled yes resolution 20
draw cone {   7.244  -1.177   0.964} {   8.043  -1.307   0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   5.835  -0.000   0.924} {   7.339  -0.000   0.964} radius 0.03 filled yes resolution 20
draw cone {   7.339  -0.000   0.964} {   8.149  -0.000   0.985} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.835   0.924   0.000} {   7.370   1.167   0.000} radius 0.03 filled yes resolution 20
draw cone {   7.370   1.167   0.000} {   8.197   1.298   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.619   1.826   0.000} {   7.097   2.306   0.000} radius 0.03 filled yes resolution 20
draw cone {   7.097   2.306   0.000} {   7.893   2.565   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.264   2.682   0.000} {   6.649   3.388   0.000} radius 0.03 filled yes resolution 20
draw cone {   6.649   3.388   0.000} {   7.395   3.768   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   4.780   3.473   0.000} {   6.037   4.386   0.000} radius 0.03 filled yes resolution 20
draw cone {   6.037   4.386   0.000} {   6.714   4.878   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   4.177   4.177   0.000} {   5.277   5.277   0.000} radius 0.03 filled yes resolution 20
draw cone {   5.277   5.277   0.000} {   5.869   5.869   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   3.473   4.780   0.000} {   4.386   6.037   0.000} radius 0.03 filled yes resolution 20
draw cone {   4.386   6.037   0.000} {   4.878   6.714   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   2.682   5.264   0.000} {   3.388   6.649   0.000} radius 0.03 filled yes resolution 20
draw cone {   3.388   6.649   0.000} {   3.768   7.395   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   1.826   5.619   0.000} {   2.306   7.097   0.000} radius 0.03 filled yes resolution 20
draw cone {   2.306   7.097   0.000} {   2.565   7.893   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   0.924   5.835   0.000} {   1.167   7.371   0.000} radius 0.03 filled yes resolution 20
draw cone {   1.167   7.371   0.000} {   1.298   8.197   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   0.000   5.908   0.000} {   0.000   7.462   0.000} radius 0.03 filled yes resolution 20
draw cone {   0.000   7.462   0.000} {   0.000   8.300   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -0.924   5.835   0.000} {  -1.167   7.371   0.000} radius 0.03 filled yes resolution 20
draw cone {  -1.167   7.371   0.000} {  -1.298   8.197   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -1.826   5.619   0.000} {  -2.306   7.097   0.000} radius 0.03 filled yes resolution 20
draw cone {  -2.306   7.097   0.000} {  -2.565   7.893   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -2.682   5.264   0.000} {  -3.388   6.649   0.000} radius 0.03 filled yes resolution 20
draw cone {  -3.388   6.649   0.000} {  -3.768   7.395   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -3.473   4.780   0.000} {  -4.386   6.037   0.000} radius 0.03 filled yes resolution 20
draw cone {  -4.386   6.037   0.000} {  -4.878   6.714   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -4.177   4.177   0.000} {  -5.277   5.277   0.000} radius 0.03 filled yes resolution 20
draw cone {  -5.277   5.277   0.000} {  -5.869   5.869   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -4.780   3.473   0.000} {  -6.037   4.386   0.000} radius 0.03 filled yes resolution 20
draw cone {  -6.037   4.386   0.000} {  -6.714   4.878   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.264   2.682   0.000} {  -6.649   3.388   0.000} radius 0.03 filled yes resolution 20
draw cone {  -6.649   3.388   0.000} {  -7.395   3.768   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.619   1.826   0.000} {  -7.097   2.306   0.000} radius 0.03 filled yes resolution 20
draw cone {  -7.097   2.306   0.000} {  -7.893   2.565   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.835   0.924   0.000} {  -7.370   1.167   0.000} radius 0.03 filled yes resolution 20
draw cone {  -7.370   1.167   0.000} {  -8.197   1.298   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.908   0.000   0.000} {  -7.462   0.000   0.000} radius 0.03 filled yes resolution 20
draw cone {  -7.462   0.000   0.000} {  -8.299   0.000   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.835  -0.924   0.000} {  -7.370  -1.167   0.000} radius 0.03 filled yes resolution 20
draw cone {  -7.370  -1.167   0.000} {  -8.197  -1.298   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.619  -1.826   0.000} {  -7.097  -2.306   0.000} radius 0.03 filled yes resolution 20
draw cone {  -7.097  -2.306   0.000} {  -7.893  -2.565   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -5.264  -2.682   0.000} {  -6.649  -3.388   0.000} radius 0.03 filled yes resolution 20
draw cone {  -6.649  -3.388   0.000} {  -7.395  -3.768   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -4.780  -3.473   0.000} {  -6.037  -4.386   0.000} radius 0.03 filled yes resolution 20
draw cone {  -6.037  -4.386   0.000} {  -6.714  -4.878   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -4.177  -4.177   0.000} {  -5.277  -5.277   0.000} radius 0.03 filled yes resolution 20
draw cone {  -5.277  -5.277   0.000} {  -5.869  -5.869   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -3.473  -4.780   0.000} {  -4.386  -6.037   0.000} radius 0.03 filled yes resolution 20
draw cone {  -4.386  -6.037   0.000} {  -4.878  -6.714   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -2.682  -5.264   0.000} {  -3.388  -6.649   0.000} radius 0.03 filled yes resolution 20
draw cone {  -3.388  -6.649   0.000} {  -3.768  -7.395   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -1.826  -5.619   0.000} {  -2.306  -7.097   0.000} radius 0.03 filled yes resolution 20
draw cone {  -2.306  -7.097   0.000} {  -2.565  -7.893   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -0.924  -5.835   0.000} {  -1.167  -7.371   0.000} radius 0.03 filled yes resolution 20
draw cone {  -1.167  -7.371   0.000} {  -1.298  -8.197   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {  -0.000  -5.908   0.000} {  -0.000  -7.462   0.000} radius 0.03 filled yes resolution 20
draw cone {  -0.000  -7.462   0.000} {  -0.000  -8.300   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   0.924  -5.835   0.000} {   1.167  -7.371   0.000} radius 0.03 filled yes resolution 20
draw cone {   1.167  -7.371   0.000} {   1.298  -8.197   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   1.826  -5.619   0.000} {   2.306  -7.097   0.000} radius 0.03 filled yes resolution 20
draw cone {   2.306  -7.097   0.000} {   2.565  -7.893   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   2.682  -5.264   0.000} {   3.388  -6.649   0.000} radius 0.03 filled yes resolution 20
draw cone {   3.388  -6.649   0.000} {   3.768  -7.395   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   3.473  -4.780   0.000} {   4.386  -6.037   0.000} radius 0.03 filled yes resolution 20
draw cone {   4.386  -6.037   0.000} {   4.878  -6.714   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   4.177  -4.177   0.000} {   5.277  -5.277   0.000} radius 0.03 filled yes resolution 20
draw cone {   5.277  -5.277   0.000} {   5.869  -5.869   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   4.780  -3.473   0.000} {   6.037  -4.386   0.000} radius 0.03 filled yes resolution 20
draw cone {   6.037  -4.386   0.000} {   6.714  -4.878   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.264  -2.682   0.000} {   6.649  -3.388   0.000} radius 0.03 filled yes resolution 20
draw cone {   6.649  -3.388   0.000} {   7.395  -3.768   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.619  -1.826   0.000} {   7.097  -2.306   0.000} radius 0.03 filled yes resolution 20
draw cone {   7.097  -2.306   0.000} {   7.893  -2.565   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.835  -0.924   0.000} {   7.370  -1.167   0.000} radius 0.03 filled yes resolution 20
draw cone {   7.370  -1.167   0.000} {   8.197  -1.298   0.000} radius 0.06 resolution 20
draw color 1050
draw cylinder {   5.908  -0.000   0.000} {   7.462  -0.000   0.000} radius 0.03 filled yes resolution 20
draw cone {   7.462  -0.000   0.000} {   8.299  -0.000   0.000} radius 0.06 resolution 20
draw color 1016
draw cylinder {   5.760   0.936  -0.924} {   7.244   1.177  -0.964} radius 0.03 filled yes resolution 20
draw cone {   7.244   1.177  -0.964} {   8.043   1.307  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   5.535   1.848  -0.924} {   6.961   2.324  -0.964} radius 0.03 filled yes resolution 20
draw cone {   6.961   2.324  -0.964} {   7.730   2.581  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   5.167   2.712  -0.924} {   6.498   3.411  -0.964} radius 0.03 filled yes resolution 20
draw cone {   6.498   3.411  -0.964} {   7.216   3.787  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   4.665   3.505  -0.924} {   5.867   4.409  -0.964} radius 0.03 filled yes resolution 20
draw cone {   5.867   4.409  -0.964} {   6.515   4.895  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   4.042   4.208  -0.924} {   5.084   5.293  -0.964} radius 0.03 filled yes resolution 20
draw cone {   5.084   5.293  -0.964} {   5.645   5.877  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   3.315   4.802  -0.924} {   4.169   6.040  -0.964} radius 0.03 filled yes resolution 20
draw cone {   4.169   6.040  -0.964} {   4.629   6.707  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   2.501   5.272  -0.924} {   3.146   6.631  -0.964} radius 0.03 filled yes resolution 20
draw cone {   3.146   6.631  -0.964} {   3.493   7.362  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   1.623   5.605  -0.924} {   2.042   7.049  -0.964} radius 0.03 filled yes resolution 20
draw cone {   2.042   7.049  -0.964} {   2.267   7.827  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   0.703   5.793  -0.924} {   0.885   7.286  -0.964} radius 0.03 filled yes resolution 20
draw cone {   0.885   7.286  -0.964} {   0.982   8.090  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -0.235   5.830  -0.924} {  -0.296   7.333  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -0.296   7.333  -0.964} {  -0.328   8.142  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -1.167   5.717  -0.924} {  -1.468   7.191  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -1.468   7.191  -0.964} {  -1.630   7.984  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -2.069   5.456  -0.924} {  -2.602   6.862  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -2.602   6.862  -0.964} {  -2.890   7.620  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -2.918   5.053  -0.924} {  -3.670   6.356  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -3.670   6.356  -0.964} {  -4.074   7.057  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -3.690   4.520  -0.924} {  -4.642   5.685  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -4.642   5.685  -0.964} {  -5.154   6.312  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -4.368   3.869  -0.924} {  -5.493   4.867  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -5.493   4.867  -0.964} {  -6.100   5.404  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -4.932   3.119  -0.924} {  -6.203   3.923  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -6.203   3.923  -0.964} {  -6.887   4.355  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -5.368   2.287  -0.924} {  -6.752   2.877  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -6.752   2.877  -0.964} {  -7.497   3.194  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -5.666   1.396  -0.924} {  -7.126   1.756  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -7.126   1.756  -0.964} {  -7.912   1.950  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -5.816   0.470  -0.924} {  -7.315   0.591  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -7.315   0.591  -0.964} {  -8.122   0.656  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -5.816  -0.470  -0.924} {  -7.315  -0.591  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -7.315  -0.591  -0.964} {  -8.122  -0.656  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -5.666  -1.396  -0.924} {  -7.126  -1.756  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -7.126  -1.756  -0.964} {  -7.912  -1.950  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -5.368  -2.287  -0.924} {  -6.752  -2.877  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -6.752  -2.877  -0.964} {  -7.497  -3.194  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -4.932  -3.119  -0.924} {  -6.203  -3.923  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -6.203  -3.923  -0.964} {  -6.887  -4.355  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -4.368  -3.869  -0.924} {  -5.493  -4.867  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -5.493  -4.867  -0.964} {  -6.100  -5.404  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -3.690  -4.520  -0.924} {  -4.642  -5.685  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -4.642  -5.685  -0.964} {  -5.154  -6.312  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -2.918  -5.053  -0.924} {  -3.670  -6.356  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -3.670  -6.356  -0.964} {  -4.074  -7.057  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -2.069  -5.456  -0.924} {  -2.602  -6.862  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -2.602  -6.862  -0.964} {  -2.890  -7.620  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -1.167  -5.717  -0.924} {  -1.468  -7.191  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -1.468  -7.191  -0.964} {  -1.630  -7.984  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {  -0.235  -5.830  -0.924} {  -0.296  -7.333  -0.964} radius 0.03 filled yes resolution 20
draw cone {  -0.296  -7.333  -0.964} {  -0.328  -8.142  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   0.703  -5.793  -0.924} {   0.885  -7.286  -0.964} radius 0.03 filled yes resolution 20
draw cone {   0.885  -7.286  -0.964} {   0.982  -8.090  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   1.623  -5.605  -0.924} {   2.042  -7.049  -0.964} radius 0.03 filled yes resolution 20
draw cone {   2.042  -7.049  -0.964} {   2.267  -7.827  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   2.501  -5.272  -0.924} {   3.146  -6.631  -0.964} radius 0.03 filled yes resolution 20
draw cone {   3.146  -6.631  -0.964} {   3.493  -7.362  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   3.315  -4.802  -0.924} {   4.169  -6.040  -0.964} radius 0.03 filled yes resolution 20
draw cone {   4.169  -6.040  -0.964} {   4.629  -6.707  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   4.042  -4.208  -0.924} {   5.084  -5.293  -0.964} radius 0.03 filled yes resolution 20
draw cone {   5.084  -5.293  -0.964} {   5.645  -5.877  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   4.665  -3.505  -0.924} {   5.867  -4.409  -0.964} radius 0.03 filled yes resolution 20
draw cone {   5.867  -4.409  -0.964} {   6.515  -4.895  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   5.167  -2.712  -0.924} {   6.498  -3.411  -0.964} radius 0.03 filled yes resolution 20
draw cone {   6.498  -3.411  -0.964} {   7.216  -3.787  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   5.535  -1.848  -0.924} {   6.961  -2.324  -0.964} radius 0.03 filled yes resolution 20
draw cone {   6.961  -2.324  -0.964} {   7.730  -2.581  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   5.760  -0.936  -0.924} {   7.244  -1.177  -0.964} radius 0.03 filled yes resolution 20
draw cone {   7.244  -1.177  -0.964} {   8.043  -1.307  -0.985} radius 0.06 resolution 20
draw color 1016
draw cylinder {   5.835  -0.000  -0.924} {   7.339  -0.000  -0.964} radius 0.03 filled yes resolution 20
draw cone {   7.339  -0.000  -0.964} {   8.149  -0.000  -0.985} radius 0.06 resolution 20
draw color  919
draw cylinder {   5.542   0.925  -1.826} {   6.884   1.149  -1.900} radius 0.03 filled yes resolution 20
draw cone {   6.884   1.149  -1.900} {   7.607   1.269  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   5.314   1.824  -1.826} {   6.601   2.266  -1.900} radius 0.03 filled yes resolution 20
draw cone {   6.601   2.266  -1.900} {   7.294   2.504  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   4.942   2.674  -1.826} {   6.138   3.322  -1.900} radius 0.03 filled yes resolution 20
draw cone {   6.138   3.322  -1.900} {   6.782   3.670  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   4.434   3.451  -1.826} {   5.508   4.287  -1.900} radius 0.03 filled yes resolution 20
draw cone {   5.508   4.287  -1.900} {   6.086   4.737  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   3.805   4.134  -1.826} {   4.727   5.135  -1.900} radius 0.03 filled yes resolution 20
draw cone {   4.727   5.135  -1.900} {   5.223   5.674  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   3.073   4.704  -1.826} {   3.817   5.843  -1.900} radius 0.03 filled yes resolution 20
draw cone {   3.817   5.843  -1.900} {   4.218   6.456  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   2.257   5.145  -1.826} {   2.804   6.391  -1.900} radius 0.03 filled yes resolution 20
draw cone {   2.804   6.391  -1.900} {   3.098   7.062  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   1.379   5.447  -1.826} {   1.713   6.766  -1.900} radius 0.03 filled yes resolution 20
draw cone {   1.713   6.766  -1.900} {   1.893   7.476  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   0.464   5.600  -1.826} {   0.576   6.955  -1.900} radius 0.03 filled yes resolution 20
draw cone {   0.576   6.955  -1.900} {   0.637   7.686  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -0.464   5.600  -1.826} {  -0.576   6.955  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -0.576   6.955  -1.900} {  -0.637   7.686  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -1.379   5.447  -1.826} {  -1.713   6.766  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -1.713   6.766  -1.900} {  -1.893   7.476  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -2.257   5.145  -1.826} {  -2.804   6.391  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -2.804   6.391  -1.900} {  -3.098   7.062  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -3.073   4.704  -1.826} {  -3.817   5.843  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -3.817   5.843  -1.900} {  -4.218   6.456  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -3.805   4.134  -1.826} {  -4.727   5.135  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -4.727   5.135  -1.900} {  -5.223   5.674  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -4.434   3.451  -1.826} {  -5.508   4.287  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -5.508   4.287  -1.900} {  -6.086   4.737  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -4.942   2.674  -1.826} {  -6.138   3.322  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -6.138   3.322  -1.900} {  -6.782   3.670  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -5.314   1.824  -1.826} {  -6.601   2.266  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -6.601   2.266  -1.900} {  -7.294   2.504  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -5.542   0.925  -1.826} {  -6.884   1.149  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -6.884   1.149  -1.900} {  -7.607   1.269  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -5.619   0.000  -1.826} {  -6.979   0.000  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -6.979   0.000  -1.900} {  -7.712   0.000  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -5.542  -0.925  -1.826} {  -6.884  -1.149  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -6.884  -1.149  -1.900} {  -7.607  -1.269  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -5.314  -1.824  -1.826} {  -6.601  -2.266  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -6.601  -2.266  -1.900} {  -7.294  -2.504  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -4.942  -2.674  -1.826} {  -6.138  -3.322  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -6.138  -3.322  -1.900} {  -6.782  -3.670  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -4.434  -3.451  -1.826} {  -5.508  -4.287  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -5.508  -4.287  -1.900} {  -6.086  -4.737  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -3.805  -4.134  -1.826} {  -4.727  -5.135  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -4.727  -5.135  -1.900} {  -5.223  -5.674  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -3.073  -4.704  -1.826} {  -3.817  -5.843  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -3.817  -5.843  -1.900} {  -4.218  -6.456  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -2.257  -5.145  -1.826} {  -2.804  -6.391  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -2.804  -6.391  -1.900} {  -3.098  -7.062  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -1.379  -5.447  -1.826} {  -1.713  -6.766  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -1.713  -6.766  -1.900} {  -1.893  -7.476  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {  -0.464  -5.600  -1.826} {  -0.576  -6.955  -1.900} radius 0.03 filled yes resolution 20
draw cone {  -0.576  -6.955  -1.900} {  -0.637  -7.686  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   0.464  -5.600  -1.826} {   0.576  -6.955  -1.900} radius 0.03 filled yes resolution 20
draw cone {   0.576  -6.955  -1.900} {   0.637  -7.686  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   1.379  -5.447  -1.826} {   1.713  -6.766  -1.900} radius 0.03 filled yes resolution 20
draw cone {   1.713  -6.766  -1.900} {   1.893  -7.476  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   2.257  -5.145  -1.826} {   2.804  -6.391  -1.900} radius 0.03 filled yes resolution 20
draw cone {   2.804  -6.391  -1.900} {   3.098  -7.062  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   3.073  -4.704  -1.826} {   3.817  -5.843  -1.900} radius 0.03 filled yes resolution 20
draw cone {   3.817  -5.843  -1.900} {   4.218  -6.456  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   3.805  -4.134  -1.826} {   4.727  -5.135  -1.900} radius 0.03 filled yes resolution 20
draw cone {   4.727  -5.135  -1.900} {   5.223  -5.674  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   4.434  -3.451  -1.826} {   5.508  -4.287  -1.900} radius 0.03 filled yes resolution 20
draw cone {   5.508  -4.287  -1.900} {   6.086  -4.737  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   4.942  -2.674  -1.826} {   6.138  -3.322  -1.900} radius 0.03 filled yes resolution 20
draw cone {   6.138  -3.322  -1.900} {   6.782  -3.670  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   5.314  -1.824  -1.826} {   6.601  -2.266  -1.900} radius 0.03 filled yes resolution 20
draw cone {   6.601  -2.266  -1.900} {   7.294  -2.504  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   5.542  -0.925  -1.826} {   6.884  -1.149  -1.900} radius 0.03 filled yes resolution 20
draw cone {   6.884  -1.149  -1.900} {   7.607  -1.269  -1.940} radius 0.06 resolution 20
draw color  919
draw cylinder {   5.619  -0.000  -1.826} {   6.979  -0.000  -1.900} radius 0.03 filled yes resolution 20
draw cone {   6.979  -0.000  -1.900} {   7.712  -0.000  -1.940} radius 0.06 resolution 20
draw color  774
draw cylinder {   5.179   0.940  -2.682} {   6.308   1.145  -2.783} radius 0.03 filled yes resolution 20
draw cone {   6.308   1.145  -2.783} {   6.915   1.255  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   4.928   1.850  -2.682} {   6.002   2.253  -2.783} radius 0.03 filled yes resolution 20
draw cone {   6.002   2.253  -2.783} {   6.580   2.469  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   4.519   2.700  -2.682} {   5.503   3.288  -2.783} radius 0.03 filled yes resolution 20
draw cone {   5.503   3.288  -2.783} {   6.033   3.605  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   3.964   3.463  -2.682} {   4.828   4.218  -2.783} radius 0.03 filled yes resolution 20
draw cone {   4.828   4.218  -2.783} {   5.293   4.624  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   3.282   4.116  -2.682} {   3.997   5.012  -2.783} radius 0.03 filled yes resolution 20
draw cone {   3.997   5.012  -2.783} {   4.382   5.495  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   2.494   4.635  -2.682} {   3.038   5.645  -2.783} radius 0.03 filled yes resolution 20
draw cone {   3.038   5.645  -2.783} {   3.330   6.189  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   1.627   5.006  -2.682} {   1.981   6.097  -2.783} radius 0.03 filled yes resolution 20
draw cone {   1.981   6.097  -2.783} {   2.172   6.684  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   0.707   5.216  -2.682} {   0.861   6.353  -2.783} radius 0.03 filled yes resolution 20
draw cone {   0.861   6.353  -2.783} {   0.943   6.964  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -0.236   5.259  -2.682} {  -0.288   6.404  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -0.288   6.404  -2.783} {  -0.315   7.021  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -1.171   5.132  -2.682} {  -1.426   6.250  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -1.426   6.250  -2.783} {  -1.564   6.852  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -2.069   4.840  -2.682} {  -2.520   5.895  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -2.520   5.895  -2.783} {  -2.762   6.463  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -2.900   4.393  -2.682} {  -3.532   5.350  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -3.532   5.350  -2.783} {  -3.872   5.865  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -3.638   3.805  -2.682} {  -4.430   4.634  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -4.430   4.634  -2.783} {  -4.857   5.080  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -4.259   3.094  -2.682} {  -5.186   3.768  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -5.186   3.768  -2.783} {  -5.686   4.131  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -4.743   2.284  -2.682} {  -5.776   2.781  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -5.776   2.781  -2.783} {  -6.332   3.049  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -5.074   1.400  -2.682} {  -6.180   1.705  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -6.180   1.705  -2.783} {  -6.775   1.870  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -5.243   0.472  -2.682} {  -6.385   0.575  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -6.385   0.575  -2.783} {  -7.000   0.630  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -5.243  -0.472  -2.682} {  -6.385  -0.575  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -6.385  -0.575  -2.783} {  -7.000  -0.630  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -5.074  -1.400  -2.682} {  -6.180  -1.705  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -6.180  -1.705  -2.783} {  -6.775  -1.870  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -4.743  -2.284  -2.682} {  -5.776  -2.781  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -5.776  -2.781  -2.783} {  -6.332  -3.049  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -4.259  -3.094  -2.682} {  -5.186  -3.768  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -5.186  -3.768  -2.783} {  -5.686  -4.131  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -3.638  -3.805  -2.682} {  -4.430  -4.634  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -4.430  -4.634  -2.783} {  -4.857  -5.080  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -2.900  -4.393  -2.682} {  -3.532  -5.350  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -3.532  -5.350  -2.783} {  -3.872  -5.865  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -2.069  -4.840  -2.682} {  -2.520  -5.895  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -2.520  -5.895  -2.783} {  -2.762  -6.463  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -1.171  -5.132  -2.682} {  -1.426  -6.250  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -1.426  -6.250  -2.783} {  -1.564  -6.852  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {  -0.236  -5.259  -2.682} {  -0.288  -6.404  -2.783} radius 0.03 filled yes resolution 20
draw cone {  -0.288  -6.404  -2.783} {  -0.315  -7.021  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   0.707  -5.216  -2.682} {   0.861  -6.353  -2.783} radius 0.03 filled yes resolution 20
draw cone {   0.861  -6.353  -2.783} {   0.943  -6.964  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   1.627  -5.006  -2.682} {   1.981  -6.097  -2.783} radius 0.03 filled yes resolution 20
draw cone {   1.981  -6.097  -2.783} {   2.172  -6.684  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   2.494  -4.635  -2.682} {   3.038  -5.645  -2.783} radius 0.03 filled yes resolution 20
draw cone {   3.038  -5.645  -2.783} {   3.330  -6.189  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   3.282  -4.116  -2.682} {   3.997  -5.012  -2.783} radius 0.03 filled yes resolution 20
draw cone {   3.997  -5.012  -2.783} {   4.382  -5.495  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   3.964  -3.463  -2.682} {   4.828  -4.218  -2.783} radius 0.03 filled yes resolution 20
draw cone {   4.828  -4.218  -2.783} {   5.293  -4.624  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   4.519  -2.700  -2.682} {   5.503  -3.288  -2.783} radius 0.03 filled yes resolution 20
draw cone {   5.503  -3.288  -2.783} {   6.033  -3.605  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   4.928  -1.850  -2.682} {   6.002  -2.253  -2.783} radius 0.03 filled yes resolution 20
draw cone {   6.002  -2.253  -2.783} {   6.580  -2.469  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   5.179  -0.940  -2.682} {   6.308  -1.145  -2.783} radius 0.03 filled yes resolution 20
draw cone {   6.308  -1.145  -2.783} {   6.915  -1.255  -2.837} radius 0.06 resolution 20
draw color  774
draw cylinder {   5.264  -0.000  -2.682} {   6.411  -0.000  -2.783} radius 0.03 filled yes resolution 20
draw cone {   6.411  -0.000  -2.783} {   7.028  -0.000  -2.837} radius 0.06 resolution 20
draw color  604
draw cylinder {   4.688   0.932  -3.473} {   5.565   1.107  -3.590} radius 0.03 filled yes resolution 20
draw cone {   5.565   1.107  -3.590} {   6.038   1.201  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   4.416   1.829  -3.473} {   5.242   2.171  -3.590} radius 0.03 filled yes resolution 20
draw cone {   5.242   2.171  -3.590} {   5.687   2.356  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   3.974   2.655  -3.473} {   4.718   3.152  -3.590} radius 0.03 filled yes resolution 20
draw cone {   4.718   3.152  -3.590} {   5.119   3.420  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   3.380   3.380  -3.473} {   4.012   4.012  -3.590} radius 0.03 filled yes resolution 20
draw cone {   4.012   4.012  -3.590} {   4.353   4.353  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   2.655   3.974  -3.473} {   3.152   4.718  -3.590} radius 0.03 filled yes resolution 20
draw cone {   3.152   4.718  -3.590} {   3.420   5.119  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   1.829   4.416  -3.473} {   2.171   5.242  -3.590} radius 0.03 filled yes resolution 20
draw cone {   2.171   5.242  -3.590} {   2.356   5.687  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   0.932   4.688  -3.473} {   1.107   5.565  -3.590} radius 0.03 filled yes resolution 20
draw cone {   1.107   5.565  -3.590} {   1.201   6.038  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   0.000   4.780  -3.473} {   0.000   5.674  -3.590} radius 0.03 filled yes resolution 20
draw cone {   0.000   5.674  -3.590} {   0.000   6.156  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -0.932   4.688  -3.473} {  -1.107   5.565  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -1.107   5.565  -3.590} {  -1.201   6.038  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -1.829   4.416  -3.473} {  -2.171   5.242  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -2.171   5.242  -3.590} {  -2.356   5.687  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -2.655   3.974  -3.473} {  -3.152   4.718  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -3.152   4.718  -3.590} {  -3.420   5.119  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -3.380   3.380  -3.473} {  -4.012   4.012  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -4.012   4.012  -3.590} {  -4.353   4.353  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -3.974   2.655  -3.473} {  -4.718   3.152  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -4.718   3.152  -3.590} {  -5.119   3.420  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -4.416   1.829  -3.473} {  -5.242   2.171  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -5.242   2.171  -3.590} {  -5.687   2.356  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -4.688   0.932  -3.473} {  -5.565   1.107  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -5.565   1.107  -3.590} {  -6.038   1.201  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -4.780   0.000  -3.473} {  -5.674   0.000  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -5.674   0.000  -3.590} {  -6.156   0.000  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -4.688  -0.932  -3.473} {  -5.565  -1.107  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -5.565  -1.107  -3.590} {  -6.038  -1.201  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -4.416  -1.829  -3.473} {  -5.242  -2.171  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -5.242  -2.171  -3.590} {  -5.687  -2.356  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -3.974  -2.655  -3.473} {  -4.718  -3.152  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -4.718  -3.152  -3.590} {  -5.119  -3.420  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -3.380  -3.380  -3.473} {  -4.012  -4.012  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -4.012  -4.012  -3.590} {  -4.353  -4.353  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -2.655  -3.974  -3.473} {  -3.152  -4.718  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -3.152  -4.718  -3.590} {  -3.420  -5.119  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -1.829  -4.416  -3.473} {  -2.171  -5.242  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -2.171  -5.242  -3.590} {  -2.356  -5.687  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -0.932  -4.688  -3.473} {  -1.107  -5.565  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -1.107  -5.565  -3.590} {  -1.201  -6.038  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {  -0.000  -4.780  -3.473} {  -0.000  -5.674  -3.590} radius 0.03 filled yes resolution 20
draw cone {  -0.000  -5.674  -3.590} {  -0.000  -6.156  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   0.932  -4.688  -3.473} {   1.107  -5.565  -3.590} radius 0.03 filled yes resolution 20
draw cone {   1.107  -5.565  -3.590} {   1.201  -6.038  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   1.829  -4.416  -3.473} {   2.171  -5.242  -3.590} radius 0.03 filled yes resolution 20
draw cone {   2.171  -5.242  -3.590} {   2.356  -5.687  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   2.655  -3.974  -3.473} {   3.152  -4.718  -3.590} radius 0.03 filled yes resolution 20
draw cone {   3.152  -4.718  -3.590} {   3.420  -5.119  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   3.380  -3.380  -3.473} {   4.012  -4.012  -3.590} radius 0.03 filled yes resolution 20
draw cone {   4.012  -4.012  -3.590} {   4.353  -4.353  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   3.974  -2.655  -3.473} {   4.718  -3.152  -3.590} radius 0.03 filled yes resolution 20
draw cone {   4.718  -3.152  -3.590} {   5.119  -3.420  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   4.416  -1.829  -3.473} {   5.242  -2.171  -3.590} radius 0.03 filled yes resolution 20
draw cone {   5.242  -2.171  -3.590} {   5.687  -2.356  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   4.688  -0.932  -3.473} {   5.565  -1.107  -3.590} radius 0.03 filled yes resolution 20
draw cone {   5.565  -1.107  -3.590} {   6.038  -1.201  -3.653} radius 0.06 resolution 20
draw color  604
draw cylinder {   4.780  -0.000  -3.473} {   5.674  -0.000  -3.590} radius 0.03 filled yes resolution 20
draw cone {   5.674  -0.000  -3.590} {   6.156  -0.000  -3.653} radius 0.06 resolution 20
draw color  432
draw cylinder {   4.073   0.930  -4.177} {   4.697   1.072  -4.301} radius 0.03 filled yes resolution 20
draw cone {   4.697   1.072  -4.301} {   5.033   1.149  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   3.764   1.813  -4.177} {   4.341   2.090  -4.301} radius 0.03 filled yes resolution 20
draw cone {   4.341   2.090  -4.301} {   4.651   2.240  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   3.266   2.605  -4.177} {   3.767   3.004  -4.301} radius 0.03 filled yes resolution 20
draw cone {   3.767   3.004  -4.301} {   4.036   3.219  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   2.605   3.266  -4.177} {   3.004   3.767  -4.301} radius 0.03 filled yes resolution 20
draw cone {   3.004   3.767  -4.301} {   3.219   4.036  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   1.813   3.764  -4.177} {   2.090   4.341  -4.301} radius 0.03 filled yes resolution 20
draw cone {   2.090   4.341  -4.301} {   2.240   4.651  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   0.930   4.073  -4.177} {   1.072   4.697  -4.301} radius 0.03 filled yes resolution 20
draw cone {   1.072   4.697  -4.301} {   1.149   5.033  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   0.000   4.177  -4.177} {   0.000   4.818  -4.301} radius 0.03 filled yes resolution 20
draw cone {   0.000   4.818  -4.301} {   0.000   5.162  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -0.930   4.073  -4.177} {  -1.072   4.697  -4.301} radius 0.03 filled yes resolution 20
draw cone {  -1.072   4.697  -4.301} {  -1.149   5.033  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -1.813   3.764  -4.177} {  -2.090   4.341  -4.301} radius 0.03 filled yes resolution 20
draw cone {  -2.090   4.341  -4.301} {  -2.240   4.651  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -2.605   3.266  -4.177} {  -3.004   3.767  -4.301} radius 0.03 filled yes resolution 20
draw cone {  -3.004   3.767  -4.301} {  -3.219   4.036  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -3.266   2.605  -4.177} {  -3.767   3.004  -4.301} radius 0.03 filled yes resolution 20
draw cone {  -3.767   3.004  -4.301} {  -4.036   3.219  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -3.764   1.813  -4.177} {  -4.341   2.090  -4.301} radius 0.03 filled yes resolution 20
draw cone {  -4.341   2.090  -4.301} {  -4.651   2.240  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -4.073   0.930  -4.177} {  -4.697   1.072  -4.301} radius 0.03 filled yes resolution 20
draw cone {  -4.697   1.072  -4.301} {  -5.033   1.149  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -4.177   0.000  -4.177} {  -4.818   0.000  -4.301} radius 0.03 filled yes resolution 20
draw cone {  -4.818   0.000  -4.301} {  -5.162   0.000  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -4.073  -0.930  -4.177} {  -4.697  -1.072  -4.301} radius 0.03 filled yes resolution 20
draw cone {  -4.697  -1.072  -4.301} {  -5.033  -1.149  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -3.764  -1.813  -4.177} {  -4.341  -2.090  -4.301} radius 0.03 filled yes resolution 20
draw cone {  -4.341  -2.090  -4.301} {  -4.651  -2.240  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -3.266  -2.605  -4.177} {  -3.767  -3.004  -4.301} radius 0.03 filled yes resolution 20
draw cone {  -3.767  -3.004  -4.301} {  -4.036  -3.219  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -2.605  -3.266  -4.177} {  -3.004  -3.767  -4.301} radius 0.03 filled yes resolution 20
draw cone {  -3.004  -3.767  -4.301} {  -3.219  -4.036  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -1.813  -3.764  -4.177} {  -2.090  -4.341  -4.301} radius 0.03 filled yes resolution 20
draw cone {  -2.090  -4.341  -4.301} {  -2.240  -4.651  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -0.930  -4.073  -4.177} {  -1.072  -4.697  -4.301} radius 0.03 filled yes resolution 20
draw cone {  -1.072  -4.697  -4.301} {  -1.149  -5.033  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {  -0.000  -4.177  -4.177} {  -0.000  -4.818  -4.301} radius 0.03 filled yes resolution 20
draw cone {  -0.000  -4.818  -4.301} {  -0.000  -5.162  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   0.930  -4.073  -4.177} {   1.072  -4.697  -4.301} radius 0.03 filled yes resolution 20
draw cone {   1.072  -4.697  -4.301} {   1.149  -5.033  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   1.813  -3.764  -4.177} {   2.090  -4.341  -4.301} radius 0.03 filled yes resolution 20
draw cone {   2.090  -4.341  -4.301} {   2.240  -4.651  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   2.605  -3.266  -4.177} {   3.004  -3.767  -4.301} radius 0.03 filled yes resolution 20
draw cone {   3.004  -3.767  -4.301} {   3.219  -4.036  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   3.266  -2.605  -4.177} {   3.767  -3.004  -4.301} radius 0.03 filled yes resolution 20
draw cone {   3.767  -3.004  -4.301} {   4.036  -3.219  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   3.764  -1.813  -4.177} {   4.341  -2.090  -4.301} radius 0.03 filled yes resolution 20
draw cone {   4.341  -2.090  -4.301} {   4.651  -2.240  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   4.073  -0.930  -4.177} {   4.697  -1.072  -4.301} radius 0.03 filled yes resolution 20
draw cone {   4.697  -1.072  -4.301} {   5.033  -1.149  -4.367} radius 0.06 resolution 20
draw color  432
draw cylinder {   4.177  -0.000  -4.177} {   4.818  -0.000  -4.301} radius 0.03 filled yes resolution 20
draw cone {   4.818  -0.000  -4.301} {   5.162  -0.000  -4.367} radius 0.06 resolution 20
draw color  281
draw cylinder {   3.344   0.937  -4.780} {   3.743   1.049  -4.900} radius 0.03 filled yes resolution 20
draw cone {   3.743   1.049  -4.900} {   3.957   1.109  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   2.967   1.804  -4.780} {   3.321   2.019  -4.900} radius 0.03 filled yes resolution 20
draw cone {   3.321   2.019  -4.900} {   3.511   2.135  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   2.370   2.538  -4.780} {   2.653   2.841  -4.900} radius 0.03 filled yes resolution 20
draw cone {   2.653   2.841  -4.900} {   2.805   3.004  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   1.598   3.083  -4.780} {   1.788   3.451  -4.900} radius 0.03 filled yes resolution 20
draw cone {   1.788   3.451  -4.900} {   1.891   3.649  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   0.707   3.400  -4.780} {   0.791   3.805  -4.900} radius 0.03 filled yes resolution 20
draw cone {   0.791   3.805  -4.900} {   0.836   4.024  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -0.237   3.464  -4.780} {  -0.265   3.878  -4.900} radius 0.03 filled yes resolution 20
draw cone {  -0.265   3.878  -4.900} {  -0.280   4.100  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -1.163   3.272  -4.780} {  -1.302   3.662  -4.900} radius 0.03 filled yes resolution 20
draw cone {  -1.302   3.662  -4.900} {  -1.376   3.872  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -2.003   2.837  -4.780} {  -2.241   3.175  -4.900} radius 0.03 filled yes resolution 20
draw cone {  -2.241   3.175  -4.900} {  -2.370   3.358  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -2.694   2.191  -4.780} {  -3.015   2.453  -4.900} radius 0.03 filled yes resolution 20
draw cone {  -3.015   2.453  -4.900} {  -3.188   2.594  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -3.185   1.383  -4.780} {  -3.565   1.548  -4.900} radius 0.03 filled yes resolution 20
draw cone {  -3.565   1.548  -4.900} {  -3.769   1.637  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -3.440   0.473  -4.780} {  -3.851   0.529  -4.900} radius 0.03 filled yes resolution 20
draw cone {  -3.851   0.529  -4.900} {  -4.071   0.560  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -3.440  -0.473  -4.780} {  -3.851  -0.529  -4.900} radius 0.03 filled yes resolution 20
draw cone {  -3.851  -0.529  -4.900} {  -4.071  -0.560  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -3.185  -1.383  -4.780} {  -3.565  -1.548  -4.900} radius 0.03 filled yes resolution 20
draw cone {  -3.565  -1.548  -4.900} {  -3.769  -1.637  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -2.694  -2.191  -4.780} {  -3.015  -2.453  -4.900} radius 0.03 filled yes resolution 20
draw cone {  -3.015  -2.453  -4.900} {  -3.188  -2.594  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -2.003  -2.837  -4.780} {  -2.241  -3.175  -4.900} radius 0.03 filled yes resolution 20
draw cone {  -2.241  -3.175  -4.900} {  -2.370  -3.358  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -1.163  -3.272  -4.780} {  -1.302  -3.662  -4.900} radius 0.03 filled yes resolution 20
draw cone {  -1.302  -3.662  -4.900} {  -1.376  -3.872  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {  -0.237  -3.464  -4.780} {  -0.265  -3.878  -4.900} radius 0.03 filled yes resolution 20
draw cone {  -0.265  -3.878  -4.900} {  -0.280  -4.100  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   0.707  -3.400  -4.780} {   0.791  -3.805  -4.900} radius 0.03 filled yes resolution 20
draw cone {   0.791  -3.805  -4.900} {   0.836  -4.024  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   1.598  -3.083  -4.780} {   1.788  -3.451  -4.900} radius 0.03 filled yes resolution 20
draw cone {   1.788  -3.451  -4.900} {   1.891  -3.649  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   2.370  -2.538  -4.780} {   2.653  -2.841  -4.900} radius 0.03 filled yes resolution 20
draw cone {   2.653  -2.841  -4.900} {   2.805  -3.004  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   2.967  -1.804  -4.780} {   3.321  -2.019  -4.900} radius 0.03 filled yes resolution 20
draw cone {   3.321  -2.019  -4.900} {   3.511  -2.135  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   3.344  -0.937  -4.780} {   3.743  -1.049  -4.900} radius 0.03 filled yes resolution 20
draw cone {   3.743  -1.049  -4.900} {   3.957  -1.109  -4.966} radius 0.06 resolution 20
draw color  281
draw cylinder {   3.473  -0.000  -4.780} {   3.887  -0.000  -4.900} radius 0.03 filled yes resolution 20
draw cone {   3.887  -0.000  -4.900} {   4.110  -0.000  -4.966} radius 0.06 resolution 20
draw color  166
draw cylinder {   2.520   0.917  -5.264} {   2.744   0.999  -5.377} radius 0.03 filled yes resolution 20
draw cone {   2.744   0.999  -5.377} {   2.864   1.042  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   2.055   1.724  -5.264} {   2.237   1.877  -5.377} radius 0.03 filled yes resolution 20
draw cone {   2.237   1.877  -5.377} {   2.335   1.959  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   1.341   2.323  -5.264} {   1.460   2.529  -5.377} radius 0.03 filled yes resolution 20
draw cone {   1.460   2.529  -5.377} {   1.524   2.640  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   0.466   2.641  -5.264} {   0.507   2.876  -5.377} radius 0.03 filled yes resolution 20
draw cone {   0.507   2.876  -5.377} {   0.529   3.002  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -0.466   2.641  -5.264} {  -0.507   2.876  -5.377} radius 0.03 filled yes resolution 20
draw cone {  -0.507   2.876  -5.377} {  -0.529   3.002  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -1.341   2.323  -5.264} {  -1.460   2.529  -5.377} radius 0.03 filled yes resolution 20
draw cone {  -1.460   2.529  -5.377} {  -1.524   2.640  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -2.055   1.724  -5.264} {  -2.237   1.877  -5.377} radius 0.03 filled yes resolution 20
draw cone {  -2.237   1.877  -5.377} {  -2.335   1.959  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -2.520   0.917  -5.264} {  -2.744   0.999  -5.377} radius 0.03 filled yes resolution 20
draw cone {  -2.744   0.999  -5.377} {  -2.864   1.042  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -2.682   0.000  -5.264} {  -2.920   0.000  -5.377} radius 0.03 filled yes resolution 20
draw cone {  -2.920   0.000  -5.377} {  -3.048   0.000  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -2.520  -0.917  -5.264} {  -2.744  -0.999  -5.377} radius 0.03 filled yes resolution 20
draw cone {  -2.744  -0.999  -5.377} {  -2.864  -1.042  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -2.055  -1.724  -5.264} {  -2.237  -1.877  -5.377} radius 0.03 filled yes resolution 20
draw cone {  -2.237  -1.877  -5.377} {  -2.335  -1.959  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -1.341  -2.323  -5.264} {  -1.460  -2.529  -5.377} radius 0.03 filled yes resolution 20
draw cone {  -1.460  -2.529  -5.377} {  -1.524  -2.640  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {  -0.466  -2.641  -5.264} {  -0.507  -2.876  -5.377} radius 0.03 filled yes resolution 20
draw cone {  -0.507  -2.876  -5.377} {  -0.529  -3.002  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   0.466  -2.641  -5.264} {   0.507  -2.876  -5.377} radius 0.03 filled yes resolution 20
draw cone {   0.507  -2.876  -5.377} {   0.529  -3.002  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   1.341  -2.323  -5.264} {   1.460  -2.529  -5.377} radius 0.03 filled yes resolution 20
draw cone {   1.460  -2.529  -5.377} {   1.524  -2.640  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   2.055  -1.724  -5.264} {   2.237  -1.877  -5.377} radius 0.03 filled yes resolution 20
draw cone {   2.237  -1.877  -5.377} {   2.335  -1.959  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   2.520  -0.917  -5.264} {   2.744  -0.999  -5.377} radius 0.03 filled yes resolution 20
draw cone {   2.744  -0.999  -5.377} {   2.864  -1.042  -5.438} radius 0.06 resolution 20
draw color  166
draw cylinder {   2.682  -0.000  -5.264} {   2.920  -0.000  -5.377} radius 0.03 filled yes resolution 20
draw cone {   2.920  -0.000  -5.377} {   3.048  -0.000  -5.438} radius 0.06 resolution 20
draw color   93
draw cylinder {   1.581   0.913  -5.619} {   1.683   0.972  -5.722} radius 0.03 filled yes resolution 20
draw cone {   1.683   0.972  -5.722} {   1.737   1.003  -5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {   0.913   1.581  -5.619} {   0.972   1.683  -5.722} radius 0.03 filled yes resolution 20
draw cone {   0.972   1.683  -5.722} {   1.003   1.738  -5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {   0.000   1.826  -5.619} {   0.000   1.943  -5.722} radius 0.03 filled yes resolution 20
draw cone {   0.000   1.943  -5.722} {   0.000   2.006  -5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {  -0.913   1.581  -5.619} {  -0.972   1.683  -5.722} radius 0.03 filled yes resolution 20
draw cone {  -0.972   1.683  -5.722} {  -1.003   1.738  -5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {  -1.581   0.913  -5.619} {  -1.683   0.972  -5.722} radius 0.03 filled yes resolution 20
draw cone {  -1.683   0.972  -5.722} {  -1.737   1.003  -5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {  -1.826   0.000  -5.619} {  -1.943   0.000  -5.722} radius 0.03 filled yes resolution 20
draw cone {  -1.943   0.000  -5.722} {  -2.006   0.000  -5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {  -1.581  -0.913  -5.619} {  -1.683  -0.972  -5.722} radius 0.03 filled yes resolution 20
draw cone {  -1.683  -0.972  -5.722} {  -1.737  -1.003  -5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {  -0.913  -1.581  -5.619} {  -0.972  -1.683  -5.722} radius 0.03 filled yes resolution 20
draw cone {  -0.972  -1.683  -5.722} {  -1.003  -1.738  -5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {  -0.000  -1.826  -5.619} {  -0.000  -1.943  -5.722} radius 0.03 filled yes resolution 20
draw cone {  -0.000  -1.943  -5.722} {  -0.000  -2.006  -5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {   0.913  -1.581  -5.619} {   0.972  -1.683  -5.722} radius 0.03 filled yes resolution 20
draw cone {   0.972  -1.683  -5.722} {   1.003  -1.738  -5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {   1.581  -0.913  -5.619} {   1.683  -0.972  -5.722} radius 0.03 filled yes resolution 20
draw cone {   1.683  -0.972  -5.722} {   1.737  -1.003  -5.778} radius 0.06 resolution 20
draw color   93
draw cylinder {   1.826  -0.000  -5.619} {   1.943  -0.000  -5.722} radius 0.03 filled yes resolution 20
draw cone {   1.943  -0.000  -5.722} {   2.006  -0.000  -5.778} radius 0.06 resolution 20
draw color   59
draw cylinder {   0.462   0.800  -5.835} {   0.485   0.839  -5.931} radius 0.03 filled yes resolution 20
draw cone {   0.485   0.839  -5.931} {   0.497   0.860  -5.983} radius 0.06 resolution 20
draw color   59
draw cylinder {  -0.462   0.800  -5.835} {  -0.485   0.839  -5.931} radius 0.03 filled yes resolution 20
draw cone {  -0.485   0.839  -5.931} {  -0.497   0.860  -5.983} radius 0.06 resolution 20
draw color   59
draw cylinder {  -0.924   0.000  -5.835} {  -0.969   0.000  -5.931} radius 0.03 filled yes resolution 20
draw cone {  -0.969   0.000  -5.931} {  -0.993   0.000  -5.983} radius 0.06 resolution 20
draw color   59
draw cylinder {  -0.462  -0.800  -5.835} {  -0.485  -0.839  -5.931} radius 0.03 filled yes resolution 20
draw cone {  -0.485  -0.839  -5.931} {  -0.497  -0.860  -5.983} radius 0.06 resolution 20
draw color   59
draw cylinder {   0.462  -0.800  -5.835} {   0.485  -0.839  -5.931} radius 0.03 filled yes resolution 20
draw cone {   0.485  -0.839  -5.931} {   0.497  -0.860  -5.983} radius 0.06 resolution 20
draw color   59
draw cylinder {   0.924  -0.000  -5.835} {   0.969  -0.000  -5.931} radius 0.03 filled yes resolution 20
draw cone {   0.969  -0.000  -5.931} {   0.993  -0.000  -5.983} radius 0.06 resolution 20
draw color   51
draw cylinder {   0.000  -0.000  -5.908} {   0.000  -0.000  -6.001} radius 0.03 filled yes resolution 20
draw cone {   0.000  -0.000  -6.001} {   0.000  -0.000  -6.051} radius 0.06 resolution 20
