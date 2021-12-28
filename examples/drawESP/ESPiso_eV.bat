Multiwfn 1.fch -ESPrhoiso 0.001 < ESPiso_eV.txt
move /Y density.cub density1.cub
move /Y totesp.cub ESP1.cub
Multiwfn 2.fch -ESPrhoiso 0.001 < ESPiso_eV.txt
move /Y density.cub density2.cub
move /Y totesp.cub ESP2.cub
Multiwfn 3.fch -ESPrhoiso 0.001 < ESPiso_eV.txt
move /Y density.cub density3.cub
move /Y totesp.cub ESP3.cub
Multiwfn 4.fch -ESPrhoiso 0.001 < ESPiso_eV.txt
move /Y density.cub density4.cub
move /Y totesp.cub ESP4.cub

move /Y *.cub "D:\study\VMD193"