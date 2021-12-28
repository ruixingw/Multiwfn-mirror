Multiwfn 1.fchk -ESPrhoiso 0.001 < ESPiso.txt
mv -f density.cub density1.cub
mv -f totesp.cub ESP1.cub
Multiwfn 2.fchk -ESPrhoiso 0.001 < ESPiso.txt
mv -f density.cub density2.cub
mv -f totesp.cub ESP2.cub
Multiwfn 3.fchk -ESPrhoiso 0.001 < ESPiso.txt
mv -f density.cub density3.cub
mv -f totesp.cub ESP3.cub
Multiwfn 4.fchk -ESPrhoiso 0.001 < ESPiso.txt
mv -f density.cub density4.cub
mv -f totesp.cub ESP4.cub