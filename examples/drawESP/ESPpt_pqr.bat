Multiwfn h2o.fch < ESPpt_pqr.txt
move /Y vtx.pqr vtx1.pqr
move /Y extrema.pqr extrema1.pqr
move /Y mol.pdb mol1.pdb
Multiwfn 2.fch < ESPpt_pqr.txt
move /Y vtx.pqr vtx2.pqr
move /Y extrema.pqr extrema2.pqr
move /Y mol.pdb mol2.pdb
Multiwfn 3.fch < ESPpt_pqr.txt
move /Y vtx.pqr vtx3.pqr
move /Y extrema.pqr extrema3.pqr
move /Y mol.pdb mol3.pdb
Multiwfn 4.fch < ESPpt_pqr.txt
move /Y vtx.pqr vtx4.pqr
move /Y extrema.pqr extrema4.pqr
move /Y mol.pdb mol4.pdb

move /Y *.pdb "D:\study\VMD193"
move /Y *.pqr "D:\study\VMD193"