                  Grand Master is running on host MATRIX
                  rank #     1 is running on host MATRIX

          ******************************************************
          *      Firefly version 8.2.0, build number 10203     *
          *   Compiled on    Monday,    19-09-2016, 15:30:23   *
          *Code development and Intel/AMD specific optimization*
          *  Copyright (c) 1994, 2016 by  Alex A. Granovsky,   *
          *          Firefly Project, Moscow, Russia.          *
          *   Some parts of this program include code due to   *
          * work of Jim Kress, Peter Burger, and Robert Ponec. *
          ******************************************************
          *             Firefly Project homepage:              *
          * http://classic.chem.msu.su/gran/firefly/index.html *
          *                      e-mail:                       *
          *               gran@classic.chem.msu.su             *
          *This program is not a free software and is provided *
          *exclusively to its registered users under the terms *
          *       of Firefly package license agreement         *
          * Unauthorized use of Firefly is strongly prohibited *
          *   This program may not be redistributed without    *
          * the specific, written permission of its developers.*
          ******************************************************

          ******************************************************
          *  PARTIALLY BASED ON  US GAMESS VERSION 6 JUN 1999, *
          *   US GAMESS VERSIONS  6 SEP 2001 AND 12 DEC 2003   *
          *             FROM IOWA STATE UNIVERSITY             *
          * M.W.SCHMIDT, K.K.BALDRIDGE, J.A.BOATZ, S.T.ELBERT, *
          *   M.S.GORDON, J.H.JENSEN, S.KOSEKI, N.MATSUNAGA,   *
          *          K.A.NGUYEN, S.J.SU, T.L.WINDUS,           *
          *       TOGETHER WITH M.DUPUIS, J.A.MONTGOMERY       *
          *         J.COMPUT.CHEM.  14, 1347-1363(1993)        *
          ******************************************************


 Core i7    / Win32  Firefly version running under Windows NT
 Running on Intel CPU:  Brand ID  0, Family  6, Model  69, Stepping  1
 CPU Brand String    :  Intel(R) Core(TM) i3-4030U CPU @ 1.90GHz        
 CPU Features        :  x87 FPU, CMOV, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, AVX, AVX2, FMA3, HTT, MWAIT, EM64T
 Data cache size     :  L1 32 KB, L2  256 KB, L3  3072 KB
 max    # of   cores/package :   8
 max    # of threads/package :  16
 max     cache sharing level :  16
 actual # of   cores/package :   2
 actual # of threads/package :   4
 actual # of threads/core    :   2
 Operating System successfully passed SSE support test.
 Operating System supports AVX/FMA.


 PARALLEL VERSION (UNIFIED) RUNNING USING    2 PROCESSES (RANKS)


 WARNING! THIS VERSION OF FIREFLY IS PROBABLY OUTDATED!
 PLEASE CHECK FIREFLY HOMEPAGE FOR INFORMATION ON UPDATES!

 EXECUTION OF FIREFLY BEGUN 15:18:20  2-OCT-2018    

            ECHO OF THE FIRST FEW INPUT CARDS -
 INPUT CARD> $BASIS GBASIS=N31 NGAUSS=6 NDFUNC=1 $END                                       
 INPUT CARD> $CONTRL CITYP=TDDFT DFTTYP=B3LYP RUNTYP=ENERGY $END                            
 INPUT CARD> $STATPT OPTTOL=0.0001 NSTEP=20 $END                                            
 INPUT CARD> $TDDFT NSTATE=4 ISTSYM=0 ISTATE=1 $END                                         
 INPUT CARD> $DATA                                                                          
 INPUT CARD>ch2o                                                                            
 INPUT CARD>C1                                                                              
 INPUT CARD> H           1.0  -0.1433741643   3.5232836526   0.9847052719                   
 INPUT CARD> C           6.0  -0.6328030344   3.6809006912   0.0004926713                   
 INPUT CARD> H           1.0   0.0589074733   3.8683760161  -0.8477363634                   
 INPUT CARD> O           8.0  -1.8309802746   3.6506396400  -0.1374615798                   
 INPUT CARD> $END                                                                           
   20000000 WORDS OF MEMORY AVAILABLE

 This job is executing on     1 unique host(s)
 Minimum number of processes per host is:    2
 Maximum number of processes per host is:    2

 On master's host, detected    4 CPU core(s) in aggregate

 Using processor-specific dynamic link DGEMM library code.


 Warning: HTT is enabled, bitmask of physically unique cores is 0x0000000A

 SMT aware parts of program will use              2 threads.

 Creating thread pool to serve up to            128 threads.


 Loading P2P interface library... loaded successfully (version 2.4).
 Initializing global P2P interface... topology done.


     BASIS OPTIONS
     -------------
     GBASIS=N31          IGAUSS=       6      POLAR=POPLE   
     NDFUNC=       1     NFFUNC=       0     DIFFSP=       F
     NPFUNC=       0      DIFFS=       F


     RUN TITLE
     ---------
 ch2o                                                                            

 THE POINT GROUP OF THE MOLECULE IS C1      
 THE ORDER OF THE PRINCIPAL AXIS IS     0

 ATOM      ATOMIC                      COORDINATES (BOHR)
           CHARGE         X                   Y                   Z
 H           1.0    -0.2709379040        6.6580411657        1.8608232782
 C           6.0    -1.1958244264        6.9558942014        0.0009310138
 H           1.0     0.1113189913        7.3101712207       -1.6019895534
 O           8.0    -3.4600512601        6.8987091023       -0.2597647386

          INTERNUCLEAR DISTANCES (ANGS.)
          ------------------------------

                    H              C              H              O         

  1  H               0.0000000      1.1104315 *    1.8755930 *    2.0306384 *  
  2  C               1.1104315 *    0.0000000      1.1104517 *    1.2064725 *  
  3  H               1.8755930 *    1.1104517 *    0.0000000      2.0306588 *  
  4  O               2.0306384 *    1.2064725 *    2.0306588 *    0.0000000    

  * ... LESS THAN  3.000


     ATOMIC BASIS SET
     ----------------
 THE CONTRACTED PRIMITIVE FUNCTIONS HAVE BEEN UNNORMALIZED
 THE CONTRACTED BASIS FUNCTIONS ARE NOW NORMALIZED TO UNITY

 SHELL TYPE PRIM    EXPONENT          CONTRACTION COEFFICIENTS

 H         

   1   S    1      18.731137    0.214935 (  0.033495) 
   1   S    2       2.825394    0.364571 (  0.234727) 
   1   S    3       0.640122    0.415051 (  0.813757) 

   2   S    4       0.161278    0.181381 (  1.000000) 

 C         

   3   S    5    3047.524880    0.536345 (  0.001835) 
   3   S    6     457.369518    0.989452 (  0.014037) 
   3   S    7     103.948685    1.597283 (  0.068843) 
   3   S    8      29.210155    2.079187 (  0.232184) 
   3   S    9       9.286663    1.774174 (  0.467941) 
   3   S   10       3.163927    0.612580 (  0.362312) 

   4   L   11       7.868272   -0.399556 ( -0.119332)     1.296082 (  0.068999) 
   4   L   12       1.881289   -0.184155 ( -0.160854)     0.993754 (  0.316424) 
   4   L   13       0.544249    0.516390 (  1.143456)     0.495953 (  0.744308) 

   5   L   14       0.168714    0.187618 (  1.000000)     0.154128 (  1.000000) 

   6   D   15       0.800000    1.113825 (  1.000000) 

 H         

   7   S   16      18.731137    0.214935 (  0.033495) 
   7   S   17       2.825394    0.364571 (  0.234727) 
   7   S   18       0.640122    0.415051 (  0.813757) 

   8   S   19       0.161278    0.181381 (  1.000000) 

 O         

   9   S   20    5484.671660    0.831724 (  0.001831) 
   9   S   21     825.234946    1.530816 (  0.013950) 
   9   S   22     188.046958    2.477149 (  0.068445) 
   9   S   23      52.964500    3.256281 (  0.232714) 
   9   S   24      16.897570    2.792893 (  0.470193) 
   9   S   25       5.799635    0.954938 (  0.358521) 

  10   L   26      15.539616   -0.617934 ( -0.110778)     3.116944 (  0.070874) 
  10   L   27       3.599934   -0.275721 ( -0.148026)     2.401438 (  0.339753) 
  10   L   28       1.013762    0.814208 (  1.130767)     1.054360 (  0.727159) 

  11   L   29       0.270006    0.266956 (  1.000000)     0.277432 (  1.000000) 

  12   D   30       0.800000    1.113825 (  1.000000) 

 TOTAL NUMBER OF SHELLS              =   12
 TOTAL NUMBER OF BASIS FUNCTIONS     =   34
 NUMBER OF ELECTRONS                 =   16
 CHARGE OF MOLECULE                  =    0
 STATE MULTIPLICITY                  =    1
 NUMBER OF OCCUPIED ORBITALS (ALPHA) =    8
 NUMBER OF OCCUPIED ORBITALS (BETA ) =    8
 TOTAL NUMBER OF ATOMS               =    4
 THE NUCLEAR REPULSION ENERGY IS       31.2237524994

     $CONTRL OPTIONS
     ---------------
     SCFTYP=RHF          RUNTYP=ENERGY       EXETYP=RUN     
     MPLEVL=       0     LOCAL =NONE         UNITS =ANGS    
     MULT  =       1     ICHARG=       0     MAXIT =      30
     NPRINT=       7     IREST =       0     COORD =UNIQUE  
     ECP   =NONE         NORMF =       0     NORMP =       0
     ITOL  =      20     ICUT  =       9     NZVAR =       0
     NOSYM =       0     INTTYP=POPLE        GEOM  =INPUT   
     PLTORB=       F     MOLPLT=       F     RPAC  =       F
     AIMPAC=       0     FRIEND=             CITYP =TDDFT   
     DFTTYP=B3LYP   

     $SYSTEM OPTIONS
     ---------------
     KDIAG =       0     MEMORY= 20000000     TIMLIM=   172800.0 SEC.
     COREFL=       F     PTIME =        F     XDR   =       F
     BALTYP=NXTVAL       CBASE = 00401000     FBASE = 5A6910C0

          ----------------
          PROPERTIES INPUT
          ----------------

     MOMENTS            FIELD           POTENTIAL          DENSITY
 IEMOM =       1   IEFLD =       0   IEPOT =       0   IEDEN =       0
 WHERE =COMASS     WHERE =NUCLEI     WHERE =NUCLEI     WHERE =NUCLEI  
 OUTPUT=BOTH       OUTPUT=BOTH       OUTPUT=BOTH       OUTPUT=BOTH    
 IEMINT=       0   IEFINT=       0                     IEDINT=       0
                                                       MORB  =       0


          ----------------   ------------------------------------
          DFT CODE OPTIONS   PROGRAM WRITTEN BY ALEX A. GRANOVSKY
          ----------------   ------------------------------------
          NRAD   =   63      LMAX   =  29 ( 302 POINTS PER SHELL)
          ANGPRN =    T      RADPRN =   T
          KAP    =   5.000   LMIN   =   7 (  26 POINTS PER SHELL)
          CUTOFF = 1.0E-10   CUTAO  = 1.0E-10
          CUTWGT = 1.0E-20   CUTORB = 1.0E-15
          CUTGG1 = 1.0E-13   CUTGG2 = 1.0E-13
          CUTGG3 = 1.0E-13   CUTGG4 = 1.0E-30
          B3LYP  =  NWCHEM   O3LYP  = DEFAULT 
          HFX    =  2.0000000000000000000E-01
          CPT2   =  0.0000000000000000000E+00
          SCS(S) =  1.0000000000000000000E+00
          SCS(T) =  1.0000000000000000000E+00



          EXTRAPOLATION IN EFFECT
          DAMPING IN EFFECT
          LEVEL SHIFTING IN EFFECT
          DIIS IN EFFECT

          ----------------------
          INTEGRAL INPUT OPTIONS
          ----------------------
 NOPK  =       1 NORDER=       0 SCHWRZ=       T

 ATTENTION! AO INTEGRALS WILL BE PACKED.
 THRESHOLD FOR PACKING PKTHR =  0.10000000D-01

     -------------------------------
     INTEGRAL TRANSFORMATION OPTIONS
     -------------------------------
     NWORD  =       0     CUTTRF = 1.0E-09
     MPTRAN =       0     DIRTRF =       F
     AOINTS =DUP          IREST  =       0

     ---------------------------------
     TDDFT PROGRAM CONTROL INFORMATION
     ---------------------------------
     NACORE =        2  NBCORE   =        2
     NSTATE =        4  ISTATE   =        1
     MULT   =        1  DIAGZN   =    DAVID
     MXVEC  =       64  NDAVIT   =       50
     DAVCVG = 3.00E-05  TDPRP    =        F
     NGSVEC =       10  TDA      =        F
     MNMEOP =        F  CHFSLV   =    DIIS 
     RDTDVC =        F  ISTSYM   =        0

     NUMBER OF CORE -A-  ORBITALS =     2
     NUMBER OF CORE -B-  ORBITALS =     2
     NUMBER OF OCC. -A-  ORBITALS =     8
     NUMBER OF OCC. -B-  ORBITALS =     8
     NUMBER OF MOLECULAR ORBITALS =    34
     NUMBER OF   BASIS  FUNCTIONS =    34


     ------------------------------------------
     THE POINT GROUP IS C1 , NAXIS= 0, ORDER= 1
     ------------------------------------------

     DIMENSIONS OF THE SYMMETRY SUBSPACES ARE
 A   =  34

 ..... DONE SETTING UP THE RUN .....

                         TIMING STATISTICS ON RANK 0:
 CPU        TIME:   STEP =      0.03 ,  TOTAL =        0.1 SECONDS (    0.0 MIN)
 WALL CLOCK TIME:   STEP =      0.13 ,  TOTAL =        0.1 SECONDS (    0.0 MIN)
 CPU UTILIZATION:   STEP =     23.73%,  TOTAL =      71.19%

          ********************
          1 ELECTRON INTEGRALS
          ********************
 ...... END OF ONE-ELECTRON INTEGRALS ......

                         TIMING STATISTICS ON RANK 0:
 CPU        TIME:   STEP =      0.00 ,  TOTAL =        0.1 SECONDS (    0.0 MIN)
 WALL CLOCK TIME:   STEP =      0.00 ,  TOTAL =        0.1 SECONDS (    0.0 MIN)
 CPU UTILIZATION:   STEP =      0.00%,  TOTAL =      70.72%

          -------------
          GUESS OPTIONS
          -------------
          GUESS =HUCKEL            NORB  =       0          NORDER=       0
          MIX   =       F          PRTMO =       F          SYMDEN=       F
          TOLZ  = 0.0E+00          TOLE  = 0.0E+00

 INITIAL GUESS ORBITALS GENERATED BY HUCKEL   ROUTINE.
 HUCKEL GUESS REQUIRES     10716 WORDS.

 LOG10 OF CONDITION NUMBER OF OVERLAP MATRIX IS:  2.79778975E+00

 SYMMETRIES FOR INITIAL GUESS ORBITALS FOLLOW.   BOTH SET(S).
     8 ORBITALS ARE OCCUPIED (    2 CORE ORBITALS).
     3=A        4=A        5=A        6=A        7=A        8=A        9=A   
    10=A       11=A       12=A       13=A       14=A       15=A       16=A   
    17=A       18=A   
 ...... END OF INITIAL ORBITAL SELECTION ......

                         TIMING STATISTICS ON RANK 0:
 CPU        TIME:   STEP =      0.00 ,  TOTAL =        0.1 SECONDS (    0.0 MIN)
 WALL CLOCK TIME:   STEP =      0.07 ,  TOTAL =        0.2 SECONDS (    0.0 MIN)
 CPU UTILIZATION:   STEP =      0.00%,  TOTAL =      45.46%

          --------------------
          2 ELECTRON INTEGRALS
          --------------------

 THE -PK- OPTION IS OFF, THE INTEGRALS ARE NOT IN SUPERMATRIX FORM.
 STORING    4998 INTEGRALS/RECORD ON DISK, USING 12 BYTES/INTEGRAL.
 TWO ELECTRON INTEGRAL EVALUATION REQUIRES   35824 WORDS OF MEMORY.
 SCHWARZ INEQUALITY OVERHEAD:       595 INTEGRALS, CPU TIME=        0.00
 II,JST,KST,LST =  1  1  1  1 NREC =         1 INTLOC =    1
 II,JST,KST,LST =  2  1  1  1 NREC =         1 INTLOC =    2
 II,JST,KST,LST =  3  1  1  1 NREC =         1 INTLOC =    5
 II,JST,KST,LST =  4  1  1  1 NREC =         1 INTLOC =   12
 II,JST,KST,LST =  5  1  1  1 NREC =         1 INTLOC =  161
 II,JST,KST,LST =  6  1  1  1 NREC =         1 INTLOC =  712
 II,JST,KST,LST =  7  1  1  1 NREC =         1 INTLOC = 2908
 II,JST,KST,LST =  8  1  1  1 NREC =         1 INTLOC = 4362
 II,JST,KST,LST =  9  1  1  1 NREC =         2 INTLOC = 1086
 II,JST,KST,LST = 10  1  1  1 NREC =         2 INTLOC = 3092
 II,JST,KST,LST = 11  1  1  1 NREC =         4 INTLOC = 4637
 II,JST,KST,LST = 12  1  1  1 NREC =         8 INTLOC = 2865
 SCHWARZ INEQUALITY TEST SKIPPED         3 INTEGRAL BLOCKS.
 TOTAL NUMBER OF NONZERO TWO-ELECTRON INTEGRALS =              165145
         34 INTEGRAL RECORDS WERE STORED ON DISK FILE  8.
 ...... END OF TWO-ELECTRON INTEGRALS .....

                         TIMING STATISTICS ON RANK 0:
 CPU        TIME:   STEP =      0.03 ,  TOTAL =        0.1 SECONDS (    0.0 MIN)
 WALL CLOCK TIME:   STEP =      0.03 ,  TOTAL =        0.2 SECONDS (    0.0 MIN)
 CPU UTILIZATION:   STEP =    101.37%,  TOTAL =      52.74%

          --------------------------
          R-B3LYP    SCF CALCULATION
          --------------------------

     NUCLEAR ENERGY =        31.2237524994
     MAXIT =   30     NPUNCH=    2
     EXTRAP=T  DAMP=T  SHIFT=T  RSTRCT=F  DIIS=T  DEM=F  SOSCF=F
     DENSITY CONV=  1.00E-05
     MEMORY REQUIRED FOR SCF STEP=    142083 WORDS.

 XC FUNCTIONAL: SLATER + BECKE 88 + HF EXCHANGE, LYP 88 + VWN 1 RPA CORRELATION 


 ITER EX DEM  TOTAL ENERGY      E CHANGE  DENSITY CHANGE    DIIS ERROR
          * * *   INITIATING DIIS PROCEDURE   * * *
   1  0  0  -114.155139912  -114.155139912   0.493202351   0.519682224
   2  1  0  -114.305404834    -0.150264923   0.277378766   0.426593018
   3  2  0  -114.482610993    -0.177206158   0.100123578   0.104407140
   4  3  0  -114.494354684    -0.011743691   0.039459835   0.056188325
   5  4  0  -114.500419286    -0.006064602   0.004947404   0.004752864
   6  5  0  -114.500471137    -0.000051852   0.000447572   0.000507657
   7  6  0  -114.500471659    -0.000000522   0.000029698   0.000032482
   8  7  0  -114.500471662    -0.000000003   0.000009598   0.000018081
   9  8  0  -114.500471663     0.000000000   0.000000552   0.000000880
  10  9  0  -114.500471663     0.000000000   0.000000047   0.000000072

          -----------------
          DENSITY CONVERGED
          -----------------
     TIME TO FORM FOCK OPERATORS =       0.84 SECONDS (       0.08 SEC/ITER)
     OF THE ABOVE TIME, DFT PART =       0.76 SECONDS (       0.08 SEC/ITER)
     TIME TO SOLVE SCF EQUATIONS =       0.00 SECONDS (       0.00 SEC/ITER)

 FINAL ENERGY IS     -114.5004716625 AFTER  10 ITERATIONS
 DFT EXCHANGE + CORRELATION ENERGY IS      -11.8867945159
 INTEGRATED TOTAL ELECTRON NUMBER  IS       15.9999982441


          ------------
          EIGENVECTORS
          ------------

                      1          2          3          4          5
                  -19.1698   -10.2891    -1.0609    -0.6375    -0.4953
                     A          A          A          A          A   
    1  H   1  S  -0.000060  -0.000387   0.027482  -0.180080  -0.194737
    2  H   1  S   0.000185   0.002147  -0.008445  -0.067874  -0.135253
    3  C   2  S  -0.000031   0.992747  -0.116578   0.163877  -0.000003
    4  C   2  S  -0.000559   0.048664   0.226116  -0.343017   0.000007
    5  C   2  X  -0.000071  -0.000958  -0.187383  -0.220997   0.047987
    6  C   2  Y  -0.000002  -0.000024  -0.004733  -0.005582   0.081875
    7  C   2  Z  -0.000008  -0.000110  -0.021574  -0.025455  -0.434759
    8  C   2  S   0.002593  -0.007735   0.092632  -0.327519   0.000011
    9  C   2  X  -0.001954  -0.001717   0.014261  -0.087172   0.017436
   10  C   2  Y  -0.000049  -0.000043   0.000360  -0.002202   0.029746
   11  C   2  Z  -0.000225  -0.000198   0.001642  -0.010038  -0.157953
   12  C   2 XX   0.000841  -0.008625   0.025657   0.013344   0.001534
   13  C   2 YY  -0.000001  -0.009909  -0.019661   0.011528   0.000066
   14  C   2 ZZ   0.000031  -0.009644  -0.016683  -0.015852  -0.001600
   15  C   2 XY   0.000025   0.000044   0.001384  -0.000648   0.001533
   16  C   2 XZ   0.000109   0.000139   0.005719   0.003773  -0.007919
   17  C   2 YZ  -0.000002  -0.000052  -0.000390   0.006180  -0.000029
   18  H   3  S  -0.000060  -0.000387   0.027481  -0.180069   0.194740
   19  H   3  S   0.000185   0.002147  -0.008445  -0.067869   0.135255
   20  O   4  S  -0.992821  -0.000220  -0.196288  -0.087171   0.000003
   21  O   4  S  -0.025954   0.000146   0.435612   0.198954  -0.000006
   22  O   4  X  -0.001219  -0.000042   0.164910  -0.100604   0.030922
   23  O   4  Y  -0.000031  -0.000001   0.004165  -0.002541   0.052741
   24  O   4  Z  -0.000140  -0.000005   0.018987  -0.011586  -0.280050
   25  O   4  S  -0.012209  -0.001414   0.370165   0.250645  -0.000012
   26  O   4  X  -0.001174  -0.001617   0.045658  -0.054901   0.016259
   27  O   4  Y  -0.000030  -0.000041   0.001153  -0.001386   0.027733
   28  O   4  Z  -0.000135  -0.000186   0.005257  -0.006323  -0.147263
   29  O   4 XX   0.007421  -0.000417   0.011305  -0.013415   0.004231
   30  O   4 YY   0.007955   0.000204  -0.003302   0.001001   0.000182
   31  O   4 ZZ   0.007911   0.000078  -0.008226  -0.001097  -0.004412
   32  O   4 XY  -0.000017  -0.000021   0.000296  -0.000470   0.004228
   33  O   4 XZ  -0.000066  -0.000067   0.002602  -0.001671  -0.021844
   34  O   4 YZ   0.000007   0.000025   0.001200   0.000383  -0.000079

                      6          7          8          9         10
                   -0.4496    -0.3992    -0.2685    -0.0421     0.1013
                     A          A          A          A          A   
    1  H   1  S   0.080656   0.000000   0.183065   0.000003   0.094751
    2  H   1  S   0.080379  -0.000001   0.284235   0.000009   1.350656
    3  C   2  S   0.029688   0.000001   0.000000   0.000002   0.132142
    4  C   2  S  -0.087055  -0.000001   0.000001  -0.000003  -0.222514
    5  C   2  X   0.359091  -0.016396  -0.016152  -0.022626  -0.246348
    6  C   2  Y   0.009075   0.353620  -0.027558   0.488051  -0.006234
    7  C   2  Z   0.041340   0.064786   0.146331   0.089413  -0.028345
    8  C   2  S  -0.025700  -0.000002  -0.000005  -0.000012  -1.849379
    9  C   2  X   0.082399  -0.009503   0.003463  -0.026594  -0.616123
   10  C   2  Y   0.002084   0.205019   0.005908   0.573512  -0.015585
   11  C   2  Z   0.009484   0.037560  -0.031367   0.105071  -0.070925
   12  C   2 XX  -0.005292   0.002177  -0.009927  -0.002504   0.010773
   13  C   2 YY   0.006968  -0.001186  -0.000428   0.001365   0.000115
   14  C   2 ZZ   0.018428  -0.000991   0.010354   0.001140   0.013872
   15  C   2 XY  -0.000061  -0.027088  -0.009923   0.031162   0.000659
   16  C   2 XZ  -0.003128  -0.004824   0.051260   0.005549  -0.000338
   17  C   2 YZ  -0.002658  -0.003248   0.000186   0.003737  -0.003033
   18  H   3  S   0.080661  -0.000001  -0.183068   0.000004   0.094755
   19  H   3  S   0.080382  -0.000003  -0.284239   0.000010   1.350690
   20  O   4  S  -0.086699   0.000001   0.000000   0.000000  -0.006117
   21  O   4  S   0.171238  -0.000001   0.000000   0.000002   0.019531
   22  O   4  X  -0.519350  -0.023188   0.063050   0.020124   0.081771
   23  O   4  Y  -0.013112   0.500382   0.107572  -0.434141   0.002072
   24  O   4  Z  -0.059801   0.091674  -0.571209  -0.079537   0.009407
   25  O   4  S   0.411494  -0.000003   0.000002  -0.000010  -0.002214
   26  O   4  X  -0.257035  -0.014288   0.045088   0.021160   0.085547
   27  O   4  Y  -0.006489   0.308303   0.076924  -0.456580   0.002170
   28  O   4  Z  -0.029596   0.056484  -0.408471  -0.083650   0.009840
   29  O   4 XX  -0.037532  -0.002615   0.003409   0.000058  -0.002318
   30  O   4 YY  -0.001570   0.001425   0.000147  -0.000031  -0.004576
   31  O   4 ZZ  -0.004060   0.001190  -0.003556  -0.000025   0.006993
   32  O   4 XY  -0.001101   0.032539   0.003408  -0.000719   0.000361
   33  O   4 XZ  -0.004522   0.005795  -0.017604  -0.000128  -0.001187
   34  O   4 YZ   0.000338   0.003902  -0.000064  -0.000086  -0.002593

                     11         12         13         14         15
                    0.1820     0.2188     0.5066     0.6236     0.6268
                     A          A          A          A          A   
    1  H   1  S   0.018749  -0.012806  -0.000001   0.553746  -0.313457
    2  H   1  S   1.559763   0.212722  -0.000003   0.254570  -0.933565
    3  C   2  S   0.000003  -0.048338  -0.000001   0.080179   0.000059
    4  C   2  S  -0.000006   0.007644   0.000008   0.434266   0.000315
    5  C   2  X   0.058476  -0.123905   0.046784   0.628360   0.081579
    6  C   2  Y   0.099779  -0.003131  -1.009271   0.015771   0.138386
    7  C   2  Z  -0.529832  -0.014256  -0.184904   0.072896  -0.734720
    8  C   2  S  -0.000074   1.593531  -0.000004  -1.514174  -0.001070
    9  C   2  X   0.144135  -1.979740  -0.053183  -1.069679  -0.246173
   10  C   2  Y   0.245996  -0.050006   1.147388  -0.026720  -0.418655
   11  C   2  Z  -1.306254  -0.227928   0.210209  -0.124868   2.222876
   12  C   2 XX   0.000794  -0.016285  -0.001486   0.127198   0.010761
   13  C   2 YY   0.000035   0.010911   0.000811  -0.086384   0.000396
   14  C   2 ZZ  -0.000828   0.020901   0.000677   0.151297  -0.011012
   15  C   2 XY   0.000794  -0.000530   0.018492   0.012238   0.010670
   16  C   2 XZ  -0.004101  -0.004950   0.003293  -0.001833  -0.055074
   17  C   2 YZ  -0.000014  -0.002420   0.002217  -0.052223  -0.000241
   18  H   3  S  -0.018755  -0.012808   0.000000   0.553287   0.314278
   19  H   3  S  -1.559628   0.212768  -0.000004   0.253119   0.933903
   20  O   4  S   0.000002   0.112827   0.000000  -0.028221  -0.000021
   21  O   4  S  -0.000002  -0.087117   0.000001   0.297475   0.000219
   22  O   4  X  -0.019367  -0.189070   0.001772   0.173088   0.040053
   23  O   4  Y  -0.033041  -0.004773  -0.038253   0.004326   0.068117
   24  O   4  Z   0.175448  -0.021771  -0.007007   0.020200  -0.361655
   25  O   4  S  -0.000027  -2.200647  -0.000002  -0.405644  -0.000312
   26  O   4  X  -0.040874  -0.892346   0.005562   0.207259   0.022273
   27  O   4  Y  -0.069720  -0.022536  -0.119995   0.005207   0.037748
   28  O   4  Z   0.370212  -0.102745  -0.021985   0.024024  -0.200423
   29  O   4 XX   0.001919   0.005838   0.005734  -0.008457   0.005597
   30  O   4 YY   0.000083   0.076042  -0.003124   0.061637   0.000288
   31  O   4 ZZ  -0.002000   0.077455  -0.002609   0.097214  -0.005776
   32  O   4 XY   0.001918  -0.001990  -0.071337  -0.001117   0.005603
   33  O   4 XZ  -0.009910  -0.009636  -0.012704  -0.014002  -0.028962
   34  O   4 YZ  -0.000036  -0.000754  -0.008554  -0.008450  -0.000112

                     16         17         18         19         20
                    0.6940     0.8139     0.8371     0.8905     0.9601
                     A          A          A          A          A   
    1  H   1  S   0.332239  -0.741936  -0.316723   0.455330   0.000001
    2  H   1  S  -0.062788   1.370027   0.699448  -1.260843   0.000013
    3  C   2  S  -0.027464   0.000009  -0.062500   0.003693   0.000000
    4  C   2  S   0.894993  -0.000154   0.323700  -1.467019   0.000011
    5  C   2  X  -0.779917  -0.077870   0.421590  -0.330486  -0.002931
    6  C   2  Y  -0.019697  -0.132721   0.010632  -0.008343   0.063308
    7  C   2  Z  -0.089802   0.704734   0.048652  -0.038109   0.011598
    8  C   2  S  -0.970179   0.000433  -1.520958   2.901145  -0.000031
    9  C   2  X   0.391355   0.125174  -0.324488   0.565680   0.021976
   10  C   2  Y   0.009880   0.213413  -0.008171   0.014284  -0.474283
   11  C   2  Z   0.045054  -1.133212  -0.037537   0.065229  -0.086897
   12  C   2 XX  -0.119693   0.035292  -0.207948  -0.177428  -0.002270
   13  C   2 YY   0.038755   0.001500   0.121026  -0.082185   0.001237
   14  C   2 ZZ   0.184126  -0.036797   0.022960   0.025619   0.001036
   15  C   2 XY  -0.000852   0.035260  -0.011998   0.000008   0.028252
   16  C   2 XZ  -0.040076  -0.182140  -0.031691  -0.026705   0.005032
   17  C   2 YZ  -0.033736  -0.000667   0.020052  -0.024879   0.003387
   18  H   3  S   0.332256   0.742086  -0.316497   0.455223  -0.000001
   19  H   3  S  -0.062822  -1.370410   0.699028  -1.260622   0.000011
   20  O   4  S   0.041550   0.000001  -0.003193   0.004381   0.000000
   21  O   4  S  -0.036677  -0.000048   0.457903   0.336975  -0.000003
   22  O   4  X  -0.400112   0.020643  -0.645501  -0.248888   0.043208
   23  O   4  Y  -0.010102   0.035082  -0.016298  -0.006293  -0.932179
   24  O   4  Z  -0.046070  -0.186267  -0.074351  -0.028643  -0.170788
   25  O   4  S   0.025255   0.000005  -0.417901  -0.810596   0.000010
   26  O   4  X  -0.187771  -0.018015   1.165108   0.364116  -0.057159
   27  O   4  Y  -0.004742  -0.030481   0.029422   0.009203   1.233191
   28  O   4  Z  -0.021617   0.161820   0.134171   0.041911   0.225939
   29  O   4 XX   0.214533  -0.021535   0.253474   0.109907   0.002476
   30  O   4 YY  -0.048512  -0.000938   0.153784   0.155385  -0.001353
   31  O   4 ZZ  -0.021133   0.022413   0.161580   0.121582  -0.001128
   32  O   4 XY   0.008291  -0.021495   0.003074  -0.002177  -0.030819
   33  O   4 XZ   0.031893   0.111037   0.012439  -0.001778  -0.005489
   34  O   4 YZ  -0.004536   0.000403  -0.001137   0.007333  -0.003695

                     21         22         23         24         25
                    1.0592     1.3773     1.5058     1.5101     1.6817
                     A          A          A          A          A   
    1  H   1  S   0.165583  -0.088917  -0.000002   0.000001   0.285204
    2  H   1  S   0.666446   0.048754   0.000012  -0.000004   0.167735
    3  C   2  S   0.000000   0.028948   0.000000   0.000000   0.025110
    4  C   2  S   0.000001   0.039230   0.000000  -0.000004   0.203176
    5  C   2  X  -0.029147  -0.166095   0.000000   0.009724   0.022659
    6  C   2  Y  -0.049725  -0.004195  -0.000018  -0.209816   0.000569
    7  C   2  Z   0.264035  -0.019124   0.000006  -0.038440   0.002612
    8  C   2  S  -0.000028   2.725538  -0.000006   0.000004  -1.023967
    9  C   2  X   0.176976  -1.724967   0.000005  -0.006304  -0.030955
   10  C   2  Y   0.301939  -0.043567   0.000015   0.136126  -0.000790
   11  C   2  Z  -1.603278  -0.198601  -0.000016   0.024939  -0.003558
   12  C   2 XX  -0.032313   0.185994  -0.006866   0.052559  -0.049419
   13  C   2 YY  -0.001395   0.027967   0.252768  -0.028668   0.447098
   14  C   2 ZZ   0.033705  -0.217263  -0.245901  -0.023889  -0.362456
   15  C   2 XY  -0.032300  -0.001716   0.078716  -0.653926  -0.035051
   16  C   2 XZ   0.166860   0.052887   0.051595  -0.116455   0.037500
   17  C   2 YZ   0.000610   0.056230  -0.748197  -0.078351   0.179314
   18  H   3  S  -0.165589  -0.088918   0.000002   0.000001   0.285227
   19  H   3  S  -0.666422   0.048764  -0.000012  -0.000005   0.167722
   20  O   4  S   0.000000   0.096248   0.000000   0.000000  -0.008207
   21  O   4  S  -0.000001   1.717004  -0.000004  -0.000002  -0.211260
   22  O   4  X   0.090351   0.304068   0.000000   0.006096  -0.018733
   23  O   4  Y   0.154158   0.007678  -0.000011  -0.131508  -0.000478
   24  O   4  Z  -0.818550   0.035008  -0.000001  -0.024093  -0.002150
   25  O   4  S   0.000009  -5.196513   0.000011   0.000007   0.638419
   26  O   4  X  -0.163502  -1.800896   0.000003   0.005598   0.237304
   27  O   4  Y  -0.278969  -0.045482  -0.000012  -0.120737   0.005999
   28  O   4  Z   1.481288  -0.207350   0.000007  -0.022119   0.027316
   29  O   4 XX   0.020843   0.586553  -0.004238  -0.042490  -0.086436
   30  O   4 YY   0.000896   0.399271   0.155912   0.023133   0.639159
   31  O   4 ZZ  -0.021739   0.322017  -0.151677   0.019355  -0.662662
   32  O   4 XY   0.020834   0.003430   0.048633   0.528650  -0.054232
   33  O   4 XZ  -0.107631   0.035178   0.031815   0.094139   0.070069
   34  O   4 YZ  -0.000388   0.018566  -0.461486   0.063424   0.288802

                     26         27         28         29         30
                    1.8500     1.9314     2.1262     2.2792     2.5411
                     A          A          A          A          A   
    1  H   1  S  -0.580117  -0.000048   0.401688  -0.244843  -0.000006
    2  H   1  S  -0.130257  -0.000013   0.051897   0.021977   0.000003
    3  C   2  S  -0.000001   0.000000   0.059449   0.060028   0.000000
    4  C   2  S   0.000006   0.000000   0.104470  -0.030505   0.000000
    5  C   2  X  -0.030308  -0.000002  -0.010386   0.156032   0.012365
    6  C   2  Y  -0.051718  -0.000004  -0.000262   0.003942  -0.266678
    7  C   2  Z   0.274628   0.000020  -0.001211   0.017975  -0.048857
    8  C   2  S   0.000025  -0.000002  -0.855709  -0.092269  -0.000002
    9  C   2  X  -0.089636  -0.000006   0.006479   0.411180   0.013012
   10  C   2  Y  -0.152916  -0.000013   0.000162   0.010386  -0.280678
   11  C   2  Z   0.811984   0.000071   0.000709   0.047359  -0.051421
   12  C   2 XX  -0.099927   0.005128  -0.250831  -0.971963  -0.076539
   13  C   2 YY  -0.004296  -0.189000   0.935124   0.362005   0.041699
   14  C   2 ZZ   0.104223   0.183871  -0.492427   0.840800   0.034836
   15  C   2 XY  -0.099891  -0.058906  -0.070744  -0.026250   0.952194
   16  C   2 XZ   0.516057  -0.038542   0.024293  -0.241352   0.169564
   17  C   2 YZ   0.001850   0.559429   0.314274  -0.116152   0.114170
   18  H   3  S   0.580106   0.000047   0.401649  -0.244825  -0.000006
   19  H   3  S   0.130245   0.000012   0.051873   0.021987   0.000003
   20  O   4  S   0.000000   0.000000  -0.001033   0.000534  -0.000001
   21  O   4  S   0.000003  -0.000002  -0.011067   0.560139   0.000004
   22  O   4  X  -0.029886  -0.000003   0.148038   0.648743  -0.000098
   23  O   4  Y  -0.050984  -0.000004   0.003739   0.016385   0.002191
   24  O   4  Z   0.270726   0.000020   0.017038   0.074697   0.000402
   25  O   4  S  -0.000017   0.000006   0.335704  -0.213624   0.000005
   26  O   4  X   0.051814   0.000006   0.065790  -0.323369  -0.022152
   27  O   4  Y   0.088408   0.000008   0.001663  -0.008169   0.477971
   28  O   4  Z  -0.469447  -0.000041   0.007595  -0.037244   0.087567
   29  O   4 XX   0.128676  -0.007446  -0.077546   0.018569  -0.081637
   30  O   4 YY   0.005488   0.274417  -0.385895   0.308469   0.044484
   31  O   4 ZZ  -0.134165  -0.266974   0.543406   0.071122   0.037149
   32  O   4 XY   0.128619   0.085529   0.032674  -0.014442   1.015734
   33  O   4 XZ  -0.664487   0.055956  -0.078230  -0.008466   0.180881
   34  O   4 YZ  -0.002351  -0.812266  -0.207460   0.051675   0.121792

                     31         32         33         34
                    2.6569     2.9108     3.6831     4.0555
                     A          A          A          A   
    1  H   1  S   0.362225   0.062920   0.079505  -0.185887
    2  H   1  S  -0.303443  -0.023731  -0.001509   0.458407
    3  C   2  S   0.000001  -0.055758   0.008000   0.474902
    4  C   2  S  -0.000006  -1.318659  -0.220452  -2.919265
    5  C   2  X  -0.013857   1.348409  -0.060128  -0.293071
    6  C   2  Y  -0.023627   0.034056  -0.001518  -0.007401
    7  C   2  Z   0.125460   0.155251  -0.006924  -0.033739
    8  C   2  S  -0.000004  -1.385369  -1.860925  -1.112982
    9  C   2  X  -0.032704   0.776202   1.023118  -0.437524
   10  C   2  Y  -0.055790   0.019605   0.025841  -0.011055
   11  C   2  Z   0.296243   0.089366   0.117793  -0.050381
   12  C   2 XX   0.208905  -0.738755   0.188161   2.147334
   13  C   2 YY   0.009013   0.349650   0.163115   1.722807
   14  C   2 ZZ  -0.217910   0.254936   0.079274   1.784170
   15  C   2 XY   0.208810  -0.033834  -0.001424   0.013822
   16  C   2 XZ  -1.078721  -0.134362   0.014177   0.049265
   17  C   2 YZ  -0.003905   0.014586   0.019045  -0.011190
   18  H   3  S  -0.362197   0.062915   0.079501  -0.185872
   19  H   3  S   0.303438  -0.023733  -0.001516   0.458389
   20  O   4  S   0.000000  -0.046762  -0.513841   0.035631
   21  O   4  S  -0.000007  -0.682625  -0.091336  -0.048769
   22  O   4  X   0.005052   0.299988  -0.254746  -0.257014
   23  O   4  Y   0.008631   0.007577  -0.006433  -0.006491
   24  O   4  Z  -0.045834   0.034539  -0.029331  -0.029591
   25  O   4  S   0.000005   2.882873   5.249686  -0.652523
   26  O   4  X   0.044895   1.769824   1.054731  -0.458441
   27  O   4  Y   0.076594   0.044697   0.026637  -0.011576
   28  O   4  Z  -0.406714   0.203772   0.121440  -0.052783
   29  O   4 XX   0.177648   1.490838  -1.537031  -0.243507
   30  O   4 YY   0.007650  -0.698007  -1.770994   0.186672
   31  O   4 ZZ  -0.185303  -0.652488  -1.757382   0.141919
   32  O   4 XY   0.177584   0.064330   0.007099  -0.013558
   33  O   4 XZ  -0.917416   0.288883   0.029752  -0.052158
   34  O   4 YZ  -0.003319   0.003325  -0.001615   0.007414

 WARNING! THIS VERSION OF FIREFLY IS PROBABLY OUTDATED!
 PLEASE CHECK FIREFLY HOMEPAGE FOR INFORMATION ON UPDATES!


                         TIMING STATISTICS ON RANK 0:
 CPU        TIME:   STEP =      0.86 ,  TOTAL =        1.0 SECONDS (    0.0 MIN)
 WALL CLOCK TIME:   STEP =      0.86 ,  TOTAL =        1.1 SECONDS (    0.0 MIN)
 CPU UTILIZATION:   STEP =    100.35%,  TOTAL =      90.03%

 COMPUTING XC FUNCTIONAL SECOND DERIVATIVES FOR SUBSEQUENT TDDFT CALCULATIONS...
 ...... END OF DFT CALCULATION ......

                         TIMING STATISTICS ON RANK 0:
 CPU        TIME:   STEP =      0.05 ,  TOTAL =        1.0 SECONDS (    0.0 MIN)
 WALL CLOCK TIME:   STEP =      0.16 ,  TOTAL =        1.3 SECONDS (    0.0 MIN)
 CPU UTILIZATION:   STEP =     28.45%,  TOTAL =      81.96%


                       ----------------------------------
                       properties for the R-B3LYP density
                       ----------------------------------

          -----------------
          ENERGY COMPONENTS
          -----------------

         WAVEFUNCTION NORMALIZATION =       1.0000000000

                ONE ELECTRON ENERGY =    -217.3646209035
                TWO ELECTRON ENERGY =      71.6403967416
           NUCLEAR REPULSION ENERGY =      31.2237524994
                                      ------------------
                       TOTAL ENERGY =    -114.5004716625

 ELECTRON-ELECTRON POTENTIAL ENERGY =      71.6403967416
  NUCLEUS-ELECTRON POTENTIAL ENERGY =    -330.8494166911
   NUCLEUS-NUCLEUS POTENTIAL ENERGY =      31.2237524994
                                      ------------------
             TOTAL POTENTIAL ENERGY =    -227.9852674502
               TOTAL KINETIC ENERGY =     113.4847957876
                 VIRIAL RATIO (V/T) =       2.0089498850

          ---------------------------------------
          MULLIKEN AND LOWDIN POPULATION ANALYSES
          ---------------------------------------

     MULLIKEN ATOMIC POPULATION IN EACH MOLECULAR ORBITAL

                      1          2          3          4          5

                  2.000000   2.000000   2.000000   2.000000   2.000000

    1            -0.000005   0.000392   0.000844   0.265656   0.295834
    2            -0.000774   2.000040   0.537254   1.178874   0.978083
    3            -0.000005   0.000392   0.000844   0.265626   0.295848
    4             2.000783  -0.000823   1.461058   0.289844   0.430235

                      6          7          8

                  2.000000   2.000000   2.000000

    1             0.063146   0.000000   0.252926
    2             0.396313   0.740234   0.089517
    3             0.063153   0.000000   0.252940
    4             1.477388   1.259766   1.404617

               ----- POPULATIONS IN EACH AO -----
                             MULLIKEN      LOWDIN
              1  H   1  S     0.53428     0.49260
              2  H   1  S     0.34451     0.38289
              3  C   2  S     1.99183     1.97468
              4  C   2  S     0.73252     0.37802
              5  C   2  X     0.74828     0.66269
              6  C   2  Y     0.45951     0.41630
              7  C   2  Z     0.75027     0.64649
              8  C   2  S     0.53319     0.34618
              9  C   2  X     0.10946     0.28036
             10  C   2  Y     0.27902     0.33003
             11  C   2  Z     0.26200     0.42910
             12  C   2 XX     0.01949     0.20791
             13  C   2 YY    -0.02686     0.10671
             14  C   2 ZZ     0.00620     0.16451
             15  C   2 XY     0.01273     0.02343
             16  C   2 XZ     0.04121     0.09178
             17  C   2 YZ     0.00069     0.00104
             18  H   3  S     0.53428     0.49146
             19  H   3  S     0.34452     0.38274
             20  O   4  S     1.99274     1.97668
             21  O   4  S     0.92239     0.72109
             22  O   4  X     0.94746     0.87543
             23  O   4  Y     0.76491     0.71276
             24  O   4  Z     1.14340     1.09902
             25  O   4  S     0.98629     0.48261
             26  O   4  X     0.39956     0.52614
             27  O   4  Y     0.50295     0.53724
             28  O   4  Z     0.65902     0.69958
             29  O   4 XX     0.00252     0.19066
             30  O   4 YY    -0.00483     0.17216
             31  O   4 ZZ    -0.01241     0.16679
             32  O   4 XY     0.01127     0.01479
             33  O   4 XZ     0.00745     0.01594
             34  O   4 YZ     0.00016     0.00021

          ----- MULLIKEN ATOMIC OVERLAP POPULATIONS -----
          (OFF-DIAGONAL ELEMENTS NEED TO BE MULTIPLIED BY 2)

             1           2           3           4

    1    0.6977139
    2    0.3310975   4.6999225
    3   -0.0949969   0.3310971   0.6977204
    4   -0.0550217   0.5574244  -0.0550219   7.8754862

          TOTAL MULLIKEN AND LOWDIN ATOMIC POPULATIONS
       ATOM         MULL.POP.    CHARGE          LOW.POP.     CHARGE
    1 H             0.878793    0.121207         0.875489    0.124511
    2 C             5.919541    0.080459         6.059222   -0.059222
    3 H             0.878799    0.121201         0.874205    0.125795
    4 O             8.322867   -0.322867         8.191085   -0.191085

          -------------------------------
          BOND ORDER AND VALENCE ANALYSIS     BOND ORDER THRESHOLD=0.050
          -------------------------------

                   BOND                       BOND                       BOND
  ATOM PAIR DIST  ORDER      ATOM PAIR DIST  ORDER      ATOM PAIR DIST  ORDER
    1   2  1.110  0.876        2   3  1.110  0.876        2   4  1.206  2.017

                       TOTAL       BONDED        FREE
      ATOM            VALENCE     VALENCE     VALENCE
    1 H                 0.921       0.921       0.000
    2 C                 3.770       3.770       0.000
    3 H                 0.921       0.921       0.000
    4 O                 2.088       2.088       0.000

          ---------------------
          ELECTROSTATIC MOMENTS
          ---------------------

 POINT   1           X           Y           Z (BOHR)    CHARGE
                -2.327646    6.927311   -0.129384        0.00 (A.U.)
         DX          DY          DZ         /D/  (DEBYE)
     2.171341    0.054842    0.250028    2.186377
 ...... END OF PROPERTY EVALUATION ......

                         TIMING STATISTICS ON RANK 0:
 CPU        TIME:   STEP =      0.05 ,  TOTAL =        1.1 SECONDS (    0.0 MIN)
 WALL CLOCK TIME:   STEP =      0.13 ,  TOTAL =        1.4 SECONDS (    0.0 MIN)
 CPU UTILIZATION:   STEP =     34.67%,  TOTAL =      77.37%
 ......END OF NBO ANALYSIS......

                         TIMING STATISTICS ON RANK 0:
 CPU        TIME:   STEP =      0.00 ,  TOTAL =        1.1 SECONDS (    0.0 MIN)
 WALL CLOCK TIME:   STEP =      0.00 ,  TOTAL =        1.4 SECONDS (    0.0 MIN)
 CPU UTILIZATION:   STEP =      0.00%,  TOTAL =      77.34%

                    ------------------------------------
                    ADIABATIC TIME DEPENDENT DFT ENERGY
                    PROGRAM WRITTEN BY ALEX A. GRANOVSKY
                    ------------------------------------

 # CORE ORBITALS      =    2
 # OCCUPIED ORBITALS  =    6
 # MOLECULAR ORBITALS =   34
 # BASIS FUNCTIONS    =   34

 NUMBER OF TDDFT SPIN-ADAPTED ANTISYMMETRIZED PRODUCTS (SAPS) IS     156

 MIN MEMORY REQ. FOR TDDFT ENERGY FOCK-LIKE BUILDS =      2312 WORDS
 MEMORY REQ. FOR SINGLE BATCH BUILDS               =     23120 WORDS
 MEMORY AVAILABLE                                  =  19816414 WORDS

 SINGLE BATCH ENERGY CALCULATION WILL BE PERFORMED

 UNIT VECTOR GUESS AT TDDFT COEFICIENTS ...

 -------------------------------------------------------------------
 USING DAVIDSON ALGORITHM TO FIND TDDFT EIGENVALUES AND EIGENVECTORS
 -------------------------------------------------------------------

 NUMBER OF STATES REQUESTED =     4
 NUMBER OF GUESS VECTORS    =    10
 MAX. NUMB. OF EXPAN. VECS. =    64
 MAX. NUMB. OF ITERATIONS   =    50
 CONVERGENCE CRITERION      =    3.0E-05

 STARTING DAVIDSON ITERATIONS

 STATE      ITERATION      ENERGY         RESIDUE NORM
   1            1     -114.3491540646     0.04625239
   2            1     -114.1619387567     0.10052260
   3            1     -114.1554323056     0.17130163
   4            1     -114.1209561739     0.02735279

   1            2     -114.3502158824     0.00165066
   2            2     -114.1660803523     0.01681942
   3            2     -114.1628170262     0.02582922
   4            2     -114.1216428429     0.00281022

   1            3     -114.3502171543     0.00002655
   2            3     -114.1662351203     0.00287956
   3            3     -114.1631499338     0.00650211
   4            3     -114.1216460357     0.00040049

   1            4     -114.3502171547     0.00000127
   2            4     -114.1662400501     0.00058069
   3            4     -114.1631684350     0.00118829
   4            4     -114.1216460932     0.00039425

   1            5     -114.3502171547     0.00000113
   2            5     -114.1662402196     0.00019356
   3            5     -114.1631690140     0.00026225
   4            5     -114.1396173357     0.05044135

   1            6     -114.3502171547     0.00000100
   2            6     -114.1662402361     0.00002383
   3            6     -114.1631690432     0.00003902
   4            6     -114.1405524808     0.01345537

   1            7     -114.3502171547     0.00000094
   2            7     -114.1662402363     0.00000451
   3            7     -114.1631690439     0.00000938
   4            7     -114.1406119994     0.00264213

   1            8     -114.3502171547     0.00000093
   2            8     -114.1662402363     0.00000452
   3            8     -114.1631690439     0.00000827
   4            8     -114.1406150948     0.00071221

   1            9     -114.3502171547     0.00000093
   2            9     -114.1662402363     0.00000431
   3            9     -114.1631690439     0.00000652
   4            9     -114.1406153055     0.00025655

   1           10     -114.3502171547     0.00000092
   2           10     -114.1662402363     0.00000420
   3           10     -114.1631690439     0.00000515
   4           10     -114.1406153315     0.00005064

   1           11     -114.3502171547     0.00000092
   2           11     -114.1662402363     0.00000420
   3           11     -114.1631690439     0.00000508
   4           11     -114.1406153327     0.00001030

 ALL STATES CONVERGED IN   11 ITERATIONS

 CONVERGED STATE    1 ENERGY=     -114.3502171547
 CONVERGED STATE    2 ENERGY=     -114.1662402363
 CONVERGED STATE    3 ENERGY=     -114.1631690439
 CONVERGED STATE    4 ENERGY=     -114.1406153327


        --------------------------------------------------------
        RESULTS FROM SPIN-ADAPTED ANTISYMMETRIZED PRODUCT (SAPS)
        BASED  ADIABATIC TIME DEPENDENT DFT ENERGY CALCULATION
        --------------------------------------------------------

 PRINTING TDDFT COEFFICIENTS LARGER THAN  0.050000

 DFT REFERENCE ENERGY  =      -114.5004716625


 EXCITED STATE   1  ENERGY=      -114.3502171547  S = 0.0  SPACE SYM = A   

          ----------------------------------------------
          EXCITATIONS & DEEXCITATIONS     SAP COEFFICENT
          FROM MO     TO MO
          ----------------------------------------------
             8          9                  -0.99985305
             9          8                   0.03518731
          ----------------------------------------------


 EXCITED STATE   2  ENERGY=      -114.1662402363  S = 0.0  SPACE SYM = A   

          ----------------------------------------------
          EXCITATIONS & DEEXCITATIONS     SAP COEFFICENT
          FROM MO     TO MO
          ----------------------------------------------
             5         10                  -0.05155293
            10          5                  -0.02048331
             8         10                  -0.99802552
            10          8                   0.02487232
          ----------------------------------------------


 EXCITED STATE   3  ENERGY=      -114.1631690439  S = 0.0  SPACE SYM = A   

          ----------------------------------------------
          EXCITATIONS & DEEXCITATIONS     SAP COEFFICENT
          FROM MO     TO MO
          ----------------------------------------------
             3          9                  -0.05031315
             9          3                  -0.01518946
             6          9                   0.99854246
             9          6                  -0.03092721
          ----------------------------------------------


 EXCITED STATE   4  ENERGY=      -114.1406153327  S = 0.0  SPACE SYM = A   

          ----------------------------------------------
          EXCITATIONS & DEEXCITATIONS     SAP COEFFICENT
          FROM MO     TO MO
          ----------------------------------------------
             4         10                   0.06892523
            10          4                   0.02207361
             5         11                   0.10745281
            11          5                   0.02933083
             6         12                  -0.07963004
            12          6                  -0.01942579
             6         16                  -0.05895584
            16          6                  -0.02875446
             7          9                  -0.87657552
             9          7                   0.09769329
             8         11                   0.45906667
            11          8                   0.02288871
             8         15                  -0.05239136
            15          8                  -0.02175942
          ----------------------------------------------

 ------------------------------------------------------------------------------
                             TDDFT EXCITATION ENERGIES
 STATE       HARTREE        EV      KCAL/MOL       CM-1   NANOMETERS  OSC. STR.
 ------------------------------------------------------------------------------
  1A      0.1502545078    4.0886     94.2861     32977.05     303.24  0.0000000
  1A      0.3342314262    9.0949    209.7334     73355.32     136.32  0.1601086
  1A      0.3373026186    9.1785    211.6606     74029.37     135.08  0.0015754
  1A      0.3598563298    9.7922    225.8133     78979.34     126.62  0.0403700


              Unrelaxed dipole moments and transition dipoles
               States                  <i|mu|j>     (e*bohr)
               i    j         x            y            z

               0    0    0.8542648    0.0215763    0.0983680
               0    1   -0.0000013   -0.0000043    0.0000072
               0    2   -0.0914241   -0.1559670    0.8281720
               0    3   -0.0038017    0.0822431    0.0150771
               0    4    0.4073972    0.0102857    0.0468753

               1    0   -0.0000013   -0.0000043    0.0000072
               1    1    0.1811258    0.0045917    0.0208528
               1    2   -0.0035458    0.0760247    0.0139264
               1    3    0.0044273    0.0075089   -0.0398601
               1    4   -0.0000023   -0.0000004    0.0000122

               2    0   -0.0914241   -0.1559670    0.8281720
               2    1   -0.0035458    0.0760247    0.0139264
               2    2   -1.7982178   -0.0454506   -0.2069596
               2    3   -0.0000160   -0.0000073    0.0000076
               2    4   -0.0986036   -0.1682960    0.8936779

               3    0   -0.0038017    0.0822431    0.0150771
               3    1    0.0044273    0.0075089   -0.0398601
               3    2   -0.0000160   -0.0000073    0.0000076
               3    3   -0.3318666   -0.0083724   -0.0382115
               3    4    0.0052754   -0.1138230   -0.0208437

               4    0    0.4073972    0.0102857    0.0468753
               4    1   -0.0000023   -0.0000004    0.0000122
               4    2   -0.0986036   -0.1682960    0.8936779
               4    3    0.0052754   -0.1138230   -0.0208437
               4    4   -0.3959418   -0.0099801   -0.0456454


 -TDDFT- ENERGY                TOOK      3.806 SECONDS

 TDDFT NATURAL ORBITAL OCCUPATION NUMBERS FOR EXCITED STATE    1 ARE
  2.0000 2.0000 2.0000 2.0000 1.9999 1.9999 1.9997 1.0026 0.9974 0.0003
  0.0001 0.0001 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
  0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
  0.0000 0.0000 0.0000 0.0000
 THERE ARE    15.0022 ELECTRONS IN PRINCIPAL TDDFT NATURAL ORBITALS.
 THERE ARE     0.9978 ELECTRONS IN SECONDARY TDDFT NATURAL ORBITALS.
 TDDFT NATURAL ORBITALS HAVE BEEN PUNCHED.

 ..... DONE WITH TDDFT ENERGY .....


                         TIMING STATISTICS ON RANK 0:
 CPU        TIME:   STEP =      3.81 ,  TOTAL =        4.9 SECONDS (    0.1 MIN)
 WALL CLOCK TIME:   STEP =      3.82 ,  TOTAL =        5.2 SECONDS (    0.1 MIN)
 CPU UTILIZATION:   STEP =     99.53%,  TOTAL =      93.61%

     -------------------------------------------------------------
     TDDFT PROPERTIES...FOR THE WAVEFUNCTION OF EXCITED STATE    1
                    (USING THE UNRELAXED DENSITY)
     -------------------------------------------------------------


                   ------------------------------------------
                   properties for the Unrelaxed TDDFT density
                   ------------------------------------------

          -----------------
          ENERGY COMPONENTS
          -----------------

         WAVEFUNCTION NORMALIZATION =       1.0000000000

                ONE ELECTRON ENERGY =    -216.7913997066
                TWO ELECTRON ENERGY =      71.2174300525
           NUCLEAR REPULSION ENERGY =      31.2237524994
                                      ------------------
                       TOTAL ENERGY =    -114.3502171547

 ELECTRON-ELECTRON POTENTIAL ENERGY =      71.2174300525
  NUCLEUS-ELECTRON POTENTIAL ENERGY =    -330.0710073346
   NUCLEUS-NUCLEUS POTENTIAL ENERGY =      31.2237524994
                                      ------------------
             TOTAL POTENTIAL ENERGY =    -227.6298247827
               TOTAL KINETIC ENERGY =     113.2796076280
                 VIRIAL RATIO (V/T) =       2.0094510349

          ---------------------------------------
          MULLIKEN AND LOWDIN POPULATION ANALYSES
          ---------------------------------------

     MULLIKEN ATOMIC POPULATION IN EACH MOLECULAR ORBITAL

                      1          2          3          4          5

                  2.000000   2.000000   2.000000   2.000000   2.000000

    1            -0.000005   0.000392   0.000844   0.265656   0.295834
    2            -0.000774   2.000040   0.537254   1.178874   0.978083
    3            -0.000005   0.000392   0.000844   0.265626   0.295848
    4             2.000783  -0.000823   1.461058   0.289844   0.430235

                      6          7          8

                  2.000000   2.000000   2.000000

    1             0.063146   0.000000   0.252926
    2             0.396313   0.740234   0.089517
    3             0.063153   0.000000   0.252940
    4             1.477388   1.259766   1.404617

               ----- POPULATIONS IN EACH AO -----
                             MULLIKEN      LOWDIN
              1  H   1  S     0.47530     0.43205
              2  H   1  S     0.28675     0.34356
              3  C   2  S     1.99183     1.97468
              4  C   2  S     0.73251     0.37802
              5  C   2  X     0.74872     0.66317
              6  C   2  Y     0.78755     0.74199
              7  C   2  Z     0.74001     0.64026
              8  C   2  S     0.53318     0.34617
              9  C   2  X     0.11014     0.28080
             10  C   2  Y     0.56968     0.58395
             11  C   2  Z     0.27689     0.42929
             12  C   2 XX     0.01902     0.20706
             13  C   2 YY    -0.02685     0.10673
             14  C   2 ZZ     0.00568     0.16351
             15  C   2 XY     0.01826     0.04254
             16  C   2 XZ     0.02177     0.04842
             17  C   2 YZ     0.00080     0.00135
             18  H   3  S     0.47529     0.43122
             19  H   3  S     0.28676     0.34346
             20  O   4  S     1.99274     1.97668
             21  O   4  S     0.92235     0.72106
             22  O   4  X     0.94249     0.87070
             23  O   4  Y     0.99343     0.95635
             24  O   4  Z     0.70144     0.66680
             25  O   4  S     0.98624     0.48259
             26  O   4  X     0.39674     0.52324
             27  O   4  Y     0.59151     0.62029
             28  O   4  Z     0.41463     0.44803
             29  O   4 XX     0.00253     0.19070
             30  O   4 YY    -0.00482     0.17218
             31  O   4 ZZ    -0.01239     0.16679
             32  O   4 XY     0.01172     0.02956
             33  O   4 XZ     0.00785     0.01628
             34  O   4 YZ     0.00024     0.00050

          ----- MULLIKEN ATOMIC OVERLAP POPULATIONS -----
          (OFF-DIAGONAL ELEMENTS NEED TO BE MULTIPLIED BY 2)

             1           2           3           4

    1    0.5233226
    2    0.3199692   5.5859106
    3   -0.0537178   0.3199670   0.5233246
    4   -0.0275213   0.3033510  -0.0275214   7.6983888

          TOTAL MULLIKEN AND LOWDIN ATOMIC POPULATIONS
       ATOM         MULL.POP.    CHARGE          LOW.POP.     CHARGE
    1 H             0.762053    0.237947         0.775615    0.224385
    2 C             6.529198   -0.529198         6.607949   -0.607949
    3 H             0.762052    0.237948         0.774680    0.225320
    4 O             7.946697    0.053303         7.841756    0.158244

          -------------------------------
          BOND ORDER AND VALENCE ANALYSIS     BOND ORDER THRESHOLD=0.050
          -------------------------------

                   BOND                       BOND                       BOND
  ATOM PAIR DIST  ORDER      ATOM PAIR DIST  ORDER      ATOM PAIR DIST  ORDER
    1   2  1.110  0.807        2   3  1.110  0.807        2   4  1.206  1.357

                       TOTAL       BONDED        FREE
      ATOM            VALENCE     VALENCE     VALENCE
    1 H                 0.902       0.786       0.117
    2 C                 3.655       2.970       0.685
    3 H                 0.902       0.786       0.117
    4 O                 2.424       1.341       1.083

          ---------------------
          ELECTROSTATIC MOMENTS
          ---------------------

 POINT   1           X           Y           Z (BOHR)    CHARGE
                -2.327646    6.927311   -0.129384        0.00 (A.U.)
         DX          DY          DZ         /D/  (DEBYE)
     0.460379    0.011671    0.053003    0.463567
 ...... END OF PROPERTY EVALUATION ......

                         TIMING STATISTICS ON RANK 0:
 CPU        TIME:   STEP =      0.00 ,  TOTAL =        4.9 SECONDS (    0.1 MIN)
 WALL CLOCK TIME:   STEP =      0.00 ,  TOTAL =        5.2 SECONDS (    0.1 MIN)
 CPU UTILIZATION:   STEP =      0.00%,  TOTAL =      93.59%
 ......END OF NBO ANALYSIS......

                         TIMING STATISTICS ON RANK 0:
 CPU        TIME:   STEP =      0.00 ,  TOTAL =        4.9 SECONDS (    0.1 MIN)
 WALL CLOCK TIME:   STEP =      0.00 ,  TOTAL =        5.2 SECONDS (    0.1 MIN)
 CPU UTILIZATION:   STEP =      0.00%,  TOTAL =      93.58%

                         RANK  0 I/O STATISTICS:
 DATA READ TOTAL =      109.222 MB,  DATA WRITTEN TOTAL =        3.092 MB

                         OVERALL I/O STATISTICS:
 DATA READ TOTAL =      219.617 MB,  DATA WRITTEN TOTAL =        5.210 MB


                         OVERALL  TIMING  STATISTICS:
 GLOBAL CPU TIME ELAPSED      =         9.8 SECONDS (     0.2 MIN)
 RANK 0 WALL CLOCK TIME       =         5.2 SECONDS (     0.1 MIN)
 JOB CPU UTILIZATION:   TOTAL =    186.86%, PER RANK AVERAGE =     93.37%

      396864 WORDS OF    DYNAMIC MEMORY USED
      192966 BYTES OF    HEAP MEMORY    USED,      130103 BYTES REMAIN IN USE

 WARNING! THIS VERSION OF FIREFLY IS PROBABLY OUTDATED!
 PLEASE CHECK FIREFLY HOMEPAGE FOR INFORMATION ON UPDATES!

 EXECUTION OF FIREFLY TERMINATED NORMALLY 15:18:26  2-OCT-2018    
