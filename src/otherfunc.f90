!-------- Main interface of various other functions (part 1)
subroutine otherfunc_main
use defvar
implicit real*8 (a-h,o-z)
do while(.true.)
	write(*,*)
	write(*,*) "              ============ Other functions (Part 1) ============ "
	write(*,*) "0 Return"
	write(*,*) "1 Draw scatter graph between two functions and generate their cube files"
	write(*,*) "2 Export .pdb/.xyz/.wfn/.wfx/.molden/.fch/.47 files or input file of QC codes"
	write(*,*) "3 Calculate molecular van der Waals Volume"
	write(*,*) "4 Integrate a function in whole space"
	write(*,*) "5 Show overlap integral between alpha and beta orbitals"
	write(*,*) "6 Monitor SCF convergence process of Gaussian"
	write(*,*) "8 Generate Gaussian input file with initial guess from fragment wavefunctions"
	write(*,*) "9 Evaluate interatomic connectivity and atomic coordination number"
! 	write(*,*) "10 Generate spherically averaged atomic radial density" !Rarely used, so, hidden
	write(*,*) "11 Calculate overlap and centroid distance between two orbitals"
	write(*,*) "12 Perform biorthogonalization between alpha and beta orbitals"
	write(*,*) "13 Calculate HOMA and Bird aromaticity index"
	write(*,*) "14 Calculate LOLIPOP (LOL Integrated Pi Over Plane)"
	write(*,*) "15 Calculate intermolecular orbital overlap"
	write(*,*) "18 Yoshizawa's electron transport route analysis"
	write(*,*) "19 Generate promolecular .wfn file from fragment wavefunctions"
	write(*,*) "20 Calculate Hellmann-Feynman forces"
	write(*,*) "21 Calculate properties based on geometry information for specific atoms"
	write(*,*) "22 Detect pi orbitals, set occupation numbers and calculate pi composition"
	write(*,*) "23 Fit function distribution to atomic value"
	write(*,*) "24 Obtain NICS_ZZ value for non-planar or tilted system"

	read(*,*) isel
	if (isel==0) then
		return
	else if (isel==1) then
		call funcvsfunc(0)
	else if (isel==2) then
		call outfile
	else if (isel==3) then
		if (MCvolmethod==1) then
			write(*,*) "100*2^i points will be used to evaluate the volume by Monte Carlo method"
			write(*,*) "The volume is defined as superposition of vdW sphere of atoms"
			do while(.true.)
				write(*,*)
				write(*,*) "Please input i, generally 10 is recommended, big system requires bigger i"
				write(*,*) "Input 0 can return"
				read(*,*) pointexp
				if (pointexp==0) exit
				call calcvolume(1,pointexp,0D0,1D0)
			end do
		else if (MCvolmethod==2) then
			write(*,*) "100*2^i points will be used to evaluate the volume by Monte Carlo method."
			write(*,"(a)") " The volume is defined as the region encompassed by the isosurface of density equals to x, &
			The box used in Monte Carlo procedure will be enlarged by k multiples vdW radius in each side."
			write(*,"(a)") " Hint: For evaluating the volume encompassed by 0.001 isosurface of density for small molecule, &
						we suggest you input 9,0.001,1.7"
			do while(.true.)
				write(*,*)
				write(*,*) "Please input i,x,k (Input 0,0,0 can return)"
				read(*,*) pointexp,tmpisoval,enlarbox
				if (pointexp==0.and.tmpisoval==0.and.enlarbox==0) exit
				call calcvolume(2,pointexp,tmpisoval,enlarbox)
			end do
		end if
	else if (isel==4) then
		if (ispecial/=1) then
			call selfunc_interface(1,ifunc)
			call intfunc(ifunc)
		else if (ispecial==1) then
			call intfunc(1)
		end if
	else if (isel==-4) then
		call intdiff(1)
	else if (isel==-5) then
		call intdiff(2)
	else if (isel==5) then
		call aboverlap
	else if (isel==6) then
		call monitorscf
	else if (isel==8) then
		call fragguess
	else if (isel==9) then
		call conn_coordnum
	else if (isel==10) then
		call sphatmraddens
	else if (isel==11) then
		call ovlpdistorb
	else if (isel==12) then
		call orb_biortho
	else if (isel==13) then
		call HOMA_Bird
	else if (isel==14) then
		call LOLIPOP
	else if (isel==15) then
		call intmolovlp
	else if (isel==18) then
		call Yoshieletrans
	else if (isel==19) then
		call genpromolwfn
	else if (isel==20) then
		call hellmann_feynman
	else if (isel==21) then
		call calcgeomprop
	else if (isel==22) then
		call detectpiorb
	else if (isel==23) then
		call fitfunc
	else if (isel==24) then
		call utilNICS_ZZ
	end if
end do
end subroutine




!! ----------------- Based on the equation for evaluating coordination number in DFT-D3 original paper
subroutine conn_coordnum
use defvar
use util
implicit real*8 (a-h,o-z)
real*8 CNmat(ncenter,ncenter),k1,k2
character c80tmp*80,selectyn
write(*,*) "Input the threshold for printing connectivity index, e.g. 0.05"
write(*,*) "If you press ENTER directly, 0.1 will be used"
read(*,"(a)") c80tmp
if (c80tmp==" ") then
	printthres=0.1D0
else
	read(c80tmp,*) printthres
end if
k1=16D0
k2=4D0/3D0
CN=0D0
CNmat=0D0
do iatm=1,ncenter
	indi=a(iatm)%index
	do jatm=iatm+1,ncenter
		if (jatm==iatm) cycle
		r=distmat(iatm,jatm)
		indj=a(jatm)%index
		sclcovsum=k2*(covr_pyy(indi)+covr_pyy(indj))
		CNmat(iatm,jatm)=1D0/( 1D0+dexp(-k1*(sclcovsum/r-1D0)) )
		if (CNmat(iatm,jatm)>=printthres) then
			write(*,"(i5,a,'  ---',i5,a,' :',f10.5,'   Nearest integer:',i3)") &
			iatm,ind2name(a(iatm)%index),jatm,ind2name(a(jatm)%index),CNmat(iatm,jatm),nint(CNmat(iatm,jatm))
		end if
	end do
end do
CNmat=CNmat+transpose(CNmat)
write(*,*)
do iatm=1,ncenter
	indi=a(iatm)%index
	write(*,"(i5,2x,a,2x,'Sum of connectivity:',f8.4,'   Sum of integer connectivity:',i3)") iatm,ind2name(indi),sum(CNmat(iatm,:)),sum(nint(CNmat(iatm,:)))
end do
write(*,*)
write(*,*) "If output connectivity matrix to connmat.txt in current folder? (y/n)"
read(*,*) selectyn
if (selectyn=='y'.or.selectyn=='Y') then
	open(10,file="connmat.txt",status="replace")
	call showmatgau(CNmat,"connectivity matrix",0,'f12.6',10)
	close(10)
	write(*,*) "Done, connmat.txt has been outputted to current folder"
end if
end subroutine



!! ----------- Output new file
subroutine outfile
use defvar
implicit real*8 (a-h,o-z)
character c200tmp*200,c200tmp2*200
write(*,*) "0 Return"
if (ifiletype==4) then !Used to convert .chg file to .pqr file
	write(*,*) "1 Output current structure and atomic charges to .pqr file"
	write(*,*) "-1 Output current structure and atomic charges to .pdb file"
else
	write(*,*) "1 Output current structure to .pdb file"
end if
write(*,*) "2 Output current structure to .xyz file"
write(*,*) "3 Output current structure and atomic charges to .chg file"
write(*,*) "4 Output current wavefunction as .wfx file"
write(*,*) "5 Output current wavefunction as .wfn file"
write(*,*) "6 Output current wavefunction as Molden input file (.molden)"
write(*,*) "7 Output current wavefunction as .fch file"
write(*,*) "8 Output current wavefunction as .47 file"
if (allocated(CObasa)) then
    write(*,*) "10 Output current structure and wavefunction to Gaussian input file"
else
    write(*,*) "10 Output current structure to Gaussian input file"
end if
if (allocated(CObasa)) then
    write(*,*) "11 Output current structure and wavefunction to GAMESS-US input file"
else
    write(*,*) "11 Output current structure to GAMESS-US input file"
end if
write(*,*) "12 Output current structure to ORCA input file"
write(*,*) "13 Output current structure to NWChem input file"
write(*,*) "14 Output current structure to MOPAC input file"
write(*,*) "15 Output current structure to PSI input file"
write(*,*) "16 Output current structure to MRCC input file"
write(*,*) "17 Output current structure to CFOUR input file"
write(*,*) "18 Output current structure to Molpro input file"
write(*,*) "19 Output current structure to Dalton input file"
write(*,*) "20 Output current structure to Molcas input file"
read(*,*) isel

if (isel==0) then
	return
else if (isel==1) then
	if (ifiletype==4) then
		write(*,*) "Input the path for pqr file, e.g. C:\ltwd.pqr"
		read(*,"(a)") c200tmp
		call outpqr(c200tmp,10)
	else
		write(*,*) "Input the path for pdb file, e.g. C:\ltwd.pdb"
		read(*,"(a)") c200tmp
		call outpdb(c200tmp,10)
	end if
else if (isel==-1.and.ifiletype==4) then
	write(*,*) "Input the path for pdb file, e.g. C:\ltwd.pdb"
	read(*,"(a)") c200tmp
	call outpdb(c200tmp,10)
else if (isel==2) then
	write(*,*) "Input the path for xyz file, e.g. C:\ltwd.xyz"
	read(*,"(a)") c200tmp
	call outxyz(c200tmp,10)
else if (isel==3) then
	write(*,*) "Input the path for chg file, e.g. C:\ltwd.chg"
	read(*,"(a)") c200tmp
	call outchg(c200tmp,10)
else if (isel==4) then
	if (.not.allocated(b)) then
		write(*,*) "Error: The input file you used does not contain GTF information!"
	else
		write(*,*) "Input the path, e.g. C:\ltwd.wfx"
		read(*,"(a)") c200tmp
		call outwfx(c200tmp,1,10)
		write(*,*) "Done!"
	end if
else if (isel==5) then
	if (.not.allocated(b)) then
		write(*,*) "Error: The input file you used does not contain GTF information!"
	else
		write(*,*) "Input the path, e.g. C:\ltwd.wfn"
		read(*,"(a)") c200tmp
		call outwfn(c200tmp,1,1,10)
		write(*,*) "Done!"
	end if
else if (isel==6) then
	if (.not.allocated(CObasa)) then
		write(*,*) "Error: This function works only when input file contains basis function information"
	else
		write(*,*) "Input the path, e.g. C:\ltwd.molden"
		read(*,"(a)") c200tmp
		write(*,*) "Exporting, please wait..."
		call outmolden(c200tmp,10)
		write(*,*) "Exporting Molden input file finished!"
	end if
else if (isel==7) then
	if (.not.allocated(CObasa)) then
		write(*,*) "Error: This function works only when input file contains basis function information"
	else
		write(*,*) "Input the path, e.g. C:\ltwd.fch"
		read(*,"(a)") c200tmp
		write(*,*) "Exporting, please wait..."
		call outfch(c200tmp,10,1)
	end if
else if (isel==8) then
	if (.not.allocated(CObasa)) then
		write(*,*) "Error: This function works only when input file contains basis function information"
	else
		write(*,*) "Input the path, e.g. C:\ltwd.47"
		read(*,"(a)") c200tmp
		write(*,*) "Exporting, please wait..."
		call out47(c200tmp,10)
	end if
else if (isel==10) then
	write(*,*) "Input the path, e.g. C:\ltwd.gjf"
	read(*,"(a)") c200tmp
	call outgjf(c200tmp,10)
else if (isel==11) then
	write(*,*) "Input the path, e.g. C:\ltwd.inp"
	read(*,"(a)") c200tmp
	call outGAMESSinp(c200tmp,10)
else if (isel==12) then
	write(*,*) "Input the path, e.g. C:\ltwd.inp"
	read(*,"(a)") c200tmp
	call outORCAinp(c200tmp,10)
else if (isel==13) then
	write(*,*) "Input the path, e.g. C:\ltwd.nw"
	read(*,"(a)") c200tmp
	call outNWCheminp(c200tmp,10)
else if (isel==14) then
	write(*,*) "Input the path, e.g. C:\ltwd.mop"
	read(*,"(a)") c200tmp
	call outMOPACinp(c200tmp,10)
else if (isel==15) then
	write(*,*) "Input the path, e.g. C:\ltwd.inp"
	read(*,"(a)") c200tmp
	call outPSIinp(c200tmp,10)
else if (isel==16) then
	c200tmp="MINP"
	call outMRCCinp(c200tmp,10)
else if (isel==17) then
	c200tmp="ZMAT"
	call outCFOURinp(c200tmp,10)
else if (isel==18) then
	write(*,*) "Input the path, e.g. C:\ltwd.inp"
	read(*,"(a)") c200tmp
	call outmolproinp(c200tmp,10)
else if (isel==19) then
	c200tmp=" "
	write(*,"(a)") " Input path of .dal file, e.g. C:\DFT.dal (directly press ENTER if you do not need it)"
	read(*,"(a)") c200tmp
	write(*,*) "Input path of .mol file, e.g. C:\ltwd.mol"
	read(*,"(a)") c200tmp2
	call outDaltoninp(c200tmp,c200tmp2,10)
else if (isel==20) then
	write(*,*) "Input the path, e.g. C:\ltwd.inp"
	read(*,"(a)") c200tmp
	call outmolcasinp(c200tmp,10)
end if
end subroutine



!! ----------- function vs. function
!iwork=0: General case
!iwork=1: NCI
!iwork=2: NCI based on promolecular approximation
!iwork=5: DORI
subroutine funcvsfunc(iwork)
use plot
use defvar
use util
use GUI
implicit real*8 (a-h,o-z)
integer iwork
real*8,allocatable :: scatterx(:),scattery(:),exchangedata(:,:,:)
character c200tmp*200,f1name*20,f2name*20

f1name="function 1"
f2name="function 2"
if (iwork==1) then !NCI
	iselfunc1=15
	iselfunc2=13
else if (iwork==2) then !NCI based on promolecular approximation
	iselfunc1=16
	iselfunc2=14
else if (iwork==5) then !DORI
	iselfunc1=15
	iuserfunc_old=iuserfunc
	iselfunc2=100
	iuserfunc=20
else
	call funclist
	if (allocated(b)) write(*,*) "Select function 1(as X axis) and function 2(as Y axis)  e.g. 15,13"
	if (.not.allocated(b)) write(*,*) "Select function 1(as X axis) and function 2(as Y axis)  e.g. 16,14"
	if (ifiletype==7) write(*,"(a)") " Note: If input 0,0, then the grid data in memory will be directly taken as function 1, &
	and you need to input path of a cube file, whose data will be taken as function 2" 
	read(*,*) iselfunc1,iselfunc2
	if (iselfunc1==4) then
		write(*,*) "Select which orbital (for function 1)? Input the index"
		read(*,*) iorbsel1
	end if
	if (iselfunc2==4) then
		write(*,*) "Select which orbital (for function 2)? Input the index"
		read(*,*) iorbsel2
	end if
end if
if (iselfunc1==15.or.iselfunc1==16) f1name="sign(lambda2)rho"
if (iselfunc2==13.or.iselfunc2==14) f2name="RDG"
if (iselfunc2==100.and.iuserfunc==20) f2name="DORI"

if (iselfunc1==0.and.iselfunc2==0) then !Directly load grid data
	if (allocated(cubmattmp)) deallocate(cubmattmp)
	write(*,*) "Input filename for cube file of function 2, e.g. C:\test.cub"
	read(*,"(a)") c200tmp
	call readcubetmp(c200tmp,inconsis)
	if (inconsis==1) then
		write(*,"(a)") " Error: The grid setting of this cube file is inconsistent with that of present grid data, exit..."
		read(*,*)
		return
	end if
else !Calculate grid data
	if (iwork==1.or.iwork==2.or.iwork==5) aug3D=1.5D0 !Smaller than default value
	call setgrid(0,igridsel)
	if (allocated(cubmat)) deallocate(cubmat)
	if (allocated(cubmattmp)) deallocate(cubmattmp)
	allocate(cubmat(nx,ny,nz),cubmattmp(nx,ny,nz),exchangedata(nx,ny,nz))
	call delvirorb(1)
	if (iselfunc1==15.and.iselfunc2==13) then !Since RDG and sign(lambda2)rho is often combined to study NCI, a special code is provided for speedup
		call savecubmat(1513,0,1)
	else if (iselfunc1==16.and.iselfunc2==14) then 
		call savecubmat(1614,0,1)
	else
		write(*,"(a)") " Processing "//trim(f2name)
		call savecubmat(iselfunc2,0,iorbsel2)
		cubmattmp=cubmat
		write(*,"(a)") " Processing "//trim(f1name)
		call savecubmat(iselfunc1,0,iorbsel1)
	end if
end if

!Store grid data to array
allocate(scatterx(nx*ny*nz),scattery(nx*ny*nz))
ii=1
do i=1,nx
	do j=1,ny
		do k=1,nz
			scatterx(ii)=cubmat(i,j,k)
			scattery(ii)=cubmattmp(i,j,k)
			ii=ii+1
		end do
	end do
end do

if (iwork==0) then
	func1stddev=stddevarray(scatterx)
	func2stddev=stddevarray(scattery)
	write(*,"(a,2E18.10)") " Standard deviation of function 1 and 2:",func1stddev,func2stddev
	pearsoncoeff=covarray(scatterx,scattery)/func1stddev/func2stddev
	write(*,"(2(a,f12.6))") " Pearson correlation coefficient r:",pearsoncoeff,"  r^2:",pearsoncoeff**2
end if

xmin=minval(scatterx)
xmax=maxval(scatterx)
ymin=minval(scattery)
ymax=maxval(scattery)
if (iselfunc1==15.and.iselfunc2==13) then !sign(lambda2)*rho vs. RDG
	xmin=-RDG_maxrho
	xmax=RDG_maxrho
	if (RDG_maxrho==0.0D0) xmin=-2.0D0
	if (RDG_maxrho==0.0D0) xmax=2.0D0
	ymin=0.0D0
	ymax=2.0D0
else if (iselfunc1==16.and.iselfunc2==14) then !sign(lambda2)*rho vs. RDG based on promolecular density
	xmin=-RDGprodens_maxrho
	xmax=RDGprodens_maxrho
	if (RDGprodens_maxrho==0.0D0) xmin=-2.0D0
	if (RDGprodens_maxrho==0.0D0) xmax=2.0D0
	ymin=0.0D0
	ymax=2.0D0
end if

write(*,*)
do while (.true.)
	write(*,"(a)") " -3 Set "//trim(f2name)//" value where value of "//trim(f1name)//" is out of a certain range"
	write(*,"(a)") " -2 Set "//trim(f2name)//" value where value of "//trim(f1name)//" is within a certain range"
	write(*,*) "-1 Draw scatter graph"
	write(*,*) "0 Exit"
	write(*,*) "1 Save the scatter graph to file"
	write(*,*) "2 Output scatter points to output.txt in current folder"
	write(*,*) "3 Output cube files to func1.cub and func2.cub in current folder"
	write(*,"(' 4 Change range of X-axis of scatter graph, current:',1PE12.4,' to',1PE12.4)") xmin,xmax
	write(*,"(' 5 Change range of Y-axis of scatter graph, current:',1PE12.4,' to',1PE12.4)") ymin,ymax
	write(*,"(a)") " 6 Show isosurface of "//trim(f1name)
	write(*,"(a)") " 7 Show isosurface of "//trim(f2name)
	write(*,"(a)") " 8 Output "//trim(f1name)//" to output.txt where "//trim(f2name)//" is within in certain range"
	read(*,*) isel
	if (isel==-2.or.isel==-3) then
		write(*,"(a)") " Input lower and upper limit of the range of "//trim(f1name)//", e.g. 0.5,2.3"
		read(*,*) rlower,rupper
		write(*,"(a)") " Input the expected value of "//trim(f2name)
		read(*,*) rsetfunc2
		if (isel==-2) then
			where (scatterx<=rupper.and.scatterx>=rlower) scattery=rsetfunc2
			where (cubmat<=rupper.and.cubmat>=rlower) cubmattmp=rsetfunc2
		else if (isel==-3) then
			where (scatterx>=rupper.or.scatterx<=rlower) scattery=rsetfunc2
			where (cubmat>=rupper.or.cubmat<=rlower) cubmattmp=rsetfunc2
		end if
		write(*,*) "Done!"
	else if (isel==-1) then
		write(*,*) "Drawing graph, please wait..."
		if ((iselfunc1==15.and.iselfunc2==13).or.(iselfunc1==16.and.iselfunc2==14)) then
			call drawscatter(scatterx,scattery,nx*ny*nz,xmin,xmax,ymin,ymax,1,"$sign({\lambda}_2)\rho$ (a.u.)","Reduced density gradient")
		else if (iselfunc1==15.and.iselfunc2==100.and.iuserfunc==20) then
			call drawscatter(scatterx,scattery,nx*ny*nz,xmin,xmax,ymin,ymax,1,"$sign({\lambda}_2)\rho$ (a.u.)","DORI")
		else
			call drawscatter(scatterx,scattery,nx*ny*nz,xmin,xmax,ymin,ymax,1)
		end if
	else if (isel==0) then
		if (iwork==5) iuserfunc=iuserfunc_old !DORI
		exit
	else if (isel==1) then
		isavepic=1
		if ((iselfunc1==15.and.iselfunc2==13).or.(iselfunc1==16.and.iselfunc2==14)) then
			call drawscatter(scatterx,scattery,nx*ny*nz,xmin,xmax,ymin,ymax,1,"$sign({\lambda}_2)\rho$ (a.u.)","Reduced density gradient")
		else if (iselfunc1==15.and.iselfunc2==100.and.iuserfunc==20) then
			call drawscatter(scatterx,scattery,nx*ny*nz,xmin,xmax,ymin,ymax,1,"$sign({\lambda}_2)\rho$ (a.u.)","DORI")
		else
			call drawscatter(scatterx,scattery,nx*ny*nz,xmin,xmax,ymin,ymax,1)
		end if
		isavepic=0
		write(*,"(a,a,a)") " Graph have been saved to ",trim(graphformat)," file with ""DISLIN"" prefix in current directory"
	else if (isel==2) then
		open(10,file="output.txt",status="replace")
		write(*,*) "Outputting output.txt..."
		write(10,"(3f11.6,2E16.8)") ((( (orgx+(i-1)*dx),(orgy+(j-1)*dy),(orgz+(k-1)*dz),cubmat(i,j,k),cubmattmp(i,j,k),k=1,nz),j=1,ny),i=1,nx)
		close(10)
		write(*,"(a)") " Finished, column 1/2/3/4/5 = X/Y/Z/"//trim(f1name)//"/"//trim(f2name)//", unit is Bohr"
		write(*,"(a)") " Obviously, if you would like to plot scatter map between "//trim(f1name)//" and "//trim(f2name)//" in external tools such as Origin, &
		the last two columns should be taken as X and Y axes data"
	else if (isel==3) then
		open(10,file="func1.cub",status="replace")
		call outcube(cubmat,nx,ny,nz,orgx,orgy,orgz,gridvec1,gridvec2,gridvec3,10)
		close(10)
		write(*,"(a)") " The cube file of "//trim(f1name)//" has been exported to func1.cub in current folder"
		exchangedata=cubmat
		cubmat=cubmattmp !cubmat store function 2 value temporarily for outcube routine
		open(10,file="func2.cub",status="replace")
		call outcube(cubmat,nx,ny,nz,orgx,orgy,orgz,gridvec1,gridvec2,gridvec3,10)
		close(10)
		write(*,"(a)") " The cube file of "//trim(f2name)//" has been exported to func2.cub in current folder"
		cubmat=exchangedata !recover function 1
	else if (isel==4) then
		write(*,*) "Input lower limit and upper limit of X axis e.g. 0,1.5"
		read(*,*) xmin,xmax
	else if (isel==5) then
		write(*,*) "Input lower limit and upper limit of Y axis e.g. 0,1.5"
		read(*,*) ymin,ymax
	else if (isel==6) then
	 	write(*,*) "Input the value of isosurface, e.g. 0.02"
		read(*,*) sur_value
		call drawisosurgui(1)
	else if (isel==7) then
		exchangedata=cubmat
		cubmat=cubmattmp
	 	write(*,*) "Input the value of isosurface, e.g. 0.02"
		read(*,*) sur_value
		call drawisosurgui(1)
		cubmat=exchangedata
	else if (isel==8) then
		write(*,*) "Input range of "//trim(f2name)//", e.g. 0.0009, 0.0011"
		read(*,*) rlowlim,uplim
		open(10,file="output.txt",status="replace")
		rmin=1D200
		rmax=-1D200
		num=0
		do i=1,nx
			do j=1,ny
				do k=1,nz
					if (cubmattmp(i,j,k)<=uplim.and.cubmattmp(i,j,k)>=rlowlim) then
						num=num+1
						if (cubmattmp(i,j,k)>rmax) rmax=cubmattmp(i,j,k)
						if (cubmattmp(i,j,k)<rmin) rmin=cubmattmp(i,j,k)
						write(10,"(3f11.6,2E16.8)") (orgx+(i-1)*dx),(orgy+(j-1)*dy),(orgz+(k-1)*dz),cubmat(i,j,k),cubmattmp(i,j,k)
					end if
				end do
			end do
		end do
		close(10)
		write(*,"(a)") " Finished, column 1/2/3/4/5 = X/Y/Z/"//trim(f1name)//"/"//trim(f2name)//", unit is Bohr"
		write(*,"(' Number of entries:',i10)") num
		if (num>=2) write(*,"(a,2E16.8)") " Min and Max of "//trim(f1name)//" in this range",rmin,rmax
	end if
	write(*,*)
end do
end subroutine





!!---------- Intermolecular MO overlap
!Both alpha and beta are taken into account, the beta index is after alpha index
!Two cases of input files
!1 First load dimer Gaussian output file (must with iop(3/33=1), then load the two monomer Gaussian output files in turn (must with pop=full)
!2 Load dimer wavefunction, compute Sbas, then respectively load two monomer wavefunctions
subroutine intmolovlp
use util
use defvar
implicit real*8 (a-h,o-z)
real*8,allocatable :: cobas1(:,:),cobas2(:,:),ovlpbasmat(:,:),orbovlp(:,:) !ovlpmat is overlap matrix of basis functions
character monofile1*200,monofile2*200,c80tmp*80

if (ifiletype==0) then !Plain text file, assumed to be gaussian output file
    open(10,file=filename,status="old")
    call loclabel(10,"NBasis=",ifound) !Number of basis functions
    read(10,*) c80tmp,nbasis
    write(*,"('The number of basis functions in the dimer',i10)") nbasis
    allocate(ovlpbasmat(nbasis,nbasis))
    write(*,*) "Loading overlap matrix of dimer, please wait..."
    call loclabel(10,"*** Overlap ***",ifound,1)
    call readmatgau(10,ovlpbasmat,1,"D14.6",7,5)
    close(10)

    ! monofile1="x\transint\db-ttf1.out"
    write(*,*)
    write(*,*) "Input Gaussian output file of monomer 1, e.g. C:\monomer1.out"
    do while(.true.)
	    read(*,"(a)") monofile1
	    inquire(file=monofile1,exist=alive)
	    if (alive) exit
	    write(*,*) "File not found, input again"
    end do
    open(10,file=monofile1,status="old")
    call loclabel(10,"NBasis=",ifound)
    read(10,*) c80tmp,nbasis1 !Number of basis functions in monomer 1
    call loclabel(10,"NBsUse=",ifound) !NbsUse must equal to the number of MOs
    read(10,*) c80tmp,nmo1
    write(*,"(' The number of basis functions in monomer 1',i10)") nbasis1
    call loclabel(10,"Beta Molecular Orbital Coefficients:",iopsh1,0) !Determine if this monomer is open-shell
	allocate(cobas1(nbasis,nmo1))
	cobas1=0D0
    if (iopsh1==1) then
	    nmo1=nmo1*2
	    write(*,"(' MOs from',i8,' to',i8,' are Alpha orbitals')") 1,nmo1/2
	    write(*,"(' MOs from',i8,' to',i8,' are Beta orbitals')") nmo1/2+1,nmo1
	    write(*,*) "Loading molecular orbital coefficients of monomer 1, please wait..."
	    call loclabel(10,"Alpha Molecular Orbital Coefficients:",ifound,1)
	    call readmatgau(10,cobas1(1:nbasis1,1:nmo1/2),0,"f10.5",21,5,3) !nbasis1+1:nbasis are empty
	    call loclabel(10,"Beta Molecular Orbital Coefficients:",ifound,1)
	    call readmatgau(10,cobas1(1:nbasis1,nmo1/2+1:),0,"f10.5",21,5,3) !nbasis1+1:nbasis are empty
    else !Closed-shell
	    write(*,"(' The number of molecular orbitals in monomer 1',i10)") nmo1
	    write(*,*) "Loading molecular orbital coefficients of monomer 1, please wait..."
	    call loclabel(10,"Molecular Orbital Coefficients:",ifound,1)
	    call readmatgau(10,cobas1(1:nbasis1,:),0,"f10.5",21,5,3) !nbasis1+1:nbasis are empty
    end if
    close(10)

    ! monofile2="x\transint\db-ttf2.out"
    write(*,*)
    write(*,*) "Input Gaussian output file of monomer 2, e.g. C:\monomer2.out"
    do while(.true.)
	    read(*,"(a)") monofile2
	    inquire(file=monofile2,exist=alive)
	    if (alive) exit
	    write(*,*) "File not found, input again"
    end do
    open(10,file=monofile2,status="old")
    call loclabel(10,"NBasis=",ifound)
    read(10,*) c80tmp,nbasis2 !Number of basis functions in monomer 1
    if (nbasis1+nbasis2/=nbasis) write(*,*) "Warning: The sum of the number of basis functions of the two monomers is unequal to dimer!"
    call loclabel(10,"NBsUse=",ifound) !NbsUse must equal to the number of MOs
    read(10,*) c80tmp,nmo2
    write(*,"(' The number of basis functions in monomer 2',i10)") nbasis2
    call loclabel(10,"Beta Molecular Orbital Coefficients:",iopsh2,0) !Determine if this monomer is open-shell
	allocate(cobas2(nbasis,nmo2))
	cobas2=0D0
    if (iopsh2==1) then
	    nmo2=nmo2*2
	    write(*,"(' MOs from',i8,' to',i8,' are Alpha orbitals')") 1,nmo2/2
	    write(*,"(' MOs from',i8,' to',i8,' are Beta orbitals')") nmo2/2+1,nmo2
	    write(*,*) "Loading molecular orbital coefficients of monomer 2, please wait..."
	    call loclabel(10,"Alpha Molecular Orbital Coefficients:",ifound,1)
	    call readmatgau(10,cobas2(nbasis1+1:,1:nmo2/2),0,"f10.5",21,5,3) !1:nbasis1 are empty
	    call loclabel(10,"Beta Molecular Orbital Coefficients:",ifound,1)
	    call readmatgau(10,cobas2(nbasis1+1:,nmo2/2+1:),0,"f10.5",21,5,3) !1:nbasis1 are empty
    else !Closed-shell
	    write(*,"(' The number of molecular orbitals in monomer 2',i10)") nmo2
	    write(*,*) "Loading molecular orbital coefficients of monomer 2, please wait..."
	    call loclabel(10,"Molecular Orbital Coefficients:",ifound,1)
	    call readmatgau(10,cobas2(nbasis1+1:,:),0,"f10.5",21,5,3) !1:nbasis1 are empty
    end if
    close(10)
    ! call showmatgau(cobas1,"111",0,"f14.8",6)

else !Using wavefunction file of dimer and monomer to do the analysis
    
    allocate(ovlpbasmat(nbasis,nbasis))
    ovlpbasmat=Sbas
    nmoall=nmo
    nbasisall=nbasis
    call dealloall
    write(*,*)
    
    write(*,*) "Input wavefunction of monomer 1, e.g. C:\monomer1.fch"
    do while(.true.)
	    read(*,"(a)") monofile1
	    inquire(file=monofile1,exist=alive)
	    if (alive) exit
	    write(*,*) "File not found, input again"
    end do
    call readinfile(monofile1,1)
    write(*,"(' The number of basis functions in monomer 1',i10)") nbasis
    nmo1=nmo
    nbasis1=nbasis
	allocate(cobas1(nbasisall,nmo))
    cobas1=0
    iopsh1=0
    if (wfntype==1) then !Unrestricted
	    write(*,"(' Note: MOs from',i8,' to',i8,' are Alpha orbitals')") 1,nmo/2
	    write(*,"(' Note: MOs from',i8,' to',i8,' are Beta orbitals')") nmo/2+1,nmo
	    cobas1(1:nbasis,1:nmo/2)=CObasa
	    cobas1(1:nbasis,nmo/2+1:)=CObasb
        iopsh1=1
    else !R or RO
        cobas1(1:nbasis,:)=CObasa
        iopsh1=0
    end if
    call dealloall
    
    write(*,*)
    write(*,*) "Input wavefunction of monomer 2, e.g. C:\monomer2.fch"
    do while(.true.)
	    read(*,"(a)") monofile2
	    inquire(file=monofile2,exist=alive)
	    if (alive) exit
	    write(*,*) "File not found, input again"
    end do
    call readinfile(monofile2,1)
    write(*,"(' The number of basis functions in monomer 2',i10)") nbasis
    nmo2=nmo
	allocate(cobas2(nbasisall,nmo))
    cobas2=0
    if (wfntype==1) then !Unrestricted
	    write(*,"(' Note: MOs from',i8,' to',i8,' are Alpha orbitals')") 1,nmo/2
	    write(*,"(' Note: MOs from',i8,' to',i8,' are Beta orbitals')") nmo/2+1,nmo
	    cobas2(nbasis1+1:,1:nmo/2)=CObasa
	    cobas2(nbasis1+1:,nmo/2+1:)=CObasb
        iopsh2=1
    else !R or RO
        cobas2(nbasis1+1:,:)=CObasa
        iopsh2=0
    end if
    call dealloall
    
    write(*,*)
    write(*,*) "Reloading the file initially loaded..."
    call readinfile(firstfilename,1)

end if

!Transform the overlap matrix in basis functions to the one between the MOs of the two monomers
write(*,*)
write(*,*) "Calculating the overlap matrix between MOs of the two monomers..."
allocate(orbovlp(nmo1,nmo2))
orbovlp=matmul(transpose(cobas1),matmul(ovlpbasmat,cobas2))

do while(.true.)
	write(*,*)
	write(*,"(a)") " Input e.g. 78,79 can print overlap integral between MO78 of monomer 1 and MO79 of monomer 2"
	write(*,"(a)") " Input o can output the whole overlap integral matrix to ovlpint.txt in current folder. Input q can exit"
	read(*,"(a)") c80tmp
	if (c80tmp(1:1)=='q') then
		exit
	else if (c80tmp(1:1)=='o') then
		open(10,file="ovlpint.txt",status="replace")
		call showmatgau(orbovlp,"",0,"f14.8",10)
		close(10)
		write(*,"(a)") " Done! The matrix has been outputted to ovlpint.txt in current folder"
		write(*,"(a)") " Note: The i,j element in the matrix corresponds to the overlap integral between MO i in monomer 1 and MO j in monomer 2"
	else
		read(c80tmp,*) idx1,idx2
		if (idx1>nmo1.or.idx1<1) then
			write(*,"(' Input error! The MO range of monomer 1 is between',i8,' and',i8)") 1,nmo1
			cycle
		end if
		if (idx2>nmo2.or.idx2<1) then
			write(*,"(' Input error! The MO range of monomer 2 is between',i8,' and',i8)") 1,nmo2
			cycle
		end if
		write(*,"(' Overlap integral is',f14.8)") orbovlp(idx1,idx2)
	end if
end do
end subroutine




!!----------- Generate fragment MO initial guess
subroutine fragguess
use util
use defvar
implicit real*8 (a-h,o-z)
integer,allocatable :: iflipfrag(:),numatom(:),numaelec(:),numbelec(:),numbas(:),ifunrestrict(:)
integer,allocatable :: wherefraga(:),wherefragb(:) !Each complex MO is come from which fragment
real*8,allocatable :: tmpcomat(:,:)
character selectyn*1,ctitle*80,c80tmp*80
character,allocatable :: namearray(:)*80

write(*,*) "How many fragments? (Including the fragment 1 that has been loaded)"
read(*,*) nfrag
allocate(iflipfrag(nfrag)) 
allocate(namearray(nfrag))
allocate(numatom(nfrag))
allocate(numbas(nfrag))
allocate(numaelec(nfrag))
allocate(numbelec(nfrag))
allocate(ifunrestrict(nfrag))
iflipfrag=0 !Don't flip spin by default
ifunrestrict=0 !All fragment are closed-shell by default
nchargetot=0 !charge of complex
nbasis=0
ncenter=0
naelec=0
nbelec=0

do i=1,nfrag
	if (i==1) then
		write(*,"(' Filename of fragment 1: ',a)") trim(filename)
	else if (i/=1) then
		do while(.true.)
			write(*,"(' Input Gaussian outputted filename of fragment',i4)") i
			read(*,"(a)") filename
			inquire(file=filename,exist=alive)
			if (alive) exit
			write(*,*) "File not found, input again"
		end do
	end if
	namearray(i)=filename

	open(10,file=filename,status="old") !Load other information of this fragment
	if (i==1) then
		call loclabel(10,"#")
		read(10,"(a)") ctitle
	end if
	call loclabel(10,"Charge =")
	read(10,"(9x,i3,15x,i2)") icharge,imulti
	call loclabel(10,"NBasis=")
	read(10,*) c80tmp,numbas(i)
	call loclabel(10,"NBsUse=")
	read(10,*) c80tmp,nbsuse
	if (nbsuse/=numbas(i)) then
		write(*,"(a)") " Error: Some linearly dependent basis functions were removed by Gaussian! You should regenerate the Gaussian output file with IOp(3/32=2)!"
		return
	end if
	call loclabel(10,"alpha electrons",ifound,1)
	read(10,*) numaelec(i),c80tmp,c80tmp,numbelec(i)
	call loclabel(10,"NAtoms",ifound)
	read(10,*) c80tmp,numatom(i)
	call loclabel(10,"Beta Molecular Orbital",ifunrestrict(i))
	close(10)

	write(*,"(' Charge and multiplicity of this fragment:',2i4)") icharge,numaelec(i)-numbelec(i)+1
	nchargetot=nchargetot+icharge
	if (imulti>1) then
		write(*,*) "Flip electron spin of this fragment? (y/n)"
		read(*,*) selectyn
		if (selectyn=='y'.or.selectyn=='Y') then
			iflipfrag(i)=1
			itmp=numaelec(i)
			numaelec(i)=numbelec(i)
			numbelec(i)=itmp
		end if
	end if
end do

nbasis=sum(numbas)
naelec=sum(numaelec)
nbelec=sum(numbelec)
itotmulti=abs(naelec-nbelec)+1
write(*,"(' Total charge and multiplicity:',2i4)") nchargetot,itotmulti
write(*,"(' Total number of alpha and beta electrons:',2i6)") nint(naelec),nint(nbelec)
write(*,"(' Total number of basis functions:',i8)") nbasis
allocate(CObasa(nbasis,nbasis))
CObasa=0D0
allocate(wherefraga(nbasis)) !If wherefraga(i)=j means alpha orbital i in complex is contributed from fragment j
if (all(ifunrestrict==0)) then !Total is restricted
	allocate(MOocc(nbasis))
else !Since at least one fragment is unrestricted, so total is unrestricted
	allocate(MOocc(2*nbasis))
	allocate(CObasb(nbasis,nbasis))
	CObasb=0D0
	allocate(wherefragb(nbasis))
end if
ncenter=sum(numatom)
allocate(a(ncenter))

iatm=1
locbas=1 !Current basis function position (index)
ioccmoa=1 !current alpha occupied orbital position (index)
ivirmoa=naelec+1 !current alpha virtual orbital position (index)
ioccmob=1
ivirmob=nbelec+1
do i=1,nfrag !Read atomic information and converged wavefunctions
	write(*,"(' Now loading fragment',i6,' ...')") i
	open(10,file=namearray(i),status="old")

	allocate(tmpcomat(numbas(i),numbas(i)))
	call loclabel(10,"Input orientation:",ifound) !I assume "nosymm" is used, so "standard orentation" does not appear
	if (ifound==0) call loclabel(10,"Coordinates in L301:",ifound)
 	if (ifound==0) call loclabel(10,"Z-Matrix orientation:",ifound) !sometimes the output of Gaussian is very weird
	if (ifound==0) then
	    write(*,"(a,/)") " Error: Atomic coordinates cannot be found!"
	    return
	end if
	do ii=1,5
		read(10,*)
	end do
	do ii=iatm,iatm+numatom(i)-1
		!Note: As usual, coordinate in Multiwfn is Bohr, but here is Angstrom for simplicity
		read(10,*) itmp,a(ii)%index,itmp2,a(ii)%x,a(ii)%y,a(ii)%z
	end do
	call loclabel(10,"Molecular Orbital Coefficients") !We first assume total is closed-shell, current is closed-shell too
	call readmatgau(10,tmpcomat,0,"f10.5",21,5,3) !i,j of tmpcomat is coefficient of i basis in j orbital
	if (iflipfrag(i)==0) then !fragment is closed-shell or needn't flip spin
		CObasa(locbas:locbas+numbas(i)-1,ioccmoa:ioccmoa+numaelec(i)-1)=tmpcomat(:,1:numaelec(i))
		CObasa(locbas:locbas+numbas(i)-1,ivirmoa:ivirmoa+(numbas(i)-numaelec(i))-1)=tmpcomat(:,numaelec(i)+1:)
	else if (iflipfrag(i)==1) then !flip spin, notice that now nbelec is already number of alpha electrons
		CObasb(locbas:locbas+numbas(i)-1,ioccmob:ioccmob+numbelec(i)-1)=tmpcomat(:,1:numbelec(i))
		CObasb(locbas:locbas+numbas(i)-1,ivirmob:ivirmob+(numbas(i)-numbelec(i))-1)=tmpcomat(:,numbelec(i)+1:)
	end if
	if (any(ifunrestrict==1)) then !Complex is open-shell, now fill beta part
		if (ifunrestrict(i)==0) then !fragment is closed-shell, beta coefficient is identical to alpha part
			CObasb(locbas:locbas+numbas(i)-1,ioccmob:ioccmob+numbelec(i)-1)=tmpcomat(:,1:numbelec(i))
			CObasb(locbas:locbas+numbas(i)-1,ivirmob:ivirmob+(numbas(i)-numbelec(i))-1)=tmpcomat(:,numbelec(i)+1:)
		else if (ifunrestrict(i)==1) then
			call loclabel(10,"Molecular Orbital Coefficients",ifound,0)
			call readmatgau(10,tmpcomat,0,"f10.5",21,5,3) !Load beta MOs information
			if (iflipfrag(i)==0) then
				CObasb(locbas:locbas+numbas(i)-1,ioccmob:ioccmob+numbelec(i)-1)=tmpcomat(:,1:numbelec(i))
				CObasb(locbas:locbas+numbas(i)-1,ivirmob:ivirmob+(numbas(i)-numbelec(i))-1)=tmpcomat(:,numbelec(i)+1:)
			else if (iflipfrag(i)==1) then !flipping spin
				CObasa(locbas:locbas+numbas(i)-1,ioccmoa:ioccmoa+numaelec(i)-1)=tmpcomat(:,1:numaelec(i))
				CObasa(locbas:locbas+numbas(i)-1,ivirmoa:ivirmoa+(numbas(i)-numaelec(i))-1)=tmpcomat(:,numaelec(i)+1:)
			end if
		end if
		wherefragb(ioccmob:ioccmob+numbelec(i)-1)=i
		wherefragb(ivirmob:ivirmob+(numbas(i)-numbelec(i))-1)=i
		ioccmob=ioccmob+numbelec(i)
		ivirmob=ivirmob+(numbas(i)-numbelec(i))
	end if
	
	wherefraga(ioccmoa:ioccmoa+numaelec(i)-1)=i
	wherefraga(ivirmoa:ivirmoa+(numbas(i)-numaelec(i))-1)=i
	ioccmoa=ioccmoa+numaelec(i) !Update ioccmoa
	ivirmoa=ivirmoa+(numbas(i)-numaelec(i)) !numbas(i)-numaelec(i) is number of virtual orbitals in current fragment

	deallocate(tmpcomat)
	iatm=iatm+numatom(i)
	locbas=locbas+numbas(i)
	close(10)
end do

MOocc=0D0
if (all(ifunrestrict==0)) then
	MOocc(1:naelec)=2D0
else
	MOocc(1:naelec)=1D0
	MOocc(nbasis+1:nbasis+nbelec)=1D0
end if
!Generate new.gjf
open(10,file="new.gjf",status="replace")
write(10,"(a,/,/,a,/,/,2i3)") trim(ctitle)//" guess=cards","Please check this file to ensure validity",nchargetot,itotmulti
do i=1,ncenter
	write(10,"(a,3f14.8)") ind2name(a(i)%index),a(i)%x,a(i)%y,a(i)%z
end do
write(10,"(/,'5(E16.9)',/,'-1')")
do i=1,nbasis !Cycle orbitals
	if (all(ifunrestrict==0)) then
		write(10,"('! Orbital:',i6,' Occ:',f10.6,' from fragment',i4)") i,MOocc(i),wherefraga(i)
	else
		write(10,"('! Alpha orbital:',i6,' Occ:',f10.6,' from fragment',i4)") i,MOocc(i),wherefraga(i)
	end if
	write(10,"(5E16.9)") (CObasa(j,i),j=1,nbasis)
end do
if (any(ifunrestrict==1)) then
	write(10,"('-1')")
	do i=1,nbasis
		write(10,"('! Beta orbital:',i6,' Occ:',f10.6,' from fragment',i4)") i,MOocc(nbasis+i),wherefragb(i)
		write(10,"(5E16.9)") (CObasb(j,i),j=1,nbasis)
	end do
end if
write(10,"('0',/)")
close(10)
write(*,*) "Input file with initial guess have been saved to new.gjf in current folder"
write(*,*) "Do not forget to manually check route section"
end subroutine



!!----------- Monitor SCF process
subroutine monitorscf
use defvar
use util
use dislin_d
implicit real*8 (a-h,o-z)
real*8,dimension(1000) :: DE,RMSDP,MaxDP,grad,stepnum(1000)=(/ (i,i=1,1000) /),constant
real*8 aimDE,aimRMSDP,aimMaxDP
character*3 DEconv,RMSDPconv,MaxDPconv
itime=0
iscfqc=0
do while(.true.)
	if (itime/=0) then
		write(*,*)
		write(*,*) "1 Redraw convergence trend of all steps"
		write(*,*) "2 Redraw convergence trend of last 5 steps"
		write(*,*) "3 Redraw convergence trend of last 10 steps"
		write(*,*) "4 Redraw convergence trend of last specific number of steps"
		read(*,*) isel
		if (isel==4) then
			write(*,"(a,i5)") " Input a number, between 1 and",ifincyc
			read(*,*) ishow
		else if (isel==2.and.ifincyc>=5) then
			ishow=5
		else if (isel==3.and.ifincyc>=10) then
			ishow=10
		end if
	end if
	!Draw all points variation at first time get into this routine
	!I bracket filename with double quotation marks, otherwise if there have "+" in the filename then Multiwfn crash
	if (isys==1) call system("copy "//""""//trim(filename)//""""//" gauout.out /y")
	if (isys==2) call system("cp "//""""//trim(filename)//""""//" gauout.out -f")
	open(10,file="gauout.out",status="old")
	call loclabel(10," Quadratic Convergence",iscfqc)

	if (iscfqc==0) then !SCF=QC was not used
		call loclabel(10," Requested convergence on RMS density matrix")
		read(10,"(45x,D8.2)") aimRMSDP
		call loclabel(10," Requested convergence on MAX density matrix",ifound,0)
		read(10,"(45x,D8.2)") aimMaxDP
		call loclabel(10," Requested convergence on             energy",ifound,0)
		read(10,"(45x,D8.2)") aimDE

		i=1
		DE(1)=0D0 !In the first cycle DE is not present, we set it to an arbitrary value
		do while(.true.)
			call loclabel(10," RMSDP=",ifound,0)
			if (ifound==0) exit
			read(10,"(7x,1PD8.2)",advance="no") RMSDP(i)
			read(10,"(7x,1PD8.2)",advance="no") MaxDP(i)
			if (i/=1) read(10,"(4x,1PD9.2)") DE(i)
			i=i+1
		end do
		ifincyc=i-1
		if (ifincyc==0) then
			write(*,*) "Error: Unable to find SCF convergence information! Did you use #P as requested?"
			write(*,*) "Press ENTER button to exit program"
			stop
		end if
		if (isel==1.or.itime==0) ishow=ifincyc !At the first time, draw all point
		istart=ifincyc-ishow+1

		write(*,*) "Step#   RMSDP  Conv?   MaxDP  Conv?     DE    Conv?"
		do i=istart,ifincyc
			RMSDPconv="NO"
			MaxDPconv="NO"
			DEconv="NO"
			if (RMSDP(i)<aimRMSDP) RMSDPconv="YES"
			if (MaxDP(i)<aimMaxDP) MaxDPconv="YES"
			if (i==1) then
				DEconv="   "
			else if (abs(DE(i))<aimDE) then
				DEconv="YES"
			end if
			write(*,"(i5,2x,2(1PD8.2,a5,2x),1PD9.2,a5)") i,RMSDP(i),RMSDPconv,MaxDP(i),MaxDPconv,DE(i),DEconv
		end do
		write(*,"(' Goal  ',2(1PD8.2,7x),1PD9.2)") aimRMSDP,aimMaxDP,aimDE
	else if (iscfqc==1) then  !For SCF=QC
		i=1
		DE(1)=0D0
		do while(.true.)
			call loclabel(10,"Iteration ",ifound,0)
			if (ifound==0) exit
			read(10,"(50x)",advance="no")
			if (i/=1) read(10,*) DE(i)
			if (i==1) read(10,*)
			backspace(10)
			read(10,"(77x)",advance="no")
			read(10,*) grad(i)
			i=i+1
		end do
		ifincyc=i-1
		if (isel==1.or.itime==0) ishow=ifincyc
		istart=ifincyc-ishow+1

		write(*,*) "Step#       Delta-E         Grad"
		do i=istart,ifincyc
			write(*,"(i5,2x,2(1PE15.5))") i,DE(i),grad(i)
		end do
	end if
	call loclabel(10,"SCF Done",ifound)
	if (ifound==1) write(*,*) "SCF done!"
	if (ifound==0) write(*,*) "SCF failed or haven't converged"
	close(10)
	if (isys==1) call system("del gauout.out")
	if (isys==2) call system("rm gauout.out -f")

	call METAFL('xwin')
	call window(200,100,1000,700)
	call SCRMOD('REVERSE')
	CALL PAGE(3300,2310)
	call disini
	call hwfont
	call WINTIT("Monitor SCF process, variations at each step")
	CALL TICKS (1, 'XY')
	CALL TICPOS("REVERS","XYZ")
	call ERRMOD("ALL","OFF")
	CALL LABDIG(-1,"X")
	CALL LABDIG(2,"Y")
!  CALL AXSSCL('ELOG','Y')
	call labels('EXP','Y')
	stepx=ceiling((ifincyc-stepnum(istart))/20D0)
	if (iscfqc==0) then
		call AXSLEN(2300,600)
		!Draw RMSDP variation
		CALL NAME('RMS density matrix','Y')
		call AXSPOS(400,700)
		CALL GRAF(stepnum(istart),dfloat(ifincyc),stepnum(istart),stepx, 0D0,maxval(RMSDP(istart:ifincyc)),0D0,maxval(RMSDP(istart:ifincyc))/7)
		CALL CURVE(stepnum(istart:ifincyc),RMSDP(istart:ifincyc),ishow)
		call dash !Show where is goal
		call color('RED')
		constant=aimRMSDP
		CALL CURVE(stepnum(istart:ifincyc),constant(istart:ifincyc),ishow)
		call color('WHITE')
		call solid
		CALL ENDGRF
		!Draw MaxDP variation
		CALL NAME('MAX density matrix','Y')
		call AXSPOS(400,1400)
		CALL GRAF(stepnum(istart),dfloat(ifincyc),stepnum(istart),stepx, 0D0,maxval(MaxDP(istart:ifincyc)),0D0,maxval(MaxDP(istart:ifincyc))/7)
		CALL CURVE(stepnum(istart:ifincyc),MaxDP(istart:ifincyc),ishow)
		call dash !Show where is goal
		call color('RED')
		constant=aimMaxDP
		CALL CURVE(stepnum(istart:ifincyc),constant(istart:ifincyc),ishow)
		call color('WHITE')
		call solid
		CALL ENDGRF
		!Draw energy variation
	! 	CALL MARKER(21)
		CALL NAME('Energy','Y')
		call AXSPOS(400,2100)
		CALL NAME('Step number','X')
		fminDE=minval(DE(istart:ifincyc))
		fmaxDE=maxval(DE(istart:ifincyc))
		CALL GRAF(stepnum(istart),dfloat(ifincyc),stepnum(istart),stepx, fminDE,fmaxDE,fminDE,(fmaxDE-fminDE)/7)
		CALL CURVE(stepnum(istart:ifincyc),DE(istart:ifincyc),ishow)
		call dot !where is Y=0
		call XAXGIT
		call dash !Show where is goal
		call color('RED')
		constant=aimMaxDP
		CALL CURVE(stepnum(istart:ifincyc),constant(istart:ifincyc),ishow)
		constant=-aimMaxDP
		CALL CURVE(stepnum(istart:ifincyc),constant(istart:ifincyc),ishow)
		CALL ENDGRF
		! Show if have converged
		call color('RED')
		call height(50)
		if (RMSDP(ifincyc)<aimRMSDP) call messag("Done",2800,400)
		if (MaxDP(ifincyc)<aimMaxDP) call messag("Done",2800,1100)
		if (abs(DE(ifincyc))<aimDE) call messag("Done",2800,1800)
	else if (iscfqc==1) then
		call AXSLEN(2400,950)
		!Draw DE variation
		CALL NAME('Energy','Y')
		call AXSPOS(400,1050)
		fminDE=minval(DE(istart:ifincyc))
		fmaxDE=maxval(DE(istart:ifincyc))
		CALL GRAF(stepnum(istart),dfloat(ifincyc),stepnum(istart),stepx, fminDE,fmaxDE,fminDE,(fmaxDE-fminDE)/7)
		CALL CURVE(stepnum(istart:ifincyc),DE(istart:ifincyc),ishow)
		call endgrf
		!Draw gradient variation
		CALL NAME('Gradient','Y')
		call AXSPOS(400,2150)
		CALL NAME('Step number','X')
		CALL GRAF(stepnum(istart),dfloat(ifincyc),stepnum(istart),stepx, 0D0,maxval(grad(istart:ifincyc)),0D0,maxval(grad(istart:ifincyc))/7)
		CALL CURVE(stepnum(istart:ifincyc),grad(istart:ifincyc),ishow)
	end if
	call disfin
	itime=1
end do
end subroutine


!!------------- Show overlap matrix of alpha and beta orbitals
subroutine aboverlap
use util
use defvar
implicit real*8 (a-h,o-z)
character selectyn*1
real*8,allocatable :: ovlpmat(:,:)
real*8 :: GTFSintmat(nprims,nprims)

if (wfntype==0.or.wfntype==2.or.wfntype==3) then
	write(*,*) "ERROR: This function is only available for unrestricted wavefunction!"
	write(*,*)
	return
end if
write(*,*) "1 Calculate overlap between all alpha and all beta orbitals"
write(*,*) "2 Calculate overlap between alpha MOs and beta MOs with corresponding indices"
read(*,*) itype
do isplit=1,nmo
	if (MOtype(isplit)==2) exit
end do
numalphaMO=isplit-1
numbetaMO=nmo-isplit+1
allocate(ovlpmat(numalphaMO,numbetaMO))
ovlpmat=0D0
write(*,*) "Please wait..."
do ii=1,nprims
	do jj=1,nprims
		GTFSintmat(ii,jj)=doSint(ii,jj)
	end do
end do

if (itype==1) then
	!$OMP PARALLEL DO SHARED(ovlpmat) PRIVATE(iorba,iorbb,accum,ii,jj) schedule(dynamic) NUM_THREADS(nthreads)
	do iorba=1,numalphaMO !alpha orbitals
		do iorbb=isplit,nmo !beta orbitals
			accum=0D0
			if (allocated(CObasa)) then
				do ii=1,nbasis
					do jj=1,nbasis
						accum=accum+CObasa(ii,iorba)*CObasb(jj,iorbb-nbasis)*Sbas(ii,jj)
					end do
				end do
			else
				do ii=1,nprims
					do jj=1,nprims
						accum=accum+CO(iorba,ii)*CO(iorbb,jj)*GTFSintmat(ii,jj)
					end do
				end do
			end if
			ovlpmat(iorba,iorbb-isplit+1)=accum
		end do
	end do
	!$OMP end parallel do

	write(*,*)
	do iorb=1,numbetaMO
		write(*,"(' Overlap between the',i6,'th alpha and beta orbitals:',f12.6)") iorb,ovlpmat(iorb,iorb)
	end do

	if (wfntype==1) then !Calculate <S**2>
		tmp=0D0
		do iorba=1,naelec !cycle occupied orbitals
			do iorbb=1,nbelec
				tmp=tmp+ovlpmat(iorba,iorbb)**2
			end do
		end do
	end if
	write(*,"(' <S**2> is ',f14.8)") (naelec-nbelec)/2*((naelec-nbelec)/2+1)+nbelec-tmp
	write(*,*)
	
	write(*,*) "Maximum pairing:"
	do i=1,numalphaMO
		imax=maxloc(abs(ovlpmat(i,:)),1)
		write(*,"(' Alpha',i6,'   Beta',i6,'   Overlap:',f12.6)") i,imax,abs(ovlpmat(i,imax))
	end do
	write(*,*)
	
	write(*,*) "If write overlap matrix to ovlpmat.txt in current folder? (y/n)"
	read(*,*) selectyn
	if (selectyn=='y'.or.selectyn=='Y') then
		open(10,file="ovlpmat.txt",status="replace")
		call showmatgau(ovlpmat,"Overlap matrix between alpha & beta orbitals",0,"1PE14.6",10)
		write(*,"(a)") " Done! The (i,j) element means the overlap integral between the ith alpha orbital and the jth beta orbital"
		close(10)
	end if
else if (itype==2) then
	do iorba=1,numbetaMO !Cycle all alpha orbitals, but the upper limit may be less than the beta ones, so use this limit
		accum=0D0
		iorbb=isplit-1+iorba
		do ii=1,nprims
			do jj=1,nprims
				accum=accum+CO(iorba,ii)*CO(iorbb,jj)*GTFSintmat(ii,jj)
			end do
		end do
		write(*,"(' Overlap between the ',i5,'th alpha and beta orbitals:',f12.6)") iorba,accum
	end do
end if
end subroutine



!!---------- Perform biorthogonalization between all alpha and beta orbitals
subroutine orb_biortho
use defvar
character selectyn
real*8 Emat(nbasis,nbasis),tmparr(nbasis),orbsingval(nmo)

if (wfntype/=1) then
	write(*,*) "ERROR: This function is only available for unrestricted SCF wavefunction!"
	write(*,*) "Press ENTER to return"
	read(*,*)
	return
end if
if (.not.allocated(CObasa)) then
	write(*,*) "ERROR: This function requires basis function information!"
	write(*,*) "Press ENTER to return"
	read(*,*)
	return
end if

!In fact, if choose option 1, the unoccupied alpha MO will keep unchanged, including its energy
!while beta MOs in the range of (naelec+1)~nbasis will be transformed at step 2 and energies are thus changed
write(*,*)
write(*,*) "1 Perform biorthogonalization only for occupied orbitals"
write(*,*) "2 Perform biorthogonalization for all orbitals"
write(*,*) "Note: Option 2 is time-consuming if there are large number of unoccupied MOs"
read(*,*) iorbrange

!Stage 1
call doorb_biortho(1,nint(naelec),1,nint(nbelec))
!Stage 2
if (nint(naelec)/=nint(nbelec)) then
	write(*,*)
	call doorb_biortho(nint(nbelec)+1,nint(naelec),nint(nbelec)+1,nbasis)
end if
!Stage 3
write(*,*)
if (iorbrange==2) then
	call doorb_biortho(nint(naelec)+1,nbasis,nint(naelec)+1,nbasis)
else
	write(*,*) "Biorthogonalization between unoccupied orbitals is skipped as requested"
end if
orbsingval=MOene

idoene=0
write(*,*)
write(*,*) "Would you also want to evaluate energy of the biorthogonalized orbitals? (y/n)"
read(*,*) selectyn
if (selectyn=='y'.or.selectyn=='Y') then
	call loadFock47(istat)
	if (istat==0) then
		write(*,*) "Unable to evaluate orbital energies!"
	else
		idoene=1
		Emat=matmul(matmul(transpose(CObasa),FmatA),CObasa)
		do iorb=1,nbasis
			MOene(iorb)=Emat(iorb,iorb)
		end do
		Emat=matmul(matmul(transpose(CObasb),FmatB),CObasb)
		do iorb=1,nbasis
			MOene(nbasis+iorb)=Emat(iorb,iorb)
		end do
		write(*,*) "Orbital energies have been successfully evaluated!"
		write(*,*)
		write(*,*) "Do you want to order each batch of orbitals according to their energies? (y/n)"
		read(*,*) selectyn
		if (selectyn=='y'.or.selectyn=='Y') then
			do itime=1,3
				if (itime==1) then
					nmobeg=1
					nmoend=nint(nbelec)
				else if (itime==2) then
					if (nint(naelec)==nint(nbelec)) cycle
					nmobeg=nint(nbelec)+1
					nmoend=nint(naelec)
				else if (itime==3) then
					if (nmoend==1) cycle
					nmobeg=nint(naelec)+1
					nmoend=nbasis
				end if
				do iorb=nmobeg,nmoend
					do jorb=iorb+1,nmoend
						avgene_i=(MOene(iorb)+MOene(iorb+nbasis))/2D0
						avgene_j=(MOene(jorb)+MOene(jorb+nbasis))/2D0
						if (avgene_i>avgene_j) then
							!alpha
							tmparr=CObasa(:,iorb)
							CObasa(:,iorb)=CObasa(:,jorb)
							CObasa(:,jorb)=tmparr
							tmpene=MOene(iorb)
							MOene(iorb)=MOene(jorb)
							MOene(jorb)=tmpene
							tmpsingval=orbsingval(iorb)
							orbsingval(iorb)=orbsingval(jorb)
							orbsingval(jorb)=tmpsingval
							!beta
							tmparr=CObasb(:,iorb)
							CObasb(:,iorb)=CObasb(:,jorb)
							CObasb(:,jorb)=tmparr
							tmpene=MOene(iorb+nbasis)
							MOene(iorb+nbasis)=MOene(jorb+nbasis)
							MOene(jorb+nbasis)=tmpene
							tmpsingval=orbsingval(iorb+nbasis)
							orbsingval(iorb+nbasis)=orbsingval(jorb+nbasis)
							orbsingval(jorb+nbasis)=tmpsingval
						end if
					end do
				end do
			end do
			write(*,*) "Ordering of biorthogonalized orbitals has done!"
		end if
	end if
end if

!Output singular values and with/without energies in final sequence
write(*,*)
write(*,*) "Exporting biortho.txt..."
open(10,file="biortho.txt",status="replace")
nmoend=nbasis
if (iorbrange==1) nmoend=nint(naelec)
write(10,*) "S = Singular value, E = Energy (in eV), O= Occupancy, A=Alpha, B=Beta"
write(10,*)
if ((selectyn=='y'.or.selectyn=='Y').and.istat==1) then !Successfully calculated energies
	do imo=1,nmoend
		write(10,"(' Orb:',i6,'   S=',f7.4,'  E(A)=',f11.3,'  O(A)=',f4.1,'  E(B)=',f11.3,'  O(B)=',f4.1)") &
		imo,orbsingval(imo),MOene(imo)*au2eV,MOocc(imo),MOene(imo+nbasis)*au2eV,MOocc(imo+nbasis)
		if (imo==nint(nbelec).and.naelec/=nbelec) write(10,*) &
		"-------------------------------------------------------------------------------"
		if (iorbrange==2.and.imo==nint(naelec)) write(10,*) &
		"-------------------------------------------------------------------------------"
	end do
	write(*,"(a)") " Done! Singular values and energies of the biorthogonalized orbitals &
	have been exported as biortho.txt in current folder"
else
	do imo=1,nmoend
		write(10,"(' Orb:',i6,'   S=',f7.4,'   O(A)=',f4.1,'   O(B)=',f4.1)") &
		imo,orbsingval(imo),MOocc(imo),MOocc(imo+nbasis)
		if (imo==nint(nbelec).and.naelec/=nbelec) &
		write(10,*) "-----------------------------------------------"
		if (iorbrange==2.and.imo==nint(naelec)) &
		write(10,*) "-----------------------------------------------"
	end do
	write(*,"(a)") " Done! Singular values of the biorthogonalized orbitals &
	have been exported as biortho.txt in current folder"
end if
close(10)

write(*,*)
write(*,*) "Exporting biortho.fch..."
call outfch("biortho.fch",10,0)
write(*,"(a)",advance="no") " Done! biortho.fch has been outputted in current folder, which contains biorthogonalized orbitals"
if (idoene==0) write(*,"(a)") " The orbital energies correspond to singular values of orbital overlap matrix"
if (idoene==1) write(*,"(a)") " The orbital energy information correspond to actual energy of the biorthogonalized orbitals"
if (iorbrange==1) write(*,"(a,i6,a,i6,a)") " Note: Since biorthgonalization between unoccupied MOs was not performed, &
the alpha and beta orbitals in the range",nint(naelec)+1," to",nbasis," is not meaningful"
write(*,*)
write(*,"(a)") " Do you want to load the biortho.fch now? If input ""y"", then you can directly visualize and analyze the biorthogonalized orbitals that just generated (y/n)"
read(*,*) selectyn
if (selectyn=='y'.or.selectyn=='Y') then
	write(*,*) "Loading biortho.fch..."
	call dealloall
	call readinfile("biortho.fch",1)
	write(*,*) "Loading finished!"
else
	write(*,"(' Reloading ',a,'...')") trim(firstfilename)
	call dealloall
	call readinfile(firstfilename,1)
end if
end subroutine
!!-------- Do biorthogonalization between specific range of alpha and beta orbitals
!Alpha range: na_beg~na_end; Beta range: nb_beg~nb_end
subroutine doorb_biortho(na_beg,na_end,nb_beg,nb_end)
use defvar
use util
implicit real*8 (a-h,o-z)
integer na_beg,na_end,nb_beg,nb_end
real*8,allocatable :: matU(:,:),matVT(:,:),matV(:,:),singval(:),ovlpmat(:,:)
! Since O = U * SIGMA * transpose(V) and U,V are unitary matrices, it clear that transpose(U) * O * V = SIGMA
write(*,"(' Doing biorthogonalization for alpha',i5,' to',i5,', Beta',i5,' to',i5,' ...')") na_beg,na_end,nb_beg,nb_end
na=na_end-na_beg+1
nb=nb_end-nb_beg+1
minnab=min(na,nb)
allocate(matU(na,na),matV(nb,nb),singval(minnab),ovlpmat(na,nb))
iprog=0
call showprog(iprog,na)
!$OMP PARALLEL DO SHARED(ovlpmat,iprog) PRIVATE(ia,imoa,ib,imob,ibas,jbas,tmpval) schedule(dynamic) NUM_THREADS(nthreads)
do ia=1,na
	imoa=na_beg+ia-1
	do ib=1,nb
		imob=nb_beg+ib-1
		tmpval=0
		do ibas=1,nbasis
			do jbas=1,nbasis
				tmpval=tmpval+CObasa(ibas,imoa)*CObasb(jbas,imob)*Sbas(ibas,jbas)
			end do
		end do
		ovlpmat(ia,ib)=tmpval
	end do
	!$OMP CRITICAL
	iprog=iprog+1
	call showprog(iprog,na)
	!$OMP end CRITICAL
end do
!$OMP END PARALLEL DO

call SVDmat(1,ovlpmat,matU,matV,singval,info)
if (info/=0) then
	write(*,*) "Error: SVD failed! The following result will be meaningless!"
	write(*,*) "Press ENTER button to continue"
	read(*,*)
	return
end if
write(*,*) "Singular values of orbital overlap matrix:"
write(*,"(8f9.4)") singval
CObasa(:,na_beg:na_end)=matmul(CObasa(:,na_beg:na_end),matU)
CObasb(:,nb_beg:nb_end)=matmul(CObasb(:,nb_beg:nb_end),matV)
MOene(na_beg:na_beg+minnab-1)=singval
MOene(nbasis+nb_beg:nbasis+nb_beg+minnab-1)=singval
end subroutine



!!---------- Integrating a function in whole space
!! ifunc: The real space function to be integrated
!
!The integration grid is directly controlled by sphpot and radpot in settings.ini, since integrand may be not proportional to electron density,
!the grid will not be adjusted automatically (parm always=1) as proposed by Becke for more efficient integration of XC functional
!
!The intval and funcval have 5 slots, the first one is used in normal case
subroutine intfunc(ifunc)
use function
use util
implicit real*8 (a-h,o-z)
real*8 intval(5),intvalold(5),funcval(radpot*sphpot,5),beckeweigrid(radpot*sphpot)
type(content) gridatm(radpot*sphpot),gridatmorg(radpot*sphpot)
real*8 ELF2,ELF2r2 !Used to evaluate spherically symmetric average ELF (or LOL)

write(*,"(' Radial points:',i5,'    Angular points:',i5,'   Total:',i10,' per center')") radpot,sphpot,radpot*sphpot
call gen1cintgrid(gridatmorg,iradcut)

call walltime(iwalltime1)
CALL CPU_TIME(time_begin)

intval=0
intvalold=0
ELFsqr=0
ELFsqrr2=0
do iatm=1,ncenter
	write(*,"(' Processing center',i6,'(',a2,')   /',i6)") iatm,a(iatm)%name,ncenter
	gridatm%x=gridatmorg%x+a(iatm)%x !Move quadrature point to actual position in molecule
	gridatm%y=gridatmorg%y+a(iatm)%y
	gridatm%z=gridatmorg%z+a(iatm)%z
	!$OMP parallel do shared(funcval) private(i,rnowx,rnowy,rnowz) num_threads(nthreads)
	do i=1+iradcut*sphpot,radpot*sphpot
		rnowx=gridatm(i)%x
		rnowy=gridatm(i)%y
		rnowz=gridatm(i)%z
		if (ispecial==0) then
			funcval(i,1)=calcfuncall(ifunc,rnowx,rnowy,rnowz)
		else if (ispecial==1) then
			funcval(i,1)=infoentro(2,rnowx,rnowy,rnowz) !Shannon entropy density, see JCP,126,191107 for example
			funcval(i,2)=Fisherinfo(1,rnowx,rnowy,rnowz) !Fisher information density, see JCP,126,191107 for example
			funcval(i,3)=weizsacker(rnowx,rnowy,rnowz) !Steric energy
		end if
	end do
	!$OMP end parallel do
	
	call gen1cbeckewei(iatm,iradcut,gridatm,beckeweigrid)
	do i=1+iradcut*sphpot,radpot*sphpot
		intval=intval+funcval(i,:)*gridatmorg(i)%value*beckeweigrid(i)
		if (ifunc==9.or.ifunc==10) then
			tmp=funcval(i,1)**2*gridatmorg(i)%value*beckeweigrid(i)
			r2=gridatm(i)%x**2+gridatm(i)%y**2+gridatm(i)%z**2
			ELFsqr=ELFsqr+tmp
			ELFsqrr2=ELFsqrr2+tmp*r2
		end if
! 		write(12,"(i6,3f12.5,2E16.8)") i,gridatm(i)%x,gridatm(i)%y,gridatm(i)%z,gridatmorg(i)%value,beckeweigrid(i)
	end do
	
	if (ispecial==0) write(*,"(' Accumulated value:',f20.10,'  Current center:',f20.10)") intval(1),intval(1)-intvalold(1)
	intvalold=intval
end do

CALL CPU_TIME(time_end)
call walltime(iwalltime2)
write(*,"(' Calculation took up CPU time',f12.2,'s, wall clock time',i10,'s')") time_end-time_begin,iwalltime2-iwalltime1
write(*,*)
if (ispecial==0) then
	write(*,"(' Final result:',f20.10)") intval(1)
else if (ispecial==1) then
	write(*,"(' Shannon entropy:   ',f23.8)") intval(1)
	write(*,"(' Fisher information:',f23.8)") intval(2)
	write(*,"(' Steric energy:     ',f23.8)") intval(3)
end if
if (ifunc==9) then
	write(*,"(/,a,E14.6,' Bohr')") " int(ELF*r2*ELF):",ELFsqrr2
	write(*,"(a,E14.6)") " int(ELF*ELF):   ",ELFsqr
	write(*,"(a,f12.6,' Bohr')") " Spherically symmetric average ELF:",dsqrt(ELFsqrr2/ELFsqr)
else if (ifunc==10) then
	write(*,"(/,a,E14.6,' Bohr')") " int(LOL*r2*LOL):",ELFsqrr2
	write(*,"(a,E14.6)") " int(LOL*LOL):   ",ELFsqr
	write(*,"(a,f12.6,' Bohr')") " Spherically symmetric average LOL:",dsqrt(ELFsqrr2/ELFsqr)
end if
end subroutine

!!----------- Silent and simplied version of subroutine intfunc, directly return the integrated result
subroutine intfunc_silent(ifunc,intval)
use function
use util
implicit real*8 (a-h,o-z)
real*8 intval,funcval(radpot*sphpot),beckeweigrid(radpot*sphpot)
type(content) gridatm(radpot*sphpot),gridatmorg(radpot*sphpot)
write(*,"(' Radial points:',i5,'    Angular points:',i5,'   Total:',i10,' per center')") radpot,sphpot,radpot*sphpot
call gen1cintgrid(gridatmorg,iradcut)
intval=0
call showprog(0,ncenter)
do iatm=1,ncenter
	call showprog(iatm,ncenter)
	gridatm%x=gridatmorg%x+a(iatm)%x !Move quadrature point to actual position in molecule
	gridatm%y=gridatmorg%y+a(iatm)%y
	gridatm%z=gridatmorg%z+a(iatm)%z
	!$OMP parallel do shared(funcval) private(i) num_threads(nthreads)
	do i=1+iradcut*sphpot,radpot*sphpot
		funcval(i)=calcfuncall(ifunc,gridatm(i)%x,gridatm(i)%y,gridatm(i)%z)
	end do
	!$OMP end parallel do
	call gen1cbeckewei(iatm,iradcut,gridatm,beckeweigrid)
	do i=1+iradcut*sphpot,radpot*sphpot
		intval=intval+funcval(i)*gridatmorg(i)%value*beckeweigrid(i)
	end do
end do
end subroutine





!------ Calculate overlap and distance between two orbitals
subroutine ovlpdistorb
use function
use util
implicit real*8 (a-h,o-z)
real*8 intval(2),funcval(radpot*sphpot,2) !1/2=overlap of norm/sqr
real*8 cenpos(3,2),cenval(radpot*sphpot,3,2) !1st: point,  2st: x/y/z,  3st: iorb,jorb
real*8 beckeweigrid(radpot*sphpot)
type(content) gridatm(radpot*sphpot),gridatmorg(radpot*sphpot)
character selectyn
integer radpotold,sphpotold,radcutold

if (iautointgrid==1) then !This setting is good balance between cost and accuracy
	radpotold=radpot
	sphpotold=sphpot
	radcutold=radcut
	radpot=20
	sphpot=170
	radcut=18 !Enlarge radcut, because for Rydberg orbital, the default radcut 10 Bohr is not sufficient
end if

do while(.true.)
	write(*,*)
	write(*,*) "Input the index of the two orbitals, e.g. 32,35"
	write(*,*) "To exit, input 0,0"
	read(*,*) iorb,jorb
	if (iorb==0.and.jorb==0) then
		radpot=radpotold
		sphpot=sphpotold
		radcut=radcutold
		return
	end if
	
	if (iautointgrid==1) then
		write(*,"(a)") " Note: The default integration grids in general should be sufficient. If you want to change, &
		set ""iautointgrid"" in settings.ini to 0, and set ""radpot"" and ""sphpot"" to expected values"
	end if
	write(*,"(' Radial points:',i5,'    Angular points:',i5,'   Total:',i10,' per center')") radpot,sphpot,radpot*sphpot
	call gen1cintgrid(gridatmorg,iradcut)

	intval=0
	cenpos=0
	do iatm=1,ncenter
		write(*,"(' Processing center',i6,'(',a2,')   /',i6)") iatm,a(iatm)%name,ncenter
		gridatm%x=gridatmorg%x+a(iatm)%x !Move quadrature point to actual position in molecule
		gridatm%y=gridatmorg%y+a(iatm)%y
		gridatm%z=gridatmorg%z+a(iatm)%z
		!$OMP parallel do shared(funcval,cenval) private(i,vali,valj,vali2,valj2,rnowx,rnowy,rnowz) num_threads(nthreads)
		do i=1+iradcut*sphpot,radpot*sphpot
			rnowx=gridatm(i)%x
			rnowy=gridatm(i)%y
			rnowz=gridatm(i)%z
			vali=fmo(rnowx,rnowy,rnowz,iorb)
			vali2=vali**2 !rho of iorb
			valj=fmo(rnowx,rnowy,rnowz,jorb)
			valj2=valj**2 !rho of jorb
			funcval(i,1)=abs(vali)*abs(valj)
			funcval(i,2)=vali2*valj2
			cenval(i,1,1)=vali2*rnowx
			cenval(i,2,1)=vali2*rnowy
			cenval(i,3,1)=vali2*rnowz
			cenval(i,1,2)=valj2*rnowx
			cenval(i,2,2)=valj2*rnowy
			cenval(i,3,2)=valj2*rnowz
		end do
		!$OMP end parallel do
		
		call gen1cbeckewei(iatm,iradcut,gridatm,beckeweigrid)
		do i=1+iradcut*sphpot,radpot*sphpot
			intval=intval+funcval(i,:)*gridatmorg(i)%value*beckeweigrid(i)
			cenpos=cenpos+cenval(i,:,:)*gridatmorg(i)%value*beckeweigrid(i)
		end do
		
	end do
	write(*,*)
	write(*,"(' X/Y/Z of centroid of electron density (Angstrom)')")
	write(*,"(' Orbital',i6,':',3f12.6)") iorb,cenpos(:,1)*b2a
	write(*,"(' Orbital',i6,':',3f12.6)") jorb,cenpos(:,2)*b2a
	write(*,"(' Centroid distance between the two orbitals:',f12.6,' Angstrom')") dsqrt(sum((cenpos(:,1)-cenpos(:,2))**2))
	write(*,"(' Overlap integral of norm of the two orbitals:',f16.10)") intval(1)
	write(*,"(' Overlap integral of square of the two orbitals:',f16.10)") intval(2)
	write(*,*)
	write(*,"(a)") " Do you want to add the two centroids as two dummy atoms? (y/n)"
	write(*,"(a)") " Note: Then you may enter main function 0 to visualize them along with corresponding orbital isosurfaces"
	read(*,*) selectyn
	if (selectyn=='y'.or.selectyn=='Y') then
		deallocate(a)
		allocate(a(ncenter+2))
		a(1:ncenter)=a_org
		a(ncenter+1)%name="Bq"
		a(ncenter+1)%index=0
		a(ncenter+1)%charge=0
		a(ncenter+1)%x=cenpos(1,1)
		a(ncenter+1)%y=cenpos(2,1)
		a(ncenter+1)%z=cenpos(3,1)
		a(ncenter+2)=a(ncenter+1)
		a(ncenter+2)%x=cenpos(1,2)
		a(ncenter+2)%y=cenpos(2,2)
		a(ncenter+2)%z=cenpos(3,2)
		ncenter=ncenter+2
		radpot=radpotold
		sphpot=sphpotold
		radcut=radcutold
		write(*,*) "Done!"
		return
	end if
end do
end subroutine


!itype=1 : Integrate (f_wfn1 - f_wfn2)**2
!itype=2 : Integrate |f_wfn1 - f_wfn2|
!where f_wfn1 means real space function for wavefunction file 1, f_wfn2 is that for wavefunction file 2
!This function was specifically written for realizing Michael G. Medvedev's idea
subroutine intdiff(itype)
use function
use util
implicit real*8 (a-h,o-z)
real*8 intval,intvalold,funcval1(radpot*sphpot),funcval2(radpot*sphpot),beckeweigrid(radpot*sphpot)
real*8 funcval1all(radpot*sphpot,ncenter) !For reuse data
type(content) gridatm(radpot*sphpot),gridatmorg(radpot*sphpot)
character*200 filename2,reusename*200

if (itype==1) write(*,*) "Note: The integrand is (f_wfn1 - f_wfn2)**2"
if (itype==2) write(*,*) "Note: The integrand is |f_wfn1 - f_wfn2|"
if (ifiletype/=2.and.ifiletype/=3) write(*,"(a)") " Hint: If you use .wfx or .wfn file as input instead, the calculation speed may be improved significantly!"
write(*,*)
write(*,*) "Select the function to be integrated over the whole space"
call funclist
read(*,*) ifunc
write(*,*)
write(*,*) "Input the filename of another wavefunction file, e.g. C:\yuri.wfn"
read(*,"(a)") filename2
write(*,*)
write(*,"(a)") " Input density cutoff (e.g. 0.5), if electron density of the firstly loaded wavefunction at a point is &
larger than this value, then corresponding integration grid will be ignored"
write(*,*) "If you do not want to enable this feature, input 0"
read(*,*) denscut

ireuse=0
write(reusename,"(a,'_',i3.3,'_',i4.4,'_',i4.4)") trim(firstfilename),ifunc,radpot,sphpot
inquire(file=reusename,exist=alive)
if (alive) then
	write(*,"(' Note: Data of reference system retrieved from ',a,' will be used')") trim(reusename)
	ireuse=1
	open(10,file=reusename,status="old")
	do iatm=1,ncenter
		read(10,*)
		read(10,*) funcval1all(:,iatm)
	end do
	close(10)
end if

write(*,"(' Radial points:',i5,'    Angular points:',i5,'   Total:',i10,' per center')") radpot,sphpot,radpot*sphpot
call gen1cintgrid(gridatmorg,iradcut)

call walltime(iwalltime1)
CALL CPU_TIME(time_begin)

intval=0
do iatm=1,ncenter
	write(*,"(' Processing center',i6,'(',a2,')   /',i6)") iatm,a(iatm)%name,ncenter
	gridatm%x=gridatmorg%x+a(iatm)%x !Move quadrature point to actual position in molecule
	gridatm%y=gridatmorg%y+a(iatm)%y
	gridatm%z=gridatmorg%z+a(iatm)%z
	
	if (ireuse==0) then !Calculate data for wfn1
		!$OMP parallel do shared(funcval1) private(i,rnowx,rnowy,rnowz) num_threads(nthreads) schedule(DYNAMIC)
		do i=1+iradcut*sphpot,radpot*sphpot
			rnowx=gridatm(i)%x
			rnowy=gridatm(i)%y
			rnowz=gridatm(i)%z
			funcval1(i)=calcfuncall(ifunc,rnowx,rnowy,rnowz)
		end do
		!$OMP end parallel do
		funcval1all(:,iatm)=funcval1
	else !Reuse data of wfn1 from external file
		funcval1=funcval1all(:,iatm)
	end if
	
	!Calculate data for wfn2
	call dealloall
	call readinfile(filename2,1)
	!$OMP parallel do shared(funcval2) private(i,rnowx,rnowy,rnowz) num_threads(nthreads) schedule(DYNAMIC)
	do i=1+iradcut*sphpot,radpot*sphpot
		rnowx=gridatm(i)%x
		rnowy=gridatm(i)%y
		rnowz=gridatm(i)%z
		funcval2(i)=calcfuncall(ifunc,rnowx,rnowy,rnowz)
	end do
	!$OMP end parallel do
	!Recover to wfn1
	call dealloall
	call readinfile(firstfilename,1)
	
	call gen1cbeckewei(iatm,iradcut,gridatm,beckeweigrid)
	do i=1+iradcut*sphpot,radpot*sphpot
		if (denscut/=0) then
			if (ifunc==1) then
				tmpdens=funcval1(i)
			else
				tmpdens=fdens(gridatm(i)%x,gridatm(i)%y,gridatm(i)%z)
			end if
			if (tmpdens>denscut) cycle
		end if
		if (itype==1) then
			intval=intval+(funcval1(i)-funcval2(i))**2 *gridatmorg(i)%value*beckeweigrid(i)
		else
			intval=intval+abs(funcval1(i)-funcval2(i)) *gridatmorg(i)%value*beckeweigrid(i)
		end if
	end do
	write(*,"(' Accumulated value:',f20.10,'  Current center:',f20.10)") intval,intval-intvalold
	intvalold=intval
end do

CALL CPU_TIME(time_end)
call walltime(iwalltime2)
write(*,"(' Calculation took up CPU time',f12.2,'s, wall clock time',i10,'s',/)") time_end-time_begin,iwalltime2-iwalltime1
write(*,"(' Final result:',f24.12)") intval

!Write calculated wfn1 data to external file for reuse in the later calculation
if (ireuse==0) then
	open(10,file=reusename,status="replace")
	do iatm=1,ncenter
		write(10,*) iatm
		write(10,*) funcval1all(:,iatm)
	end do
	close(10)
	write(*,"(/,' Data of the firstly loaded file have been exported to ',a,' for possible later use')") trim(reusename)
end if
end subroutine



!!!------------ Calculate molecular volume by Monte Carlo method
!If isoval == 0, then evaluate volume by approximate method, if >0, evaluate the volume based on rho
!imethod=1: Use superposition of vdW sphere to define vdW region, =2: use isosurface of electron density to define it
subroutine calcvolume(imethod,pointexp,mcisoval,enlarbox)
use defvar
use function
implicit real*8 (a-h,o-z)
real*8 maxx,maxy,maxz,minx,miny,minz,lengthx,lengthy,lengthz,nowx,nowy,nowz,rx,ry,rz,r2,tmp
real*8 pointexp,mcisoval,enlarbox,boxvol
integer :: in,ntot,iatm,imethod,intmp
maxx=maxval( a(:)%x+enlarbox*vdwr(a(:)%index) )
maxy=maxval( a(:)%y+enlarbox*vdwr(a(:)%index) )
maxz=maxval( a(:)%z+enlarbox*vdwr(a(:)%index) )
minx=minval( a(:)%x-enlarbox*vdwr(a(:)%index) )
miny=minval( a(:)%y-enlarbox*vdwr(a(:)%index) )
minz=minval( a(:)%z-enlarbox*vdwr(a(:)%index) )
lengthx=maxx-minx
lengthy=maxy-miny
lengthz=maxz-minz
boxvol=lengthx*lengthy*lengthz
ntot=100*2**pointexp !More big more accurate

write(*,"(' Number of points used:',i10,', ',f10.3,' points per Bohr^3 in average')") ntot,ntot/(boxvol)
write(*,"(' Box size:',f12.3,' Bohr^3')") boxvol
write(*,*) "Please wait..."
in=0

if (imethod==1) then !I found if imethod=1 is parallelized too, the speed is much lowered!
	do i=1,ntot
		CALL RANDOM_NUMBER(nowx)
		CALL RANDOM_NUMBER(nowy)
		CALL RANDOM_NUMBER(nowz)
		nowx=nowx*lengthx+minx
		nowy=nowy*lengthy+miny
		nowz=nowz*lengthz+minz
		do iatm=1,ncenter
			rx=a(iatm)%x-nowx
			ry=a(iatm)%y-nowy
			rz=a(iatm)%z-nowz
			r2=rx*rx+ry*ry+rz*rz
			tmp=vdwr(a(iatm)%index)
			if (r2<=tmp*tmp) then
				in=in+1
				exit
			end if
		end do
	end do
else if (imethod==2) then
	!$OMP PARALLEL SHARED(in) PRIVATE(i,intmp,nowx,nowy,nowz) NUM_THREADS(nthreads)
	intmp=0
	!$OMP DO schedule(dynamic)
	do i=1,ntot
		CALL RANDOM_NUMBER(nowx)
		CALL RANDOM_NUMBER(nowy)
		CALL RANDOM_NUMBER(nowz)
		nowx=nowx*lengthx+minx
		nowy=nowy*lengthy+miny
		nowz=nowz*lengthz+minz
		if (fdens(nowx,nowy,nowz)>mcisoval) intmp=intmp+1
	end do
	!$OMP end do
	!$OMP CRITICAL
	in=in+intmp
	!$OMP end CRITICAL
	!$OMP END PARALLEL
end if

tmp=boxvol*(dfloat(in)/ntot)
write(*,"(' Molecular volume:',f9.3,' Bohr^3, (',f9.3,' Angstrom^3,',f9.3,' cm^3/mol)')") tmp,tmp*b2a**3,tmp*b2a**3*avogacst/1D24
end subroutine



!!--------- Calculate HOMA and Bird aromaticity index
subroutine HOMA_Bird
use defvar
use util
implicit real*8 (a-h,o-z)
integer :: numHOMAatm=6,HOMAatm(1000),numBirdatm=5,Birdatm(1000)
real*8 :: HOMArefbond(nelesupp,nelesupp)=-1D0,HOMAsigma(nelesupp,nelesupp)=0D0
real*8 :: Birda(nelesupp,nelesupp)=-1D0,Birdb(nelesupp,nelesupp)=-1D0
real*8 :: BirdVref(100)=-1
real*8,allocatable :: BirdN(:)
character C200inp*200

if (all(HOMArefbond==-1D0)) then !If =-1, means missing reference value
	write(*,*) "Note: Default parameters are taken from J. Chem. Inf. Comput. Sci.,33,70"
	HOMArefbond(6,6)=1.388D0
	HOMArefbond(6,7)=1.334D0
	HOMArefbond(7,6)=HOMArefbond(6,7)
	HOMArefbond(6,8)=1.265D0
	HOMArefbond(8,6)=HOMArefbond(6,8)
	HOMArefbond(6,15)=1.698D0
	HOMArefbond(15,6)=HOMArefbond(6,15)
	HOMArefbond(6,16)=1.677D0
	HOMArefbond(16,6)=HOMArefbond(6,16)
	HOMArefbond(7,7)=1.309D0
	HOMArefbond(7,8)=1.248D0
	HOMArefbond(8,7)=HOMArefbond(7,8)
	HOMAsigma(6,6)=257.7D0
	HOMAsigma(6,7)=93.52D0
	HOMAsigma(7,6)=HOMAsigma(6,7)
	HOMAsigma(6,8)=157.38D0
	HOMAsigma(8,6)=HOMAsigma(6,8)
	HOMAsigma(6,15)=118.91D0
	HOMAsigma(15,6)=HOMAsigma(6,15)
	HOMAsigma(6,16)=94.09D0
	HOMAsigma(16,6)=HOMAsigma(6,16)
	HOMAsigma(7,7)=130.33D0
	HOMAsigma(7,8)=57.21D0
	HOMAsigma(8,7)=HOMAsigma(7,8)
end if
if (all(Birda==-1D0)) then !If =-1, means missing reference value
	write(*,*) "Note: Default a and b parameters are taken from Tetrahedron,57,5715"
	Birda(6,6)=6.80D0
	Birdb(6,6)=1.71D0
	Birda(6,7)=6.48D0
	Birda(7,6)=Birda(6,7)
	Birdb(6,7)=2.00D0
	Birdb(7,6)=Birdb(6,7)
	Birda(6,8)=5.75D0
	Birda(8,6)=Birda(6,8)
	Birdb(6,8)=1.85D0
	Birdb(8,6)=Birdb(6,8)
	Birda(6,16)=11.9D0
	Birda(16,6)=Birda(6,16)
	Birdb(6,16)=2.59D0
	Birdb(16,6)=Birdb(6,16)
	Birda(7,8)=4.98D0
	Birda(8,7)=Birda(7,8)
	Birdb(7,8)=1.41D0
	Birdb(8,7)=Birdb(7,8)
	Birda(7,7)=5.28D0
	Birdb(7,7)=1.41D0
end if
if (all(BirdVref==-1D0)) then
	BirdVref(5)=35D0
	BirdVref(6)=33.2D0	
end if

do while(.true.)
	write(*,*)
	write(*,*) "-1 Return"
	write(*,*) "0 Start calculation for HOMA!"
	write(*,*) "1 Adjust parameters for HOMA calculation"
	write(*,*) "2 Start calculation for Bird aromaticity index!"
	write(*,*) "3 Adjust a and b parameters for Bird aromaticity index calculation"
	write(*,*) "4 Adjust reference V parameter for Bird aromaticity index calculation"
	read(*,*) isel
	if (isel==-1) then
		exit
	else if (isel==0) then
		write(*,*) "Current reference bond length (in Angstrom) and sigma paramters:"
		do iref=1,nelesupp
			do jref=iref,nelesupp
				if (HOMArefbond(iref,jref)/=-1) write(*,"(' ',a,a,a,a,2f12.4)") ind2name(iref),'-',ind2name(jref),':',HOMArefbond(iref,jref),HOMAsigma(iref,jref)
			end do
		end do
		write(*,*)
		do while(.true.)
			write(*,*) "Input indices of the atoms involved, e.g. 1,5,6,7,8,12"
			write(*,*) "(Input q can return)"
			read(*,"(a)") c200inp
			if (c200inp=='q'.or.c200inp=='Q') exit
			call str2arr(c200inp,numHOMAatm,HOMAatm)
			HOMAval=1D0
			write(*,*) "        Atom pair         Contribution  Bond length(Angstrom)"
			do iidx=1,numHOMAatm
				jidx=iidx+1
				if (iidx==numHOMAatm) jidx=1
				iatm=HOMAatm(iidx) !Actual atom index in present system
				jatm=HOMAatm(jidx)
				iatmeleidx=a(iatm)%index !Index in periodic table
				jatmeleidx=a(jatm)%index
				refbondlen=HOMArefbond(iatmeleidx,jatmeleidx)
				refsigma=HOMAsigma(iatmeleidx,jatmeleidx)
				if (refbondlen==-1D0) then
					write(*,"(' Error: Missing reference parameter for ',a,'-',a)") ind2name(iatmeleidx),ind2name(jatmeleidx)
					exit
				end if
				paircontri=-refsigma/numHOMAatm*(refbondlen-distmat(iatm,jatm)*b2a)**2
				write(*,"(i5,'(',a,')  --',i5,'(',a,'):',f15.6,f16.6)") iatm,ind2name(iatmeleidx),jatm,ind2name(jatmeleidx),paircontri,distmat(iatm,jatm)*b2a
				HOMAval=HOMAval+paircontri
				if (iidx==numHOMAatm) write(*,"(a,f12.6)") " HOMA value is",HOMAval
			end do
			write(*,*)
		end do
	else if (isel==1) then
		write(*,*) "Current reference bond length (in Angstrom) and sigma paramters:"
		do iref=1,nelesupp
			do jref=iref,nelesupp
				if (HOMArefbond(iref,jref)/=-1) write(*,"(' ',a,a,a,a,2f12.5)") ind2name(iref),'- ',ind2name(jref),':',HOMArefbond(iref,jref),HOMAsigma(iref,jref)
			end do
		end do
		do while(.true.)
			write(*,*) "Input two element indices and new bond length and sigma parameter"
			write(*,"(a)") " e.g. 6,7,1.334,93.52 means set reference bond length and sigma for C-N to 1.334 Angstrom and 93.52 respectively"
			write(*,*) "(Input q can return)"
			read(*,"(a)") C200inp
			if (C200inp(1:1)=='q'.or.C200inp(1:1)=='Q') exit
			read(C200inp,*) itmp,jtmp,refbondtmp,sigmatmp
			HOMArefbond(itmp,jtmp)=refbondtmp
			HOMArefbond(jtmp,itmp)=refbondtmp
			HOMAsigma(itmp,jtmp)=sigmatmp
			HOMAsigma(jtmp,itmp)=sigmatmp
			write(*,*) "Done!"
		end do
	
	else if (isel==2) then
		write(*,*) "Current a and b paramters:"
		do iref=1,nelesupp
			do jref=iref,nelesupp
				if (Birda(iref,jref)/=-1) write(*,"(' ',a,a,a,a,2f12.5)") ind2name(iref),'- ',ind2name(jref),':',Birda(iref,jref),Birdb(iref,jref)
			end do
		end do
		write(*,*)
		write(*,*) "Current reference V paramters:"	
		do itmp=1,size(BirdVref)
			if (BirdVref(itmp)==-1) cycle
			write(*,"(i4,' centers, value:',f10.4)") itmp,BirdVref(itmp)
		end do
		write(*,*)
		do while(.true.)
			write(*,*) "Input indices of the atoms involved, e.g. 1,5,6,7,8,12"
			write(*,*) "(Input q can return)"
			read(*,"(a)") c200inp
			if (c200inp=='q'.or.c200inp=='Q') exit
			call str2arr(c200inp,numBirdatm,Birdatm)
			if (BirdVref(numBirdatm)==-1) then
				write(*,*) "Error: Missing reference V parameter for this number of centers!"
				exit
			end if
			allocate(BirdN(numBirdatm))
			write(*,*) "        Atom pair             N term   Bond length(Angstrom)"
			do iidx=1,numBirdatm
				jidx=iidx+1
				if (iidx==numBirdatm) jidx=1
				iatm=Birdatm(iidx) !Actual atom index in present system
				jatm=Birdatm(jidx)
				iatmeleidx=a(iatm)%index !Index in periodic table
				jatmeleidx=a(jatm)%index
				Birdanow=Birda(iatmeleidx,jatmeleidx)
				Birdbnow=Birdb(iatmeleidx,jatmeleidx)
				if (Birdanow==-1D0) then
					write(*,"(' Error: Missing a and b parameter for ',a,'-',a)") ind2name(iatmeleidx),ind2name(jatmeleidx)
					exit
				end if
				BirdN(iidx)=Birdanow/(distmat(iatm,jatm)*b2a)**2-Birdbnow
				write(*,"(i5,'(',a,')  --',i5,'(',a,'):',f15.6,f16.6)") iatm,ind2name(iatmeleidx),jatm,ind2name(jatmeleidx),BirdN(iidx),distmat(iatm,jatm)*b2a
				if (iidx==numBirdatm) then
					avgBirdN=sum(BirdN)/numBirdatm
					BirdVnow=dsqrt(sum((BirdN(:)-avgBirdN)**2)/numBirdatm)*100/avgBirdN
					Birdval=100*(1-BirdVnow/BirdVref(numBirdatm))
					write(*,"(a,f12.6)") " Bird aromaticity index is",Birdval
				end if
			end do
			deallocate(BirdN)
			write(*,*)
		end do
	else if (isel==3) then
		write(*,*) "Current a and b paramters:"
		do iref=1,nelesupp
			do jref=iref,nelesupp
				if (Birda(iref,jref)/=-1) write(*,"(' ',a,a,a,a,2f12.5)") ind2name(iref),'- ',ind2name(jref),':',Birda(iref,jref),Birdb(iref,jref)
			end do
		end do
		do while(.true.)
			write(*,*) "Input two element indices and a and b parameter"
			write(*,"(a)") " e.g. 6,7,6.941,2.205 means set a and b parameter for C-N to 6.941 and 2.205 respectively"
			write(*,*) "(Input q can return)"
			read(*,"(a)") C200inp
			if (C200inp(1:1)=='q'.or.C200inp(1:1)=='Q') exit
			read(C200inp,*) itmp,jtmp,Birdatmp,Birdbtmp
			Birda(itmp,jtmp)=Birdatmp
			Birda(jtmp,itmp)=Birda(itmp,jtmp)
			Birdb(itmp,jtmp)=Birdbtmp
			Birdb(jtmp,itmp)=Birdb(itmp,jtmp)
			write(*,*) "Done!"
		end do
	else if (isel==4) then
		write(*,*) "Current reference V paramters:"	
		do itmp=1,size(BirdVref)
			if (BirdVref(itmp)==-1) cycle
			write(*,"(i4,' centers, value:',f10.4)") itmp,BirdVref(itmp)
		end do
		write(*,*) "Set the parameter for how many centers? e.g. 5"
		read(*,*) nBirdcentmp
		write(*,*) "Set to which value? e.g. 33.4"
		read(*,*) BirdVref(nBirdcentmp)
		write(*,*) "Done!"
	end if
end do
end subroutine



!!!------------ Calculate LOLIPOP (LOL Integrated Pi Over Plane), see Chem. Commun., 48, 9239-9241 (2012)
!!!------ Kawaii moe loli, pop, pop!
subroutine LOLIPOP
use defvar
use util
use GUI
implicit real*8 (a-h,o-z)
real*8 :: intradi=1.94D0,grdspc=0.08D0,LOLiso=0.55D0,disaway=0.5D0,vdwmulti=0.8D0
real*8 :: MOocc_old(nmo)
integer :: piorb(nmo),npiorb=0
integer :: ringidx(100),nringidx=0
character*3000 c3000tmp
! piorb(1:3)=(/ 17,20,21 /)
! npiorb=3
write(*,*) "## Kawaii moe loli, pop, pop!"
write(*,*)
write(*,*) "           =================  Calculate LOLIPOP  ================="
do while(.true.)
	write(*,*) "-1 Return"
	write(*,*) "0 Start calculation!"
	write(*,"(a,i7)") " 1 Choose pi orbitals, current number:",npiorb
	write(*,"(a,f8.4,a)") " 2 Set grid spacing, current:",grdspc," Bohr"
	write(*,"(a,f8.4,a)") " 3 Set integration radius, current:",intradi," Angstrom"
	write(*,"(a,f8.4,a)") " 4 Set the distance away the plane, current:",disaway," Angstrom"
	read(*,*) isel

	if (isel==-1) then
		return
	else if (isel==1) then
		write(*,*) "Input the indices of pi orbitals, e.g. 17,20-25,36,37"
		read(*,"(a)") c3000tmp
		call str2arr(c3000tmp,npiorb,piorb)
	else if (isel==2) then
		write(*,*) "Input grid spacing in Bohr, e.g. 0.05"
		read(*,*) grdspc
	else if (isel==3) then
		write(*,*) "Input integration radius in Angstrom, e.g. 1.94"
		read(*,*) intradi
	else if (isel==4) then
		write(*,*) "Input the distance away the plane in Angstrom, e.g. 0.5"
		read(*,*) disaway
		
	else if (isel==0) then
		if (npiorb==0) then
			write(*,*) "Error: You have to use option 1 to choose which orbitals are pi orbitals"
			write(*,*)
			cycle
		end if
		write(*,*) "Input the indices of the atoms constituted the ring, e.g. 4,5,6,7,8,9"
		read(*,"(a)") c3000tmp
		call str2arr(c3000tmp,nringidx,ringidx)
! 		nringidx=6
! 		ringidx(1:6)=(/1,2,3,4,5,6/)
		cenx=sum(a( ringidx(1:nringidx) )%x)/nringidx
		ceny=sum(a( ringidx(1:nringidx) )%y)/nringidx
		cenz=sum(a( ringidx(1:nringidx) )%z)/nringidx
		write(*,"(' Geometry center of the ring:',3f12.6,' Angstrom')") cenx*b2a,ceny*b2a,cenz*b2a
		endx=maxval( a(ringidx(1:nringidx))%x+vdwmulti*vdwr(a(ringidx(1:nringidx))%index) )
		endy=maxval( a(ringidx(1:nringidx))%y+vdwmulti*vdwr(a(ringidx(1:nringidx))%index) )
		endz=maxval( a(ringidx(1:nringidx))%z+vdwmulti*vdwr(a(ringidx(1:nringidx))%index) )
		orgx=minval( a(ringidx(1:nringidx))%x-vdwmulti*vdwr(a(ringidx(1:nringidx))%index) )
		orgy=minval( a(ringidx(1:nringidx))%y-vdwmulti*vdwr(a(ringidx(1:nringidx))%index) )
		orgz=minval( a(ringidx(1:nringidx))%z-vdwmulti*vdwr(a(ringidx(1:nringidx))%index) )
		dvol=grdspc**3
		write(*,"('Spatial range of grid data:')")
		write(*,"('X is from',f10.4,'  to',f10.4,' Bohr')") orgx,endx
		write(*,"('Y is from',f10.4,'  to',f10.4,' Bohr')") orgy,endy
		write(*,"('Z is from',f10.4,'  to',f10.4,' Bohr')") orgz,endz
		write(*,"('Differential element:',f12.6,' Bohr**3')") dvol
		xlength=endx-orgx
		ylength=endy-orgy
		zlength=endz-orgz
		dx=grdspc;gridvec1=0;gridvec1(1)=dx
		dy=grdspc;gridvec2=0;gridvec2(2)=dy
		dz=grdspc;gridvec3=0;gridvec3(3)=dz
		nx=nint(xlength/dx)+1
		ny=nint(ylength/dy)+1
		nz=nint(zlength/dz)+1
		write(*,"('Number of point in x,y,z:',3i6,'  Total:',i10)") nx,ny,nz,nx*ny*nz
		write(*,*)
		write(*,"(' Pi orbitals:')")
		write(*,"(15i5)") piorb(1:npiorb)
		if (allocated(cubmat)) deallocate(cubmat)
		allocate(cubmat(nx,ny,nz))
		MOocc_old=MOocc !Backup
		do imo=1,nmo
			if (all(piorb(1:npiorb)/=imo)) MOocc(imo)=0D0 !Set occupation number of all orbitals to zero except for pi orbitals
		end do
		call savecubmat(10,0,1) !Calculate LOL
		write(*,*)
		accum=0D0
		atm1x=a(ringidx(1))%x !Use 1,3,5 atoms in the ring to define the ring plane
		atm1y=a(ringidx(1))%y
		atm1z=a(ringidx(1))%z
		atm3x=a(ringidx(3))%x
		atm3y=a(ringidx(3))%y
		atm3z=a(ringidx(3))%z
		atm5x=a(ringidx(5))%x
		atm5y=a(ringidx(5))%y
		atm5z=a(ringidx(5))%z
		disple2crit=(disaway/b2a)**2
		discen2crit=(intradi/b2a)**2
		do iz=1,nz
			do iy=1,ny
				do ix=1,nx
					xtmp=orgx+(ix-1)*dx
					ytmp=orgy+(iy-1)*dy
					ztmp=orgz+(iz-1)*dz
					valtmp=cubmat(ix,iy,iz)
					call pointprjple(atm1x,atm1y,atm1z,atm3x,atm3y,atm3z,atm5x,atm5y,atm5z,xtmp,ytmp,ztmp,xprj,yprj,zprj) !project grid point to the plane defined by atom 1,3,5
					if (valtmp>LOLiso) then
						disple2=(xtmp-xprj)**2+(ytmp-yprj)**2+(ztmp-zprj)**2 !The vertical distance**2 to the plane
						discen2=(xtmp-cenx)**2+(ytmp-ceny)**2+(ztmp-cenz)**2 !The distance**2 to ring center
						if (disple2>disple2crit.and.discen2<discen2crit) accum=accum+valtmp
					end if
				end do
			end do
		end do
! 		call drawisosurgui(1) !If you want to visualize isosurface, just comment out this line
		write(*,"(' LOLIPOP value is',f12.6)") accum*dvol
		MOocc=MOocc_old !Recover occupation number
	end if
	write(*,*)
end do
end subroutine


!!---- Yoshizawa's electron transport route analysis
!!Based on Eqs. 2 and 3 of Account of chemical research, 45, 1612
!Use Gaussian output file as input, should specify pop=(full,nboread). Only closed-shell is supported
subroutine Yoshieletrans
use defvar
use util
use GUI
implicit real*8 (a-h,o-z)
character c80tmp*80,selectyn,c200tmp*200
real*8,allocatable :: NAOMO(:,:),mat(:,:),vallist(:)
integer,allocatable :: idx1(:),idx2(:) !Used to sort capacity
integer,allocatable :: atompi(:) !The ith element is the index of expected pi-AO of atom i
integer,allocatable :: piatm2atm(:) !Convert the index of the atom having pi-AO to absolute atom index
real*8 :: outcritval=0.01D0,outcritdistlow=0D0,outcritdisthigh=9999D0

!Load basic information about present system
if (ifiletype==0) then
    open(10,file=filename,status="old")
    call loclabel(10,"NBsUse=",ifound) !NbsUse must equal to the number of MOs
    if (ifound==0) then
	    write(*,"(a)") " Error: The input file you used does not meet requirement! Please carefully check Section 3.100.18 of the manual!"
	    write(*,*) "Press ENTER to return"
	    read(*,*)
	    return
    end if
    read(10,*) c80tmp,nmo
    call loclabel(10,"NAtoms=",ifound)
    read(10,*) c80tmp,ncenter
    call loclabel(10,"alpha electrons",ifound)
    read(10,*) naelec,c80tmp,c80tmp,nbelec
    if (naelec/=nbelec) then
	    write(*,*) "Error: Only closed-shell wavefunction is supported!"
	    write(*,*)
	    return
    end if
    !Load geometry
    allocate(a(ncenter))
    call loclabel(10,"Standard orientation:",ifound)
    if (ifound==0) then
	    write(*,*) "Error: Cannot found ""Standard orientation"" section!"
	    write(*,*) "Press ENTER button to exit"
        read(*,*)
	    return
    end if
    read(10,*)
    read(10,*)
    read(10,*)
    read(10,*)
    read(10,*)
    do iatm=1,ncenter !Assume that the coordinate is Angstrom
	    read(10,*) inouse,a(iatm)%index,inouse,a(iatm)%x,a(iatm)%y,a(iatm)%z
	    a(iatm)%name=ind2name(a(iatm)%index)
    end do
    a%x=a%x/b2a
    a%y=a%y/b2a
    a%z=a%z/b2a
    if (allocated(MOene)) deallocate(MOene)
    allocate(MOene(nmo))
    iHOMO=nint(naelec)
    iLUMO=iHOMO+1
    write(*,"(' Number of atoms:',i8)") ncenter
    write(*,"(' Total number of MOs:',i8)") nmo
    write(*,"(' Number of occupied MOs:',i8)") nint(naelec)
    write(*,"(' HOMO is orbital',i7,'           LUMO is orbital',i7)") iHOMO,iLUMO
    call loclabel(10,"occ. eigenvalues",ifound)
    do itime=1,ceiling(iHOMO/5D0) !Load occupied orbital energies
	    read(10,"(a)") c80tmp
	    ilow=(itime-1)*5+1
	    ihigh=itime*5
	    if (ihigh>=iHOMO) ihigh=iHOMO
	    read(c80tmp(29:),*) MOene(ilow:ihigh)
    end do
    do itime=1,ceiling((nmo-iHOMO)/5D0) !Load unoccupied orbital energies
	    read(10,"(a)") c80tmp
	    ilow=(itime-1)*5+1+iHOMO
	    ihigh=itime*5+iHOMO
	    if (ihigh>=nmo) ihigh=nmo
	    read(c80tmp(29:),*) MOene(ilow:ihigh)
    end do
    ! write(*,*) "Energies of occupied MOs (a.u.):"
    ! write(*,"(7f11.5)") MOene(:iHOMO)
    ! write(*,*) "Energies of unoccupied MOs (a.u.):"
    ! write(*,"(7f11.5)") MOene(iLUMO:)
else
    if (wfntype/=0) then
	    write(*,*) "Error: Only closed-shell wavefunction is supported!"
	    write(*,*) "Press ENTER button to exit"
        read(*,*)
	    return
    end if
    write(*,"(a)") " Input file name containing information outputted by NBO program with NAOMO keyword, e.g. C:\test.out"
    do while(.true.)
	    read(*,"(a)") c200tmp
	    inquire(file=c200tmp,exist=alive)
	    if (alive) exit
	    write(*,*) "Cannot find the file, input again"
    end do
    open(10,file=c200tmp,status="old")
end if

write(*,*)
write(*,*) "The molecule is in which plane?  1=XY  2=YZ  3=XZ"
read(*,*) iplesel

!Load information outputted by NBO program
allocate(atompi(ncenter))
atompi=0 !0 means the atom does not have corresponding pi orbital
call loclabel(10,"NATURAL POPULATIONS",ifound,1)
read(10,*)
read(10,*)
read(10,*)
read(10,*)
numNAO=0
do iatm=1,ncenter
	do while(.true.)
		read(10,"(a)") c80tmp
		if (c80tmp==" ") exit
		numNAO=numNAO+1
		if (iplesel==1.and.index(c80tmp,"Val")/=0.and.index(c80tmp,"pz")/=0) atompi(iatm)=numNAO
		if (iplesel==2.and.index(c80tmp,"Val")/=0.and.index(c80tmp,"px")/=0) atompi(iatm)=numNAO
		if (iplesel==3.and.index(c80tmp,"Val")/=0.and.index(c80tmp,"py")/=0) atompi(iatm)=numNAO
	end do
end do

write(*,"(' Number of natural atomic orbitals:',i8,/)") numNAO
numpiatom=count(atompi/=0)
write(*,"(' The number of atoms with expected pi atomic orbital:',i6)") numpiatom
numpair=numpiatom*(numpiatom-1)/2 !Number of pi-atom pairs
allocate(mat(numpiatom,numpiatom),piatm2atm(numpiatom))
itmp=0
do iatm=1,ncenter
	if (atompi(iatm)/=0) then
		write(*,"(' Atom:',i6,'       pi-NAO:',i6)") iatm,atompi(iatm)
		itmp=itmp+1
		piatm2atm(itmp)=iatm
	end if
end do

write(*,*) "Loading NAOMO matrix..."
allocate(NAOMO(numNAO,nmo))
call loclabel(10,"MOs in the NAO basis:",ifound,0) !Don't rewind
if (ifound==0) then
	write(*,*) "Error: Cannot found ""MOs in NAO basis"" section!"
	write(*,*) "Press ENTER button to exit"
    read(*,*)
	return
end if
!Check format before reading, NBO6 use different format to NBO3
read(10,"(a)") c80tmp
backspace(10)
if (c80tmp(2:2)==" ") then !NBO6
	call readmatgau(10,NAOMO,0,"f8.4 ",17,8,3)
else !NBO3
	call readmatgau(10,NAOMO,0,"f8.4 ",16,8,3)
end if

close(10)

iHOMO=nint(naelec)
iLUMO=iHOMO+1
Fermiene=(MOene(iHOMO)+MOene(iLUMO))/2D0
iorblow=1
iorbhigh=nmo
do while (.true.)
	write(*,*)
	write(*,*) "        ======= Yoshizawa's electron transport route analysis ======="
	write(*,*) "-10 Return"
	write(*,"(a,f10.4,a,f10.4,a)") " -4 Set distance criterion, current: From",outcritdistlow," to",outcritdisthigh," Angstrom"
	write(*,"(a,f12.8)") " -3 Set value criterion, current:",outcritval
	write(*,"(a,f12.6)") " -2 Set Fermi energy level, current (a.u.):",Fermiene
	write(*,"(a,i6,a,i6)") " -1 Select the range of MOs to be considered, current: from",iorblow,' to',iorbhigh
	write(*,*) "0 View molecular structure"
	write(*,*) "1 Output detail of electron transport probability between two atoms"
	write(*,*) "2 Output and rank all electron transport routes in the system"
	write(*,*) "3 Output and rank all electron transport routes for an atom"
	read(*,*) isel

	if (isel==-10) then
		return
	else if (isel==0) then
		call drawmolgui
	else if (isel==-1) then
		write(*,"(' Note: HOMO is orbital',i7,', LUMO is orbital',i7)") iHOMO,iLUMO
		write(*,*) "Input orbital range, e.g. 3,55 means all pi orbitals from orbital 3 to 55"
		write(*,*) "If input 0,0, then all MOs will be taken into account"
		read(*,*) iorblow,iorbhigh
		if (iorblow==0.and.iorbhigh==0) then
			iorblow=1
			iorbhigh=nmo
		else
			if (iorbhigh>nmo) iorbhigh=nmo
			if (iorblow<1) iorblow=1
		end if
	else if (isel==-2) then
		write(*,"(' Note: Energy of HOMO and LUMO is',2f12.6,' a.u., respectively')") MOene(iHOMO),MOene(iLUMO)
		write(*,*) "Input Fermi energy level in a.u., e.g. -0.005"
		read(*,*) Fermiene
	else if (isel==-3) then
		write(*,*) "Input the value criterion, e.g. 0.02"
		write(*,*) "Note: The routes whose value is smaller than this value will not be shown"
		read(*,*) outcritval
	else if (isel==-4) then
		write(*,*) "Input lower and upper limits of distance criterion (in Angstrom), e.g. 1.5,3.2"
		write(*,*) "Note: Only the routes within this range are possible to be shown"
		read(*,*) outcritdistlow,outcritdisthigh
	else if (isel==1) then
		write(*,*) "Input two atoms, e.g. 3,5"
		read(*,*) iatm,jatm
		transtot=0
		write(*,"(a,f10.6,a)") " Note: The MOs having contribution <",outcritval," will not be shown"
		write(*,*) "Note: HOMO is marked by asterisk"
		do iorb=iorblow,iorbhigh
			transtmp=NAOMO(atompi(iatm),iorb)*NAOMO(atompi(jatm),iorb)/(Fermiene-MOene(iorb))
			transtot=transtot+transtmp
			if (iorb==iHOMO) write(*,"('* MO:',i6,'   Energy(a.u.):',f12.6,'   Contribution:',f12.6)") iorb,MOene(iorb),transtmp
			if (iorb/=iHOMO.and.abs(transtmp)>=outcritval) write(*,"('  MO:',i6,'   Energy(a.u.):',f12.6,'   Contribution:',f12.6)") iorb,MOene(iorb),transtmp
! 			write(*,"(3f12.6,/)") NAOMO(atompi(iatm),iorb),NAOMO(atompi(jatm),iorb),Fermiene-MOene(iorb)
		end do
		write(*,"(' Total value is',f12.6)") transtot
		if (iorblow/=iHOMO.or.iorbhigh/=iLUMO) then
			contriHOMO=NAOMO(atompi(iatm),iHOMO)*NAOMO(atompi(jatm),iHOMO)/(Fermiene-MOene(iHOMO))
			contriLUMO=NAOMO(atompi(iatm),iLUMO)*NAOMO(atompi(jatm),iLUMO)/(Fermiene-MOene(iLUMO))
			write(*,"(/,' If only consider HOMO and LUMO, the value is',f12.6)") contriHOMO+contriLUMO
		end if
		write(*,"(' Distance of the route is',f12.6,' Angstrom')") dsqrt( (a(iatm)%x-a(jatm)%x)**2+(a(iatm)%y-a(jatm)%y)**2+(a(iatm)%z-a(jatm)%z)**2 )*b2a
	else if (isel==2) then
		allocate(vallist(numpair),idx1(numpair),idx2(numpair))
		mat=0D0
		itmp=0
		do iatm=1,numpiatom !The index of pi-atom, must convert to actual atom index by piatm2atm
			do jatm=iatm+1,numpiatom
				itmp=itmp+1
				tmpval=0D0
				do iorb=iorblow,iorbhigh
					tmpval=tmpval+NAOMO(atompi(piatm2atm(iatm)),iorb)*NAOMO(atompi(piatm2atm(jatm)),iorb)/(Fermiene-MOene(iorb))
				end do
				mat(iatm,jatm)=tmpval
				vallist(itmp)=tmpval
				idx1(itmp)=iatm
				idx2(itmp)=jatm
			end do
		end do
		mat=mat+transpose(mat)
		!Sort
		do i=1,numpair
			do j=i+1,numpair
				if ( abs(vallist(i))<abs(vallist(j)) ) then
					temp=vallist(i)
					vallist(i)=vallist(j)
					vallist(j)=temp
					itemp=idx1(i)
					idx1(i)=idx1(j)
					idx1(j)=itemp
					itemp=idx2(i)
					idx2(i)=idx2(j)
					idx2(j)=itemp
				end if
			end do
		end do
		!Output ranked values
		write(*,*) "Electron transport route, ranked by transmission probability"
		write(*,"(' Note: The routes whose absolute value <',f10.6,' will not be shown')") outcritval
		write(*,"(' Note: The routes whose distance <',f10.4,' or >',f10.4,' Angstrom will not be shown')") outcritdistlow,outcritdisthigh
		do ipair=1,numpair
			if (abs(vallist(ipair))<outcritval) exit
			iatm=piatm2atm(idx1(ipair))
			jatm=piatm2atm(idx2(ipair))
			dist=dsqrt( (a(iatm)%x-a(jatm)%x)**2+(a(iatm)%y-a(jatm)%y)**2+(a(iatm)%z-a(jatm)%z)**2 )*b2a
			if (dist>=outcritdistlow.and.dist<=outcritdisthigh) write(*,"(' Atom',i5,' -- Atom',i5,'  Value and distance:',2f12.6)") iatm,jatm,vallist(ipair),dist
		end do
		write(*,*) "Note: The units of the distances are Angstrom"
		write(*,*)
		write(*,*) "If output above data to result.txt in current folder? (y/n)"
		read(*,*) selectyn
		if (selectyn=='y'.or.selectyn=='Y') then
			open(10,file="result.txt",status="replace")
			do ipair=1,numpair
				if (abs(vallist(ipair))<outcritval) exit
				iatm=piatm2atm(idx1(ipair))
				jatm=piatm2atm(idx2(ipair))
				dist=dsqrt( (a(iatm)%x-a(jatm)%x)**2+(a(iatm)%y-a(jatm)%y)**2+(a(iatm)%z-a(jatm)%z)**2 )*b2a
				if (dist>=outcritdistlow.and.dist<=outcritdisthigh) write(10,"(2i6,2f12.6)") iatm,jatm,vallist(ipair),dist
			end do
			write(*,"(a)") " Done, the data has been outputted to result.txt in current folder. The first two columns correspond &
			to atom indices, the third one corresponds to value, the fourth one is route distance (in Angstrom)."
			close(10)
		end if
		!Output matrix
		write(*,*)
		write(*,*) "If export electron transport probability matrix to current folder? (y/n)"
		read(*,*) selectyn
		if (selectyn=='y'.or.selectyn=='Y') then
			open(10,file="transcapamat.txt",status="replace")
			call showmatgau(mat,"Electron transport probability matrix",0,"f14.8",10)
			close(10)
			write(*,*) "Done, the matrix has been outputted to transcapamat.txt in current folder"
			write(*,*) "Conversion between element in the matrix and actual atom index:"
			do iatm=1,numpiatom
				write(*,"(' Element',i6,'    -->   Atom',i6)") iatm,piatm2atm(iatm)
			end do
		end if
		deallocate(vallist,idx1,idx2)
	else if (isel==3) then
		write(*,*) "Input atom index, e.g. 5"
		read(*,*) iatm
		if (atompi(iatm)==0) then
			write(*,*) "Error: The atom does not have expected pi atomic orbital!"
			cycle
		end if
		allocate(vallist(numpiatom),idx1(numpiatom))
		do jatm=1,numpiatom !Calculate capacity to all other atoms
			tmpval=0D0
			do iorb=iorblow,iorbhigh
				tmpval=tmpval+NAOMO(atompi(piatm2atm(iatm)),iorb)*NAOMO(atompi(piatm2atm(jatm)),iorb)/(Fermiene-MOene(iorb))
			end do
			vallist(jatm)=tmpval
			idx1(jatm)=jatm
		end do
		vallist(iatm)=0D0 !The two sites are identical and meaningless
		!Sort
		do i=1,numpiatom
			do j=i+1,numpiatom
				if ( abs(vallist(i))<abs(vallist(j)) ) then
					temp=vallist(i)
					vallist(i)=vallist(j)
					vallist(j)=temp
					itemp=idx1(i)
					idx1(i)=idx1(j)
					idx1(j)=itemp
				end if
			end do
		end do
		write(*,*) "Electron transport route, ranked by transmission probability"
		write(*,"(' Note: The routes whose absolute value <',f10.6,' will not be shown')") outcritval
		write(*,"(' Note: The routes whose distance <',f10.4,' or >',f10.4,' Angstrom will not be shown')") outcritdistlow,outcritdisthigh
		do ipair=1,numpiatom
			jatm=piatm2atm(idx1(ipair))
			if (jatm==iatm) cycle
			if (abs(vallist(ipair))<outcritval) exit
			dist=dsqrt( (a(iatm)%x-a(jatm)%x)**2+(a(iatm)%y-a(jatm)%y)**2+(a(iatm)%z-a(jatm)%z)**2 )*b2a
			if (dist>=outcritdistlow.and.dist<=outcritdisthigh) write(*,"(' To atom',i6,'    Value and distance (Angstrom):',2f12.6)") jatm,vallist(ipair),dist
		end do
		deallocate(vallist,idx1)
	end if
end do
end subroutine


!!----------- Detect pi orbital and set occupation number
subroutine detectpiorb
use defvar
use function
use util
implicit real*8 (a-h,o-z)
integer piorblist(nmo) !1 means this orbital is expected pi orbital
real*8,allocatable :: tmparr(:)
real*8 :: thresdens=0.03D0,thressingle=0.85D0
integer :: ionlyocc=1,idebug=0
integer,allocatable :: atmrange(:)
character c2000tmp*2000,c200tmp*200
real*8 CObasa_LMO(nbasis,nbasis),CObasb_LMO(nbasis,nbasis)

if (.not.allocated(b)) then
	write(*,*) "Error: wavefunction information is not presented but needed!"
	write(*,*) "Press ENTER button to return"
	read(*,*)
	return
end if
write(*,*) "Choose current situation:"
write(*,*) "-1: Orbitals are in localized form (e.g. LMO, NBO)"
write(*,"(a)") "  0: Orbitals are in delocalized form (e.g. MO, natural orbital, NTO). The system must be exactly planar"
read(*,*) iorbform

piorblist=0
pinelec=0D0
if (iorbform==0) then !Delocalized case
    thres=0.05D0
    avgx=sum(a(:)%x)/ncenter
    avgy=sum(a(:)%y)/ncenter
    avgz=sum(a(:)%z)/ncenter
	if ( all(abs(a(:)%x-avgx)<thres) ) then
		iplane=2
		write(*,*) "This system is expected to be in YZ plane"
	else if ( all(abs(a(:)%y-avgy)<thres) ) then
		iplane=3
		write(*,*) "This system is expected to be in XZ plane"
	else if ( all(abs(a(:)%z-avgz)<thres) ) then
		iplane=1
		write(*,*) "This system is expected to be in XY plane"
	else
		write(*,"(a)") " Error: Unable to detect the system plane! To use this mode, all atoms must be in XY or XZ or YZ plane!"
		write(*,*) "Press ENTER button to return"
		read(*,*)
		return
	end if

	write(*,*) "Expected pi orbitals, occupation numbers and orbital energies (eV):"
	do imo=1,nmo
		do iprim=1,nprims
			GTFtype=b(iprim)%type
			if (iplane==1) then
				if ( (GTFtype==1.or.GTFtype==2.or.GTFtype==3).and.abs(co(imo,iprim))>0.001D0 ) exit !Orbital has S,X,Y component, so this is not pi-Z
			else if (iplane==2) then
				if ( (GTFtype==1.or.GTFtype==3.or.GTFtype==4).and.abs(co(imo,iprim))>0.001D0 ) exit !Orbital has S,Y,Z component, so this is not pi-X
			else if (iplane==3) then
				if ( (GTFtype==1.or.GTFtype==2.or.GTFtype==4).and.abs(co(imo,iprim))>0.001D0 ) exit !Orbital has S,X,Z component, so this is not pi-Y
			end if
			if (iprim==nprims) then
				piorblist(imo)=1
				pinelec=pinelec+MOocc(imo)
				write(*,"(i6,2f14.6)") imo,MOocc(imo),MOene(imo)*au2ev
			end if
		end do
	end do
    isel=0
        
else if (iorbform==-1) then !LMO case
	if (.not.allocated(CObasa)) then
		write(*,"(a)") " Error: Basis function information is not presented but needed! See Section 2.5 of Multiwfn manual for detail"
		write(*,*) "Press ENTER button to return"
		read(*,*)
		return
	end if
	do while(.true.)
		write(*,*)
        write(*,"(a)") " -1 Detect pi orbitals and then evaluate pi composition for orbitals in another wavefunction file"
		write(*,*) "0 Detect pi orbitals and then set occupation numbers"
		write(*,"(a,f7.1,' %')") " 1 Set threshold for identifying single-center orbitals, current:",thressingle*100
		write(*,"(a,f8.4,' a.u.')") " 2 Set density threshold for identifying pi orbitals, current:",thresdens
		if (ionlyocc==1) write(*,*) "3 Switch the orbitals in consideration, current: occupied orbitals"
		if (ionlyocc==0) write(*,*) "3 Switch the orbitals in consideration, current: all orbitals"
		if (idebug==1) write(*,*) "4 Switch outputting debug information, current: Yes"
		if (idebug==0) write(*,*) "4 Switch outputting debug information, current: No"
		if (.not.allocated(atmrange)) write(*,*) "5 Set constraint of atom range, current: undefined"
		if (allocated(atmrange)) write(*,"(a,i6,a)") " 5 Set constraint of atom range, current: ",natmrange," atoms"
		read(*,*) isel
		if (isel==0.or.isel==-1) then
			exit
		else if (isel==1) then
			write(*,*) "Input the threshold composition, e.g. 0.8"
			write(*,"(a)") " Note: If you input for example 0.8, then in an orbital, if an atom has contribution larger than 80%, &
			then this orbital will be regarded as single-center orbital and will not be taken into account further"
			read(*,*) thressingle
		else if (isel==2) then
			write(*,*) "Input the threshold density in a.u., e.g. 0.02"
			write(*,"(a)") " Note: Assume that in an orbital, A and B are the two atoms having maximum contributions, &
			the orbital will be regarded as pi orbital if electron densities at two representative points between A and B &
            are both smaller than the threshold density"
			read(*,*) thresdens
		else if (isel==3) then
			if (ionlyocc==1) then
				ionlyocc=0
			else if (ionlyocc==0) then
				ionlyocc=1
			end if
		else if (isel==4) then
			if (idebug==1) then
				idebug=0
			else if (idebug==0) then
				idebug=1
			end if
		else if (isel==5) then
			if (allocated(atmrange)) deallocate(atmrange)
			write(*,"(a)") " Input index range of the atoms, e.g. 2,3,7-10. &
			Only LMOs with two largest contributing atoms belonging to this index range will be considered"
			read(*,"(a)") c2000tmp
			call str2arr(c2000tmp,natmrange)
			allocate(atmrange(natmrange))
			call str2arr(c2000tmp,natmrange,atmrange)
		end if
	end do
    
    write(*,*)
	write(*,*) "Expected pi orbitals, occupation numbers and orbital energies (eV):"
	allocate(tmparr(ncenter))
	do imo=1,nmo
		if (ionlyocc==1.and.MOocc(imo)==0) cycle
		!Compute atomic composition via Mulliken method (better than SCPA for LMOs, in particular for lone pair type of LMO, the SCPA shows it is too delocalized)
		call orbatmcomp(1,imo,tmparr) !Record composition of atoms in current orbital derived by Mulliken method
		!Find atom with maximum contribution (imax) and that with second maximum contribution (imax2)
		imax=maxloc(tmparr,1)
		tmpmax=-1D99
		do iatm=1,ncenter
			if (iatm==imax) cycle
			if (tmparr(iatm)>tmpmax) then
				imax2=iatm
				tmpmax=tmparr(iatm)
			end if
		end do
		MOocc=0
		MOocc(imo)=2D0
		!Here we use two points rather than solely using midpoint to detect rho, because the latter will regard sigma* type of virtual LMOs as pi orbitals
		tmpx=0.7D0*a(imax)%x+0.3D0*a(imax2)%x
		tmpy=0.7D0*a(imax)%y+0.3D0*a(imax2)%y
		tmpz=0.7D0*a(imax)%z+0.3D0*a(imax2)%z
		dens1=fdens(tmpx,tmpy,tmpz)
		tmpx=0.3D0*a(imax)%x+0.7D0*a(imax2)%x
		tmpy=0.3D0*a(imax)%y+0.7D0*a(imax2)%y
		tmpz=0.3D0*a(imax)%z+0.7D0*a(imax2)%z
		dens2=fdens(tmpx,tmpy,tmpz)
		MOocc=MOocc_org
		if (idebug==1) write(*,"(' Orb:',i5,'  max:',i5,f6.1,'%  max2:',i5,f6.1,'%  rho1:',f9.5,'  rho2:',f9.5)") &
        imo,imax,tmparr(imax)*100,imax2,tmparr(imax2)*100,dens1,dens2
		if (tmparr(imax)>thressingle) cycle !Pass single center LMO (Lone pair, inner-core)
		if (allocated(atmrange)) then
			if (all(atmrange/=imax).or.all(atmrange/=imax2)) cycle
		end if
		if (dens1<thresdens.and.dens2<thresdens) then
			piorblist(imo)=1
			pinelec=pinelec+MOocc(imo)
			write(*,"(i6,2f14.6)") imo,MOocc(imo),MOene(imo)*au2ev
		end if
	end do
	deallocate(tmparr)
end if

npiorb=count(piorblist==1)
if (npiorb==0) then
	write(*,*) "No pi orbital was found!"
	return
end if
write(*,"(' Total number of pi orbitals:',i6)") npiorb
write(*,"(' Total number of electrons in pi orbitals:',f12.6)") pinelec
if (iorbform==0.and.imodwfn==0) then !Only for MOs, one can safely separate inner and valence orbitals
	call getninnerele(ninnerele,0)
	ndelelec=ninnerele/2
	write(*,"(' Total number of inner electrons:',i6)") ninnerele
end if
write(*,*)

if (isel==0) then !Set occupation number
    write(*,*) "How to deal with these orbitals?"
    write(*,*) "0 Do nothing"
    write(*,*) "1 Set occupation number of these pi orbitals to zero"
    write(*,*) "2 Set occupation number of all other orbitals to zero"
    if (iorbform==0.and.imodwfn==0) then
	    write(*,*) "3 Set occupation number of valence pi orbitals to zero"
	    write(*,*) "4 Set occupation number of all except for valence pi orbitals to zero"
    end if
    read(*,*) isel

    if (isel/=0) then
	    if (isel==1) then
		    where (piorblist==1) MOocc=0
	    else if (isel==2) then
		    where (piorblist==0) MOocc=0
	    else if (isel==3.or.isel==4) then
		    if (wfntype==1.or.wfntype==4) then !UHF and U-post-HF wfn
			    do isplit=1,nmo !Where the first beta orbital appear now
				    if (motype(isplit)==2) exit
			    end do
			    do imo=1,nmo
				    if (isel==3) then
					    if (piorblist(imo)==1.and.imo<=isplit.and.imo>ndelelec) MOocc(imo)=0 !alpha part
					    if (piorblist(imo)==1.and.imo>isplit.and.imo>(isplit+ndelelec)) MOocc(imo)=0 !beta part
				    else if (isel==4) then
					    if (imo<=isplit) then !alpha part
						    if (piorblist(imo)==0.or.imo<=ndelelec) MOocc(imo)=0
					    else !beta part
						    if (piorblist(imo)==0.or.imo<=(isplit+ndelelec)) MOocc(imo)=0
					    end if
				    end if
			    end do
		    else if (wfntype==0.or.wfntype==2.or.wfntype==3) then !Restricted(=0) or RO(=2) or post-R(=3) wavefunction	
			    do imo=1,nmo
				    if (isel==3.and.piorblist(imo)==1.and.imo>ndelelec) MOocc(imo)=0
				    if (isel==4.and.(piorblist(imo)==0.or.imo<=ndelelec)) MOocc(imo)=0
			    end do
		    end if
	    end if
	    imodwfn=1
	    call updatenelec
	    write(*,*) "Done!"
	    if (allocated(CObasa)) then
		    write(*,*) "Updating density matrix..."
		    call genP
		    write(*,*) "Density matrix has been updated"
	    end if
    end if
    write(*,*)

else if (isel==-1) then !Calculate pi composition for orbitals in another file
    CObasa_LMO=CObasa
    if (allocated(CObasb)) CObasb_LMO=CObasb
    call dealloall
    write(*,*) "Input a file containing other set of orbitals, e.g. C:\riko.fch"
    do while(.true.)
	    read(*,"(a)") c200tmp
	    inquire(file=c200tmp,exist=alive)
	    if (alive) exit
	    write(*,*) "Cannot find the file, input again!"
    end do
    call readinfile(c200tmp,1)
    write(*,*) "Input threshold for printing (%), e.g. 85"
    write(*,*) "If press ENTER directly, 50% will be employed as the threshold"
    read(*,"(a)") c200tmp
    if (c200tmp==" ") then
        thres=0.5D0
    else
        read(c200tmp,*) thres
        thres=thres/100
    end if
    if (wfntype==0.or.wfntype==2.or.wfntype==3) then !Restricted orbitals
        do iorb=1,nmo
            if (ionlyocc==1.and.MOocc(iorb)==0) cycle !Only consider occupied orbitals
            picomp=0
            do ilmo=1,nmo
                if (piorblist(ilmo)==0) cycle !LMO is not pi
                coeff=sum(matmul(transpose(CObasa_lmo(:,ilmo:ilmo)),matmul(Sbas,CObasa(:,iorb:iorb))))
                picomp=picomp+coeff**2
            end do
            if (picomp>thres) write(*,"(' Orbital',i6,' (Occ=',f8.5')   pi composition:',f8.3,'%')") iorb,MOocc(iorb),picomp*100
        end do
    else if (wfntype==1.or.wfntype==4) then !Unrestricted orbitals
        write(*,*) "===== Alpha part ====="
        do iorb=1,nmo/2
            if (ionlyocc==1.and.MOocc(iorb)==0) cycle !Only consider occupied orbitals
            picomp=0
            do ilmo=1,nmo/2
                if (piorblist(ilmo)==0) cycle !LMO is not pi
                coeff=sum(matmul(transpose(CObasa_lmo(:,ilmo:ilmo)),matmul(Sbas,CObasa(:,iorb:iorb))))
                picomp=picomp+coeff**2
            end do
            if (picomp>thres) write(*,"(' Orbital',i6,' (Occ=',f8.5')   pi composition:',f8.3,'%')") iorb,MOocc(iorb),picomp*100
        end do
        write(*,*)
        write(*,*) "===== Beta part ====="
        do iorb=1,nmo/2
            if (ionlyocc==1.and.MOocc(iorb+nbasis)==0) cycle !Only consider occupied orbitals
            picomp=0
            do ilmo=1,nmo/2
                if (piorblist(ilmo+nbasis)==0) cycle !LMO is not pi
                coeff=sum(matmul(transpose(CObasb_lmo(:,ilmo:ilmo)),matmul(Sbas,CObasb(:,iorb:iorb))))
                picomp=picomp+coeff**2
            end do
            if (picomp>thres) write(*,"(' Orbital',i6,' (Occ=',f8.5')   pi composition:',f8.3,'%')") iorb,MOocc(iorb),picomp*100
        end do
    
    end if
    write(*,*)
    write(*,*) "Note: Current wavefunction corresponds to the file just loaded"
end if
end subroutine






!!--------- A utility used to generate Bq coordinate for calculating NICS_ZZ, and obtain NICS_ZZ for non-planar system
subroutine utilNICS_ZZ
use util
use defvar
implicit real*8 (a-h,o-z)
character c1000*1000,c200*200
real*8 hesstmp(3,3)
write(*,*) "Input center coordinate of the ring (in Angstrom), e.g. 2.0,2.4,1.1"
write(*,*) "(If use Bohr as unit, the first letter should be ""b"", e.g. b3.0,3.8,2.2"
read(*,"(a)") c1000
if (c1000(1:1)=='b') then
	read(c1000(2:),*) tmpx,tmpy,tmpz
else
	read(c1000,*) tmpx,tmpy,tmpz
	tmpx=tmpx/b2a
	tmpy=tmpy/b2a
	tmpz=tmpz/b2a
end if
write(*,*) "Input indices of three atoms to define a plane, e.g. 3,4,9"
read(*,*) iatm1,iatm2,iatm3
call pointABCD(a(iatm1)%x,a(iatm1)%y,a(iatm1)%z,a(iatm2)%x,a(iatm2)%y,a(iatm2)%z,a(iatm3)%x,a(iatm3)%y,a(iatm3)%z,xnor,ynor,znor,rnouse) !Normal vector is (xnor,ynor,znor)
facnorm=sqrt(xnor**2+ynor**2+znor**2)
xnor=xnor/facnorm !Normalize normal vector, then (xnor,ynor,znor) is the unit vector normal to the plane defined by iatm1,iatm2,iatm3
ynor=ynor/facnorm
znor=znor/facnorm
write(*,"(' The unit normal vector is',3f14.8)") xnor,ynor,znor
write(*,"(a)") " The X,Y,Z coordinate of the points below and above 1 Angstrom of the plane from the point you defined, respectively:"
write(*,"(3f16.10,' Angstrom')") (tmpx-xnor/b2a)*b2a,(tmpy-ynor/b2a)*b2a,(tmpz-znor/b2a)*b2a
write(*,"(3f16.10,' Angstrom')") (tmpx+xnor/b2a)*b2a,(tmpy+ynor/b2a)*b2a,(tmpz+znor/b2a)*b2a
write(*,*)
write(*,"(a)") " Now input magnetic shielding tensor outputted by your ab-initio program, then Multiwfn will calculate the shielding value in the direction perpendicular to the plane. &
You can also input ""q"" to return"
! If you are a Gaussian user, you can also directly copy NMR shielding tensor (such as below) from Gaussian output file to present window
! (Note that this is not always a symmetric matrix)
!    XX=     6.2246   YX=   -54.8190   ZX=    94.7322
!    XY=    -3.1233   YY=    51.8304   ZY=     4.0216
!    XZ=    96.6247   YZ=    38.6474   ZZ=    94.4768
write(*,*) "Input XX,YX,ZX component of the tensor, e.g. 12.5150,-2.3289,3.7151"
read(*,"(a)") c1000
if (index(c1000,'q')/=0) then
	return
else if (index(c1000,"XX")==0) then
	read(c1000,*) hesstmp(1,1),hesstmp(1,2),hesstmp(1,3)
	write(*,*) "Input XY,YY,ZY of the tensor, e.g. -2.3289,10.0169,-2.2232"
	read(*,*) hesstmp(2,1),hesstmp(2,2),hesstmp(2,3)
	write(*,*) "Input XZ,YZ,ZZ of the tensor, e.g. 3.7151,-2.2232,12.1699"
	read(*,*) hesstmp(3,1),hesstmp(3,2),hesstmp(3,3)
else
	read(c1000,*) c200,hesstmp(1,1),c200,hesstmp(1,2),c200,hesstmp(1,3)
	read(*,"(a)") c1000	
	read(c1000,*) c200,hesstmp(2,1),c200,hesstmp(2,2),c200,hesstmp(2,3)
	read(*,"(a)") c1000			
	read(c1000,*) c200,hesstmp(3,1),c200,hesstmp(3,2),c200,hesstmp(3,3)
end if
write(*,*) "The magnetic shielding tensor you inputted is:"
write(*,"(' XX=',f12.6,'   YX=',f12.6,'   ZX=',f12.6)") hesstmp(1,:)
write(*,"(' XY=',f12.6,'   YY=',f12.6,'   ZY=',f12.6)") hesstmp(2,:)
write(*,"(' XZ=',f12.6,'   YZ=',f12.6,'   ZZ=',f12.6)") hesstmp(3,:)
shieldperpen=xnor*xnor*hesstmp(1,1)+xnor*ynor*hesstmp(1,2)+xnor*znor*hesstmp(1,3)+&
			 ynor*xnor*hesstmp(2,1)+ynor*ynor*hesstmp(2,2)+ynor*znor*hesstmp(2,3)+&
			 znor*xnor*hesstmp(3,1)+znor*ynor*hesstmp(3,2)+znor*znor*hesstmp(3,3)
write(*,"(' The shielding value normal to the plane is',f20.10)") shieldperpen
write(*,"(' The NICS_ZZ value is thus',f20.10,/)") -shieldperpen
end subroutine


!!------- A general routine used to fit atomic value from function value on vdW surface or on a set of given points
!Similar to routine "fitesp", but for universal purpose
subroutine fitfunc
use util
use defvar
use function
implicit real*8 (a-h,o-z)
character*200 addcenfile,extptfile
character selectyn,c80tmp
integer :: nlayer=4 !Number of fitting layers
real*8 :: funcfitvdwr(0:nelesupp)=-1D0,sclvdwlayer(100)=(/1.4D0,1.6D0,1.8D0,2.0D0,(2.2D0,i=5,100)/)
real*8,allocatable :: funcptval(:),funcptx(:),funcpty(:),funcptz(:),Bmat(:),Amat(:,:),Amatinv(:,:),atmval(:)
real*8,allocatable :: fitcenx(:),fitceny(:),fitcenz(:),fitcenvdwr(:),disptcen(:),origsphpt(:,:)
densperarea=6D0*b2a**2 !Point density per Angstrom**2 for MK, in order to convert to Bohr**2, multiply by b2a**2
iaddcen=0 !If give Additional center
iuseextpt=0 !If use external points
iskipfunccalc=0 !If read function value from external file directly rather than calculate here
iconstot=0

do while(.true.)
	write(*,*)
	write(*,*) "-2 Load additional fitting centers from external file"
	if (iuseextpt==0) write(*,*) "-1 Use fitting points recorded in external file instead of generating them"
	write(*,*) "0 Return"
	write(*,*) "1 Select a real space function and start calculation!!!"
	if (iuseextpt==0) then
		write(*,"(' 2 Set number of points per Angstrom^2, current:',f10.3)") densperarea/b2a**2 !Temporary convert to Angstrom**2 for convention
		write(*,"(' 3 Set number of layers per atom, current:',i4)") nlayer
		write(*,"(' 4 Set the scale factor of van der Waals radii in each layer')")
	end if
	if (iconstot==1) write(*,"(a,f14.7)") " 5 Set constraint for total value, current:",constotval
	if (iconstot==0) write(*,*) "5 Set constraint for total value, current: Not used"
	read(*,*) isel
	
	if (isel==-2) then
		iaddcen=1
		write(*,*) "Input the name of the file recording coordinates of additional fitting centers"
		read(*,"(a)") addcenfile
		write(*,*) "Done!"
	else if (isel==-1) then
		iuseextpt=1
		write(*,*) "Input the name of the file recording coordinates of fitting points"
		read(*,"(a)") extptfile
		write(*,*) "OK, the points recorded in this file will be used as fitting points"
	else if (isel==0) then
		Return
	else if (isel==1) then
		exit
	else if (isel==2) then
		write(*,*) "Input new value"
		read(*,*) densperarea
		densperarea=densperarea*b2a**2
	else if (isel==3) then
		write(*,*) "Input new value"
		read(*,*) nlayer
	else if (isel==4) then
		write(*,*) "Current values:"
		do ilayer=1,nlayer
			write(*,"(' Layer',i4,' :',f8.4)") ilayer,sclvdwlayer(ilayer)
		end do
		write(*,*)
		do ilayer=1,nlayer
			write(*,"(a,i4,',  e.g. 1.5')") " Input value for layer",ilayer
			read(*,*) sclvdwlayer(ilayer)
		end do
	else if (isel==5) then
		write(*,*) "Input a value, e.g. 3.0"
		write(*,*) "If input ""u"", constraint will not be applied to total value during fitting"
		read(*,"(a)") c80tmp
		if (c80tmp(1:1)=='u') then
			iconstot=0
		else
			iconstot=1
			read(c80tmp,*) constotval
		end if
	end if
end do

!Set vdW radius for MK, copied from GetvdW routine (utilam)
funcfitvdwr(1:17)=(/1.20d0,1.20d0,1.37d0,1.45d0,1.45d0,1.50d0,1.50d0,1.40d0,1.35d0,1.30d0,1.57d0,1.36d0,1.24d0,1.17d0,1.80d0,1.75d0,1.70d0/)
funcfitvdwr(1:17)=funcfitvdwr(1:17)/b2a
write(*,*) "Atomic radii used:"
do ielem=1,nelesupp
	if (any(a%index==ielem).and.funcfitvdwr(ielem)/=-1D0) write(*,"(' Element:',a,'     vdW radius (Angstrom):',f6.3)") ind2name(ielem),funcfitvdwr(ielem)*b2a
end do

!Check sanity and complete vdW radius table for all involved elements
do iatm=1,ncenter
	if (funcfitvdwr(a(iatm)%index)==-1D0) then
		write(*,"(' vdW radius used in fitting for element ',a,' is missing, input the radius (Bohr)')") ind2name(a(iatm)%index)
		write(*,"(a)") " Hint: If you do not know how to deal with the problem, simply input 3.4. (However, the radius of 3.4 Bohr may be not very appropriate for current element)" 
		read(*,*) funcfitvdwr(a(iatm)%index)
	end if
end do

!Check total number of fitting centers
naddcen=0
if (iaddcen==1) then
	open(10,file=addcenfile,status="old")
	read(10,*) naddcen
end if
nfitcen=ncenter+naddcen
allocate(fitcenx(nfitcen),fitceny(nfitcen),fitcenz(nfitcen),fitcenvdwr(nfitcen),disptcen(nfitcen))

!Generate information of fitting centers
do iatm=1,ncenter
	fitcenx(iatm)=a(iatm)%x
	fitceny(iatm)=a(iatm)%y
	fitcenz(iatm)=a(iatm)%z
	fitcenvdwr(iatm)=funcfitvdwr(a(iatm)%index) !vdW radius for each fitting center
end do
if (iaddcen==1) then
	do icen=ncenter+1,ncenter+naddcen
		read(10,*) fitcenx(icen),fitceny(icen),fitcenz(icen)
		fitcenvdwr(icen)=0D0
	end do
	close(10)
end if

write(*,*)
if (iuseextpt==0) then !Count number and generate coordinates of fitting points
	cutinnerscl=minval(sclvdwlayer(1:nlayer))
	write(*,"(' Note: If distance between a fitting point and any atom is smaller than',f6.3,' multiplied by corresponding vdW radius, then the point will be discarded')") cutinnerscl
	nfuncfitpt=0
	maxsphpt=nint(4D0*pi*(maxval(fitcenvdwr)*maxval(sclvdwlayer))**2 *densperarea) !Find maximal possible number of points in unit sphere to allocate temporary origsphpt
	allocate(origsphpt(3,maxsphpt))
	do icen=1,ncenter !Rather than nfitcen.   Count how many possible fitting points in total
		do ilayer=1,nlayer
			numsphpt=nint(4D0*pi*(fitcenvdwr(icen)*sclvdwlayer(ilayer))**2 *densperarea)
			nfuncfitpt=nfuncfitpt+numsphpt
		end do
	end do
	allocate(funcptval(nfuncfitpt),funcptx(nfuncfitpt),funcpty(nfuncfitpt),funcptz(nfuncfitpt)) !Currently nfuncfitpt is upper limit
	ifuncpt=0
	do icen=1,ncenter
		do ilayer=1,nlayer
			radius=fitcenvdwr(icen)*sclvdwlayer(ilayer)
			numsphpt=nint(4D0*pi*radius**2 *densperarea)
			call unitspherept(origsphpt,numsphpt) !Input expected number of point in unit sphere, return actual number of points
			origsphpt(:,1:numsphpt)=origsphpt(:,1:numsphpt)*radius
			origsphpt(1,1:numsphpt)=origsphpt(1,1:numsphpt)+fitcenx(icen) !Move unit sphere to atomic center
			origsphpt(2,1:numsphpt)=origsphpt(2,1:numsphpt)+fitceny(icen)
			origsphpt(3,1:numsphpt)=origsphpt(3,1:numsphpt)+fitcenz(icen)
			do ipt=1,numsphpt
				tmpx=origsphpt(1,ipt)
				tmpy=origsphpt(2,ipt)
				tmpz=origsphpt(3,ipt)
				iok=1
				do icen2=1,ncenter
					if (icen2==icen) cycle
					disptcensq=(fitcenx(icen2)-tmpx)**2+(fitceny(icen2)-tmpy)**2+(fitcenz(icen2)-tmpz)**2 !distance between point and center
					if (disptcensq<(fitcenvdwr(icen2)*cutinnerscl)**2) then !Less than vdW RADIUS*cutinner of atom icen2, it should be ommitted
						iok=0
						exit
					end if
				end do
				if (iok==1) then
					ifuncpt=ifuncpt+1
					funcptx(ifuncpt)=tmpx
					funcpty(ifuncpt)=tmpy
					funcptz(ifuncpt)=tmpz
				end if
			end do
		end do
	end do
	nfuncfitpt=ifuncpt
	deallocate(origsphpt)
	write(*,"(' Number of fitting points used:',i10)") nfuncfitpt
	
else if (iuseextpt==1) then !Directly use external fitting points
	open(10,file=extptfile,status="old")
	read(10,*) nfuncfitpt
	if (nfuncfitpt<0) then
		iskipfunccalc=1 !If the number of fitting points is negative, that means the fourth column records function value and needn't to be recalculated
		write(*,*) "Function value of all fitting points are read from external file directly"
	end if
	nfuncfitpt=abs(nfuncfitpt)
	write(*,"(' Number of fitting points used:',i10)") nfuncfitpt
	allocate(funcptval(nfuncfitpt),funcptx(nfuncfitpt),funcpty(nfuncfitpt),funcptz(nfuncfitpt))
	do i=1,nfuncfitpt
		if (iskipfunccalc==0) read(10,*) funcptx(i),funcpty(i),funcptz(i)
		if (iskipfunccalc==1) read(10,*) funcptx(i),funcpty(i),funcptz(i),funcptval(i)
	end do
	close(10)
end if

!Generate function value of fitting points
if (iskipfunccalc==0) then
	write(*,*) "Select the real space function to be fitted"
	call selfunc_interface(1,ifuncsel)
	write(*,*) "Calculating function value, please wait..."
	itmp=1
	do ipt=1,nfuncfitpt
		if (ipt>=itmp*300) then
			call showprog(ipt,nfuncfitpt)
			itmp=itmp+1
		end if
		funcptval(ipt)=calcfuncall(ifuncsel,(funcptx(ipt)),(funcpty(ipt)),(funcptz(ipt)))
	end do
	write(*,*) "Done!"
end if

matdim=nfitcen+1
allocate(Bmat(matdim),Amat(matdim,matdim),Amatinv(matdim,matdim),atmval(matdim))
Amat=0D0
do icen=1,nfitcen
	do jcen=icen,nfitcen
		do ipt=1,nfuncfitpt
			dis1=dsqrt( (funcptx(ipt)-fitcenx(icen))**2 + (funcpty(ipt)-fitceny(icen))**2 + (funcptz(ipt)-fitcenz(icen))**2 )
			dis2=dsqrt( (funcptx(ipt)-fitcenx(jcen))**2 + (funcpty(ipt)-fitceny(jcen))**2 + (funcptz(ipt)-fitcenz(jcen))**2 )
			Amat(icen,jcen)=Amat(icen,jcen)+1D0/dis1/dis2
		end do
	end do
end do
Amat=Amat+transpose(Amat)
do i=1,nfitcen
	Amat(i,i)=Amat(i,i)/2D0
end do
Amat(matdim,:)=1D0
Amat(:,matdim)=1D0
Amat(matdim,matdim)=0D0
Bmat=0D0
do icen=1,nfitcen
	do ipt=1,nfuncfitpt
		dis=dsqrt( (funcptx(ipt)-fitcenx(icen))**2 + (funcpty(ipt)-fitceny(icen))**2 + (funcptz(ipt)-fitcenz(icen))**2 )
		Bmat(icen)=Bmat(icen)+funcptval(ipt)/dis
	end do
end do
Bmat(matdim)=constotval !Constraint on the sum of all values
if (iconstot==1) then
	Amatinv=invmat(Amat,matdim)
	atmval=matmul(Amatinv,Bmat)
else
	Amatinv(1:nfitcen,1:nfitcen)=invmat(Amat(1:nfitcen,1:nfitcen),nfitcen)
	atmval(1:nfitcen)=matmul(Amatinv(1:nfitcen,1:nfitcen),Bmat(1:nfitcen))
end if

!Output summary
write(*,*) " Center       X           Y           Z             Value"
do i=1,ncenter
	write(*,"(i6,a,3f12.6,f16.6)") i,ind2name(a(i)%index),fitcenx(i),fitceny(i),fitcenz(i),atmval(i)
end do
do i=ncenter+1,ncenter+naddcen
	write(*,"(i6,2x,3f12.6,f16.6)") i,fitcenx(i),fitceny(i),fitcenz(i),atmval(i)
end do
write(*,"(' Sum of values:',f12.6)") sum(atmval(1:nfitcen))
!Calculate RMSE and RRMSE
RMSE=0D0
do ipt=1,nfuncfitpt
	atmvaleval=0D0 !Function value evaluated from atomic value by 1/r12
	do icen=1,nfitcen
		dis=dsqrt( (funcptx(ipt)-fitcenx(icen))**2 + (funcpty(ipt)-fitceny(icen))**2 + (funcptz(ipt)-fitcenz(icen))**2 )
		atmvaleval=atmvaleval+atmval(icen)/dis
	end do
	RMSE=RMSE+(funcptval(ipt)-atmvaleval)**2
end do
RRMSE=dsqrt(RMSE/sum(funcptval(1:nfuncfitpt)**2))
RMSE=dsqrt(RMSE/nfuncfitpt)
write(*,"(' RMSE:',f12.6,'   RRMSE:',f12.6)") RMSE,RRMSE

write(*,*)
write(*,"(a)") " If output coordinates and function value of all fitting points to funcfitpt.txt in current folder? (y/n)"
read(*,*) selectyn
if (selectyn=='y'.or.selectyn=="Y") then
	open(10,file="funcfitpt.txt",status="replace")
	write(10,*) nfuncfitpt
	do ipt=1,nfuncfitpt
		write(10,"(3f13.7,E20.10)") funcptx(ipt),funcpty(ipt),funcptz(ipt),funcptval(ipt)
	end do
	write(*,*) "Data have been outputted to funcfitpt.txt in current folder"
	write(*,"(a)") " All units are in a.u. The first line shows the number of fitting points, &
	the first three columns are X,Y,Z coordinates, the last column corresponds to function value"
	close(10)
end if
end subroutine



!!!------------------- Calculate properties based on atom geometry information
subroutine calcgeomprop
use defvar
use util
implicit real*8 (a-h,o-z)
character c2000tmp*2000
integer,allocatable :: calcatom(:)
do while(.true.)
	write(*,*) "Input the indices of the atoms to be taken into account"
	write(*,*) "e.g. 1,3-6,8,10-11 means the atoms 1,3,4,5,6,8,10,11 will be considered"
	write(*,*) "Input ""all"" will analyze the whole system"
	write(*,*) "Input ""size"" will report size information of the whole system"
	write(*,*) "Input q can exit"
	read(*,"(a)") c2000tmp
	if (c2000tmp(1:1)=='q'.or.c2000tmp(1:1)=='Q') then
		exit
	else if (c2000tmp(1:4)=='size') then
		call calcmolsize
	else
		if (allocated(calcatom)) deallocate(calcatom)
		if (index(c2000tmp,"all")/=0) then
			ncalcatom=ncenter
			allocate(calcatom(ncalcatom))
			do itmp=1,ncalcatom
				calcatom(itmp)=itmp
			end do
		else
			call str2arr(c2000tmp,ncalcatom)
			allocate(calcatom(ncalcatom))
			call str2arr(c2000tmp,ncalcatom,calcatom)
		end if
		call calcmolinfo(calcatom,ncalcatom)
	end if
	write(*,*)
end do
end subroutine
!!----- Show some molecular information based on geometry
!atmarray records which atoms will be taken into account, natmarr elements are there
subroutine calcmolinfo(atmarr,natmarr)
use util
use defvar
implicit real*8 (a-h,o-z)
integer atmarr(natmarr)
real*8 inertia(3,3),eigvalint(3),eigvecmatint(3,3)
totmass=sum(atmwei(a(atmarr(:))%index))
avgx=sum(a(atmarr(:))%x)/natmarr
avgy=sum(a(atmarr(:))%y)/natmarr
avgz=sum(a(atmarr(:))%z)/natmarr
cenmassx=sum(a(atmarr(:))%x*atmwei(a(atmarr(:))%index))/totmass
cenmassy=sum(a(atmarr(:))%y*atmwei(a(atmarr(:))%index))/totmass
cenmassz=sum(a(atmarr(:))%z*atmwei(a(atmarr(:))%index))/totmass
rgyr=dsqrt( sum( atmwei(a(atmarr(:))%index)* ((a(atmarr(:))%x-cenmassx)**2+(a(atmarr(:))%y-cenmassy)**2+(a(atmarr(:))%z-cenmassz)**2) ) / totmass )
totnucchg=sum(a(atmarr(:))%charge)
dipnucx=sum(a(atmarr(:))%x*a(atmarr(:))%charge)
dipnucy=sum(a(atmarr(:))%y*a(atmarr(:))%charge)
dipnucz=sum(a(atmarr(:))%z*a(atmarr(:))%charge)
dipnucnorm=dsqrt(dipnucx**2+dipnucy**2+dipnucz**2)
eleint=0D0
do iatmidx=1,natmarr
	iatm=atmarr(iatmidx)
	do jatmidx=iatmidx+1,natmarr
		jatm=atmarr(jatmidx)
		eleint=eleint+a(iatm)%charge*a(jatm)%charge/distmat(iatm,jatm)
	end do
end do
write(*,"(' Mass of these atoms:',f14.6,' amu')") totmass
write(*,"(' Geometry center (X/Y/Z):',3f14.8,' Angstrom')") avgx*b2a,avgy*b2a,avgz*b2a
write(*,"(' Center of mass (X/Y/Z): ',3f14.8,' Angstrom')") cenmassx*b2a,cenmassy*b2a,cenmassz*b2a
if (ifiletype==4) then !chg file
	write(*,"(' Sum of atomic charges:',f20.8)") totnucchg
	write(*,"(' Dipole from atomic charges (Norm):',E12.5,' a.u.',E13.5,' Debye')") dipnucnorm,dipnucnorm*au2debye
	write(*,"(' Dipole from atomic charges (X/Y/Z):',3E12.5,' a.u.')") dipnucx,dipnucy,dipnucz
	write(*,"(' Electrostatic interaction energy between atomic charges:',/,f17.8,' a.u.',f20.5,' kcal/mol',f20.5,' KJ/mol')") eleint,eleint*au2kcal,eleint*au2KJ
else if (ifiletype/=4) then
	write(*,"(' Sum of nuclear charges:',f20.8)") totnucchg
	write(*,"(' Center of nuclear charges (X/Y/Z):',3f13.7,' Ang')") dipnucx*b2a/totnucchg,dipnucy*b2a/totnucchg,dipnucz*b2a/totnucchg
	write(*,"(' Dipole from nuclear charges (Norm):',E12.5,' a.u.',E13.5,' Debye')") dipnucnorm,dipnucnorm*au2debye
	write(*,"(' Dipole from nuclear charges (X/Y/Z):',3E12.5,' a.u.')") dipnucx,dipnucy,dipnucz
	write(*,"(' Electrostatic interaction energy between nuclear charges:',/,E18.10,' a.u.',E18.10,' kcal/mol',E18.10,' KJ/mol')") eleint,eleint*au2kcal,eleint*au2KJ
end if
write(*,"(' Radius of gyration:',f14.8,' Angstrom')") rgyr*b2a
xmin=a(atmarr(1))%x
ymin=a(atmarr(1))%y
zmin=a(atmarr(1))%z
xmax=xmin
ymax=ymin
zmax=zmin
ixmax=atmarr(1)
iymax=atmarr(1)
izmax=atmarr(1)
ixmin=atmarr(1)
iymin=atmarr(1)
izmin=atmarr(1)
do idx=1,natmarr
	iatm=atmarr(idx)
	if (a(iatm)%x>xmax) then
		xmax=a(iatm)%x
		ixmax=iatm
	end if
	if (a(iatm)%y>ymax) then
		ymax=a(iatm)%y
		iymax=iatm
	end if
	if (a(iatm)%z>zmax) then
		zmax=a(iatm)%z
		izmax=iatm
	end if
	if (a(iatm)%x<xmin) then
		xmin=a(iatm)%x
		ixmin=iatm
	end if
	if (a(iatm)%y<ymin) then
		ymin=a(iatm)%y
		iymin=iatm
	end if
	if (a(iatm)%z<zmin) then
		zmin=a(iatm)%z
		izmin=iatm
	end if
end do
write(*,"(' Minimum X is',f14.8,' Angstrom, at atom',i6,'(',a,')')") xmin,ixmin,a(ixmin)%name
write(*,"(' Minimum Y is',f14.8,' Angstrom, at atom',i6,'(',a,')')") ymin,iymin,a(iymin)%name
write(*,"(' Minimum Z is',f14.8,' Angstrom, at atom',i6,'(',a,')')") zmin,izmin,a(izmin)%name
write(*,"(' Maximum X is',f14.8,' Angstrom, at atom',i6,'(',a,')')") xmax,ixmax,a(ixmax)%name
write(*,"(' Maximum Y is',f14.8,' Angstrom, at atom',i6,'(',a,')')") ymax,iymax,a(iymax)%name
write(*,"(' Maximum Z is',f14.8,' Angstrom, at atom',i6,'(',a,')')") zmax,izmax,a(izmax)%name
if (natmarr>=2) then
	rmindist=1D50
	rmaxdist=0
	do iidx=1,natmarr
		i=atmarr(iidx)
		do jidx=iidx+1,natmarr
			j=atmarr(jidx)
			if (distmat(i,j)<rmindist) then
				rmindist=distmat(i,j)
				imindist=i
				jmindist=j
			end if
			if (distmat(i,j)>rmaxdist) then
				rmaxdist=distmat(i,j)
				imaxdist=i
				jmaxdist=j
			end if
		end do
	end do
	write(*,"(' Maximum distance is',f12.6,' Angstrom, between atom',i6,'(',a,') and',i6,'(',a,')')") rmaxdist*b2a,imaxdist,a(imaxdist)%name,jmaxdist,a(jmaxdist)%name
	write(*,"(' Minimum distance is',f12.6,' Angstrom, between atom',i6,'(',a,') and',i6,'(',a,')')") rmindist*b2a,imindist,a(imindist)%name,jmindist,a(jmindist)%name
end if
do iatmidx=1,natmarr
	iatm=atmarr(iatmidx)
	disttmp=dsqrt((avgx-a(iatm)%x)**2+(avgy-a(iatm)%y)**2+(avgz-a(iatm)%z)**2)
	if (iatmidx==1.or.disttmp>distmax) then
		distmax=disttmp
		idistmax=iatm
	end if
	if (iatmidx==1.or.disttmp<distmin) then
		distmin=disttmp
		idistmin=iatm
	end if
end do
write(*,"(' The atom closest to geometry center is',i8,'(',a,')  Dist:',f12.6,' Angstrom')") idistmin,a(idistmin)%name,distmin*b2a
write(*,"(' The atom farthest to geometry center is',i7,'(',a,')  Dist:',f12.6,' Angstrom')") idistmax,a(idistmax)%name,distmax*b2a
write(*,*)
inertia(1,1)=sum(atmwei(a(atmarr(:))%index)*( (a(atmarr(:))%y-cenmassy)**2+(a(atmarr(:))%z-cenmassz)**2) )*b2a*b2a
inertia(2,2)=sum(atmwei(a(atmarr(:))%index)*( (a(atmarr(:))%x-cenmassx)**2+(a(atmarr(:))%z-cenmassz)**2) )*b2a*b2a
inertia(3,3)=sum(atmwei(a(atmarr(:))%index)*( (a(atmarr(:))%x-cenmassx)**2+(a(atmarr(:))%y-cenmassy)**2) )*b2a*b2a
inertia(1,2)=-sum(atmwei(a(atmarr(:))%index)*(a(atmarr(:))%x-cenmassx)*(a(atmarr(:))%y-cenmassy))*b2a*b2a
inertia(2,1)=inertia(1,2)
inertia(1,3)=-sum(atmwei(a(atmarr(:))%index)*(a(atmarr(:))%x-cenmassx)*(a(atmarr(:))%z-cenmassz))*b2a*b2a
inertia(3,1)=inertia(1,3)
inertia(2,3)=-sum(atmwei(a(atmarr(:))%index)*(a(atmarr(:))%y-cenmassy)*(a(atmarr(:))%z-cenmassz))*b2a*b2a
inertia(3,2)=inertia(2,3)
call showmatgau(inertia,"Moments of inertia tensor (amu*Angstrom^2)")
write(*,"(' The moments of inertia relative to X,Y,Z axes (amu*Angstrom^2):',/,3E16.8)") inertia(1,1),inertia(2,2),inertia(3,3)
rotcstA=planckc/(8D0*pi**2*inertia(1,1)*amu2kg*1D-20)/1D9 !1D-20 used to convert Angstrom to meter, GHz=1D9Hz, 1Hz=1/s
rotcstB=planckc/(8D0*pi**2*inertia(2,2)*amu2kg*1D-20)/1D9
rotcstC=planckc/(8D0*pi**2*inertia(3,3)*amu2kg*1D-20)/1D9
write(*,"(' Rotational constant relative to X,Y,Z axes (GHz):',/,3f16.8)") rotcstA,rotcstB,rotcstC
call diagmat(inertia,eigvecmatint,eigvalint,300,1D-12)
call showmatgau(eigvecmatint,"Principal axes (each column vector)")
write(*,"(' The moments of inertia relative to principal axes (amu*Angstrom^2): ',/,3E16.8)") eigvalint
write(*,"(' Rotational constant relative to principal axes (GHz):',/,3f16.8)") planckc/(8D0*pi**2*eigvalint(1:3)*amu2kg*1D-20)/1D9
end subroutine

!!----- Calculate molecular diameter and length, width and height of the system
subroutine calcmolsize
use defvar
use util
use GUI
implicit real*8 (a-h,o-z)
real*8 inertia(3,3),eigvalarr(3),eigvecmat(3,3),vec3(1,3)

rmaxdist=0
do iatm=1,ncenter
	do jatm=iatm+1,ncenter
		if (distmat(iatm,jatm)>rmaxdist) then
			rmaxdist=distmat(iatm,jatm)
			imaxdist=iatm
			jmaxdist=jatm
		end if
	end do
end do
diameter=rmaxdist+vdwr(a(imaxdist)%index)+vdwr(a(jmaxdist)%index)
write(*,"(' Farthest distance:',i5,'(',a,')  ---',i5,'(',a,'):',f10.3, ' Angstrom')") imaxdist,a(imaxdist)%name,jmaxdist,a(jmaxdist)%name,rmaxdist*b2a
write(*,"(' vdW radius of',i5,'(',a,'):',f6.3,' Angstrom')") imaxdist,a(imaxdist)%name,vdwr(a(imaxdist)%index)*b2a
write(*,"(' vdW radius of',i5,'(',a,'):',f6.3,' Angstrom')") jmaxdist,a(jmaxdist)%name,vdwr(a(jmaxdist)%index)*b2a
write(*,"(' Diameter of the system:',f10.3,' Angstrom')") diameter*b2a
write(*,"(' Radius of the system:',f10.3,' Angstrom')") diameter*b2a/2
totmass=sum(atmwei(a%index))
cenmassx=sum(a%x*atmwei(a%index))/totmass
cenmassy=sum(a%y*atmwei(a%index))/totmass
cenmassz=sum(a%z*atmwei(a%index))/totmass
inertia(1,1)=sum(atmwei(a%index)*( (a%y-cenmassy)**2+(a%z-cenmassz)**2) )*b2a*b2a
inertia(2,2)=sum(atmwei(a%index)*( (a%x-cenmassx)**2+(a%z-cenmassz)**2) )*b2a*b2a
inertia(3,3)=sum(atmwei(a%index)*( (a%x-cenmassx)**2+(a%y-cenmassy)**2) )*b2a*b2a
inertia(1,2)=-sum(atmwei(a%index)*(a%x-cenmassx)*(a%y-cenmassy))*b2a*b2a
inertia(2,1)=inertia(1,2)
inertia(1,3)=-sum(atmwei(a%index)*(a%x-cenmassx)*(a%z-cenmassz))*b2a*b2a
inertia(3,1)=inertia(1,3)
inertia(2,3)=-sum(atmwei(a%index)*(a%y-cenmassy)*(a%z-cenmassz))*b2a*b2a
inertia(3,2)=inertia(2,3)
! call showmatgau(inertia,"Moments of inertia tensor (amu*Angstrom^2)")
call diagmat(inertia,eigvecmat,eigvalarr,300,1D-12)
! call showmatgau(eigvecmat,"Principal axes (each column vector)")
!Rotate the system to principle axis orientation
do iatm=1,ncenter
	vec3(1,1)=a(iatm)%x
	vec3(1,2)=a(iatm)%y
	vec3(1,3)=a(iatm)%z
	vec3(1,1:3)=matmul(vec3(1,1:3),eigvecmat)
	a(iatm)%x=vec3(1,1)
	a(iatm)%y=vec3(1,2)
	a(iatm)%z=vec3(1,3)
end do
!Make center of mass as origin
a%x=a%x-cenmassx
a%y=a%y-cenmassy
a%z=a%z-cenmassz
!Find minimum and maximum of the system, vdW radius has been taken into account
iminx=minloc(a%x,1)
imaxx=maxloc(a%x,1)
iminy=minloc(a%y,1)
imaxy=maxloc(a%y,1)
iminz=minloc(a%z,1)
imaxz=maxloc(a%z,1)
orgx=a(iminx)%x-vdwr(a(iminx)%index)
endx=a(imaxx)%x+vdwr(a(imaxx)%index)
orgy=a(iminy)%y-vdwr(a(iminy)%index)
endy=a(imaxy)%y+vdwr(a(imaxy)%index)
orgz=a(iminz)%z-vdwr(a(iminz)%index)
endz=a(imaxz)%z+vdwr(a(imaxz)%index)
xlen=endx-orgx
ylen=endy-orgy
zlen=endz-orgz
write(*,"(' Length of the three sides:',3f10.3,' Angstrom')") xlen*b2a,ylen*b2a,zlen*b2a
do while(.true.)
	write(*,*)
	write(*,*) "0 Return"
	write(*,*) "1 Visualize the new orientation and molecular box"
	write(*,*) "2 Export the geometry in new orientation as new.pdb in current folder"
	read(*,*) isel
	if (isel==0) then
		a%x=a_org%x
		a%y=a_org%y
		a%z=a_org%z
		exit
	else if (isel==1) then
		ishowdatarange=1 !Draw box range
		call miniGUI
		ishowdatarange=0
	else if (isel==2) then
		a%x=a%x-orgx !Temporarily make orgx,orgy,orgz as 0,0,0
		a%y=a%y-orgy
		a%z=a%z-orgz
		open(10,file="new.pdb",status="replace")
		write(10,"('REMARK   Generated by Multiwfn, Totally',i10,' atoms')") ncenter
		write(10,"('CRYST1',3f9.3,3f7.2,' P 1           1')") xlen*b2a,ylen*b2a,zlen*b2a,90D0,90D0,90D0
		do i=1,ncenter
			write(10,"(a6,i5,1x,a4,1x,a3, 1x,a1,i4,4x,3f8.3,2f6.2,10x,a2)") &
			"HETATM",i,' '//ind2name_up(a(i)%index)//' ',"MOL",'A',1,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a,1.0,0.0,adjustr(ind2name_up(a(i)%index))
		end do
		close(10)
		write(*,*) "Exporting new.pdb file finished!"
		a%x=a%x+orgx
		a%y=a%y+orgy
		a%z=a%z+orgz
	end if
end do
end subroutine



!! ------------ Generate promolecular wavefunction of a molecule consisted of several fragment wavefunctions
! Any format of wavefunction file can be used as input, all orbitals including the virtual ones are stored to _all arrays first,
! and then the virtual ones will be skipped in the output of .wfn files.
! After using this module, the present wavefunction will become the promolecular wavefunction
subroutine genpromolwfn
use defvar
implicit real*8 (a-h,o-z)
character selectyn*1,c200tmp*200
character,allocatable :: namearray(:)*200
type(atomtype),allocatable :: a_all(:)
type(primtype),allocatable :: b_all(:)
real*8,allocatable :: MOene_all(:),MOocc_all(:),CO_all(:,:),tmparr(:)
integer,allocatable :: iopshfrag(:),iflipspin(:)

write(*,*) "How many fragments?  (Including the fragment 1 that has been loaded)"
read(*,*) nfrag
allocate(namearray(nfrag),iopshfrag(nfrag),iflipspin(nfrag))
do i=1,nfrag
	if (i==1) then
		write(*,"(' Filename of fragment 1: ',a)") trim(filename)
		namearray(1)=filename
	else if (i/=1) then
		do while(.true.)
			write(*,"(/,' Input wavefunction file of fragment',i4)") i
			write(*,*) "(Any format of wavefunction file may be used, e.g. .wfn/.wfx/.fch/.molden)"
			read(*,"(a)") c200tmp
			inquire(file=c200tmp,exist=alive)
			if (alive) exit
			write(*,*) "File not found, input again"
		end do
		namearray(i)=c200tmp
	end if
end do
!Detect if need to treat this promolecule as open-shell
iopsh=0
do i=1,nfrag
	call dealloall
	call readinfile(namearray(i),1)
	if (wfntype==1.or.wfntype==4) then
		iopsh=1
		exit
	end if
end do

!Gain some basic informations so that the arrays can be allocated
nprims_all=0
ncenter_all=0
nmo_all=0
nmoa_all=0
nmob_all=0
iopshfrag=0 !Assume all fragments are closed-shell
iflipspin=0 !Assume don't flip spin
do i=1,nfrag
	call dealloall
	write(*,"(/,' Loading ',a)") trim(namearray(i))
	call readinfile(namearray(i),1)
	nprims_all=nprims_all+nprims
	ncenter_all=ncenter_all+ncenter
	if (iopsh==1) then !Open-shell treatment
		if (wfntype==1.or.wfntype==4) then !This is open-shell fragment
			iopshfrag(i)=1
			nmoatmp=count(MOtype==1)
			nmobtmp=count(MOtype==2)
			write(*,*) "If flip electron spin for this fragment? (y/n)"
			read(*,*) selectyn
			if (selectyn=="y") then
				iflipspin(i)=1
				nmoa_all=nmoa_all+nmoatmp
				nmob_all=nmob_all+nmobtmp
			else
				nmoa_all=nmoa_all+nmobtmp
				nmob_all=nmob_all+nmoatmp
			end if
		else !This is closed-shell fragment, separate it as equivalent alpha and beta parts
			nmoa_all=nmoa_all+nmo
			nmob_all=nmob_all+nmo
		end if
	else !Closed-shell treatment
		nmo_all=nmo_all+nmo
	end if
end do
if (iopsh==1) nmo_all=nmoa_all+nmob_all

allocate(a_all(ncenter_all),b_all(nprims_all),MOene_all(nmo_all),MOocc_all(nmo_all),CO_all(nmo_all,nprims_all),tmparr(nprims_all))
CO_all=0
write(*,*)
write(*,"(' The total number of atoms:',i6)") ncenter_all
write(*,"(' The total number of orbitals:',i6)") nmo_all
write(*,"(' The total number of GTFs:',i6)") nprims_all
if (iopsh==0) write(*,"(' The total number of orbitals:',i6)") nmo_all
if (iopsh==1) write(*,"(' The total number of alpha and beta orbitals:',2i6)") nmoa_all,nmob_all
write(*,*)

!Read information from fragment wavefunction file
icenter=1
iprim=1
imo=1
imoa=1
imob=nmoa_all+1
do i=1,nfrag
	call dealloall
	call readinfile(namearray(i),1)
	a_all(icenter:icenter+ncenter-1)=a
	b_all(iprim:iprim+nprims-1)=b
	b_all(iprim:iprim+nprims-1)%center=b_all(iprim:iprim+nprims-1)%center+(icenter-1)
	if (iopsh==0) then !Promolecule is closed-shell
		MOene_all(imo:imo+nmo-1)=MOene
		MOocc_all(imo:imo+nmo-1)=MOocc
		CO_all(imo:imo+nmo-1,iprim:iprim+nprims-1)=CO
		imo=imo+nmo
	else if (iopsh==1) then !Promolecule is open-shell
		if (iopshfrag(i)==0) then !Closed-shell fragment
			MOene_all(imoa:imoa+nmo-1)=MOene
			MOocc_all(imoa:imoa+nmo-1)=MOocc/2D0
			CO_all(imoa:imoa+nmo-1,iprim:iprim+nprims-1)=CO
			imoa=imoa+nmo
			MOene_all(imob:imob+nmo-1)=MOene
			MOocc_all(imob:imob+nmo-1)=MOocc/2D0
			CO_all(imob:imob+nmo-1,iprim:iprim+nprims-1)=CO
			imob=imob+nmo
		else !Open-shell fragment
			do isep=nmo,1,-1 !Find where is the separation of alpha and beta MOs in this fragment
				if (MOtype(isep)==1) exit
			end do
			nmoatmp=count(MOtype==1)
			nmobtmp=count(MOtype==2)
			if (iflipspin(i)==0) then
				MOene_all(imoa:imoa+nmoatmp-1)=MOene(1:isep) !Alpha part
				MOocc_all(imoa:imoa+nmoatmp-1)=MOocc(1:isep)
				CO_all(imoa:imoa+nmoatmp-1,iprim:iprim+nprims-1)=CO(1:isep,:)
				imoa=imoa+nmoatmp
				MOene_all(imob:imob+nmobtmp-1)=MOene(isep+1:nmo) !Beta part
				MOocc_all(imob:imob+nmobtmp-1)=MOocc(isep+1:nmo)
				CO_all(imob:imob+nmobtmp-1,iprim:iprim+nprims-1)=CO(isep+1:nmo,:)
				imob=imob+nmobtmp
			else if (iflipspin(i)==1) then
				MOene_all(imoa:imoa+nmobtmp-1)=MOene(isep+1:nmo) !Alpha part
				MOocc_all(imoa:imoa+nmobtmp-1)=MOocc(isep+1:nmo)
				CO_all(imoa:imoa+nmobtmp-1,iprim:iprim+nprims-1)=CO(isep+1:nmo,:)
				imoa=imoa+nmobtmp
				MOene_all(imob:imob+nmoatmp-1)=MOene(1:isep) !Beta part
				MOocc_all(imob:imob+nmoatmp-1)=MOocc(1:isep)
				CO_all(imob:imob+nmoatmp-1,iprim:iprim+nprims-1)=CO(1:isep,:)
				imob=imob+nmoatmp
			end if
		end if
	end if
	icenter=icenter+ncenter
	iprim=iprim+nprims
end do

!Store the data to global arrays so that they can be outputted by "outwfn" subroutine
call dealloall
allocate(a(ncenter_all),b(nprims_all),MOene(nmo_all),MOocc(nmo_all),CO(nmo_all,nprims_all))
ncenter=ncenter_all
nprims=nprims_all
nmo=nmo_all
a=a_all
b=b_all
MOocc=MOocc_all
MOene=MOene_all
CO=CO_all
totenergy=0
virialratio=2

!Determine wavefunction type
if (all(nint(MOocc)==MOocc)) then
	wfntype=0
	if (iopsh==0) then
		wfntype=0
		if (any(MOocc==1D0)) wfntype=2
	else if (iopsh==1) then
		wfntype=1
	end if
else
	wfntype=3
	if (iopsh==1) wfntype=4
end if
! !Though MOtype is not used in the outputted .wfn file, after using this module the present wavefunction will be updated to the promolecular one
! !MOtype as well as distance matrix will be used after if we want to do more later
! allocate(MOtype(nmo))
! MOtype=0
! if (wfntype==1.or.wfntype==4) then
! 	MOtype(1:nmoa_all)=1
! 	MOtype(nmoa_all+1:nmo)=2
! end if
! call gendistmat
!!! I commented above codes, because there may be many vacancy (zero-occupied MOs) in present wavefunction,
!!! with this wavefunction many properies cannot be calculated properly, however they will be automatically removed during "outwfn".
!!! So I'd rather recover to the first loaded system.

!Sort the orbitals according to energy/occupation
if (iopsh==0) ntime=1 !Close shell
if (iopsh==1) ntime=2 !Open shell
do itime=1,ntime
	if (iopsh==0) then
		ilow=1
		ihigh=nmo
	else if (iopsh==1) then !First time sort alpha orbitals, the second time sort beta orbitals
		if (itime==1) then
			ilow=1
			ihigh=nmoa_all
		else
			ilow=nmoa_all+1
			ihigh=nmo_all
		end if
	end if
	do i=ilow,ihigh
		do j=i+1,ihigh
			if (wfntype==0.or.wfntype==1.or.wfntype==2) then !SCF wavefunction, sort according to energy from low to high
				if (MOene(i)<=MOene(j)) cycle
			else !Post-SCF wavefunction, sort according to occupation number, sort according to energy from high to low
				if (MOocc(i)>=MOocc(j)) cycle
			end if
			temp=MOene(i)
			MOene(i)=MOene(j)
			MOene(j)=temp
			temp=MOocc(i)
			MOocc(i)=MOocc(j)
			MOocc(j)=temp
			tmparr=CO(i,:)
			CO(i,:)=CO(j,:)
			CO(j,:)=tmparr
		end do
	end do
end do
call outwfn("promol.wfn",1,1,10) !Note that the unoccupied MOs are automatically skipped
write(*,*) "New .wfn file has been outputted to promol.wfn in current folder"

!Recover to the first loaded system
call dealloall
call readinfile(firstfilename,1)

end subroutine




!! ----------- Calculate Hellmann-Feynman forces at each nucleus
subroutine hellmann_feynman
use defvar
use function
implicit real*8 (a-h,o-z)
real*8 HFforce_nuc(ncenter,3),HFforce_ele(ncenter,3),HFforce_tot(ncenter,3)
write(*,*) "Note: All units below are Hartree/Bohr"
write(*,*)
write(*,*) "Hellmann-Feynman forces contributed by electrons:"
write(*,*) "   Atom            X               Y               Z            Total"
diff=1D-5
do iatm=1,ncenter
	HFforce_ele(iatm,1)=(eleesp(a(iatm)%x+diff,a(iatm)%y,a(iatm)%z)-eleesp(a(iatm)%x-diff,a(iatm)%y,a(iatm)%z))/(2*diff)
	HFforce_ele(iatm,2)=(eleesp(a(iatm)%x,a(iatm)%y+diff,a(iatm)%z)-eleesp(a(iatm)%x,a(iatm)%y-diff,a(iatm)%z))/(2*diff)
	HFforce_ele(iatm,3)=(eleesp(a(iatm)%x,a(iatm)%y,a(iatm)%z+diff)-eleesp(a(iatm)%x,a(iatm)%y,a(iatm)%z-diff))/(2*diff)
	HFforce_ele(iatm,:)=-HFforce_ele(iatm,:)*a(iatm)%charge !force is negative of gradient(1st derivative)
	write(*,"(i5,'(',a,')',4f16.8)") iatm,a(iatm)%name,HFforce_ele(iatm,:),dsqrt(sum(HFforce_ele(iatm,:)**2))
end do

write(*,*)
write(*,*) "Hellmann-Feynman forces contributed by nuclear charges:"
write(*,*) "   Atom            X               Y               Z            Total"
HFforce_nuc=0
do iatm=1,ncenter
	do jatm=1,ncenter
		if (jatm==iatm) cycle
		forcetmp=a(iatm)%charge*a(jatm)%charge/distmat(iatm,jatm)**3
		HFforce_nuc(iatm,1)=HFforce_nuc(iatm,1)+forcetmp*(a(iatm)%x-a(jatm)%x)
		HFforce_nuc(iatm,2)=HFforce_nuc(iatm,2)+forcetmp*(a(iatm)%y-a(jatm)%y)
		HFforce_nuc(iatm,3)=HFforce_nuc(iatm,3)+forcetmp*(a(iatm)%z-a(jatm)%z)
	end do
	write(*,"(i5,'(',a,')',4f16.8)") iatm,a(iatm)%name,HFforce_nuc(iatm,:),dsqrt(sum(HFforce_nuc(iatm,:)**2))
end do

write(*,*)
HFforce_tot=HFforce_ele+HFforce_nuc
write(*,*) "Total Hellmann-Feynman forces:"
write(*,*) "   Atom            X               Y               Z            Total"
do iatm=1,ncenter
	write(*,"(i5,'(',a,')',4f16.8)") iatm,a(iatm)%name,HFforce_tot(iatm,:),dsqrt(sum(HFforce_tot(iatm,:)**2))
end do
end subroutine


!!------ Calculate attractive energy between an orbital and nuclei in a fragment
subroutine attene_orb_fragnuc
use function
use util
implicit real*8 (a-h,o-z)
character c2000tmp*2000
real*8 intval,intvalold,funcval(radpot*sphpot),beckeweigrid(radpot*sphpot)
type(content) gridatm(radpot*sphpot),gridatmorg(radpot*sphpot)
write(*,*) "Input orbital index, e.g. 5"
read(*,*) iorb
write(*,"(a)") " Input atomic indices to define the fragment. e.g. 1,3-6,8,10-11 means the atoms 1,3,4,5,6,8,10,11 will constitute the fragment"
read(*,"(a)") c2000tmp
if (allocated(frag1)) deallocate(frag1)
call str2arr(c2000tmp,nfragatm)
allocate(frag1(nfragatm))
call str2arr(c2000tmp,nfragatm,frag1)
write(*,"(i6,' atoms are selected',/)") nfragatm

write(*,"(' Radial points:',i5,'    Angular points:',i5,'   Total:',i10,' per center')") radpot,sphpot,radpot*sphpot
call gen1cintgrid(gridatmorg,iradcut)
intval=0
intvalold=0
do iatm=1,ncenter
	write(*,"(' Processing center',i6,'(',a2,')   /',i6)") iatm,a(iatm)%name,ncenter
	gridatm%x=gridatmorg%x+a(iatm)%x !Move quadrature point to actual position in molecule
	gridatm%y=gridatmorg%y+a(iatm)%y
	gridatm%z=gridatmorg%z+a(iatm)%z
	!$OMP parallel do shared(funcval) private(i,jatmtmp,jatm,potnuc,rnowx,rnowy,rnowz) num_threads(nthreads)
	do i=1+iradcut*sphpot,radpot*sphpot
		rnowx=gridatm(i)%x
		rnowy=gridatm(i)%y
		rnowz=gridatm(i)%z
		potnuc=0
		do jatmtmp=1,nfragatm
			jatm=frag1(jatmtmp)
			potnuc=potnuc+a(jatm)%charge/dsqrt((rnowx-a(jatm)%x)**2+(rnowy-a(jatm)%y)**2+(rnowz-a(jatm)%z)**2)
		end do
 		funcval(i)=-potnuc*fmo(rnowx,rnowy,rnowz,iorb)**2
	end do
	!$OMP end parallel do
	call gen1cbeckewei(iatm,iradcut,gridatm,beckeweigrid)
	do i=1+iradcut*sphpot,radpot*sphpot
		intval=intval+funcval(i)*gridatmorg(i)%value*beckeweigrid(i)
	end do
	write(*,"(' Accumulated value:',f20.10,'  Current center:',f20.10)") intval,intval-intvalold
	intvalold=intval
end do
write(*,"(' Final result:',f14.6,' Hartree  ',f14.3,' kJ/mol')") intval,intval*au2kJ
end subroutine




!!----------- Output spherically averaged atomic radial density, can be used for generating promolecular density
subroutine sphatmraddens
use defvar
use util
use function
implicit real*8 (a-h,o-z)
real*8,allocatable :: potx(:),poty(:),potz(:),potw(:),radpos(:),sphavgval(:)
truncrho=1D-8
rlow=0D0
rhigh=12
nsphpt=2030
nradpt=200 !Totally 200 radial points, but the number of point is truncated at truncrho
allocate(potx(nsphpt),poty(nsphpt),potz(nsphpt),potw(nsphpt),radpos(nradpt),sphavgval(nradpt))
call Lebedevgen(nsphpt,potx,poty,potz,potw)
ifinish=0
iprogstp=20
iprogcrit=iprogstp
write(*,*) "Calculating..."
!$OMP PARALLEL DO SHARED(sphavgval,radpos,ifinish,iprogcrit) PRIVATE(irad,radx,radr,isph,rnowx,rnowy,rnowz,tmpval) schedule(dynamic) NUM_THREADS(nthreads)
do irad=1,nradpt
	radx=cos(irad*pi/(nradpt+1))
	radr=(1+radx)/(1-radx) !Becke transform
	radpos(irad)=radr
	tmpval=0
	do isph=1,nsphpt
		rnowx=potx(isph)*radr
		rnowy=poty(isph)*radr
		rnowz=potz(isph)*radr
		tmpval=tmpval+fdens(rnowx,rnowy,rnowz)*potw(isph)
	end do
	sphavgval(irad)=tmpval !Spherically average density
    ifinish=ifinish+1
    if (ifinish==iprogcrit) then
		call showprog(ifinish,nradpt)
		iprogcrit=iprogcrit+iprogstp
	end if
end do
!$OMP END PARALLEL DO
open(10,file="sphavgval.txt",status="replace")
itmp=0
do irad=nradpt,1,-1
	if (sphavgval(irad)>truncrho) itmp=itmp+1
end do
write(10,"(a,i3,a)") "else if (iele==",a(1)%index,") then  !"
write(10,"('	npt=',i5)") itmp
itmp=0
do irad=nradpt,1,-1
	if (sphavgval(irad)>truncrho) then
		itmp=itmp+1
		write(10,"('	rhoarr(',i3,')=',f25.10,'D0')") itmp,sphavgval(irad)
	end if
end do
close(10)
write(*,*) "The result has been output to sphavgval.txt in current folder"
write(*,*) "The second column is radial distance (Bohr), the third column is value"
end subroutine