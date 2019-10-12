!!--------- Use suitable routine to read in the file, infomode=0/1 show/don't show info
subroutine readinfile(thisfilename,infomode)
use defvar
implicit real*8 (a-h,o-z)
character(len=*) thisfilename
character fchname*200,moldenname*200,c80tmp*20
integer infomode,inamelen
inamelen=len_trim(thisfilename)
if (infomode==0) write(*,*) "Please wait..."
if (thisfilename(inamelen-2:inamelen)=="fch".or.thisfilename(inamelen-3:inamelen)=="fchk"&
.or.thisfilename(inamelen-3:inamelen)=="FCH".or.thisfilename(inamelen-3:inamelen)=="FChk"&
.or.thisfilename(inamelen-3:inamelen)=="FCHK") then
	call readfch(thisfilename,infomode)
else if (thisfilename(inamelen-2:inamelen)=="wfn".or.thisfilename(inamelen-2:inamelen)=="WFN") then
	call readwfn(thisfilename,infomode)
else if (thisfilename(inamelen-2:inamelen)=="wfx".or.thisfilename(inamelen-2:inamelen)=="WFX") then
	call readwfx(thisfilename,infomode)
else if (thisfilename(inamelen-2:inamelen)=="chg".or.thisfilename(inamelen-2:inamelen)=="CHG") then
	call readchg(thisfilename,infomode)
else if (thisfilename(inamelen-2:inamelen)=="pqr".or.thisfilename(inamelen-2:inamelen)=="PQR") then
	call readpdb_pqr(thisfilename,infomode,1)
else if (thisfilename(inamelen-2:inamelen)=="pdb".or.thisfilename(inamelen-2:inamelen)=="PDB") then
	call readpdb_pqr(thisfilename,infomode,0)
else if (thisfilename(inamelen-2:inamelen)=="xyz".or.thisfilename(inamelen-2:inamelen)=="XYZ") then
	call readxyz(thisfilename,infomode,1)
else if (thisfilename(inamelen-1:inamelen)=="31") then
	call read31(thisfilename,infomode)
else if (thisfilename(inamelen-2:inamelen)=="grd") then
	call readgrd(thisfilename,infomode,0)
else if (thisfilename(inamelen-2:inamelen)=="vti") then
	call readvti(thisfilename,infomode,0)
else if (thisfilename(inamelen-2:inamelen)=="gro") then
	call readgro(thisfilename,infomode)
else if (thisfilename(inamelen-2:inamelen)=="cub".or.thisfilename(inamelen-3:inamelen)=="cube") then
	call readcube(thisfilename,infomode,0)
else if (thisfilename(inamelen-2:inamelen)=="gms") then
	call readgms(thisfilename,infomode)
else if (thisfilename(inamelen-2:inamelen)=="mol") then
	call readmol(thisfilename,infomode)
else if (thisfilename(inamelen-3:inamelen)=="mol2") then
	call readmol2(thisfilename,infomode)
else if (thisfilename(inamelen-2:inamelen)=="gjf") then
	call readgjf(thisfilename,infomode)
else if (index(thisfilename,".molden")/=0.or.index(thisfilename,".molden.input")/=0.or.index(thisfilename,"molden.inp")/=0) then
	!ORCA uses .molden.input as suffix, Dalton uses molden.inp
	call readmolden(thisfilename,infomode)
else if (thisfilename(inamelen-2:inamelen)=="chk") then
	if (formchkpath==" ") then
		write(*,"(a)") " Error: .chk file is not directly supported. You should use formchk to convert it to fch/fchk before loading. &
		Alternatively, you can set ""formchkpath"" parameter in settings.ini to actual path, so that Multiwfn can directly open .chk file"
		write(*,*) "Press ENTER button to exit"
		read(*,*)
		stop
	end if
	inquire(file=formchkpath,exist=alive)
	if (alive==.false.) then
		write(*,"(a)") " Note: Albeit ""formchkpath"" parameter in settings.ini has been defined, &
		the formchk executable file cannot be located, therefore the .chk file cannot be directly opened by Multiwfn"
		write(*,*) "Press ENTER button to exit"
		read(*,*)
		stop
	else
		call chk2fch(fchname)
		inquire(file=fchname,exist=alive)
		if (alive) then
            !Check file content, conversion failure may also result in file, but the content is empty
            open(10,file=fchname,status="old")
            read(10,"(a)",iostat=ierror) c80tmp
            close(10)
            if (c80tmp==" ".or.ierror/=0) then
			    write(*,*) "Error: formchk conversion failed!"
			    write(*,*) "Press ENTER button to exit"
			    read(*,*)
			    stop
            end if
			call readfch(fchname,infomode)
			call delfile(fchname)
		else
			write(*,*) "Error: formchk conversion failed!"
			write(*,*) "Press ENTER button to exit"
			read(*,*)
			stop
		end if
	end if
else if (thisfilename(inamelen-2:inamelen)=="gbw") then
	if (orca_2mklpath==" ") then
		write(*,"(a)") " Error: .gbw file is not directly supported. You should use orca_2mkl to convert it to .molden before loading. &
		Alternatively, you can set ""orca_2mklpath"" parameter in settings.ini to actual path, so that Multiwfn can directly open .gbw file"
		write(*,*) "Press ENTER button to exit"
		read(*,*)
		stop
	end if
	inquire(file=orca_2mklpath,exist=alive)
	if (alive==.false.) then
		write(*,"(a)") " Note: Albeit ""orca_2mklpath"" parameter in settings.ini has been defined, &
		the orca_2mkl executable file cannot be located, therefore the .gbw file cannot be directly opened by Multiwfn"
		write(*,*) "Press ENTER button to exit"
		read(*,*)
		stop
	else
		call gbw2molden(moldenname)
		inquire(file=moldenname,exist=alive)
		if (alive) then
			call readmolden(moldenname,infomode)
			call delfile(moldenname)
		else
			write(*,*) "Error: orca_2mkl conversion failed!"
			write(*,*) "Press ENTER button to exit"
			read(*,*)
			stop
		end if
	end if
else
	ifiletype=0
end if

!Determine how to supply EDF information for the file containing GTF information when pseudopotential basis set is involved
if (allocated(b)) then
	if (any(a%index/=nint(a%charge)) .and..not.allocated(b_EDF)) then
		if (isupplyEDF==0) then !Do nothing
			continue
		else if (isupplyEDF==1) then !Supply EDF from .wfx file
			call readEDFatmwfx
		else if (isupplyEDF==2) then !Supply EDF from bulit-in library
			call readEDFlib(infomode)
		end if	
	end if
end if
end subroutine


!!-------- Convert chk to fch/fchk via formchk, return the path of generated (may be failed) .fch/fchk file
subroutine chk2fch(fchname)
use defvar
implicit real*8 (a-h,o-z)
character*200 fchname,c200tmp
inamelen=len_trim(filename)
if (isys==1) then
	c200tmp=trim(formchkpath)//' "'//trim(filename)//'" > NUL'
else
	c200tmp=trim(formchkpath)//' "'//trim(filename)//'" > /dev/null'
end if
write(*,"(a)") " Running: "//trim(c200tmp)
call system(trim(c200tmp))
fchname=filename
if (isys==1) then
	fchname(inamelen-2:inamelen)="fch"
else
	fchname(inamelen-2:inamelen+1)="fchk"
end if
end subroutine


!!-------- Convert gbw to molden via orca_2mkl, return the path of generated (may be failed) .molden file
subroutine gbw2molden(gbwname)
use defvar
implicit real*8 (a-h,o-z)
character*200 gbwname,c200tmp
inamelen=len_trim(filename)
ico=index(filename,'.',back=.true.)
if (isys==1) then
	c200tmp=trim(orca_2mklpath)//' "'//trim(filename(:ico-1))//'" -molden > NUL'
else
	c200tmp=trim(orca_2mklpath)//' "'//trim(filename(:ico-1))//'" -molden > /dev/null'
end if
write(*,"(a)") " Running: "//trim(c200tmp)
call system(trim(c200tmp))
gbwname=trim(filename(:ico))//"molden.input"
end subroutine


!!-----------------------------------------------------------------
!!------------------------- Read gaussian formatted check file
!Some temporary arrays, including shelltype, shell2atom, shellcon, primexp, concoeff will be stored to corresponding global arrays after some revisions
!infomode=0 means output info, =1 silent
subroutine readfch(name,infomode)
use defvar
use util
implicit real*8 (a-h,o-z)
character(len=*) name
character selectyn,c80*80,fchtitle*79 !c80 for temporary store text
integer,allocatable :: shelltype(:),shell2atom(:),shellcon(:) !Degree of shell contraction
integer,allocatable :: tmparrint(:)
real*8,allocatable :: primexp(:),concoeff(:),SPconcoeff(:),amocoeff(:,:),bmocoeff(:,:),tmpmat(:,:),tmparr(:)
integer :: s2f(-5:5,21)=0 !Give shell type & orbital index to get functype
real*8 conv5d6d(6,5),conv7f10f(10,7),conv9g15g(15,9),conv11h21h(21,11)
real*8 conv5d6dtr(5,6),conv7f10ftr(7,10),conv9g15gtr(9,15),conv11h21htr(11,21)
!For backing up spherical basis functions
integer,allocatable :: shelltype5D(:),MOtype5D(:)
real*8,allocatable :: CObasa5D(:,:),CObasb5D(:,:),Sbas5D(:,:),Dbas5D(:,:,:),Magbas5D(:,:,:),MOocc5D(:),MOene5D(:),CO5D(:,:)
real*8,external :: normgau
ifiletype=1
imodwfn=0
s2f(-5,1:11)=(/ -32,-31,-30,-29,-28,-27,-26,-25,-24,-23,-22 /)
s2f(-4,1:9)=(/ -21,-20,-19,-18,-17,-16,-15,-14,-13 /)
s2f(-3,1:7)=(/ -12,-11,-10,-9,-8,-7,-6 /)
s2f(-2,1:5)=(/ -5,-4,-3,-2,-1 /)
s2f(-1,1:4)=(/ 1,2,3,4 /)
s2f(0,1)=1
s2f(1,1:3)=(/ 2,3,4 /)
s2f(2,1:6)=(/ 5,6,7,8,9,10 /)
s2f(3,1:10)=(/ 11,12,13,17,14,15,18,19,16,20 /) !Note: The sequence of f functions in Multiwfn is not identical to .fch, so convert here. While spdgh are identical
s2f(4,1:15)=(/ 21,22,23,24,25,26,27,28,29,30,31,32,33,34,35 /)
s2f(5,1:21)=(/ 36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56 /)
call gensphcartab(1,conv5d6d,conv7f10f,conv9g15g,conv11h21h)
conv5d6dtr=transpose(conv5d6d)
conv7f10ftr=transpose(conv7f10f)
conv9g15gtr=transpose(conv9g15g)
conv11h21htr=transpose(conv11h21h)

if (infomode==0.and.ifchprog==2) write(*,*) "Note: This fch file is regarded as produced by Q-Chem!"
open(10,file=name,access="sequential",status="old")
read(10,"(a)") fchtitle
if (infomode==0) write(*,*) "Loading various information of the wavefunction"
isaveNO=0
isaveNBOocc=0
isaveNBOene=0
if (index(fchtitle,'saveNBOocc')/=0.or.index(fchtitle,'SaveNBOocc')/=0) isaveNBOocc=1
if (index(fchtitle,'saveNBOene')/=0.or.index(fchtitle,'SaveNBOene')/=0) isaveNBOene=1
if (index(fchtitle,'saveNO')/=0.or.index(fchtitle,'SaveNO')/=0) isaveNO=1
if ((isaveNBOocc==1.or.isaveNBOene==1).and.infomode==0) write(*,*) "The file contains NBO information"
if (isaveNO==1.and.infomode==0) write(*,*) "The file contains natural orbitals information"
read(10,"(a)") c80
if (c80(11:11)=="R") wfntype=0
if (c80(11:11)=="U") wfntype=1
if (c80(11:12)=="RO") wfntype=2
call loclabel(10,'Number of electrons')
read(10,"(49x,f12.0)") nelec
read(10,"(49x,f12.0)") naelec
read(10,"(49x,f12.0)") nbelec
if (naelec/=nbelec.and.wfntype==0) wfntype=1 !This is often redundant, but considering that sometimes U is not properly recognized, this maybe useful
call loclabel(10,'Number of basis functions')
read(10,"(49x,i12)") nbasis
nindbasis=nbasis
call loclabel(10,'Number of independent functions',ifound,maxline=1000) !G09
if (ifound==0) call loclabel(10,'Number of independant functions',ifound,maxline=1000) !G03
if (ifound==1) read(10,"(49x,i12)") nindbasis !Number of linear independant functions
if (isaveNBOocc==1.or.isaveNBOene==1.or.isaveNO==1) then
	if (wfntype==0) wfntype=3
	if (wfntype==1) wfntype=4
end if

virialratio=2D0
call loclabel(10,'Virial Ratio',ifound)
if (ifound==1) read(10,"(49x,1PE22.15)") virialratio
totenergy=0.0D0
call loclabel(10,'Total Energy',ifound) !if no this entry, loclabel return ifound=0, else =1
if (ifound==1) read(10,"(49x,1PE22.15)") totenergy
call loclabel(10,'Atomic numbers')
read(10,"(49x,i12)") ncenter
allocate(tmparrint(ncenter))
read(10,"(6i12)") tmparrint
if (any(tmparrint==0).and.infomode==0) then
	write(*,*)
	write(*,*) "Ghost atoms (Bq) are found in this file. Do you want to load them? (y/n)"
	write(*,"(a)") " Note: If all Bq atoms have corresponding basis functions, then they should be loaded. &
	However, if no basis functions are assigned to them, in general they should not be loaded, otherwise problems or crashes may occur &
	when performing analyses based on wavefunction"
	read(*,*) selectyn
	if (selectyn=='n'.or.selectyn=='N') ncenter=count(tmparrint/=0)
end if
if (allocated(a)) deallocate(a)
allocate(a(ncenter))
a%index=tmparrint(1:ncenter)
a%name=ind2name(a%index)
call loclabel(10,'Nuclear charges') !If ECP was used, nuclear charge /= atomic number
read(10,*)
read(10,"(5(1PE16.8))") (a(i)%charge,i=1,ncenter)
call loclabel(10,'Current cartesian coordinates')
read(10,*)
read(10,"(5(1PE16.8))") (a(i)%x,a(i)%y,a(i)%z,i=1,ncenter)
call loclabel(10,'Shell types')
read(10,"(49x,i12)") nshell
allocate(shelltype(nshell))
read(10,"(6i12)") (shelltype(i),i=1,nshell)

!Note that Multiwfn allows cartesian and spherical harmonic basis functions mixed together. If any basis function is spherical harmonic type, then isphergau=1.
!Only the spherical harmonic ones will be treated specially
if (infomode==0) write(*,"(' The highest angular moment basis functions is ',a)") shtype2name(maxval(abs(shelltype))) 
isphergau=0
if (any(shelltype<=-2)) isphergau=1
if (any(abs(shelltype)>5).and.infomode==0) then
	write(*,"(a)") " Error: GTFs with angular moment higher h are found, while Multiwfn currently only support up to h. Press Enter button to exit"
	read(*,*)
	stop
end if

if (infomode==0) write(*,*) "Loading basis set definition..."
call loclabel(10,'Number of primitives per shell')
read(10,*)
allocate(shellcon(nshell))
read(10,"(6i12)") (shellcon(i),i=1,nshell)
call loclabel(10,'Shell to atom map')
read(10,*)
allocate(shell2atom(nshell))
read(10,"(6i12)") (shell2atom(i),i=1,nshell)
call loclabel(10,'Primitive exponents')
read(10,"(49x,i12)") nprimshell
allocate(primexp(nprimshell))
read(10,"(5(1PE16.8))") (primexp(i),i=1,nprimshell)
call loclabel(10,'Contraction coefficients')
read(10,*)
allocate(concoeff(nprimshell))
read(10,"(5(1PE16.8))") (concoeff(i),i=1,nprimshell)
read(10,"(a)") c80
if (index(c80,"P(S=P) Contraction coefficients")/=0) then
	backspace(10)
	read(10,*)
	allocate(SPconcoeff(nprimshell))
	read(10,"(5(1PE16.8))") (SPconcoeff(i),i=1,nprimshell)
end if

if (infomode==0) write(*,*) "Loading orbitals..."
!Note: Some basis maybe removed by linear dependence checking, hence the number of orbitals in .fch may less than nbasis(always equals to nmo in Multiwfn)
!Hence when reading information involving the number of orbitals in .fch, use nindbasis instead nmo
!The expansion coefficients, energies in those undefined orbitals are all set to zero
if (wfntype==0.or.wfntype==2.or.wfntype==3) then !Restricted/restricted open-shell, saveNO/NBO
	nmo=nbasis
	allocate(MOene(nmo))
	allocate(MOocc(nmo))
	allocate(MOtype(nmo))
	allocate(amocoeff(nmo,nbasis))
	MOtype=0
	MOocc=0D0
	MOene=0D0
	amocoeff=0D0
	if (wfntype==0.or.wfntype==3) then
		MOocc(1:nint(nelec/2))=2.0D0
	else if (wfntype==2) then
		MOocc(1:nbelec)=2.0D0  !Alpha electrons is always more than beta counterpart
		MOocc(nbelec+1:naelec)=1D0
		MOtype(nbelec+1:naelec)=1
	end if
	call loclabel(10,'Alpha Orbital Energies',ifound) 
	if (ifound==0) call loclabel(10,'orbital energies') !PSI4 since 1.2 uses "orbital energies" for closed-shell cases
	read(10,*)
	read(10,"(5(1PE16.8))") (MOene(i),i=1,nindbasis)
	call loclabel(10,'Alpha MO coefficients',ifound)
	if (ifound==0) call loclabel(10,'MO coefficients') !PSI4 since 1.2 uses "MO coefficients" for closed-shell cases
	read(10,*)
	read(10,"(5(1PE16.8))") ((amocoeff(imo,ibasis),ibasis=1,nbasis),imo=1,nindbasis)
else if (wfntype==1.or.wfntype==4) then !Unrestricted wavefunction, open-shell post-HF wavefunction
	!Note that PSI4 since 1.2 uses lower case, this has been taken into account
	nmo=2*nbasis
	allocate(MOene(nmo))
	allocate(MOocc(nmo))
	allocate(MOtype(nmo))
	allocate(amocoeff(nbasis,nbasis))
	allocate(bmocoeff(nbasis,nbasis))
	MOocc=0D0
	MOocc(1:naelec)=1D0
	MOocc(nbasis+1:nbasis+nbelec)=1D0
	MOtype(1:nbasis)=1
	MOtype(nbasis+1:nmo)=2
	MOene=0D0
	amocoeff=0D0
	bmocoeff=0D0
	call loclabel(10,'Alpha Orbital Energies',ifound)
	if (ifound==0) call loclabel(10,'alpha orbital energies') !For PSI4
	read(10,*)
	read(10,"(5(1PE16.8))") (MOene(i),i=1,nindbasis)
	call loclabel(10,'Beta Orbital Energies',ifound)
	if (ifound==0) call loclabel(10,'beta orbital energies',ifound) !For PSI4
	if (ifound==0) then !For fch file generated by MCSCF for systems with multiplicity >1, no beta orbital will be presented 
		write(*,*) "Error: Beta orbital information was not found but expected!"
		write(*,*) "Press ENTER button to exit"
		read(*,*)
		stop
	end if
	read(10,*)
	read(10,"(5(1PE16.8))") (MOene(i),i=nbasis+1,nbasis+nindbasis)

	call loclabel(10,'lpha MO coefficients') !Compatible with PSI4
	read(10,*)
	read(10,"(5(1PE16.8))") ((amocoeff(imo,ibasis),ibasis=1,nbasis),imo=1,nindbasis)
	call loclabel(10,'eta MO coefficients') !Compatible with PSI4
	read(10,*)
	read(10,"(5(1PE16.8))") ((bmocoeff(imo,ibasis),ibasis=1,nbasis),imo=1,nindbasis)
end if

if (isaveNBOocc==1.or.isaveNO==1) then
	MOocc=MOene
	MOene=0.0D0
end if
if (isaveNBOene==1) MOocc=0.0D0 !For saveNBO, the automatically determined occupation number is meaningless
where (MOocc==1000) MOocc=0.0D0 !When saveNBO is used, the latest several occupation/energy of NBO are 1000, we modify them to zero
where (MOene==1000) MOene=0.0D0

close(10)

!!!!!! Reading have finished, now generate basis information

!Backup spherical basis information (some of them may be Cartesian ones) with 5D suffix (of course, may be actually 7f, 9g, 11h...),
!convert them to cartesian type temporarily, at final stage recover them back, namely get Sbas, Ptot... in spherical basis
if (isphergau==1) then
	allocate(shelltype5D(nshell))
	shelltype5D=shelltype
	where (shelltype<=-2) shelltype=-shelltype !Convert to cartesian type
	nbasis5D=nbasis
	nbasis=0
	do i=1,nshell
		nbasis=nbasis+shtype2nbas(shelltype(i))
	end do
end if

!Allocate space for arrays
nprims=0
do i=1,nshell
	nprims=nprims+shtype2nbas(shelltype(i))*shellcon(i)
end do
allocate(b(nprims),co(nmo,nprims),basshell(nbasis),bascen(nbasis),bastype(nbasis),primstart(nbasis),&
primend(nbasis),primconnorm(nprims),basstart(ncenter),basend(ncenter))
!Fill Cobasa and CObasb
if (isphergau==0) then
	allocate(CObasa(nbasis,nbasis))
	CObasa=transpose(amocoeff)
	if (wfntype==1.or.wfntype==4) then
		allocate(CObasb(nbasis,nbasis))
		CObasb=transpose(bmocoeff)
	end if
else if (isphergau==1) then
	allocate(CObasa(nbasis,nbasis),CObasa5D(nbasis5D,nbasis5D))
	CObasa5D=transpose(amocoeff)
	CObasa=0D0
	if (wfntype==1.or.wfntype==4) then
		allocate(CObasb(nbasis,nbasis),CObasb5D(nbasis5D,nbasis5D))
		CObasb5D=transpose(bmocoeff)
		CObasb=0D0
	end if
	!Map 5D coefficient to 6D coefficient
	ipos5D=1
	ipos6D=1
	do ish=1,nshell
		ishtyp5D=shelltype5D(ish)
		ishtyp6D=shelltype(ish)
		numshorb5D=shtype2nbas(ishtyp5D)
		numshorb6D=shtype2nbas(ishtyp6D)
		if (ishtyp5D>=-1) then !S or P or SP or other Cartesian shells
			CObasa(ipos6D:ipos6D+numshorb6D-1,1:nbasis5D)=CObasa5D(ipos5D:ipos5D+numshorb5D-1,:)
			if (wfntype==1.or.wfntype==4) CObasb(ipos6D:ipos6D+numshorb6D-1,1:nbasis5D)=CObasb5D(ipos5D:ipos5D+numshorb5D-1,:)			
		else if (ishtyp5D==-2) then
			!5D->6D
			CObasa(ipos6D:ipos6D+5,1:nbasis5D)=matmul(conv5d6d,CObasa5D(ipos5D:ipos5D+4,:))
			if (wfntype==1.or.wfntype==4) CObasb(ipos6D:ipos6D+5,1:nbasis5D)=matmul(conv5d6d,CObasb5D(ipos5D:ipos5D+4,:))
		else if (ishtyp5D==-3) then
			!7F->10F
			CObasa(ipos6D:ipos6D+9,1:nbasis5D)=matmul(conv7f10f,CObasa5D(ipos5D:ipos5D+6,:))
			if (wfntype==1.or.wfntype==4) CObasb(ipos6D:ipos6D+9,1:nbasis5D)=matmul(conv7f10f,CObasb5D(ipos5D:ipos5D+6,:))
		else if (ishtyp5D==-4) then
			!9G->15G
			CObasa(ipos6D:ipos6D+14,1:nbasis5D)=matmul(conv9g15g,CObasa5D(ipos5D:ipos5D+8,:))
			if (wfntype==1.or.wfntype==4) CObasb(ipos6D:ipos6D+14,1:nbasis5D)=matmul(conv9g15g,CObasb5D(ipos5D:ipos5D+8,:))
		else if (ishtyp5D==-5) then
			!11H->21H
			CObasa(ipos6D:ipos6D+20,1:nbasis5D)=matmul(conv11h21h,CObasa5D(ipos5D:ipos5D+10,:))
			if (wfntype==1.or.wfntype==4) CObasb(ipos6D:ipos6D+20,1:nbasis5D)=matmul(conv11h21h,CObasb5D(ipos5D:ipos5D+10,:))
		end if
		ipos5D=ipos5D+numshorb5D
		ipos6D=ipos6D+numshorb6D
	end do
end if

if (infomode==0) write(*,*) "Converting basis function information to GTF information..."
!Distribute exponent, functype to every GTF and generate CO(:,:) from amocoeff/bmocoeff
!Fill: b,basshell,bascen,bastype,co,primstart,primend,primconnorm
k=1 !Current position of GTF
iexp=1
ibasis=1 !Current position of basis
!Note: Below commented with !!! means the line associated to setting basis information
do i=1,nshell !cycle each shell
	b(k:k+shellcon(i)*shtype2nbas(shelltype(i))-1)%center=shell2atom(i)
	basshell(ibasis:ibasis+shtype2nbas(shelltype(i))-1)=i !!! Set basis attributed to which shell
	bascen(ibasis:ibasis+shtype2nbas(shelltype(i))-1)=shell2atom(i) !!! Set basis attributed to which center
	do j=1,shtype2nbas(shelltype(i)) !cycle each basis(orbital) in each shell
		b(k:k+shellcon(i)-1)%type=s2f(shelltype(i),j)
		bastype(ibasis)=s2f(shelltype(i),j) !!! set basis type
		primstart(ibasis)=k !!! From where the GTFs attributed to ibasis'th basis
		primend(ibasis)=k+shellcon(i)-1 !!! To where the GTFs attributed to ibasis'th basis
        !write(*,*) i,j,ibasis,primstart(ibasis),primend(ibasis)
		do l=1,shellcon(i) !cycle each GTF in each basis in each shell
			b(k)%exp=primexp(iexp+l-1)
			tnormgau=normgau(b(k)%type,b(k)%exp)  !!!Normalization coefficient of GTFs
			if (ifchprog==2) tnormgau=1D0 !In the .fch file of Q-chem, normalization factor of GTF has already been multiplied into contraction coefficient of GTFs
			temp=concoeff(iexp+l-1)  !!!Contraction coefficient of GTFs
			if (shelltype(i)==-1.and.j/=1) temp=SPconcoeff(iexp+l-1)
			primconnorm(k)=temp*tnormgau !Combines contraction and normalization coefficient
			do imo=1,nmo
				if (wfntype==0.or.wfntype==2.or.wfntype==3) then !R or RO
					co(imo,k)=CObasa(ibasis,imo)*temp*tnormgau
				else if (wfntype==1.or.wfntype==4) then !U
					if (isphergau==1) then
						if (imo<=nbasis5D) co(imo,k)=CObasa(ibasis,imo)*temp*tnormgau
						if (imo>nbasis5D) co(imo,k)=CObasb(ibasis,imo-nbasis5D)*temp*tnormgau
					else
						if (imo<=nbasis) co(imo,k)=CObasa(ibasis,imo)*temp*tnormgau
						if (imo>nbasis) co(imo,k)=CObasb(ibasis,imo-nbasis)*temp*tnormgau
					end if
				end if
			end do
			k=k+1
		end do
		ibasis=ibasis+1
	end do
	iexp=iexp+shellcon(i)
end do

!Generate overlap matrix and dipole moment integral matrix for Cartesian Gaussian basis functions
if (infomode==0) write(*,*) "Generating overlap matrix..."
allocate(Sbas(nbasis,nbasis))
call genSbas
if (igenDbas==1) then
	if (infomode==0) write(*,*) "Generating electric dipole moment integral matrix..."
	allocate(Dbas(3,nbasis,nbasis))
	call genDbas
end if
if (igenMagbas==1) then
	if (infomode==0) write(*,*) "Generating magnetic dipole moment integral matrix..."
	allocate(Magbas(3,nbasis,nbasis))
	call genMagbas
end if

!Convert information from Cartesian basis to spherical basis
if (isphergau==1) then
	if (iloadasCart==1) then !For special purpose, keep Cartesian basis functions, e.g. convert spherical .fch/.molden to .47 file
		!Currently nbasis and dimension of all matrix (except for CO) correspond to full Cartesian case, but nmo &
		!and related arrays as well as CO still correspond to spherical harmonic case and thus need to be "expanded", the MO energies are artifically set to 100
		allocate(MOocc5D(nmo),MOene5D(nmo),MOtype5D(nmo),CO5D(nmo,nprims))
		MOocc5D=MOocc
		MOene5D=MOene
		MOtype5D=MOtype
		CO5D=CO
		deallocate(MOocc,MOene,MOtype,CO)
		if (wfntype==0.or.wfntype==2.or.wfntype==3) nmo=nbasis !R, RO
		if (wfntype==1.or.wfntype==4) nmo=2*nbasis !U
		allocate(MOocc(nmo),MOene(nmo),MOtype(nmo),CO(nmo,nprims))
		MOocc=0
		MOene=100
		CO=0
		if (wfntype==0.or.wfntype==2.or.wfntype==3) then !R, RO
			MOtype=0
			MOocc(1:nbasis5D)=MOocc5D
			MOene(1:nbasis5D)=MOene5D
			MOtype(1:nbasis5D)=MOtype5D
			CO(1:nbasis5D,:)=CO5D
		else !U
			MOtype(:nbasis)=1
			MOtype(nbasis+1:)=2
			MOocc(:nbasis5D)=MOocc5D(:nbasis5D)
			MOocc(nbasis+1:nbasis+nbasis5D)=MOocc5D(nbasis5D+1:)
			MOene(:nbasis5D)=MOene5D(:nbasis5D)
			MOene(nbasis+1:nbasis+nbasis5D)=MOene5D(nbasis5D+1:)
			MOtype(:nbasis5D)=MOtype5D(:nbasis5D)
			MOtype(nbasis+1:nbasis+nbasis5D)=MOtype5D(nbasis5D+1:)
			CO(:nbasis5D,:)=CO5D(:nbasis5D,:)
			CO(nbasis+1:nbasis+nbasis5D,:)=CO5D(nbasis5D+1:,:)
		end if
		isphergau=0
		
	else !Commonly case, transform to spherical harmonic functions
		if (infomode==0) write(*,*) "Back converting basis function information from Cartesian to spherical type..."
		!Map cartesian overlap matrix to spherical
		allocate(sbas5D(nbasis5D,nbasis5D))
		if (igenDbas==1) allocate(Dbas5D(3,nbasis5D,nbasis5D))
		if (igenMagbas==1) allocate(Magbas5D(3,nbasis5D,nbasis5D))
		ipos5D=1
		ipos6D=1
		do ish=1,nshell
			ishtyp5D=shelltype5D(ish)
			ishtyp6D=shelltype(ish)
			numshorb5D=shtype2nbas(ishtyp5D)
			numshorb6D=shtype2nbas(ishtyp6D)
			!Now contract columns of Sbas
			if (ishtyp5D>=-1) sbas(:,ipos5D:ipos5D+numshorb5D-1)=sbas(:,ipos6D:ipos6D+numshorb6D-1) !S, P, SP or other Cartesian shells
			if (ishtyp5D==-2) sbas(:,ipos5D:ipos5D+numshorb5D-1)=matmul(sbas(:,ipos6D:ipos6D+numshorb6D-1),conv5d6d)
			if (ishtyp5D==-3) sbas(:,ipos5D:ipos5D+numshorb5D-1)=matmul(sbas(:,ipos6D:ipos6D+numshorb6D-1),conv7f10f)
			if (ishtyp5D==-4) sbas(:,ipos5D:ipos5D+numshorb5D-1)=matmul(sbas(:,ipos6D:ipos6D+numshorb6D-1),conv9g15g)
			if (ishtyp5D==-5) sbas(:,ipos5D:ipos5D+numshorb5D-1)=matmul(sbas(:,ipos6D:ipos6D+numshorb6D-1),conv11h21h)
			!Now contract rows of Sbas
			if (ishtyp5D>=-1) sbas(ipos5D:ipos5D+numshorb5D-1,:)=sbas(ipos6D:ipos6D+numshorb6D-1,:) !S, P, SP or other Cartesian shells
			if (ishtyp5D==-2) sbas(ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv5d6dtr,sbas(ipos6D:ipos6D+numshorb6D-1,:))
			if (ishtyp5D==-3) sbas(ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv7f10ftr,sbas(ipos6D:ipos6D+numshorb6D-1,:))
			if (ishtyp5D==-4) sbas(ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv9g15gtr,sbas(ipos6D:ipos6D+numshorb6D-1,:))
			if (ishtyp5D==-5) sbas(ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv11h21htr,sbas(ipos6D:ipos6D+numshorb6D-1,:))
			
			if (igenDbas==1) then
				do idir=1,3
					!Now contract columns of Dbas
					if (ishtyp5D>=-1) Dbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=Dbas(idir,:,ipos6D:ipos6D+numshorb6D-1) !S, P, SP or other Cartesian shells
					if (ishtyp5D==-2) Dbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Dbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv5d6d)
					if (ishtyp5D==-3) Dbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Dbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv7f10f)
					if (ishtyp5D==-4) Dbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Dbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv9g15g)
					if (ishtyp5D==-5) Dbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Dbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv11h21h)
					!Now contract rows of Dbas
					if (ishtyp5D>=-1) Dbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=Dbas(idir,ipos6D:ipos6D+numshorb6D-1,:) !S, P, SP or other Cartesian shells
					if (ishtyp5D==-2) Dbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv5d6dtr,Dbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
					if (ishtyp5D==-3) Dbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv7f10ftr,Dbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
					if (ishtyp5D==-4) Dbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv9g15gtr,Dbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
					if (ishtyp5D==-5) Dbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv11h21htr,Dbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
				end do
			end if
			if (igenMagbas==1) then
				do idir=1,3
					!Now contract columns of Magbas
					if (ishtyp5D>=-1) Magbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=Magbas(idir,:,ipos6D:ipos6D+numshorb6D-1) !S, P, SP or other Cartesian shells
					if (ishtyp5D==-2) Magbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Magbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv5d6d)
					if (ishtyp5D==-3) Magbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Magbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv7f10f)
					if (ishtyp5D==-4) Magbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Magbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv9g15g)
					if (ishtyp5D==-5) Magbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Magbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv11h21h)
					!Now contract rows of Magbas
					if (ishtyp5D>=-1) Magbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=Magbas(idir,ipos6D:ipos6D+numshorb6D-1,:) !S, P, SP or other Cartesian shells
					if (ishtyp5D==-2) Magbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv5d6dtr,Magbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
					if (ishtyp5D==-3) Magbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv7f10ftr,Magbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
					if (ishtyp5D==-4) Magbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv9g15gtr,Magbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
					if (ishtyp5D==-5) Magbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv11h21htr,Magbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
				end do
			end if
			ipos5D=ipos5D+numshorb5D
			ipos6D=ipos6D+numshorb6D
		end do
		sbas5D=sbas(1:nbasis5D,1:nbasis5D)
		if (igenDbas==1) Dbas5D=Dbas(:,1:nbasis5D,1:nbasis5D)
		if (igenMagbas==1) Magbas5D=Magbas(:,1:nbasis5D,1:nbasis5D)
	! 	where (abs(sbas5D)<1D-9) sbas5D=0D0 !Ignore too small value to avoid confusing insight
	!Test if the sbas generated by Multiwfn is consistent with Gaussian IOP(3/33=1)
	! 	open(15,file="x\cof3_5d.out",status="old")
	! 	allocate(tmpmat(nbasis5D,nbasis5D))
	! 	call loclabel(15,"*** Overlap ***")
	! 	call readmatgau(15,tmpmat,1)
	! 	close(15)
	! 	write(*,*) maxval(abs(tmpmat-sbas5D))

		!Recover spherical Gaussian basis function information
		nbasis=nbasis5D
		shelltype=shelltype5D
		ibasis=1
		do i=1,nshell
			basshell(ibasis:ibasis+shtype2nbas(shelltype(i))-1)=i
			bascen(ibasis:ibasis+shtype2nbas(shelltype(i))-1)=shell2atom(i)
			do j=1,shtype2nbas(shelltype(i))
				bastype(ibasis)=s2f(shelltype(i),j)
				ibasis=ibasis+1
			end do
		end do
		deallocate(CObasa)
		allocate(CObasa(nbasis,nbasis))
		CObasa=CObasa5D
		if (wfntype==1.or.wfntype==4) then
			deallocate(CObasb)
			allocate(CObasb(nbasis,nbasis))
			CObasb=CObasb5D
		end if
		deallocate(sbas)
		allocate(sbas(nbasis,nbasis))
		sbas=sbas5D
		if (igenDbas==1) then
			deallocate(Dbas)
			allocate(Dbas(3,nbasis,nbasis))
			Dbas=Dbas5D
		end if
		if (igenMagbas==1) then
			deallocate(Magbas)
			allocate(Magbas(3,nbasis,nbasis))
			Magbas=Magbas5D
		end if
	end if
end if

!Split SP shell to S and P shells
noldshell=nshell
noldprimshell=nprimshell
ibasis=1
do i=1,nshell !Count how many basis shells and primitive shells after split SP as S and P, and meantime update basshell
	if (shelltype(i)==-1) then
		nshell=nshell+1
		nprimshell=nprimshell+shellcon(i)
		basshell(ibasis+1:nbasis)=basshell(ibasis+1:nbasis)+1 !The shell index of the basis function after current one should be augmented by 1, since current shell is splitted
	end if
	ibasis=ibasis+shtype2nbas(shelltype(i))
end do
allocate(shtype(nshell),shcen(nshell),shcon(nshell),primshexp(nprimshell),primshcoeff(nprimshell)) !Global array
jsh=1 !New basis shell index
iprsh=1 !Old primitive shell index
jprsh=1 !New primitive shell index
do ish=1,noldshell !Finally determine global arrays shtype,shcen,shcon,primshexp,primshcoeff shell arrays, in which SP shells are not presented
	ncon=shellcon(ish)
	if (shelltype(ish)/=-1) then !Normal shell
		shtype(jsh)=shelltype(ish)
		shcen(jsh)=shell2atom(ish)
		shcon(jsh)=ncon
		primshexp(jprsh:jprsh+ncon-1)=primexp(iprsh:iprsh+ncon-1)
		primshcoeff(jprsh:jprsh+ncon-1)=concoeff(iprsh:iprsh+ncon-1)
		jsh=jsh+1
		jprsh=jprsh+ncon
	else !SP shell
		shtype(jsh)=0 !S
		shtype(jsh+1)=1 !P
		shcen(jsh:jsh+1)=shell2atom(ish)
		shcon(jsh:jsh+1)=ncon
		primshexp(jprsh:jprsh+ncon-1)=primexp(iprsh:iprsh+ncon-1)
		primshexp(jprsh+ncon:jprsh+2*ncon-1)=primexp(iprsh:iprsh+ncon-1)
		primshcoeff(jprsh:jprsh+ncon-1)=concoeff(iprsh:iprsh+ncon-1)
		primshcoeff(jprsh+ncon:jprsh+2*ncon-1)=SPconcoeff(iprsh:iprsh+ncon-1)
		jsh=jsh+2
		jprsh=jprsh+2*ncon
	end if
	iprsh=iprsh+ncon
end do

!Generate basstart and basend
nowcen=0
indcen=0
basstart=0 !Bq atom without basis function will have 0 for basstart and basend
basend=0
do ibasis=1,nbasis
	if (bascen(ibasis)/=nowcen) then
		nowcen=bascen(ibasis)
		indcen=indcen+1
		basstart(indcen)=ibasis
		if (indcen/=1) basend(indcen-1)=ibasis-1
	end if
end do
basend(ncenter)=nbasis

!Generate one-particle density matrix for basis functions
if (igenP==1) then
	if (infomode==0) then
		if (isaveNO==0) write(*,*) "Generating density matrix based on SCF orbitals..."
		if (isaveNO==1) write(*,*) "Generating density matrix based on natural orbitals..."
	end if
	call genP
end if

!Output summary of present wavefunction
if (infomode==0) then
	write(*,*)
	write(*,"(' Total/Alpha/Beta electrons:',3f12.4)") nelec,naelec,nbelec
	write(*,"(' Net charge:',f12.5,'      Expected multiplicity:',i5)") sum(a(:)%charge)-nelec,nint(naelec-nbelec)+1
	write(*,"(' Atoms:',i7,',  Basis functions:',i7,',  GTFs:',i7)") ncenter,nbasis,nprims
	write(*,"(' Total energy:',f21.12,' Hartree,   Virial ratio:',f12.8)") totenergy,virialratio
	if (wfntype==0) then
		write(*,"(' This is a restricted single-determinant wavefunction')")
		write(*,"(' Orbitals from 1 to',i6,' are occupied')") nint(nelec/2)
	else if (wfntype==1) then
		write(*,"(' This is an unrestricted single-determinant wavefunction')")
		write(*,"(' Orbitals from ',i6,' to',i6,' are alpha, from',i6,' to',i6,' are occupied')") 1,nbasis,1,nint(naelec)
		write(*,"(' Orbitals from ',i6,' to',i6,' are beta,  from',i6,' to',i6,' are occupied')") nbasis+1,nmo,nbasis+1,nbasis+nint(nbelec)
	else if (wfntype==2) then
		write(*,"(' This is a restricted open-shell wavefunction')")
		write(*,"(' Orbitals from',i6,' to',i6,' are doubly occupied')") 1,nint(nbelec)
		write(*,"(' Orbitals from',i6,' to',i6,' are singly occupied')") nint(nbelec)+1,nint(naelec)
	else if (wfntype==3) then
		write(*,"(' This is a restricted post-HF wavefunction')")
	else if (wfntype==4) then
		write(*,"(' This is an unrestricted post-HF wavefunction')")
		write(*,"(' Orbitals from ',i6,' to',i6,' are alpha, from',i6,' to',i6,' are beta')") 1,nbasis,nbasis+1,nmo
	end if
	write(*,"(' Title line of this file: ',a)") trim(fchtitle)
end if

!Find out index of HOMO, will be used in some cases, only for RHF
if (wfntype==0) then
	do idxHOMO=nmo,1,-1
		if (nint(MOocc(idxHOMO))==2D0) exit
	end do
end if
end subroutine



!!--------------------------------------------------------------------
!!-------------- Read Gaussian formatted check file for AdNDP analysis
!Cobasa is not read from .fch file, which has already written by AdNDP module, which contains AO expansion coefficients of candidate or saved orbitals
subroutine readfchadndp(fchfilename,iusespin,orbocc,adndpCObas,numorb)
use defvar
use util
implicit real*8 (a-h,o-z)
character fchfilename*200,c80*80
integer iusespin
real*8 orbocc(numorb),adndpCObas(nbasis,numorb)
integer,allocatable :: shelltype(:),shell2atom(:),shellcon(:) !Degree of shell contraction
real*8,allocatable :: primexp(:),concoeff(:),SPconcoeff(:)
integer :: s2f(-5:5,21)=0 !Give shell type & orbital index to get functype
real*8 conv5d6d(6,5),conv7f10f(10,7),conv9g15g(15,9),conv11h21h(21,11)
 !For backing up spherical basis functions
integer,allocatable :: shelltype5D(:)
real*8,allocatable :: CObasa5D(:,:)
real*8,external :: normgau
nmo=numorb
s2f(-5,1:11)=(/ -32,-31,-30,-29,-28,-27,-26,-25,-24,-23,-22 /)
s2f(-4,1:9)=(/ -21,-20,-19,-18,-17,-16,-15,-14,-13 /)
s2f(-3,1:7)=(/ -12,-11,-10,-9,-8,-7,-6 /)
s2f(-2,1:5)=(/ -5,-4,-3,-2,-1 /)
s2f(-1,1:4)=(/ 1,2,3,4 /)
s2f(0,1)=1
s2f(1,1:3)=(/ 2,3,4 /)
s2f(2,1:6)=(/ 5,6,7,8,9,10 /)
s2f(3,1:10)=(/ 11,12,13,17,14,15,18,19,16,20 /)
s2f(4,1:15)=(/ 21,22,23,24,25,26,27,28,29,30,31,32,33,34,35 /)
s2f(5,1:21)=(/ 36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56 /)
call gensphcartab(1,conv5d6d,conv7f10f,conv9g15g,conv11h21h)

open(10,file=fchfilename,access="sequential",status="old")

! if (.not.allocated(a)) then !Hasn't loaded some information before
! 	call loclabel(10,'Atomic numbers')
! 	read(10,"(49x,i12)") ncenter
! 	allocate(a(ncenter))
! 	read(10,"(6i12)") (a(i)%index,i=1,ncenter)
! 	a%name=ind2name(a%index)
	call loclabel(10,'Nuclear charges') !If ECP was used, nuclear charge /= atomic number
	read(10,*)
	read(10,"(5(1PE16.8))") (a(i)%charge,i=1,ncenter)
	call loclabel(10,'Current cartesian coordinates')
	read(10,*)
	read(10,"(5(1PE16.8))") (a(i)%x,a(i)%y,a(i)%z,i=1,ncenter)
! end if

call loclabel(10,'Shell types')
read(10,"(49x,i12)") nshell
allocate(shelltype(nshell))
read(10,"(6i12)") (shelltype(i),i=1,nshell)
isphergau=0
if (any(shelltype<=-2)) isphergau=1
call loclabel(10,'Number of primitives per shell')
read(10,*)
allocate(shellcon(nshell))
read(10,"(6i12)") (shellcon(i),i=1,nshell)
call loclabel(10,'Shell to atom map')
read(10,*)
allocate(shell2atom(nshell))
read(10,"(6i12)") (shell2atom(i),i=1,nshell)
call loclabel(10,'Primitive exponents')
read(10,"(49x,i12)") nprimshell
allocate(primexp(nprimshell))
read(10,"(5(1PE16.8))") (primexp(i),i=1,nprimshell)
call loclabel(10,'Contraction coefficients')
read(10,*)
allocate(concoeff(nprimshell))
read(10,"(5(1PE16.8))") (concoeff(i),i=1,nprimshell)
read(10,"(a)") c80
if (index(c80,"P(S=P) Contraction coefficients")/=0) then
	backspace(10)
	read(10,*)
	allocate(SPconcoeff(nprimshell))
	read(10,"(5(1PE16.8))") (SPconcoeff(i),i=1,nprimshell)
end if

if (allocated(MOene)) deallocate(MOene,MOocc,MOtype)
allocate(MOene(nmo),MOocc(nmo),MOtype(nmo))
MOene=0D0
MOocc=orbocc
MOtype=iusespin

if (isphergau==1) then
	allocate(shelltype5D(nshell))
	shelltype5D=shelltype
	where (shelltype<=-2) shelltype=-shelltype !Convert to cartesian type
	nbasis5D=nbasis
	nbasis=0
	do i=1,nshell
		nbasis=nbasis+shtype2nbas(shelltype(i))
	end do
end if

!Allocate space for arrays
nprims=0
do i=1,nshell
	nprims=nprims+shtype2nbas(shelltype(i))*shellcon(i)
end do
if (.not.allocated(b)) allocate(b(nprims))
if (allocated(CO)) deallocate(CO)
allocate(CO(nmo,nprims))

if (isphergau==0) then
	if (allocated(CObasa)) deallocate(CObasa)
	allocate(CObasa(nbasis,nmo))
	CObasa=adndpCObas
else if (isphergau==1) then
	if (allocated(CObasa)) deallocate(CObasa)
	allocate(CObasa(nbasis,nmo),CObasa5D(nbasis5D,nmo))
	CObasa5D=adndpCObas
	CObasa=0D0
	
	!Map 5D coefficient to 6D coefficient
	ipos5D=1
	ipos6D=1
	do ish=1,nshell
		ishtyp5D=shelltype5D(ish)
		ishtyp6D=shelltype(ish)
		numshorb5D=shtype2nbas(ishtyp5D)
		numshorb6D=shtype2nbas(ishtyp6D)
		if (ishtyp5D==0.or.ishtyp5D==1.or.ishtyp5D==-1) then !S or P or SP
			CObasa(ipos6D:ipos6D+numshorb6D-1,1:nmo)=CObasa5D(ipos5D:ipos5D+numshorb5D-1,1:nmo)
		else if (ishtyp5D==-2) then
			!5D->6D
			CObasa(ipos6D:ipos6D+5,1:nmo)=matmul(conv5d6d,CObasa5D(ipos5D:ipos5D+4,1:nmo))
		else if (ishtyp5D==-3) then
			!7F->10F
			CObasa(ipos6D:ipos6D+9,1:nmo)=matmul(conv7f10f,CObasa5D(ipos5D:ipos5D+6,1:nmo))
		else if (ishtyp5D==-4) then
			!9G->15G
			CObasa(ipos6D:ipos6D+14,1:nmo)=matmul(conv9g15g,CObasa5D(ipos5D:ipos5D+8,1:nmo))
		else if (ishtyp5D==-5) then
			!11H->21H
			CObasa(ipos6D:ipos6D+20,1:nmo)=matmul(conv11h21h,CObasa5D(ipos5D:ipos5D+10,1:nmo))
		end if
		ipos5D=ipos5D+numshorb5D
		ipos6D=ipos6D+numshorb6D
	end do
end if

!Distribute exponent, functype to every GTF and generate CO(:,:) from CObasa
k=1 !current position of GTF
iexp=1
ibasis=1 !current position of basis
!Note: Below commented with !!! means the line associated to setting basis information
do i=1,nshell !cycle each shell
	b(k:k+shellcon(i)*shtype2nbas(shelltype(i))-1)%center=shell2atom(i)
	do j=1,shtype2nbas(shelltype(i)) !cycle each basis(orbital) in each shell
		b(k:k+shellcon(i)-1)%type=s2f(shelltype(i),j)
		do l=1,shellcon(i) !cycle each GTF in each basis in each shell
			b(k)%exp=primexp(iexp+l-1)
			tnormgau=normgau(b(k)%type,b(k)%exp)
			temp=concoeff(iexp+l-1)
			if (shelltype(i)==-1.and.j/=1) temp=SPconcoeff(iexp+l-1)
			do imo=1,nmo
				CO(imo,k)=CObasa(ibasis,imo)*temp*tnormgau
			end do
			k=k+1
		end do
		ibasis=ibasis+1
	end do
	iexp=iexp+shellcon(i)
end do

close(10)
end subroutine



!!------------------------------------------------------------------------
!!------------- Read .chg file that only contain atomic charge information
subroutine readchg(name,infomode) !infomode=0 means output info, =1 silent
use defvar
use util
implicit real*8 (a-h,o-z)
character(len=*) name
integer i
real*8 dipx,dipy,dipz,tdip
ifiletype=4
open(10,file=name,access="sequential",status="old")
ncenter=totlinenum(10,1)
allocate(a(ncenter))
do i=1,ncenter
	read(10,*) a(i)%name,a(i)%x,a(i)%y,a(i)%z,a(i)%charge
	do j=0,nelesupp
		if (a(i)%name==ind2name(j)) then
			a(i)%index=j
			exit
		end if
		if (j==nelesupp) write(*,*) "Warning: Found unknown element!"
	end do
end do
close(10)
a%x=a%x/b2a
a%y=a%y/b2a
a%z=a%z/b2a
dipx=sum(a(:)%x*a(:)%charge)
dipy=sum(a(:)%y*a(:)%charge)
dipz=sum(a(:)%z*a(:)%charge)
if (infomode==0) then
	write(*,"(' Totally',i8,' atoms')") ncenter
	write(*,"(' Sum of all atomic charges:',f12.6)") sum(a%charge)
	write(*,"(' Component of electric dipole moment:')")
	write(*,"(' X=',f12.6,' a.u.  (',f12.6,' Debye )')") dipx,dipx*au2debye
	write(*,"(' Y=',f12.6,' a.u.  (',f12.6,' Debye )')") dipy,dipy*au2debye
	write(*,"(' Z=',f12.6,' a.u.  (',f12.6,' Debye )')") dipz,dipz*au2debye
	tdip=dsqrt(dipx**2+dipy**2+dipz**2)
	write(*,"(' Total electric dipole moment:',f12.6,' a.u.  (',f12.6,' Debye )')") tdip,tdip*au2debye
end if
end subroutine



!!----------------------------------------------------------------
!!-------------------- Read .pdb or .pqr file --------------------
! infomode=0: Output summary, =1: do not
! ipqr=0: read as pdb, =1: read as pqr
subroutine readpdb_pqr(name,infomode,ipqr)
use defvar
use util
implicit real*8 (a-h,o-z)
integer infomode,ipqr
character(len=*) name
character test*6,tmpname*4,tmpname_up*4,element*3
character c80tmp*80
ifiletype=5
open(10,file=name,access="sequential",status="old")
ncenter=0
do while(.true.)
	read(10,"(a6)",iostat=ierror) test
	if (ierror/=0) exit
	if (test=="HETATM".OR.test=="ATOM  ") ncenter=ncenter+1
end do
rewind(10)
allocate(a(ncenter))
i=0
do while(.true.)
	read(10,"(a6)",iostat=ierror) test
	if (ierror/=0) exit
	if (test=="HETATM".or.test=="ATOM  ") then
		backspace(10)
		i=i+1
		read(10,"(12x,a4,14x,3f8.3,22x,a3)") tmpname,a(i)%x,a(i)%y,a(i)%z,element
		if (ipqr==1) then !Use free-format to load atomic charge
			backspace(10)
			read(10,"(a)") c80tmp
			read(c80tmp(55:76),*) a(i)%charge
		end if
		element=adjustl(element)
		ifound=0
		!Use "element" term to determine actual element
		do j=1,nelesupp
			if (ind2name_up(j)==element(1:2).or.ind2name(j)==element(1:2)) then
				a(i)%index=j
				ifound=1
				exit
			end if
		end do
		!"Element" term is missing, use atomic name to determine element
		if (ifound==0) then
			tmpname_up=adjustl(tmpname)
			call strlc2uc(tmpname_up)
			do j=1,nelesupp
				if (ind2name_up(j)==tmpname_up(1:1)//' ' .or. ind2name_up(j)==tmpname_up(1:2) .or. & !Recognize e.g. C, N11, Li
				((ichar(tmpname_up(1:1))<=57).and.ind2name_up(j)==tmpname_up(2:2)//' ')) then !Recognize such as 1H5*
					a(i)%index=j
					ifound=1
					exit
				end if
			end do
		end if
		if (ifound==0) then
			write(*,"(3a)") " Warning: Found unknown element """,tmpname,""" , assume it is carbon"
			a(i)%index=12
		end if
		if (ipqr==0) a(i)%charge=a(i)%index
		a(i)%name=ind2name(a(i)%index)
	end if
end do
close(10)
a%x=a%x/b2a
a%y=a%y/b2a
a%z=a%z/b2a
if (infomode==0) then
	write(*,"(' Totally',i8,' atoms')") ncenter
	if (ipqr==1) write(*,"(' Sum of atomic charges:',f12.6)") sum(a%charge)
end if

end subroutine



!!--------------------------------------------------------
!!-------------------- Read .xyz file --------------------
! infomode=0: Output summary, =1: do not
! iopen=0 means don't open and close file in this routine, used to continuously read trjectory. =1 means do these
subroutine readxyz(name,infomode,iopen) 
use defvar
use util
implicit real*8 (a-h,o-z)
integer infomode,iopen
character(len=*) name
character*79 titleline
ifiletype=5
if (iopen==1) open(10,file=name,access="sequential",status="old")
ncenter=0
read(10,*) ncenter
read(10,"(a)") titleline
if (allocated(a)) deallocate(a)
allocate(a(ncenter))
do i=1,ncenter
	read(10,*) a(i)%name,a(i)%x,a(i)%y,a(i)%z
	call lc2uc(a(i)%name(1:1)) !Convert to upper case
	call uc2lc(a(i)%name(2:2)) !Convert to lower case
	do j=1,nelesupp
		if ( a(i)%name==ind2name(j) ) then
			a(i)%index=j
			exit
		end if
	end do
	if (j==nelesupp+1) then !Only use the first letter of atom name to try to assign. For example OW, HW may be in .xyz file
		do j=1,nelesupp
			if ( ind2name(j)(2:2)==" ".and.a(i)%name(1:1)==ind2name(j)(1:1) ) then
				a(i)%index=j
				exit
			end if
		end do
	end if
	if (j==nelesupp+1) then
		write(*,"(3a)") " Warning: Found unknown element """,a(i)%name,""" , assume it is carbon"
		a(i)%index=12
		write(*,*) "Press ENTER button to continue"
		read(*,*)
	end if
end do
if (iopen==1) close(10)
a%x=a%x/b2a
a%y=a%y/b2a
a%z=a%z/b2a
a%charge=a%index
a%name=ind2name(a%index)
if (infomode==0) then
	write(*,"(a)") titleline
	write(*,"(' Totally',i8,' atoms')") ncenter
end if
end subroutine




!!---------------------------------------------------
!!-------------------- Read .gro --------------------
! infomode=0: Output summary, =1: do not
subroutine readgro(name,infomode)
use defvar
use util
implicit real*8 (a-h,o-z)
integer infomode
character(len=*) name
character resname*5,resname_up*5,tmpname*5,tmpname_up*5
ifiletype=15
open(10,file=name,status="old")
read(10,*)
read(10,*) ncenter
allocate(a(ncenter))
do iatm=1,ncenter
    read(10,"(i5,2a5,i5,3f8.3)") inouse,resname,tmpname,inouse,a(iatm)%x,a(iatm)%y,a(iatm)%z
	tmpname_up=adjustl(tmpname)
    resname_up=adjustl(resname)
	call strlc2uc(tmpname_up)
	call strlc2uc(resname_up)
    ifound=0
    if (tmpname_up==resname_up) then !If resname and atomname are identical, for example NA, CL, CA, then they should be ion, directly recognize
        do i=1,nelesupp
            if (ind2name_up(i)==tmpname_up) then
			    a(iatm)%index=i
			    ifound=1
			    exit
		    end if
        end do
    end if
    if (ifound==0) then !Guess element according to atomname
	    do i=1,nelesupp
		    if (ind2name_up(i)==tmpname_up(1:1)//' ' .or. ind2name_up(i)==tmpname_up(1:2) .or. & !Recognize e.g. C, N11, Li
		    ((ichar(tmpname_up(1:1))<=57).and.ind2name_up(i)==tmpname_up(2:2)//' ')) then !Recognize such as 1H5*
			    a(iatm)%index=i
			    ifound=1
			    exit
		    end if
	    end do
    end if
    if (ifound==0) then
	    write(*,"(a)") " Warning: Element of """//trim(tmpname)//""" cannot be recognized, assume it is carbon"
	    a(iatm)%index=12
    end if
    a(iatm)%name=ind2name(a(iatm)%index)
end do
close(10)
a%x=a%x*10/b2a
a%y=a%y*10/b2a
a%z=a%z*10/b2a
if (infomode==0) write(*,"(' Totally',i8,' atoms')") ncenter
end subroutine




!!--------------------------------------------------------
!!-------------------- Read .gjf file --------------------
! Only support Cartesian coordinate currently
! infomode=0: Output summary, =1: do not
subroutine readgjf(name,infomode) 
use defvar
use util
implicit real*8 (a-h,o-z)
integer infomode
character(len=*) name
character c200tmp*200
ifiletype=12
open(10,file=name,status="old")
iskip=0 !How many lines before charge and multiplicity line
iblank=0
do while(.true.)
	read(10,"(a)") c200tmp
	iskip=iskip+1
	if (c200tmp==" ") iblank=iblank+1
	if (iblank==2) exit
end do
read(10,*) netcharge,multi

ncenter=0
do while(.true.)
	read(10,"(a)") c200tmp
	if (c200tmp==" ") exit
	ncenter=ncenter+1
end do

if (allocated(a)) deallocate(a)
allocate(a(ncenter))
rewind(10)
do i=1,iskip
	read(10,*)
end do
read(10,*)

do i=1,ncenter
	read(10,*,iostat=ierror) a(i)%name,a(i)%x,a(i)%y,a(i)%z
	if (ierror/=0) then
		write(*,"(a)") " Error: Unable to load atom information. The input file is too complicated &
        or Z-matrix is used, Multiwfn does not support these cases"
		write(*,*) "Press ENTER button to exit program"
		read(*,*)
		stop
	end if
	if (iachar(a(i)%name(1:1))>=48.and.iachar(a(i)%name(1:1))<=57) then
		read(a(i)%name,*) a(i)%index
	else
		call lc2uc(a(i)%name(1:1)) !Convert to upper case
		call uc2lc(a(i)%name(2:2)) !Convert to lower case
        if (a(i)%name(1:1)=='X') then !Dummy atom will be finally recognized as Bq
            a(i)%index=0
            cycle
        end if
		do j=1,nelesupp
			if ( a(i)%name==ind2name(j) ) then
				a(i)%index=j
				exit
			end if
		end do
	end if
end do
close(10)
a%x=a%x/b2a
a%y=a%y/b2a
a%z=a%z/b2a
a%charge=a%index
a%name=ind2name(a%index)
nelec=sum(a%index)
naelec=(nelec+multi-1)/2
nbelec=nelec-naelec
if (infomode==0) then
	write(*,"(' Totally',i8,' atoms')") ncenter
	write(*,"(' The number of alpha and beta electrons:',2i8)") nint(naelec),nint(nbelec)
end if
end subroutine



!!------------------- Read MDL .mol file (V2000)-------------------
! Format description: https://en.wikipedia.org/wiki/Chemical_table_file
! infomode=0: Output summary, =1: do not
subroutine readmol(name,infomode) 
use defvar
use util
implicit real*8 (a-h,o-z)
integer infomode
character(len=*) name
ifiletype=11
open(10,file=name,status="old")
read(10,"(a)") titleline
read(10,*)
read(10,*)
read(10,"(2i3)") ncenter,nbond
if (allocated(a)) deallocate(a)
allocate(a(ncenter))
do i=1,ncenter
	read(10,*) a(i)%x,a(i)%y,a(i)%z,a(i)%name
	call lc2uc(a(i)%name(1:1)) !Convert to upper case
	call uc2lc(a(i)%name(2:2)) !Convert to lower case
	do j=1,nelesupp
		if ( a(i)%name==ind2name(j) ) then
			a(i)%index=j
			exit
		end if
	end do
end do
if (allocated(connmat)) deallocate(connmat)
allocate(connmat(ncenter,ncenter))
connmat=0
do ibond=1,nbond
	read(10,"(3i3)") i,j,ntmp
	connmat(i,j)=ntmp
	connmat(j,i)=ntmp
end do
close(10)
a%x=a%x/b2a
a%y=a%y/b2a
a%z=a%z/b2a
a%charge=a%index
if (infomode==0) write(*,"(' Totally',i8,' atoms')") ncenter
end subroutine

!------------- Only read connectivity from .mol file -----------------
subroutine readmolconn(name) 
use defvar
implicit real*8 (a-h,o-z)
character(len=*) name
open(10,file=name,status="old")
read(10,*)
read(10,*)
read(10,*)
read(10,"(2i3)") ntmp,nbond
do i=1,ntmp
	read(10,*)
end do
if (allocated(connmat)) deallocate(connmat)
allocate(connmat(ncenter,ncenter))
connmat=0
do ibond=1,nbond
	read(10,"(3i3)") i,j,ntmp
	connmat(i,j)=ntmp
	connmat(j,i)=ntmp
end do
close(10)
end subroutine




!!------------------- Read .mol2 file -------------------
! infomode=0: Output summary, =1: do not
subroutine readmol2(name,infomode) 
use defvar
use util
implicit real*8 (a-h,o-z)
integer infomode
character(len=*) name
character c80tmp*80
ifiletype=13
open(10,file=name,status="old")
call loclabel(10,"@<TRIPOS>MOLECULE")
read(10,*)
read(10,"(a)") titleline
read(10,*) ncenter,nbond

if (allocated(a)) deallocate(a)
allocate(a(ncenter))
call loclabel(10,"@<TRIPOS>ATOM")
read(10,*)
do i=1,ncenter
	read(10,*) inouse,c80tmp,a(i)%x,a(i)%y,a(i)%z,a(i)%name
	ico=index(a(i)%name,'.')
	if (ico/=0) a(i)%name(ico:)=" "
	call lc2uc(a(i)%name(1:1)) !Convert to upper case
	call uc2lc(a(i)%name(2:2)) !Convert to lower case
	do j=1,nelesupp
		if ( a(i)%name==ind2name(j) ) then
			a(i)%index=j
			exit
		end if
	end do
end do

if (allocated(connmat)) deallocate(connmat)
allocate(connmat(ncenter,ncenter))
call loclabel(10,"@<TRIPOS>BOND")
read(10,*)
connmat=0
!gview records aromatic carbon as type of 4 in .mol, but record it as Ar
!When loading such kind of atom, Multiwfn still recognize it as type 4
do ibond=1,nbond
	read(10,*) inouse,i,j,c80tmp
	if (c80tmp=="ar".or.c80tmp=="Ar") then
		ntmp=4
	else
		read(c80tmp,*) ntmp
	end if
	connmat(i,j)=ntmp
	connmat(j,i)=ntmp
end do
close(10)
a%x=a%x/b2a
a%y=a%y/b2a
a%z=a%z/b2a
a%charge=a%index
if (infomode==0) write(*,"(' Totally',i8,' atoms')") ncenter
end subroutine




!!-----------------------------------------------------------------
!! Read .31 and one of .32 to .40 file generated by NBO program
!! After loading, only GTF information is yielded, while basis functions are discarded
subroutine read31(name,infomode) !mode=0 means output wfn property,=1 not
use defvar
use util
implicit real*8 (a-h,o-z)
character(len=*) name
character :: name2*200=" ",chartemp*80,tmpc2*2
integer infomode
integer bastype2func(500) !Convert basis type in NBO (NBO5.0 Program Manual P103) to function type in .wfn
integer,allocatable :: shellcon(:),shell2atom(:),shellnumbas(:),shell2prmshl(:),bastype31(:),shellnumbas5D(:)
real*8,allocatable :: orbcoeff(:,:),prmshlexp(:),cs(:),cp(:),cd(:),cf(:),cg(:),orbcoeff5D(:,:)
real*8 :: conv5d6d(6,5)=0D0,conv7f10f(10,7)=0D0,conv9g15g(15,9)=0D0
real*8,external :: normgau
ifiletype=6
bastype2func(1)=1 !s
bastype2func(101)=2 !x
bastype2func(102)=3 !y
bastype2func(103)=4 !z
bastype2func(201)=5 !xx
bastype2func(202)=8 !xy
bastype2func(203)=9 !xz
bastype2func(204)=6 !yy
bastype2func(205)=10 !yz
bastype2func(206)=7 !zz
bastype2func(301)=11 !xxx
bastype2func(302)=14 !xxy
bastype2func(303)=15 !xxz
bastype2func(304)=17 !xyy
bastype2func(305)=20 !xyz
bastype2func(306)=18 !xzz
bastype2func(307)=12 !yyy
bastype2func(308)=16 !yyz
bastype2func(309)=19 !yzz
bastype2func(310)=13 !zzz
!Below g sequence comes from line 47384 in NBO_5 src
bastype2func(401)=35 !XXXX
bastype2func(402)=34 !XXXY
bastype2func(403)=33 !XXXZ
bastype2func(404)=32 !XXYY
bastype2func(405)=31 !XXYZ
bastype2func(406)=30 !XXZZ
bastype2func(407)=29 !XYYY
bastype2func(408)=28 !XYYZ
bastype2func(409)=27 !XYZZ
bastype2func(410)=26 !XZZZ
bastype2func(411)=25 !YYYY
bastype2func(412)=24 !YYYZ
bastype2func(413)=23 !YYZZ
bastype2func(414)=22 !YZZZ
bastype2func(415)=21 !ZZZZ
!Conversion relationship used below can be found in http://sobereva.com/97
!Used to convert coefficient matrix from 5D to 6D
!5D sequence in .31: 255   252   253   254   251
!namely -0.5*XX-0.5*YY+ZZ, XZ, YZ, ��3/2*(XX-YY), XY
!to 6D: XX,XY,XZ,YY,YZ,ZZ, namely 201~206, the indexes are consecutive
conv5d6d(1,1)=-0.5D0
conv5d6d(4,1)=-0.5D0
conv5d6d(6,1)=1D0
conv5d6d(3,2)=1D0
conv5d6d(5,3)=1D0
conv5d6d(1,4)=sqrt(3D0)/2D0
conv5d6d(4,4)=-sqrt(3D0)/2D0
conv5d6d(2,5)=1D0
!Used to convert coefficient matrix from 7F to 10F, Standard f set
!7F sequence in .31: 351   352   353   354   355   356   357
!to 10F: XXX,XXY,XXZ,XYY,XYZ,XZZ,YYY,YYZ,YZZ,ZZZ, namely 301~310, the indexes are consecutive
conv7f10f(3,1)=-0.721962098225322D0
conv7f10f(8,1)=-0.721962098225322D0
conv7f10f(10,1)=0.481308065483548D0
conv7f10f(1,2)=-0.281160203343101D0
conv7f10f(4,2)=-0.281160203343101D0
conv7f10f(6,2)=1.1246408133724D0
conv7f10f(2,3)=-0.281160203343101D0
conv7f10f(7,3)=-0.281160203343101D0
conv7f10f(9,3)=1.1246408133724D0
conv7f10f(3,4)=sqrt(3D0)/2D0
conv7f10f(8,4)=-sqrt(3D0)/2D0
conv7f10f(5,5)=1D0
conv7f10f(1,6)=0.369693511996758D0
conv7f10f(4,6)=-1.10908053599027D0
conv7f10f(2,7)=1.10908053599027D0
conv7f10f(7,7)=-0.369693511996758D0

write(*,*) "Input filename with suffix ranging from .32 to .40 (e.g. ltwd.35)"
write(*,*) "Note: 32=PNAO 33=NAO 34=PNHO 35=NHO 36=PNBO 37=NBO 38=PNLMO 39=NLMO 40=MO"
do while(.true.)
	read(*,"(a)") name2
	if (name2(1:2)=="32".or.name2(1:2)=="33".or.name2(1:2)=="34".or.name2(1:2)=="35"&
	.or.name2(1:2)=="36".or.name2(1:2)=="37".or.name2(1:2)=="38".or.name2(1:2)=="39".or.name2(1:2)=="40") then
		tmpc2=name2(1:2)
		name2(1:len(name))=name
		name2(len_trim(name2)-1:len_trim(name2))=tmpc2
	end if
	inquire(file=name2,exist=alive)
	if (alive.eqv..true.) exit
	write(*,*) "File not found, input again"
end do

itmplen=len_trim(name2)
if (name2(itmplen-1:itmplen)=="32") write(*,*) "Loading .32 file(PNAO)"
if (name2(itmplen-1:itmplen)=="33") write(*,*) "Loading .33 file(NAO)"
if (name2(itmplen-1:itmplen)=="34") write(*,*) "Loading .34 file(PNHO)"
if (name2(itmplen-1:itmplen)=="35") write(*,*) "Loading .35 file(NHO)"
if (name2(itmplen-1:itmplen)=="36") write(*,*) "Loading .36 file(PNBO)"
if (name2(itmplen-1:itmplen)=="37") write(*,*) "Loading .37 file(NBO)"
if (name2(itmplen-1:itmplen)=="38") write(*,*) "Loading .38 file(PNLMO)"
if (name2(itmplen-1:itmplen)=="39") write(*,*) "Loading .39 file(NLMO)"
if (name2(itmplen-1:itmplen)=="40") write(*,*) "Loading .40 file(MO)"
open(10,file=name,access="sequential",status="old")
read(10,*)
read(10,*)
read(10,*)
read(10,*) ncenter,nshell,nprimshell
allocate(a(ncenter),shellcon(nshell),shell2atom(nshell),shellnumbas(nshell),shell2prmshl(nshell))
allocate(prmshlexp(nprimshell),cs(nprimshell),cp(nprimshell),cd(nprimshell),cf(nprimshell),cg(nprimshell))
allocate(bastype31(nshell*15)) !We don't know how many basis before read the file, so use the maximum value(up to g function)
read(10,*)
do i=1,ncenter
	read(10,*) a(i)%index,a(i)%x,a(i)%y,a(i)%z
	a(i)%charge=a(i)%index !.31 doesn't record charge and index separately, so we have to make them indentical
end do
a%name=ind2name(a%index)
a%x=a%x/b2a
a%y=a%y/b2a
a%z=a%z/b2a
read(10,*)
j=1
do i=1,nshell
	read(10,*) shell2atom(i),shellnumbas(i),shell2prmshl(i),shellcon(i)
	read(10,*) bastype31(j:j+shellnumbas(i)-1)
	j=j+shellnumbas(i)
end do
isphergau=0
if (any(bastype31==251).or.any(bastype31==351).or.any(bastype31==451)) isphergau=1
!The conversion relationship between Cartesian and pure functions used by NBO is different to mainstream quantum chemistry code, the relationship for f is documented
!in the NBO manual, however there is no way to find out that for g-type. So if g-type is involved, Cartesian type must be used.
if (any(bastype31==451)) then
	write(*,"(a)") " Error: Multiwfn does not support spherical harmonic Gaussian functions with g or higher angular moment in NBO plot files. &
	If you used Gaussian to generate them, you should add ""6d 10f"" keywords and regenerate these files"
	write(*,*) "Press ENTER button to exit"
	read(*,*)
	stop
end if
read(10,*)
read(10,*) prmshlexp
read(10,*)
read(10,*) cs
read(10,*)
read(10,*) cp
read(10,*)
read(10,*) cd
read(10,*)
read(10,*) cf
read(10,"(a)",iostat=ierror) chartemp
if (ierror==0) then
	backspace(10)
	read(10,*) cg
end if
close(10)

totenergy=0D0
virialratio=0D0
nbasis=sum(shellnumbas) !Calculate the number of basis
write(*,"(' Expected number of basis functions:',i10)") nbasis
open(10,file=name2,access="sequential",status="old")
read(10,*)
read(10,*)
read(10,*)
read(10,"(a)") chartemp
naelec=0D0
nbelec=0D0
!Note: When diffuse functions are used, although the number of AO in NBO plot files are always identical to nbasis, 
!the number of NAOs and thus the resulting NBOs etc. may be smaller than nbasis (and can also be different to Nbsuse in Gaussian output file)
if (chartemp(1:11)==" ALPHA SPIN".or.chartemp(1:11)==" alpha spin") then
	wfntype=4
	nmo=2*nbasis
	allocate(orbcoeff(nbasis,nmo),MOocc(nmo),MOene(nmo),MOtype(nmo))
	MOtype(1:nbasis)=1 !alpha
	MOtype(nbasis+1:nmo)=2 !beta
	orbcoeff=0
	MOocc=0
	nload=0
	do iorb=1,nbasis !Note that the occupation section may be erronesouly loaded as orbital coefficient here when linear dependency problem occur
		read(10,*,iostat=ierror) orbcoeff(1:nbasis,iorb)
		if (ierror/=0) exit
		nload=nload+1
	end do
	if (name2(itmplen-1:itmplen)=="37".or.name2(itmplen-1:itmplen)=="39") then !Need to load occupation
		if (nload/=nbasis) then
			write(*,"(/,a)") " Warning: The number of orbitals is smaller than basis functions! This is often because diffuse functions are used. &
			Please now input the actual number of NAOs (you can easily find it from output of NBO program), e.g. 374"
			read(*,*) nNAOs
			call loclabel(10,chartemp(1:11))
			read(10,*)
			do iorb=1,nNAOs
				read(10,*) orbcoeff(1:nbasis,iorb)
			end do
			orbcoeff(1:nbasis,nNAOs+1:nbasis)=0
		else
			nNAOs=nbasis
		end if
		read(10,*) MOocc(1:nNAOs)
	end if
	if (chartemp(1:11)==" ALPHA SPIN") then
		call loclabel(10," BETA  SPIN",ifound)
	else
		call loclabel(10," beta  spin",ifound)
	end if
	if (ifound==1) then
		read(10,*)
		do iorb=nbasis+1,nbasis+nNAOs
			read(10,*) orbcoeff(1:nbasis,iorb)
		end do
		if (name2(itmplen-1:itmplen)=="37".or.name2(itmplen-1:itmplen)=="39") read(10,*) MOocc(nbasis+1:nbasis+nNAOs)
	else
		write(*,*) "Warning: Beta-spin information are not found in this file!"
		write(*,*)
	end if
	naelec=sum(MOocc(1:nbasis))
	nbelec=sum(MOocc(nbasis+1:nmo))
	nelec=naelec+nbelec
else !Close shell system
	wfntype=3
	nmo=nbasis
	allocate(orbcoeff(nbasis,nmo),MOocc(nmo),MOene(nmo),MOtype(nmo))
	MOtype=0
	orbcoeff=0
	MOocc=0
	nload=0
	backspace(10)
	do iorb=1,nmo
		read(10,*,iostat=ierror) orbcoeff(1:nbasis,iorb)
		if (ierror/=0) exit
		nload=nload+1
	end do
	if (name2(itmplen-1:itmplen)=="37".or.name2(itmplen-1:itmplen)=="39") then
		if (nload/=nbasis) then
			write(*,"(/,a)") " Warning: The number of orbitals is smaller than basis functions! This is often because diffuse functions are used. &
			Please now input the actual number of NAOs (you can easily find it from output of NBO program), e.g. 374"
			read(*,*) nNAOs
            rewind(10)
            read(10,*)
            read(10,*)
            read(10,*)
			do iorb=1,nNAOs
				read(10,*) orbcoeff(1:nbasis,iorb)
			end do
			orbcoeff(1:nbasis,nNAOs+1:nbasis)=0
		else
			nNAOs=nbasis
		end if
		read(10,*) MOocc(1:nNAOs)
	end if
	nelec=sum(MOocc)
	naelec=nelec/2
	nbelec=naelec
end if
close(10)
MOene=0D0

!Temporarily convert spherical harmonic Gaussian functions' information to cartesian type
if (isphergau==1) then
	!Calculate how many cartesian basis after conversion
	nbasis5D=nbasis
	nbasis=0
	do i=1,nshell
		if (shellnumbas(i)==5) then !D
			nbasis=nbasis+6
		else if (shellnumbas(i)==7) then !F
			nbasis=nbasis+10
		else if (shellnumbas(i)==9) then !G
			nbasis=nbasis+15
		else !S,P,SP, or cartesian shells
			nbasis=nbasis+shellnumbas(i)
		end if
	end do
	allocate(shellnumbas5D(nshell))
	shellnumbas5D=shellnumbas !Backup
	where(shellnumbas==5) shellnumbas=6 !Convert number of orbitals in each shell from 5D to cartesian type
	where(shellnumbas==7) shellnumbas=10
	where(shellnumbas==9) shellnumbas=15

	allocate(orbcoeff5D(nbasis5D,nmo))
	orbcoeff5D=orbcoeff !Backup
	deallocate(orbcoeff)
	allocate(orbcoeff(nbasis,nmo)) !Enlarge size from spherical type to cartesian type
	orbcoeff=0D0

	deallocate(bastype31)
	allocate(bastype31(nbasis)) !Enlarge size from spherical type to cartesian type

	!Generate cartesian .31 basis type, the indexes are consecutive, in line with conv5d6d and conv7f10f
	i=1
	do ish=1,nshell
		if (shellnumbas(ish)==1) then !s
			bastype31(i)=1
		else if (shellnumbas(ish)==3) then !p
			bastype31(i)=101
			bastype31(i+1)=102
			bastype31(i+2)=103
		else if (shellnumbas(ish)==4) then !sp
			bastype31(i)=1
			bastype31(i+1)=101
			bastype31(i+2)=102
			bastype31(i+3)=103
		else if (shellnumbas(ish)==6) then !d
			do j=1,6
				bastype31(i+j-1)=200+j
			end do
		else if (shellnumbas(ish)==10) then !f
			do j=1,10
				bastype31(i+j-1)=300+j
			end do
		else if (shellnumbas(ish)==15) then !g
			do j=1,15
				bastype31(i+j-1)=400+j
			end do
		end if
		i=i+shellnumbas(ish)
	end do
	
	!Map 5D coefficient to 6D coefficient
	ipos5D=1
	ipos6D=1
	do ish=1,nshell
		n5D=shellnumbas5D(ish)
		n6D=shellnumbas(ish)
		if (n5D==1.or.n5D==3.or.n5D==4) then !S or P or SP
			if (wfntype==3) orbcoeff(ipos6D:ipos6D+n6D-1,1:nbasis5D)=orbcoeff5D(ipos5D:ipos5D+n5D-1,:)
			if (wfntype==4) orbcoeff(ipos6D:ipos6D+n6D-1,1:2*nbasis5D)=orbcoeff5D(ipos5D:ipos5D+n5D-1,:)
		else if (n5D==5) then
			!5D->6D
			if (wfntype==3) orbcoeff(ipos6D:ipos6D+5,1:nbasis5D)=matmul(conv5d6d,orbcoeff5D(ipos5D:ipos5D+4,:))
			if (wfntype==4) orbcoeff(ipos6D:ipos6D+5,1:2*nbasis5D)=matmul(conv5d6d,orbcoeff5D(ipos5D:ipos5D+4,:))
		else if (n5D==7) then
			!7F->10F
			if (wfntype==3) orbcoeff(ipos6D:ipos6D+9,1:nbasis5D)=matmul(conv7f10f,orbcoeff5D(ipos5D:ipos5D+6,:))
			if (wfntype==4) orbcoeff(ipos6D:ipos6D+9,1:2*nbasis5D)=matmul(conv7f10f,orbcoeff5D(ipos5D:ipos5D+6,:))
		else if (n5D==9) then
			!9G->15G
			if (wfntype==3) orbcoeff(ipos6D:ipos6D+14,1:nbasis5D)=matmul(conv9g15g,orbcoeff5D(ipos5D:ipos5D+8,:))
			if (wfntype==4) orbcoeff(ipos6D:ipos6D+14,1:2*nbasis5D)=matmul(conv9g15g,orbcoeff5D(ipos5D:ipos5D+8,:))
		end if
		ipos5D=ipos5D+n5D
		ipos6D=ipos6D+n6D
	end do
end if

nprims=0
do i=1,nshell
	nprims=nprims+shellcon(i)*shellnumbas(i)
end do
allocate(b(nprims),co(nmo,nprims))

iGTF=1 !current GTF index
ibasis=1
do i=1,nshell !cycle each shell
	b(iGTF:iGTF+shellcon(i)*shellnumbas(i)-1)%center=shell2atom(i)
	do j=1,shellnumbas(i) !cycle each basis function in each shell
		b(iGTF:iGTF+shellcon(i)-1)%type=bastype2func(bastype31(ibasis))
		do k=1,shellcon(i) !cycle each GTF in each basis in each shell
			iprmshlpos=shell2prmshl(i)+k-1
			b(iGTF)%exp=prmshlexp(iprmshlpos)
			if (bastype31(ibasis)==1) then !s
				contract=cs(iprmshlpos)
			else if (bastype31(ibasis)<=200) then !p
				contract=cp(iprmshlpos)
			else if (bastype31(ibasis)<=300) then !d
			!Contract coefficient in .31 contains normalization coefficient, however for d type, the
			!normalization coefficient is for XX,YY,ZZ, for XY,XZ,YZ, we need refresh normalization coefficient
				contract=cd(iprmshlpos)
				if (bastype31(ibasis)==202.or.bastype31(ibasis)==203.or.bastype31(ibasis)==205) then
					valnorm31=normgau(5,prmshlexp(iprmshlpos)) !Normalization coefficient for XX,YY,ZZ are identical
					valnormnew=normgau(8,prmshlexp(iprmshlpos)) !Normalization coefficient for XY,XZ,YZ are identical
					contract=contract/valnorm31*valnormnew
				end if
			else if (bastype31(ibasis)<=400) then !f
				contract=cf(iprmshlpos)
				!For f shell, in .31 normalization coefficient is for XXX,YYY,ZZZ, now refresh
				if (bastype31(ibasis)/=301.and.bastype31(ibasis)/=307.and.bastype31(ibasis)/=310) then !not XXX,YYY,ZZZ
					valnorm31=normgau(11,prmshlexp(iprmshlpos))
					if (bastype31(ibasis)==302.or.bastype31(ibasis)==303.or.bastype31(ibasis)==304&
					.or.bastype31(ibasis)==306.or.bastype31(ibasis)==308.or.bastype31(ibasis)==309) then
						valnormnew=normgau(14,prmshlexp(iprmshlpos)) !XXY,XXZ,XYY,XZZ,YYZ,YZZ are identical
					else if (bastype31(ibasis)==305) then 
						valnormnew=normgau(20,prmshlexp(iprmshlpos)) !XYZ
					end if
					contract=contract/valnorm31*valnormnew
				end if
			else if (bastype31(ibasis)<=500) then !g
				contract=cg(iprmshlpos)
				!For g shell, in .31 normalization coefficient is for XXXX,YYYY,ZZZZ, now refresh
				!Note: I haven't verified that, since .37 outputted by NBO3.1 in Gaussian didn't contains g information
				nt=bastype31(ibasis) !now type
				if (nt/=401.and.nt/=411.and.nt/=415) then !not XXXX,YYYY,ZZZZ (4,0)
					valnorm31=normgau(21,prmshlexp(iprmshlpos))
					if (nt==402.or.nt==403.or.nt==407.or.nt==410.or.nt==412.or.nt==414) then
						valnormnew=normgau(22,prmshlexp(iprmshlpos)) !XXXY,XXXZ,XYYY,XZZZ,YYYZ,YZZZ (3,1)
					else if (nt==404.or.nt==406.or.nt==413) then
						valnormnew=normgau(23,prmshlexp(iprmshlpos)) !XXYY,XXZZ,YYZZ (2,2)
					else if (nt==405.or.nt==408.or.nt==409) then
						valnormnew=normgau(27,prmshlexp(iprmshlpos)) !XYZZ,XYYZ,XXYZ (2,1,1)
					end if
					contract=contract/valnorm31*valnormnew
				end if
			end if
			CO(:,iGTF)=orbcoeff(ibasis,:)*contract
			iGTF=iGTF+1
		end do
		ibasis=ibasis+1
	end do
end do
if (isphergau==1) nbasis=nbasis5D

if (infomode==0) then
	if (name2(itmplen-1:itmplen)=="37".or.name2(itmplen-1:itmplen)=="39") write(*,"(' Total/Alpha/Beta electrons:',3f12.4)") nelec,naelec,nbelec
	write(*,"(' Atoms:',i6,',  Basis functions:',i6,',  Orbitals:',i6,',  GTFs:',i6)") ncenter,nbasis,nmo,nprims
	if (wfntype==3) write(*,*) " This is closed-shell system"
	if (wfntype==4) then
		write(*,*) " This is open-shell system"
		write(*,"(' Orbitals from 1 to',i6,' are alpha, from',i6,' to',i6,' are beta')") nbasis,nbasis+1,nmo
	end if
	write(*,*)
end if
end subroutine




!!-----------------------------------------------------------------
!!---------------- Read Gaussian cube file and store in cubmat
!infomode=0 means output cube details, =1 do not, =2 also do not print loading process
!ionlygrid=1 means only read grid data, but do not perturb atom information, =0 means do all
subroutine readcube(cubname,infomode,ionlygrid)
use defvar
implicit real*8 (a-h,o-z)
character(len=*) cubname
character*79 titleline1,titleline2
integer infomode,ionlygrid
integer,allocatable :: mo_serial(:)
real*8,allocatable :: temp_readdata(:),tmpreadcub(:,:,:)
type(content) maxv,minv
if (ionlygrid==0) ifiletype=7
open(10,file=cubname,access="sequential",status="old")
read(10,"(a)") titleline1
read(10,"(a)") titleline2
read(10,*) ncentertmp,orgx,orgy,orgz
read(10,*) nx,gridvec1
read(10,*) ny,gridvec2
read(10,*) nz,gridvec3
if (ionlygrid==0) ncenter=ncentertmp
dx=gridvec1(1)
dy=gridvec2(2)
dz=gridvec3(3)
endx=orgx+dx*(nx-1) !endx,y,z are global array in defvar
endy=orgy+dy*(ny-1)
endz=orgz+dz*(nz-1)
if (infomode==0) write(*,*) 

mo_number=0
if (ncenter<0) then
	mo_number=1 !This cube file contains at least one MO data
	ncenter=abs(ncenter)
end if
if (ionlygrid==0) then
	if (allocated(a)) deallocate(a)
	allocate(a(ncenter))
end if
if (allocated(cubmat)) deallocate(cubmat)
allocate(cubmat(nx,ny,nz))

if (infomode==0) then
	write(*,*) "Title line of this file:"
	write(*,"(a)") trim(titleline1)
	write(*,"(a)") trim(titleline2)
	write(*,*)
	write(*,"(' Total number of atoms:',i8)") ncenter
	write(*,"(' Translation vector:        X           Y           Z     (Bohr)')")
	write(*,"(a20,3f12.6)") "Vector 1: ",gridvec1
	write(*,"(a20,3f12.6)") "Vector 2: ",gridvec2
	write(*,"(a20,3f12.6)") "Vector 3: ",gridvec3
	write(*,"(' The range of x is from ',f12.6,' to ',f12.6,' Bohr,' i5,' points')") ,orgx,orgx+(nx-1)*dx,nx
	write(*,"(' The range of y is from ',f12.6,' to ',f12.6,' Bohr,',i5,' points')") ,orgy,orgy+(ny-1)*dy,ny
	write(*,"(' The range of z is from ',f12.6,' to ',f12.6,' Bohr,',i5,' points')") ,orgz,orgz+(nz-1)*dz,nz
	write(*,"(' Total number of grid points:',i10)") nx*ny*nz
	write(*,"(' This grid data will take up at least',i6,' MB memory')") nx*ny*nz*8/1024/1024
end if

if (ionlygrid==0) then
	do i=1,ncenter
		read(10,*) a(i)%index,a(i)%charge,a(i)%x,a(i)%y,a(i)%z !%value is its charge, if ECP was used, it not equal to atomindex
	end do
	a%name=ind2name(a%index)
else if (ionlygrid==1) then
	do i=1,ncentertmp
		read(10,*) !Do not read atomic information, simply skip
	end do
end if
if (infomode<2) write(*,*)

if (mo_number==1) then
	read(10,"(i5)",advance="no") mo_number !Get actual number of MO
	if (mo_number>1) then
		allocate(mo_serial(mo_number))
		allocate(temp_readdata(nz*mo_number))
		read(10,*) mo_serial
		write(*,"(' There are ',i6,' MOs in this cube file, the serial numbers are: ')") mo_number
		do i=1,mo_number
			write(*,"(' Number ',i6,', corresponds to MO',i6)") i,mo_serial(i)
		end do
		write(*,*) "Which MO do you want to load? Input the serial number"
		do while(.true.)
			read(*,*) mo_select
			if (mo_select>0.and.mo_select<=mo_number) exit
			write(*,*) "Invalid input, input again"
		end do
	else
		read(10,*) !Only one MO, pass the MO serial line
	end if
end if

if (infomode<2) write(*,*) "Loading grid data, please wait..."
if (mo_number==0.or.mo_number==1) then !Commonly case, below code has the best compatibility
	allocate(tmpreadcub(nz,ny,nx))
	read(10,*) tmpreadcub(:,:,:)
	do i=1,nx
		do j=1,ny
			do k=1,nz
				cubmat(i,j,k)=tmpreadcub(k,j,i)
			end do
		end do
	end do
	deallocate(tmpreadcub)
else !Load specified of many orbitals
	ii=0
	do i=1,nx !a(x,y,z)
		do j=1,ny
			read(10,*) temp_readdata
			cubmat(i,j,:)=temp_readdata(mo_select:size(temp_readdata):mo_number)
		end do
		progress=dfloat(i)/nx*100
		if (progress>ii.and.infomode<2) then
			ii=ii+10
			write(*,"(f6.1,'%')") progress
		end if
	end do
	if (infomode<2) write(*,"(f6.1,'%')") 100D0
end if
close(10)

!Calculate statistical information
maxv%value=cubmat(1,1,1)
maxv%x=orgx
maxv%y=orgy
maxv%z=orgz
minv%value=cubmat(1,1,1)
minv%x=orgx
minv%y=orgy
minv%z=orgz
sumuppos=0D0
sumupneg=0D0
do k=1,nz
	do j=1,ny
		do i=1,nx
			if (cubmat(i,j,k)>0) sumuppos=sumuppos+cubmat(i,j,k)
			if (cubmat(i,j,k)<0) sumupneg=sumupneg+cubmat(i,j,k)
			if (cubmat(i,j,k)>maxv%value) then
				maxv%value=cubmat(i,j,k)
				maxv%x=orgx+(i-1)*dx
				maxv%y=orgy+(j-1)*dy
				maxv%z=orgz+(k-1)*dz
			end if
			if (cubmat(i,j,k)<minv%value) then
				minv%value=cubmat(i,j,k)
				minv%x=orgx+(i-1)*dx
				minv%y=orgy+(j-1)*dy
				minv%z=orgz+(k-1)*dz
			end if
		end do
	end do
end do

if (infomode==0) then
	fminivol=dx*dy*dz
	write(*,*)
	write(*,"(' The minimum value:',E16.8,' at',3f12.6,' Bohr')") minv%value,minv%x,minv%y,minv%z
	write(*,"(' The maximum value:',E16.8,' at',3f12.6,' Bohr')") maxv%value,maxv%x,maxv%y,maxv%z
	write(*,"(' Differential element:',f15.10,' Bohr^3')") fminivol
	write(*,"(' Summing up positive value in grid file:  ',f30.10)") sumuppos
	write(*,"(' After multiplied by differential element:',f30.10)") sumuppos*fminivol
	write(*,"(' Summing up negative value in grid file:  ',f30.10)") sumupneg
	write(*,"(' After multiplied by differential element:',f30.10)") sumupneg*fminivol
	write(*,"(' Summing up all value in grid file:       ',f30.10)") sumuppos+sumupneg
	write(*,"(' After multiplied by differential element:',f30.10)") (sumuppos+sumupneg)*fminivol

    if (sum(gridvec1*gridvec2*gridvec3)/=0) then
        write(*,*)
        write(*,*) "   Warning! Warning! Warning! Warning! Warning! Warning! Warning! Warning!"
        write(*,"(a)") " The grid is not rectangle, in this case, many outputs (including the ones shown above) and &
        analysis results are completely meaningless. &
        Only the grid data calculation function in main function 13 must work normally."
        write(*,*) "Press ENTER button to continue"
        read(*,*)
    end if
end if

end subroutine


!!-----------------------------------------------------------------
!!---- Read Gaussian cube file and save to cubmattmp, this is a simple version of readcube, can only be invoked after cubmat has been loaded
!don't read atomic information, don't modify loaded grid infomation such as nx/y/z,orgx/y/z...
!and don't output statistic information, don't specify coordinate for grid points...
!infomode=1: Print loading information, =2: do not print anything
!inconsis is returned value, 1 means the grid setting of this cube file is inconsistent with that of cubmat
subroutine readcubetmp(cubname,infomode,inconsis)
use defvar
implicit real*8 (a-h,o-z)
character(len=*) cubname
integer inconsis
integer,allocatable :: mo_serial(:)
real*8,allocatable :: temp_readdata(:)
real*8 gridvectmp1(3),gridvectmp2(3),gridvectmp3(3)

open(10,file=cubname,access="sequential",status="old")
read(10,*)
read(10,*)
read(10,*) ncentertmp
read(10,*) nxtmp,gridvectmp1
read(10,*) nytmp,gridvectmp2
read(10,*) nztmp,gridvectmp3
inconsis=0
if (nxtmp/=nx.or.nytmp/=ny.or.nztmp/=nz.or.&
maxval(abs(gridvec1-gridvectmp1))>0.005D0.or.maxval(abs(gridvec2-gridvectmp2))>0.005D0.or.maxval(abs(gridvec3-gridvectmp3))>0.005D0) then
	write(*,"(a)") " Error: The grid setting of this cube file is inconsistent with that of the grid data stored in memory!"
    write(*,*) "Press ENTER button to return"
    read(*,*)
	inconsis=1
	return
end if

mo_number=0
if (ncentertmp<0) then
	mo_number=1 !This cube file contains at least one MO data
	ncentertmp=abs(ncentertmp)
end if
if (allocated(cubmattmp)) deallocate(cubmattmp)
allocate(cubmattmp(nx,ny,nz)) !nx,ny,nz is identical to cubmat that already loaded into memory

do i=1,ncentertmp
	read(10,*) !Skip a(i)%index,a(i)%charge,a(i)%x,a(i)%y,a(i)%z !%value is its charge, if ECP was used, it not equal to atomindex
end do

if (mo_number==1) then
	read(10,"(i5)",advance="no") mo_number !Get actual number of MO
	if (mo_number>1) then
		allocate(mo_serial(mo_number))
		allocate(temp_readdata(nz*mo_number))
		read(10,*) mo_serial
		write(*,"(' There are ',i6,' MOs in this grid file, the serial number are: ')") mo_number
		do i=1,mo_number
			write(*,"(' Number ',i6,' : MO= ',i5)") i,mo_serial(i)
		end do
		write(*,*) "Which MO do you want to load? Input the serial number"
		do while(.true.)
			read(*,*) mo_select
			if (mo_select>0.and.mo_select<=mo_number) exit
			write(*,*) "Invalid input, input again"
		end do
	else
		read(10,*) !Only one MO, pass the MO serial line
	end if
end if

if (infomode<2) write(*,*)
if (infomode<2) write(*,*) "Loading grid data, please wait..."
!Load data
ii=0
do i=1,nx !a(x,y,z)
	do j=1,ny
		if (mo_number==0.or.mo_number==1) then
			read(10,*) cubmattmp(i,j,:)
		else !Load the specified MO from vast of MOs
			read(10,*) temp_readdata
			cubmattmp(i,j,:)=temp_readdata(mo_select:size(temp_readdata):mo_number)
		end if
	end do
	progress=dfloat(i)/nx*100
	if (progress>ii.and.infomode<2) then
		ii=ii+10
		write(*,"(f6.1,'%')") progress
	end if
end do
if (infomode<2) write(*,*) "Done!"
close(10)
end subroutine



!!-----------------------------------------------------------------
!!----------- Load Dmol3 .grd file, see format description in Material Studio help file
!infomode=0 means output grd details, =1 not
!ionlygrid=1 means only read grid data, but do not perturb any other variables, =0 means do all
subroutine readgrd(grdname,infomode,ionlygrid)
use defvar
implicit real*8 (a-h,o-z)
character(len=*) grdname
character*79 titleline
integer infomode,ionlygrid
type(content) maxv,minv
if (ionlygrid==0) ifiletype=8
open(10,file=grdname,access="sequential",status="old")
read(10,"(a)") titleline
read(10,*)
read(10,*) flenx,fleny,flenz,anga,angb,angc !Notice that the length unit is in Angstrom in .grd file
read(10,*) nx,ny,nz !Here nx,ny,nz are total space (between neighbour grid point) in each direction
read(10,*) ifast,ixback,ixforw,iyback,iyforw,izback,izforw
if (anga/=90.or.angb/=90.or.angc/=90) then
	write(*,*) "Error: Only .grd file of rectangle grid is supported in Multiwfn!"
    write(*,*) "Press ENTER button to exit"
	read(*,*)
	stop
else if (ifast/=1) then !ifast=1 means x varies fastest
	write(*,*) "Error: The first integer in the fifth line must be 1! Press ENTER button to exit"
	read(*,*)
	stop
end if
dx=flenx/nx/b2a
dy=fleny/ny/b2a
dz=flenz/nz/b2a
gridvec1(1)=dx;gridvec2(2)=dy;gridvec3(3)=dz
nx=nx+1 !Convert the number of spacings to the number of points
ny=ny+1
nz=nz+1
allocate(cubmat(nx,ny,nz))
orgx=-dx*abs(ixback)
orgy=-dy*abs(iyback)
orgz=-dz*abs(izback)
endx=orgx+dx*(nx-1) !endx,y,z are global array in defvar
endy=orgy+dy*(ny-1)
endz=orgz+dz*(nz-1)

if (infomode==0) then
	write(*,"(' Title line of this file: ',a)") trim(titleline)
	write(*,*)
	write(*,"(' Translation vectors in X/Y/Z (Bohr):',3f12.6)") dx,dy,dz
	write(*,"(' The range of x is from ',f12.6,' to ',f12.6,' Bohr,' i5,' points')") ,orgx,endx,nx
	write(*,"(' The range of y is from ',f12.6,' to ',f12.6,' Bohr,',i5,' points')") ,orgy,endy,ny
	write(*,"(' The range of z is from ',f12.6,' to ',f12.6,' Bohr,',i5,' points')") ,orgz,endz,nz
	write(*,"(' Total number of grid points:',i10)") nx*ny*nz
	write(*,"(' This grid data will take up at least',i6,' MB memory')") nx*ny*nz*8*4/1024/1024
end if
write(*,*)
write(*,*) "Loading grid data, please wait..."
ii=0
do k=1,nz   !a(x,y,z)
	do j=1,ny
		do i=1,nx
			read(10,*) cubmat(i,j,k)
		end do
	end do
	if (dfloat(k)/nz*100>ii) then
		ii=ii+10
		write(*,"(f6.1,'%')") dfloat(k)/nz*100
	end if
end do
write(*,"(f6.1,'%')") 100D0
write(*,*) "Done!"
write(*,*)
close(10)

!Perform statistic
maxv%value=cubmat(1,1,1)
maxv%x=orgx
maxv%y=orgy
maxv%z=orgz
minv%value=cubmat(1,1,1)
minv%x=orgx
minv%y=orgy
minv%z=orgz
sumuppos=0.0D0
sumupneg=0.0D0
do k=1,nz
	do j=1,ny
		do i=1,nx
			if (cubmat(i,j,k)>0) sumuppos=sumuppos+cubmat(i,j,k)
			if (cubmat(i,j,k)<0) sumupneg=sumupneg+cubmat(i,j,k)
			if (cubmat(i,j,k)>maxv%value) then
				maxv%value=cubmat(i,j,k)
				maxv%x=orgx+(i-1)*dx
				maxv%y=orgy+(j-1)*dy
				maxv%z=orgz+(k-1)*dz
			end if
			if (cubmat(i,j,k)<minv%value) then
				minv%value=cubmat(i,j,k)
				minv%x=orgx+(i-1)*dx
				minv%y=orgy+(j-1)*dy
				minv%z=orgz+(k-1)*dz
			end if
		end do
	end do
end do

if (infomode==0) then
	fminivol=dx*dy*dz
	write(*,"(' The minimum value:',E16.8,' at',3f12.6,' Bohr')") minv%value,minv%x,minv%y,minv%z
	write(*,"(' The maximum value:',E16.8,' at',3f12.6,' Bohr')") maxv%value,maxv%x,maxv%y,maxv%z
	write(*,"(' Differential element:',f15.10,' Bohr^3')") fminivol
	write(*,"(' Summing up positive value in grid file:  ',f30.10)") sumuppos
	write(*,"(' After multiplied by differential element:',f30.10)") sumuppos*fminivol
	write(*,"(' Summing up negative value in grid file:  ',f30.10)") sumupneg
	write(*,"(' After multiplied by differential element:',f30.10)") sumupneg*fminivol
	write(*,"(' Summing up all value in grid file:       ',f30.10)") sumuppos+sumupneg
	write(*,"(' After multiplied by differential element:',f30.10)") (sumuppos+sumupneg)*fminivol
end if
end subroutine

!!-----------------------------------------------------------------
!!----------- Load Dmol3 .grd file and save to cubmattmp
!If inconsis==1, that means the grid setting of this grd file is inconsistent with cubmat stored in memory
subroutine readgrdtmp(grdname,inconsis)
use defvar
implicit real*8 (a-h,o-z)
character(len=*) grdname
open(10,file=grdname,access="sequential",status="old")
read(10,*)
read(10,*)
read(10,*)
read(10,*) nxtmp,nytmp,nztmp
read(10,*)
if (nxtmp+1/=nx.or.nytmp+1/=ny.or.nztmp+1/=nz) then
	write(*,"(a)") " Error: The grid setting of this grd file is inconsistent with that of the grid data stored in memory!"
	inconsis=1
	return
end if

if (allocated(cubmattmp)) deallocate(cubmattmp)
allocate(cubmattmp(nx,ny,nz))
write(*,*)
write(*,*) "Loading data, please wait..."
do k=1,nz   !a(x,y,z)
	do j=1,ny
		do i=1,nx
			read(10,*) cubmattmp(i,j,k)
		end do
	end do
end do
write(*,*) "Grid data loading completed!"
close(10)
end subroutine




!!------------------------------------------------
!!----------- Load vti file, which can be yielded by ParaView and GIMIC. The code was adapted from readgrd
!infomode=0 means output grd details, =1 not
!ionlygrid=1 means only read grid data, but do not perturb any other variables, =0 means do all
subroutine readvti(vtiname,infomode,ionlygrid)
use defvar
use util
implicit real*8 (a-h,o-z)
character(len=*) vtiname
character c1000tmp*1000
integer infomode,ionlygrid
type(content) maxv,minv
if (ionlygrid==0) ifiletype=14
open(10,file=vtiname,status="old")

call loclabel(10,"<ImageData")
call readxmlline(10,c1000tmp)
i=strcharpos(c1000tmp,'"',1)
j=strcharpos(c1000tmp,'"',2)
read(c1000tmp(i+1:j-1),*) ibeg,iend,jbeg,jend,kbeg,kend
nx=iend-ibeg+1
ny=jend-jbeg+1
nz=kend-kbeg+1
i=strcharpos(c1000tmp,'"',3)
j=strcharpos(c1000tmp,'"',4)
read(c1000tmp(i+1:j-1),*) orgx,orgy,orgz
i=strcharpos(c1000tmp,'"',5)
j=strcharpos(c1000tmp,'"',6)
read(c1000tmp(i+1:j-1),*) dx,dy,dz
gridvec1=0;gridvec2=0;gridvec3=0
gridvec1(1)=dx;gridvec2(2)=dy;gridvec3(3)=dz

allocate(cubmat(nx,ny,nz))
endx=orgx+dx*(nx-1) !endx,y,z are global array in defvar
endy=orgy+dy*(ny-1)
endz=orgz+dz*(nz-1)

if (infomode==0) then
	write(*,"(' Translation vectors in X/Y/Z (Bohr):',3f12.6)") dx,dy,dz
	write(*,"(' The range of x is from ',f12.6,' to ',f12.6,' Bohr,' i5,' points')") ,orgx,endx,nx
	write(*,"(' The range of y is from ',f12.6,' to ',f12.6,' Bohr,',i5,' points')") ,orgy,endy,ny
	write(*,"(' The range of z is from ',f12.6,' to ',f12.6,' Bohr,',i5,' points')") ,orgz,endz,nz
	write(*,"(' Total number of grid points:',i10)") nx*ny*nz
	write(*,"(' This grid data will take up at least',i6,' MB memory')") nx*ny*nz*8*4/1024/1024
end if
write(*,*)
write(*,*) "Loading grid data, please wait..."
call loclabel(10,"<DataArray")
call readxmlline(10,c1000tmp)
if (index(c1000tmp,"scalars")==0) then
    write(*,*) "Error: Only vti file recording scalar data is supported!"
    write(*,*) "Press ENTER button to exit program"
    read(*,*)
    stop
end if
read(10,*) (((cubmat(i,j,k),i=1,nx),j=1,ny),k=1,nz)
write(*,*) "Done!"
write(*,*)
close(10)

!Perform statistic
maxv%value=cubmat(1,1,1)
maxv%x=orgx
maxv%y=orgy
maxv%z=orgz
minv%value=cubmat(1,1,1)
minv%x=orgx
minv%y=orgy
minv%z=orgz
sumuppos=0.0D0
sumupneg=0.0D0
do k=1,nz
	do j=1,ny
		do i=1,nx
			if (cubmat(i,j,k)>0) sumuppos=sumuppos+cubmat(i,j,k)
			if (cubmat(i,j,k)<0) sumupneg=sumupneg+cubmat(i,j,k)
			if (cubmat(i,j,k)>maxv%value) then
				maxv%value=cubmat(i,j,k)
				maxv%x=orgx+(i-1)*dx
				maxv%y=orgy+(j-1)*dy
				maxv%z=orgz+(k-1)*dz
			end if
			if (cubmat(i,j,k)<minv%value) then
				minv%value=cubmat(i,j,k)
				minv%x=orgx+(i-1)*dx
				minv%y=orgy+(j-1)*dy
				minv%z=orgz+(k-1)*dz
			end if
		end do
	end do
end do

if (infomode==0) then
	fminivol=dx*dy*dz
	write(*,"(' The minimum value:',E16.8,' at',3f12.6,' Bohr')") minv%value,minv%x,minv%y,minv%z
	write(*,"(' The maximum value:',E16.8,' at',3f12.6,' Bohr')") maxv%value,maxv%x,maxv%y,maxv%z
	write(*,"(' Differential element:',f15.10,' Bohr^3')") fminivol
	write(*,"(' Summing up positive value in grid file:  ',f30.10)") sumuppos
	write(*,"(' After multiplied by differential element:',f30.10)") sumuppos*fminivol
	write(*,"(' Summing up negative value in grid file:  ',f30.10)") sumupneg
	write(*,"(' After multiplied by differential element:',f30.10)") sumupneg*fminivol
	write(*,"(' Summing up all value in grid file:       ',f30.10)") sumuppos+sumupneg
	write(*,"(' After multiplied by differential element:',f30.10)") (sumuppos+sumupneg)*fminivol
end if
end subroutine

!Return content enclosed by "<" in current line and the next ">" in a file of XML format. At most read 1000 characters
!Before invoking this routine, current position must be the line containing <
subroutine readxmlline(fileid,str)
character str*1000,c1000tmp2*1000
integer fileid,itmp,jtmp
read(fileid,"(a)") str
if (index(str,'>')==0) then
    do while(.true.)
        read(10,"(a)") c1000tmp2
        str=trim(str)//trim(c1000tmp2)
        if (index(c1000tmp2,'>')/=0) exit
    end do
end if
itmp=index(str,'<')
jtmp=index(str,'>')
str=str(itmp+1:jtmp-1)
end subroutine




!!-----------------------------------------------------------------
!!-------------------- Read .wfn file, infomode=0 means output wfn property, =1 not
subroutine readwfn(name,infomode)
use defvar
use util
implicit real*8 (a-h,o-z)
CHARACTER(LEN=*) name
character*80 wfntitle,lastline,c80tmp*80
real*8,allocatable :: tmpCO(:,:),tmpMOocc(:),tmpMOene(:)
integer,allocatable :: tmpMOtype(:)
integer i,j,infomode
!Original .wfn format doesn't support g, however the .wfn outputted by Multiwfn, Molden2AIM and G09 since B.01 formally supports g
!Below is the g sequence used in Molden2AIM, .wfx, .molden and the .wfn outputted by Multiwfn and G09 since B.01
! 21 XXXX 22 YYYY 23 ZZZZ 24 XXXY 25 XXXZ
! 26 XYYY 27 YYYZ 28 XZZZ 29 YZZZ 30 XXYY
! 31 XXZZ 32 YYZZ 33 XXYZ 34 XYYZ 35 XYZZ
!Below is the g sequence internally used in Multiwfn, identical to .fch
! 21 ZZZZ 22 YZZZ 23 YYZZ 24 YYYZ 25 YYYY
! 26 XZZZ 27 XYZZ 28 XYYZ 29 XYYY 30 XXZZ
! 31 XXYZ 32 XXYY 33 XXXZ 34 XXXY 35 XXXX
! convGseq is used to convert g used in .wfn to the internal sequence of Multiwfn 
! I assume that h sequence is identical to .wfx and Multiwfn
integer :: convGseq(35)=(/ (0,i=1,20), 35,25,21,34,33, 29,24,26,22,32, 30,23,31,28,27 /)
ifiletype=2
imodwfn=0
open(10,file=name,access="sequential",status="old")
read(10,"(a)") wfntitle
read(10,"(a8,i15,13x,i7,11x,i9)") c80tmp,nmo,nprims,ncenter
ibasmode=1
if (index(c80tmp,"SLATER")/=0) ibasmode=2
if (ibasmode==2) then
	write(*,"(a)") " Error: Multiwfn does not support the wfn file recording Slater type orbitals! Press ENTER button to exit"
	read(*,*)
	stop
end if
if (index(wfntitle,"Run Type")/=0) then
	write(*,"(a)") " Warning: It seems that this .wfn file was generated by ORCA. Notice that the .wfn file generated by ORCA is often non-standard, &
	and usually makes Multiwfn crash. Using .molden file as input file instead is recommended."
end if

allocate(a(ncenter))
allocate(b(nprims))
allocate(co(nmo,nprims))
allocate(MOocc(nmo))
allocate(MOene(nmo))
allocate(MOtype(nmo))

do i=1,ncenter
	read(10,"(a24,3f12.8,10x,f5.1)") c80tmp,a(i)%x,a(i)%y,a(i)%z,a(i)%charge
	read(c80tmp,*) a(i)%name
	call lc2uc(a(i)%name(1:1))
	call uc2lc(a(i)%name(2:2))
	do j=0,nelesupp
		if (a(i)%name==ind2name(j)) then
			a(i)%index=j
			exit
		end if
		if (j==nelesupp) then
			write(*,"(3a,i5)") " Warning: Found unknown element ",a(i)%name," with index of",i
			write(*,*) "The atom will be recognized as hydrogen internally without nuclear charge"
			write(*,*) "Press ENTER button to continue"
			read(*,*)
			a(i)%index=1
		end if
	end do
end do
read(10,"(20x,20i3)") (b(i)%center,i=1,nprims)
read(10,"(20x,20i3)") (b(i)%type,i=1,nprims)
do i=1,nprims	
	if (b(i)%type>=21.and.b(i)%type<=35) b(i)%type=convGseq(b(i)%type)
end do

!Read exponents
read(10,"(10x,5E14.7)") (b(i)%exp,i=1,nprims)

!From Gaussian09 B.01, if ECP is used, additional CENTER, TYPE, EXPONENTS field present to represent EDF(electron density functions) likewise .wfx file
!However such wfn file is foolish, the number of GTFs used to represent EDF is not explicitly recorded, so we must use trick to guess the number
!The coefficient of EDF is not written, how we use these EDF information? Obviously impossible! So we just skip these mad fields
read(10,"(a)") c80tmp
if (c80tmp(1:6)=="CENTRE") then
	call loclabel(10,"MO",ifound,0)
! 	ncenline=1
! 	do while(.true.) !Move to the first line of TYPE field, and count we passed how many rows (the number of lines of CENTER field)
! 		read(10,"(a)") c80tmp
! 		if (c80tmp(1:6)/="CENTRE") exit
! 		ncenline=ncenline+1
! 	end do
! 	backspace(10)
! 	backspace(10) !Return to the last line of CENTER field
! 	read(10,"(a)") c80tmp
! 	nEDFprims=(len_trim(c80tmp)-20)/3+(ncenline-1)*20
! 	do itmp=1,ncenline !Move to the first line of CENTER field, make ready to load EDF information
! 		backspace(10)
! 	end do
! 	allocate(b_EDF(nEDFprims),CO_EDF(nEDFprims))
! 	read(10,"(20x,20i3)") (b_EDF(i)%center,i=1,nEDFprims)
! 	read(10,"(20x,20i3)") (b_EDF(i)%type,i=1,nEDFprims)
! 	read(10,"(10x,5E14.7)") (b_EDF(i)%exp,i=1,nEDFprims)
else
	backspace(10)
end if

!Read orbitals
do i=1,nmo
	read(10,"(a)") c80tmp
	do ichar=1,80
		if (c80tmp(ichar:ichar)=='=') then
			read(c80tmp(ichar+1:),*) MOocc(i)
			exit
		end if
	end do
	do ichar=80,1,-1
		if (c80tmp(ichar:ichar)=='=') then
			read(c80tmp(ichar+1:),*) MOene(i)
			exit
		end if
	end do
! 	read(10,"(5E16.8)") (co(i,j),j=1,nprims) ! Note: row/column of CO denote MO/basis function respectively, in contrary to convention
	read(10,"(5E16.8)") co(i,:)
end do
read(10,*)
!Use free format to read in energy and virial to ensure compatibility
read(10,"(a)") lastline
iequalsign1=0
iequalsign2=0
do i=1,80
	if (lastline(i:i)=='=') then
		iequalsign1=i
		exit
	end if
end do
do i=80,1,-1
	if (lastline(i:i)=='=') then
		iequalsign2=i
		exit
	end if
end do
totenergy=0
virialratio=2
if (iequalsign1/=0) read(lastline(iequalsign1+1:),*) totenergy
if (iequalsign1==0) write(*,*) "Warning: Unable to find system energy in this file!"
if (iequalsign2/=0) read(lastline(iequalsign2+1:),*) virialratio
if (iequalsign2==0) write(*,*) "Warning: Unable to find Virial in this file!"
call loclabel(10,"$MOSPIN $END",ifoundmospin,0) !Also read spin-type from $MOSPIN $END field outputted by Molden2AIM since v2.0.5, if detected
if (ifoundmospin==1) then !Have defined spin-type explicitly, don't reset spin-type by guessing later
	if (infomode==0) write(*,*) "Note: Found $MOSPIN $END field, orbital spin-types are loaded"
	read(10,*)
	read(10,*) MOtype
	where (MOtype==3) MOtype=0
end if

close (10)

!Determine type of wavefunction
if (sum(MOocc)==2*nmo.and.all(int(MOocc)==MOocc)) then
	wfntype=0 !This is restricted wavefunction
	MOtype=0
else if (sum(MOocc)==nmo.and.all(int(MOocc)==MOocc)) then
	wfntype=1 !This is unrestricted wavefunction
	if (ifoundmospin==0) then
		MOtype=1 !Set all MO is alpha
		do i=2,nmo !if nmo=1, i will be set to 2, and no errors will appear
			if (MOene(i)<MOene(i-1)) exit
		end do
		MOtype(i:nmo)=2 !beta
	end if
else if (any(MOocc/=int(MOocc))) then
	if (nint(maxval(MOocc))==2) then !maximum occupation close to 2, so considered as restricted Post-HF wavefunction
		wfntype=3
		MOtype=0
	else
		wfntype=4 !This is unrestricted Post-HF wavefunction
		if (ifoundmospin==0) then
			MOtype=0
			do i=2,nmo
				if (MOocc(i)>MOocc(i-1)) then
					MOtype(1:i-1)=1
					MOtype(i:nmo)=2
					exit
				end if
			end do
		end if
	end if
else
	wfntype=2 !This is RO wavefunction
	MOtype=0
	do i=1,nmo
		if (MOocc(i)==1) MOtype(i)=1 !alpha
	end do
end if
!Count electrons
call updatenelec

!Sort orbitals so that the orbitals with same spin-type are contiguous, because the wfn file outputted by Molden2AIM is not always in line with this convention
if (ifoundmospin==1.and.(wfntype==1.or.wfntype==4)) then
	allocate(tmpCO(nmo,nprims),tmpMOtype(nmo),tmpMOocc(nmo),tmpMOene(nmo))
	ipos=0
	do itime=1,2
		do imo=1,nmo
			if ((itime==1.and.MOtype(imo)==1).or.(itime==2.and.MOtype(imo)==2)) then !Move alpha orbitals to tmp arrays
				ipos=ipos+1
				tmpCO(ipos,:)=CO(imo,:)
				tmpMOocc(ipos)=MOocc(imo)
				tmpMOene(ipos)=MOene(imo)
				tmpMOtype(ipos)=MOtype(imo)
			end if
		end do
	end do
	CO=tmpCO
	MOocc=tmpMOocc
	MOene=tmpMOene
	MOtype=tmpMOtype
	write(*,*) "Note: Sequence of orbitals has been sorted according to spin-type"
	deallocate(tmpCO,tmpMOocc,tmpMOene,tmpMOtype)
end if

!Summary
if (infomode==0) then
	write(*,*)
	write(*,"(' Total energy:',f21.12,' Hartree,   Virial ratio:',f12.8)") totenergy,virialratio
	write(*,"(' Total/Alpha/Beta electrons:',3f12.4)") nelec,naelec,nbelec
	write(*,"(' Net charge:',f12.5,'    Expected multiplicity:',i5)") sum(a(:)%charge)-nelec,nint(naelec-nbelec)+1
	write(*,"(' The number of orbitals:',i6,',  Atoms:',i7,',  GTFs:',i7)") nmo,ncenter,nprims
	if (wfntype==0) write(*,"(' This is restricted closed-shell single-determinant wavefunction')")
	if (wfntype==1) write(*,"(' This is unrestricted single-determinant wavefunction')")
	if (wfntype==2) write(*,"(' This is restricted open-shell wavefunction')")
	if (wfntype==3) write(*,"(' This is closed-shell post-HF wavefunction')")
	if (wfntype==4) write(*,"(' This is open-shell post-HF wavefunction')")
	if (wfntype==1.or.wfntype==4) then
		do i=1,nmo
			if (MOtype(i)==2) exit
		end do
		write(*,"(' Orbitals from 1 to',i6,' are alpha type, from',i6,' to',i6,' are beta type')") i-1,i,nmo
	end if
	write(*,"(' Title line of this file: ',a)") trim(wfntitle)
end if
end subroutine



!!-----------------------------------------------------------------
!!-------- Read .wfx. mode=0 means output related information, =1 Do not output
subroutine readwfx(name,infomode)
use defvar
use util
implicit real*8 (a-h,o-z)
CHARACTER(LEN=*) name
character spintype*20
integer infomode
!Below is the g sequence used in Molden2AIM, .wfx, .molden and the .wfn outputted by Multiwfn and G09 since B.01
! 21 XXXX 22 YYYY 23 ZZZZ 24 XXXY 25 XXXZ
! 26 XYYY 27 YYYZ 28 XZZZ 29 YZZZ 30 XXYY
! 31 XXZZ 32 YYZZ 33 XXYZ 34 XYYZ 35 XYZZ
!Below is the g sequence internally used in Multiwfn, identical to .fch
! 21 ZZZZ 22 YZZZ 23 YYZZ 24 YYYZ 25 YYYY
! 26 XZZZ 27 XYZZ 28 XYYZ 29 XYYY 30 XXZZ
! 31 XXYZ 32 XXYY 33 XXXZ 34 XXXY 35 XXXX
! convGseq is used to convert g used in .wfx to the internal sequence of Multiwfn
! PS: spdfh sequence in .wfx is identical to Multiwfn
integer :: convGseq(35)=(/ (0,i=1,20), 35,25,21,34,33, 29,24,26,22,32, 30,23,31,28,27 /)
ifiletype=3
imodwfn=0
open(10,file=name,access="sequential",status="old")
call loclabel(10,"<Number of Nuclei>")
read(10,*)
read(10,*) ncenter
if (allocated(a)) deallocate(a)
allocate(a(ncenter))
call loclabel(10,"<Number of Primitives>")
read(10,*)
read(10,*) nprims
allocate(b(nprims))
call loclabel(10,"<Number of Occupied Molecular Orbitals>")
read(10,*)
read(10,*) nmo
allocate(MOocc(nmo),MOene(nmo),MOtype(nmo),co(nmo,nprims))
call loclabel(10,"<Nuclear Names>")
read(10,*)
read(10,*) a%name
do i=1,ncenter !Multiwfn does not allow number included in atom name
	if (iachar(a(i)%name(2:2))<=57.and.iachar(a(i)%name(2:2))>=48) a(i)%name(2:2)=' '
end do
call loclabel(10,"<Atomic Numbers>")
read(10,*)
read(10,*) a%index
call loclabel(10,"<Nuclear Charges>")
read(10,*)
read(10,*) a%charge
call loclabel(10,"<Nuclear Cartesian Coordinates>")
read(10,*)
read(10,*) ((a(i)%x,a(i)%y,a(i)%z),i=1,ncenter)
call loclabel(10,"<Number of Electrons>")
read(10,*)
read(10,*) nelec
call loclabel(10,"<Number of Alpha Electrons>")
read(10,*)
read(10,*) naelec
call loclabel(10,"<Number of Beta Electrons>")
read(10,*)
read(10,*) nbelec
call loclabel(10,"<Primitive Centers>")
read(10,*)
read(10,*) b%center
call loclabel(10,"<Primitive Types>")
read(10,*)
read(10,*) b%type
!The g sequence in .wfx is not identical to Multiwfn, convert them here
do i=1,nprims	
	if (b(i)%type>=21.and.b(i)%type<=35) b(i)%type=convGseq(b(i)%type)
end do
call loclabel(10,"<Primitive Exponents>")
read(10,*)
read(10,*) b%exp
!------ Process EDF information
call loclabel(10,"<Number of EDF Primitives>",ifound)
if (ifound==1.and.readEDF==1) then
	write(*,*) "Loading electron density functions (EDF) field in this file..."
	read(10,*)
	read(10,*) nEDFprims
	allocate(b_EDF(nEDFprims),CO_EDF(nEDFprims))
	call loclabel(10,"<EDF Primitive Centers>")
	read(10,*)
	read(10,*) b_EDF%center
	call loclabel(10,"<EDF Primitive Types>")
	read(10,*)
	read(10,*) b_EDF%type !We assume all the type index is 1 (S type)
	if (maxval(b_EDF%type)>1) then
		write(*,*) "ERROR: All GTFs of electron density function must be S type! Press ENTER button to exit"
		read(*,*)
		stop
	end if
	call loclabel(10,"<EDF Primitive Exponents>")
	read(10,*)
	read(10,*) b_EDF%exp
	call loclabel(10,"<EDF Primitive Coefficients>")
	read(10,*)
	read(10,*) CO_EDF
	call loclabel(10,"<Number of Core Electrons>")
	read(10,*)
	read(10,*) nEDFelec
	if (infomode==0) write(*,"(a,i6,a)") " Note: EDF information represents",nEDFelec," inner-core electrons"
end if
call loclabel(10,"<Molecular Orbital Occupation Numbers>")
read(10,*)
read(10,*) MOocc
call loclabel(10,"<Molecular Orbital Energies>")
read(10,*)
read(10,*) MOene
call loclabel(10,"<Molecular Orbital Spin Types>")
read(10,*)
do i=1,nmo
	read(10,"(a20)") spintype
	if (adjustl(spintype)=="Alpha and Beta") MOtype(i)=0 !adjustl is needed, because the wfx outputted by ORCA is non-standard
	if (adjustl(spintype)=="Alpha") MOtype(i)=1
	if (adjustl(spintype)=="Beta") MOtype(i)=2
end do
call loclabel(10,"<Molecular Orbital Primitive Coefficients>")
read(10,*)
do i=1,nmo
	read(10,*)
	read(10,*)
	read(10,*)
	read(10,*) CO(i,:)
end do
call loclabel(10,"<Energy = T + Vne + Vee + Vnn>",ifound,0) !Don't rewind, otherwise Multiwfn will scan lots of information of MO field
read(10,*)
read(10,*) totenergy
call loclabel(10,"<Virial Ratio (-V/T)>",ifound,0)
read(10,*)
read(10,*) virialratio
close(10)

!Angular moment of GTF should be no higher than h
if (any(b%type>56)) then
	write(*,"(' Warning: Angular moment of one or more GTFs exceeds h, Multiwfn is unable to deal with this case! Its/their contributions will be discarded')")
	write(*,*) "Press ENTER button to continue"
	read(*,*)
	do iGTF=1,nprims
		if (b(iGTF)%type>56) then
			b(iGTF)%type=1 !Assume it is S type
			CO(:,iGTF)=0D0
		end if
	end do
end if

if ( all(MOocc==nint(MOocc)) ) then
	wfntype=2
	if (nmo==nint(nelec)) wfntype=1
	if (nmo==nint(nelec)/2) wfntype=0
else !post-HF
	if (naelec==nbelec) wfntype=3
	if (naelec/=nbelec) wfntype=4
end if

if (infomode==0) then
	write(*,*)
	write(*,"(' Total energy:',f21.12,' Hartree,   Virial ratio:',f12.8)") totenergy,virialratio
	write(*,"(' Total/Alpha/Beta electrons:',3f12.4)") nelec,naelec,nbelec
	write(*,"(' Number of orbital:',i6,',  Atoms:',i7,',  GTFs:',i7)") nmo,ncenter,nprims
	if (wfntype==0) write(*,"(' This is restricted closed-shell single-determinant wavefunction')")
	if (wfntype==1) write(*,"(' This is unrestricted single-determinant wavefunction')")
	if (wfntype==2) write(*,"(' This is restricted open-shell wavefunction')")
	if (wfntype==3) write(*,"(' This is closed-shell post-HF wavefunction')")
	if (wfntype==4) write(*,"(' This is open-shell post-HF wavefunction')")
	if (wfntype==1.or.wfntype==4) then
		do i=1,nmo
			if (MOtype(i)==2) exit
		end do
		write(*,"(' Orbitals from 1 to',i6,' are alpha type, from',i6,' to',i6,' are beta type')") i-1,i,nmo
	end if
	write(*,*)
end if
end subroutine



!!------- Load EDF information from external atomic .wfx files
subroutine readEDFatmwfx
use defvar
use util
implicit real*8 (a-h,o-z)
character elewfxfilename(110)*200,c200tmp*200
real*8,allocatable :: EDFCOtmp(:),EDFexptmp(:)
integer,allocatable :: EDFtypetmp(:)
integer atmsel(nelesupp,ncenter),natmsel(nelesupp)
iwfxtime=1
nEDFprims=0
nEDFelec=0
do while(.true.)
	write(*,*) "Load the inner-core density (EDF information) for which element? e.g. Fe"
	write(*,*) "You can also input atomic indices, e.g. 5,8-10,31 means selecting 5,8,9,10,31"
	write(*,*) "Note: If finished, input ""q"""
	read(*,"(a)") c200tmp
	itmp=ichar(c200tmp(1:1))
	if (c200tmp=='q') then
		exit
	else if (itmp>=48.and.itmp<=57) then !Inputted is atomic indices
		call str2arr(c200tmp,natmsel(iwfxtime),atmsel(iwfxtime,:))
	else !Inputted is element name
		call lc2uc(c200tmp(1:1)) !Make the first/second character in upper/lower case
		call uc2lc(c200tmp(2:2))
		natmsel(iwfxtime)=0
		do iatm=1,ncenter
			if (a(iatm)%name==c200tmp(1:2)) then
				natmsel(iwfxtime)=natmsel(iwfxtime)+1
				atmsel(iwfxtime,natmsel(iwfxtime))=iatm
			end if
		end do
	end if
	if (natmsel(iwfxtime)==0) then
		write(*,*) "No atoms are selected, input again"
		write(*,*)
	else if (natmsel(iwfxtime)>0) then
		write(*,"(' The number of atoms selected is',i7,',  including:')") natmsel(iwfxtime)
		write(*,"(12i6)") atmsel(iwfxtime,1:natmsel(iwfxtime))
		write(*,*)
		write(*,*) "Load EDF information from which file? e.g. C:\ltwd\Fe_lanl2.wfx"
		do while(.true.)
			read(*,*) elewfxfilename(iwfxtime)
			inquire(file=elewfxfilename(iwfxtime),exist=alive)
			if (alive) exit
			write(*,*) "Cannot find this file, input again"
		end do
		open(10,file=elewfxfilename(iwfxtime),status="old") !Count how many EDF GTFs in this file
		call loclabel(10,"<Number of EDF Primitives>",ifound)
		if (ifound==0) then
			write(*,*) "Error: Unable to find EDF information from this file!"
			cycle
		end if
		read(10,*)
		read(10,*) nEDFtmp
		call loclabel(10,"<Number of Core Electrons>")
		read(10,*)
		read(10,*) nEDFelectmp
		close(10)
		write(*,"(' The number of EDF primitives in this file is',i5,/)") nEDFtmp
		nEDFprims=nEDFprims+nEDFtmp*natmsel(iwfxtime)
		nEDFelec=nEDFelec+nEDFelectmp*natmsel(iwfxtime)
		iwfxtime=iwfxtime+1
	end if
end do
nwfxtime=iwfxtime-1
write(*,"(' The total number of EDF primitives is',i7)") nEDFprims
write(*,"(' The total number of inner-core electrons represented by EDF is',i8)") nEDFelec
allocate(b_EDF(nEDFprims),CO_EDF(nEDFprims))
ipos=1
do iwfxtime=1,nwfxtime
	open(10,file=elewfxfilename(iwfxtime),status="old")
	call loclabel(10,"<Number of EDF Primitives>",ifound)
	read(10,*)
	read(10,*) nEDFtmp
	allocate(EDFCOtmp(nEDFtmp),EDFexptmp(nEDFtmp),EDFtypetmp(nEDFtmp))
	call loclabel(10,"<EDF Primitive Types>")
	read(10,*)
	read(10,*) EDFtypetmp
	call loclabel(10,"<EDF Primitive Exponents>")
	read(10,*)
	read(10,*) EDFexptmp
	call loclabel(10,"<EDF Primitive Coefficients>")
	read(10,*)
	read(10,*) EDFCOtmp
	do iatm=1,natmsel(iwfxtime)
		b_EDF(ipos:ipos+nEDFtmp-1)%type=EDFtypetmp
		b_EDF(ipos:ipos+nEDFtmp-1)%exp=EDFexptmp
		b_EDF(ipos:ipos+nEDFtmp-1)%center=atmsel(iwfxtime,iatm)
		CO_EDF(ipos:ipos+nEDFtmp-1)=EDFCOtmp
		ipos=ipos+nEDFtmp
	end do
	deallocate(EDFCOtmp,EDFexptmp,EDFtypetmp)
	close(10)
end do
write(*,*) "The EDF information have been loaded"
end subroutine


!!------ Load EDF information from EDFlib provided by Zork
!See http://bbs.keinsci.com/forum.php?mod=viewthread&tid=5354 for description
!infomode=0/1 show/don't show info
subroutine readEDFlib(infomode)
use defvar
implicit real*8 (a-h,o-z)
real*8 EDFcoeff(100),EDFexp(100)
if (infomode==0) write(*,"(a)") " Loading electron density functions (EDF) information from &
built-in EDF library... The library is freely available at https://github.com/zorkzou/Molden2AIM"
nEDFprims=0
nEDFelec=0
!First time, find total number of EDF GTFs so that b_EDF and CO_EDF can be allocated
do iatm=1,ncenter
	natmcore=a(iatm)%index-nint(a(iatm)%charge)
	if (natmcore==0) cycle
	if (a(iatm)%index==0) cycle !Skip Bq
	nEDFelec=nEDFelec+natmcore
	call EDFLIB(a(iatm)%index,natmcore,nfun,EDFexp,EDFcoeff)
	if (infomode==0) write(*,"(1x,a,'(',i5,')      Core electrons:',i3,'     EDF primitive GTFs:',i3)") a(iatm)%name,iatm,natmcore,nfun
	if (nfun==0) then
		if (infomode==0) write(*,*) "Warning: Unable to find proper EDF information for this atom!"
		if (infomode==0) write(*,*) "Press Enter button to skip loading EDF information for this atom"
		read(*,*)
	end if
	nEDFprims=nEDFprims+nfun
end do
if (infomode==0) write(*,"(' The number of total inner-core electrons:',i6)") nEDFelec
if (infomode==0) write(*,"(' The number of total EDF primitive GTFs:',i6)") nEDFprims

allocate(b_EDF(nEDFprims),CO_EDF(nEDFprims))
ifun=0
b_EDF%type=0
do iatm=1,ncenter
	natmcore=a(iatm)%index-nint(a(iatm)%charge)
	if (natmcore==0) cycle
	call EDFLIB(a(iatm)%index,natmcore,nfun,EDFexp,EDFcoeff)
	if (nfun==0) cycle !Didn't find corresponding EDF information
	b_EDF(ifun+1:ifun+nfun)%exp=EDFexp(1:nfun)
	b_EDF(ifun+1:ifun+nfun)%center=iatm
	CO_EDF(ifun+1:ifun+nfun)=EDFcoeff(1:nfun)
	ifun=ifun+nfun
end do
if (infomode==0) write(*,"(a,/)") " Loading EDF library finished!"
end subroutine



!!-----------------------------------------------------------------
!!--------- Read Molden input file, get coordinate, basis function and GTF information
!Known issue:
!CFour sometimes fail (e.g. benzene)
subroutine readmolden(name,infomode) !infomode=0 means output info, =1 silent
use defvar
use util
implicit real*8 (a-h,o-z)
character(len=*) name
character c80*80,c80tmp*80,symtmp*4
integer,allocatable :: shelltype(:),shellcon(:),shell2atom(:) !The definition of shelltype is identical to .fch
integer :: s2f(-5:5,21)=0 !Give shell type & orbital index to get functype
real*8,allocatable :: primexp(:),concoeff(:)
real*8,allocatable :: amocoeff(:,:),bmocoeff(:,:)
real*8 conv5d6d(6,5),conv7f10f(10,7),conv9g15g(15,9),conv11h21h(21,11)
real*8 conv5d6dtr(5,6),conv7f10ftr(7,10),conv9g15gtr(9,15),conv11h21htr(11,21)
!For backing up spherical basis functions
integer,allocatable :: shelltype5D(:),MOtype5D(:)
character*4,allocatable :: MOsym5D(:)
real*8,allocatable :: CObasa5D(:,:),CObasb5D(:,:),Sbas5D(:,:),Dbas5D(:,:,:),Magbas5D(:,:,:),MOene5D(:),MOocc5D(:),CO5D(:,:)
real*8,external :: normgau
ifiletype=9
imodwfn=0
s2f(-5,1:11)=(/ -32,-31,-30,-29,-28,-27,-26,-25,-24,-23,-22 /)
s2f(-4,1:9)=(/ -21,-20,-19,-18,-17,-16,-15,-14,-13 /)
s2f(-3,1:7)=(/ -12,-11,-10,-9,-8,-7,-6 /)
s2f(-2,1:5)=(/ -5,-4,-3,-2,-1 /)
s2f(-1,1:4)=(/ 1,2,3,4 /)
s2f(0,1)=1
s2f(1,1:3)=(/ 2,3,4 /)
s2f(2,1:6)=(/ 5,6,7,8,9,10 /)
!---------- The sequence of f functions in Multiwfn (=wfn=wfx) is not identical to Molden, so convert here
!11  12  13  14  15  16  17  18  19  20  !Multiwfn sequence
!XXX YYY ZZZ XXY XXZ YYZ XYY XZZ YZZ XYZ
!xxx yyy zzz xyy xxy xxz xzz yzz yyz xyz !Molden sequence
s2f(3,1:10)=(/ 11,12,13,17,14,15,18,19,16,20 /)
!---------- The sequence of g functions in Multiwfn (=fch) is not identical to Molden, so convert here
! 21   22   23   24   25   26   27   28   29   30   31   32   33   34   35  !Multiwfn sequence
!ZZZZ YZZZ YYZZ YYYZ YYYY XZZZ XYZZ XYYZ XYYY XXZZ XXYZ XXYY XXXZ XXXY XXXX
!xxxx yyyy zzzz xxxy xxxz yyyx yyyz zzzx zzzy xxyy xxzz yyzz xxyz yyxz zzxy !Molden sequence
s2f(4,1:15)=(/ 35,25,21,34,33,29,24,26,22,32,30,23,31,28,27 /)
!---------- The sequence of h functions in Multiwfn (=fch=wfx) is not identical to Molden, so convert here
!Note that h angular moment is not formally supported by .molden format!!!
! 36    37    38    39    40    41    42    43    44    45    46  !Multiwfn sequence
!ZZZZZ YZZZZ YYZZZ YYYZZ YYYYZ YYYYY XZZZZ XYZZZ XYYZZ XYYYZ XYYYY
!xxxxx yyyyy zzzzz xxxxy xxxxz xyyyy xzzzz yyyyz yzzzz xxxyy xxxzz !Molden sequence
! 47    48    49    50    51    52    53    54    55    56  !Multiwfn sequence
!XXZZZ XXYZZ XXYYZ XXYYY XXXZZ XXXYZ XXXYY XXXXZ XXXXY XXXXX
!xxyyy xxzzz yyyzz yyzzz xxxyz xyyyz xyzzz xxyyz xxyzz xyyzz !Molden sequence
s2f(5,1:21)=(/ 56,41,36,55,54,46,42,40,37,53,51,50,47,39,38,52,45,43,49,48,44 /)

call gensphcartab(2,conv5d6d,conv7f10f,conv9g15g,conv11h21h)
conv5d6dtr=transpose(conv5d6d)
conv7f10ftr=transpose(conv7f10f)
conv9g15gtr=transpose(conv9g15g)
conv11h21htr=transpose(conv11h21h)

open(10,file=name,access="sequential",status="old")

if (infomode==0) write(*,*) "Loading various information of the wavefunction"
!!!!! Load atom information
call loclabel(10,"[Atoms]",ifound)
if (ifound==0) call loclabel(10,"[ATOMS]",ifound)
ilenunit=1 !Default length unit is a.u. =2 means Angstrom
read(10,"(a)") c80
if (index(c80,"Angs")/=0) ilenunit=2
ncenter=0
do while(.true.) !Count the number of atoms
	read(10,"(a)") c80
	if (c80==" ".or.index(c80,"[")/=0) then !Have passed atom field
		exit
	end if
	ncenter=ncenter+1
end do
if (allocated(a)) deallocate(a)
allocate(a(ncenter))
call loclabel(10,"[Atoms]",ifound) !Return to [Atoms]
if (ifound==0) call loclabel(10,"[ATOMS]",ifound)
read(10,"(a)") c80
!NOTICE: molden input file has a severe drawback, namely atomic charge is not explicitly recorded, this will be problematic when ECP is used
!In Multiwfn, atomic index is determined according to atomic name, while "atom number" column is read as atomic charge. Therefore, if you already konw
!current file have used ECP, then you can open the file and manually change the atomic number to atomic charge.
do iatm=1,ncenter
	read(10,*) c80,nouse,a(iatm)%charge,a(iatm)%x,a(iatm)%y,a(iatm)%z
	call lc2uc(c80(1:1)) !Convert to upper case
	call uc2lc(c80(2:2)) !Convert to lower case
	do i=1,nelesupp
		if (iachar(c80(2:2))>=48.and.iachar(c80(2:2))<=57) c80(2:2)=' ' !Dalton adds number after element name
		if (c80(1:2)==ind2name(i)) then
			a(iatm)%index=i
			a(iatm)%name=ind2name(i)
			exit
		end if
	end do
	if (i==nelesupp+1) then
		write(*,"(' Error: Unable to recognize atom name ',a)") trim(c80)
		write(*,*) "Please check your input file. Now press Enter button to exit"
		read(*,*)
		stop
	end if
end do
if (ilenunit==2) then !Angstrom->a.u.
	a%x=a%x/b2a
	a%y=a%y/b2a
	a%z=a%z/b2a
end if

!Detect the Molden input file is produced by which program. Special treatment is needed for ORCA, xtb and CFOUR
!If iorca/ixtb/icfour=1, that means this file was generated by orca/xtb/CFOUR
rewind(10)
iorca=0
ixtb=0
icfour=0
do while(.true.)
	read(10,"(a)") c80
	if (index(c80,"[GTO]")/=0) exit
	call struc2lc(c80)
	if (index(c80,"orca")/=0) then
		iorca=1
		exit
	else if (index(c80,"cfour")/=0) then
		icfour=1
		exit
	end if
end do
if (iorca==1.and.infomode==0) write(*,"(a)") " This file is recognized to be generated by ORCA because there is ""orca"" word in title line. &
Special treatments are applied..."
if (icfour==1.and.infomode==0) write(*,"(a)") " This file is recognized to be generated by CFour because there is ""cfour"" word in title line. &
Special treatments are applied..."
if (iorca==0.and.icfour==0) then !Test if it may be generated by xtb using the first three lines
	rewind(10)
	read(10,"(a)") c80
	if (c80=="[Molden Format]") then
		read(10,"(a)") c80
		if (c80=="[Title]") then
			read(10,"(a)") c80
			if (c80=="[Atoms] AU") then
				ixtb=1
				if (ixtb==1.and.infomode==0) write(*,*) "This file is found to be generated by xtb! Special treatment is applied..."
			end if
		end if
	end if
end if

!!!!! Load basis set, and build up GTF information
if (infomode==0) write(*,*) "Loading basis set definition..."
call loclabel(10,"[GTO]",ifound)
if (ifound==0) then
	write(*,*) "Error: Unable to locate [GTO] field! Press ENTER button to exit"
	write(*,*) "Note: [STO] is currently not supported"
	read(*,*)
	stop
end if

!First time, we count how many shells are there to allocate proper size of allocatable arrays
nshell=0
nprimshell=0
read(10,*)
do while(.true.)
	read(10,*) iatm
	do while(.true.)
		read(10,*) c80,ncon
		nshell=nshell+1
		iaddnprmsh=0
		do ish=1,ncon
			read(10,*) tmpv1,tmpv2
			if (tmpv2/=0D0) iaddnprmsh=iaddnprmsh+1 !Many GTF shells outputted by Molpro have zero concontraction coefficient, they shouldn't be read in
		end do
		nprimshell=nprimshell+iaddnprmsh
		if (index(c80,"sp")/=0.or.index(c80,"SP")/=0) then !sp shell will be separated as s and p
			nshell=nshell+1
			nprimshell=nprimshell+iaddnprmsh
		end if
		read(10,"(a)") c80
		if (c80==" ") exit
		backspace(10)
	end do
	read(10,"(a)") c80
	if (c80==" ".or.index(c80,"[")/=0) exit !Finished reading [GTO] field
	backspace(10)
end do

!Second time, read basis set information actually
allocate(shelltype(nshell),shellcon(nshell),shell2atom(nshell))
allocate(primexp(nprimshell),concoeff(nprimshell))
call loclabel(10,"[GTO]",ifound)
shellcon=0
ishell=0
iprimshell=0
read(10,*) 
do while(.true.)
	read(10,*) iatm
	do while(.true.)
		read(10,*) c80,ncon
		ishell=ishell+1
		shell2atom(ishell)=iatm
		!Determine shell type of basis function, here we first assume they are all Cartesian type
		if (index(c80,"sp")/=0.or.index(c80,"SP")/=0) then
			shelltype(ishell)=-1
		else if (index(c80,"s")/=0.or.index(c80,"S")/=0) then
			shelltype(ishell)=0
		else if (index(c80,"p")/=0.or.index(c80,"P")/=0) then
			shelltype(ishell)=1
		else if (index(c80,"d")/=0.or.index(c80,"D")/=0) then
			shelltype(ishell)=2
		else if (index(c80,"f")/=0.or.index(c80,"F")/=0) then
			shelltype(ishell)=3
		else if (index(c80,"g")/=0.or.index(c80,"G")/=0) then
			shelltype(ishell)=4
		else if (index(c80,"h")/=0.or.index(c80,"H")/=0) then
			shelltype(ishell)=5
		end if
		iprimshellold=iprimshell
		do ish=1,ncon !Read exponents and contraction coefficients. For SP, here load the S one
			read(10,*) exptmp,concoefftmp
			if (concoefftmp==0D0) cycle !The shell with zero contraction coefficients will be ripped out
			iprimshell=iprimshell+1
			shellcon(ishell)=shellcon(ishell)+1
			primexp(iprimshell)=exptmp
			if (iorca==1.or.ixtb==1) then !ORCA/xtb doesn't present SP shell in Molden input file, so don't worry about -1
				!The normalization factor of spherical harmonic GTFs are weirdly multiplied into contraction coefficients,&
				!so here normalization factor is eliminated out from contraction coefficients to meet common convention
				rnorm=renormgau_ORCA(primexp(iprimshell),shelltype(ishell))
				concoefftmp=concoefftmp/rnorm
			end if
			concoeff(iprimshell)=concoefftmp
! 			write(*,"(2i4,2f18.10)") iatm,shelltype(ishell),primexp(iprimshell),concoefftmp
		end do
		nprmshadd=iprimshell-iprimshellold
		!For ORCA (or may be also other programs), d,f,g basis functions are not properly normalized, &
		!e.g. for ORCA at current stage d are normalized to 3, so renormalization (i.e. modifying contraction coefficients) is required and thus performed here
		!The LCAO coefficients always correspond to normalized basis functions (but someones normalized to -1, will be fixed at later stage)
		if (shelltype(ishell)/=-1) call renormmoldengau(nprmshadd,shelltype(ishell),&
		primexp(iprimshell-nprmshadd+1:iprimshell),concoeff(iprimshell-nprmshadd+1:iprimshell))
		
		if (shelltype(ishell)==-1) then !Separate SP shell as S and P shells
			shelltype(ishell)=0 !s
			call renormmoldengau(nprmshadd,shelltype(ishell),primexp(iprimshell-nprmshadd+1:iprimshell),concoeff(iprimshell-nprmshadd+1:iprimshell))
			ishell=ishell+1
			shelltype(ishell)=1 !p
			shellcon(ishell)=shellcon(ishell-1)
			shell2atom(ishell)=shell2atom(ishell-1)
			primexp(iprimshell+1:iprimshell+nprmshadd)=primexp(iprimshellold+1:iprimshell)
			do itmp=1,ncon !Backspace and load P contract coefficient
				backspace(10)
			end do
			do itmp=1,ncon
				read(10,*) exptmp,rnouse,concoefftmp
				if (concoefftmp==0D0) cycle
				iprimshell=iprimshell+1
				concoeff(iprimshell)=concoefftmp
			end do
			call renormmoldengau(nprmshadd,shelltype(ishell),primexp(iprimshell-nprmshadd+1:iprimshell),concoeff(iprimshell-nprmshadd+1:iprimshell))
		end if
		read(10,"(a)") c80
		if (c80==" ") exit
		backspace(10)
	end do
	read(10,"(a)") c80
	if (c80==" ".or.index(c80,"[")/=0) exit !Finished reading [GTO] field
	backspace(10)
end do

!Determine if the basis functions are Cartesian or spherical harmonic type. Admixture cartesian and spherical type are permitted
isphergau=0 !Default is Cartesian type
i5D=0
i7F=0
i10Flabel=0
i9G=0
i11H=0
imaxL=maxval(shelltype)
if (infomode==0) write(*,"(' The highest angular moment basis functions is ',a)") shtype2name(imaxL) 
if (imaxL>=2) then
	rewind(10)
	do while(.true.)
		read(10,"(a)") c80
		if (index(c80,'[5D')/=0.or.index(c80,'[5d')/=0) i5D=1
		if (index(c80,'[7F')/=0.or.index(c80,'[7f')/=0) i7F=1
		if (index(c80,'10F')/=0) i10Flabel=1
		if (index(c80,'9G')/=0.or.index(c80,'9g')/=0) i9G=1
		if (index(c80,'11H')/=0.or.index(c80,'11h')/=0) i11H=1
		if (index(c80,'[MO]')/=0) exit
	end do
	if (i5D==1) then
		i7F=1 !By default, using 5D also implies 7F is used, unless 10F is explicitly specified
		if (i10Flabel==1) i7F=0
	end if
end if

if (i5D==1.or.i7F==1.or.i9G==1.or.i11H==1) isphergau=1
if (i5D==1) where(shelltype==2) shelltype=-2
if (i7F==1) where(shelltype==3) shelltype=-3
if (i9G==1) where(shelltype==4) shelltype=-4
if (i11H==1) where(shelltype==5) shelltype=-5
if (infomode==0) then
	if (isphergau==0) then
		write(*,*) "All basis functions are Cartesian type"
	else if (isphergau==1) then
		if (i5D==1.and.any(abs(shelltype)==2)) write(*,*) "All D basis functions are spherical harmonic type"
		if (i7F==1.and.any(abs(shelltype)==3)) write(*,*) "All F basis functions are spherical harmonic type"
		if (i9G==1.and.any(abs(shelltype)==4)) write(*,*) "All G basis functions are spherical harmonic type"
		if (i11H==1.and.any(abs(shelltype)==5)) write(*,*) "All H basis functions are spherical harmonic type"
	end if
end if
nbasis=0
do ishell=1,nshell
	nbasis=nbasis+shtype2nbas(shelltype(ishell))
end do

!!!!! Load orbital information. The sequence: Alpha(high occ / low ene) -> Alpha(low occ / high ene) -> Beta(high occ / low ene) -> Beta(low occ / high ene)
!Close shell orbitals are formally marked as "Alpha" spin. For singly occupied orbitals of ROHF, the spin marker are also Alpha
if (infomode==0) write(*,*) "Loading orbitals..."
nmo=nbasis
! Here I don't use call loclabel(10,"Beta",ibeta) to check if there are Beta orbitals, because for very large file, this will be quite expensive
! I assume that when the first orbital has occupation number <1.05, then the wavefunction must be unrestricted
ibeta=0
call loclabel(10,"[MO]")
do while(.true.)
	read(10,"(a)") c80
	if (index(c80,"OCCUP=")/=0.or.index(c80,"Occup=")/=0) then
		read(c80,*) c80tmp,occtmp
		if (occtmp<1.05D0) ibeta=1
		exit
	end if
end do
!Allocate size for arrays
if (ibeta==0) then
	nmo=nbasis
	allocate(amocoeff(nmo,nbasis),MOocc(nmo),MOene(nmo),MOtype(nmo),MOsym(nmo))
	amocoeff=0D0
else if (ibeta==1) then
	nmo=2*nbasis
	allocate(amocoeff(nbasis,nbasis),bmocoeff(nbasis,nbasis),MOocc(nmo),MOene(nmo),MOtype(nmo),MOsym(nmo))
	amocoeff=0D0
	bmocoeff=0D0
end if
MOsym=" "
MOocc=0D0
MOene=0D0
!Start to load orbitals
call loclabel(10,"[MO]")
read(10,*)
iMOa=0
iMOb=0
iloadorb=0
do while(.true.)
	iloadorb=iloadorb+1
	read(10,"(a)") c80 !Test if it is "Sym=", some programs do not output this field
	isym=index(c80,"Sym")
	if (isym==0) isym=index(c80,"SYM")
	if (isym/=0) then
		ieq=index(c80,'=')
		read(c80(ieq+1:),*) symtmp
		!Remove digitals before the IRREP, e.g. 23B1 should be changed to B1
		do jtmp=1,len(symtmp)
			if (ichar(symtmp(jtmp:jtmp))<48.or.ichar(symtmp(jtmp:jtmp))>57) exit !Find the first position of non-digital
		end do
		symtmp=symtmp(jtmp:len(symtmp))
	else
		symtmp="?"
		backspace(10)
	end if
	
	read(10,"(a)") c80 !Read orbital energy. Since there may be no spacing between = and energy (Bagel program), position of = should be tested
	!write(*,"(a)") trim(c80)
	ieq=index(c80,'=')
	read(c80(ieq+1:),*) enetmp
 	!write(*,*) iloadorb,enetmp,nbasis,nmo  !<<------ If encountering problem when loading MOs, using this to locate the problematic MO
	
	read(10,"(a)") c80 !Read orbital spin
	ispintmp=1 !Alpha
	if (index(c80,"Beta")/=0) ispintmp=2 !Beta
	read(10,*) c80,occtmp !Read orbital occupation number
	if (ispintmp==1) then
		iMOa=iMOa+1
		MOocc(iMOa)=occtmp
		MOene(iMOa)=enetmp
		MOsym(iMOa)=symtmp
		do while(.true.) !This loading way is compatible with Dalton and .molden by all other programs
			read(10,*,iostat=ierror) itmp,tmpval
			if (ierror/=0) then
				backspace(10)
				exit
			end if
			amocoeff(iMOa,itmp)=tmpval
		end do
	else
		iMOb=iMOb+1
		MOocc(nbasis+iMOb)=occtmp
		MOene(nbasis+iMOb)=enetmp
		MOsym(nbasis+iMOb)=symtmp
		do while(.true.)
			read(10,*,iostat=ierror) itmp,tmpval
			if (ierror/=0) then
				backspace(10)
				exit
			end if
			bmocoeff(iMOb,itmp)=tmpval
		end do
	end if
	
	read(10,"(a)",iostat=ierror) c80 !Test if the ending of [MO] field is reached
	if (ierror/=0.or.c80==" ".or.c80(1:1)=='[') exit
	backspace(10)
end do

!Fix orbital coefficients for ORCA. ORCA is rather rather frantic, the F(+3,-3) and G(+3,-3,+4,-4) in ORCA are normalized to -1 rather than 1,
!therefore the sign of their coefficients in all orbitals must be inverted! Hey ORCA, why did you do this!????? Totally non-understandable!
if (iorca==1) then
	ibasis=0
	do ishell=1,nshell
		if (shelltype(ishell)==-3) then
			amocoeff(:,ibasis+6:ibasis+7)=-amocoeff(:,ibasis+6:ibasis+7)
			if (ibeta==1) bmocoeff(:,ibasis+6:ibasis+7)=-bmocoeff(:,ibasis+6:ibasis+7)
		else if (shelltype(ishell)==-4) then
			amocoeff(:,ibasis+6:ibasis+9)=-amocoeff(:,ibasis+6:ibasis+9)
			if (ibeta==1) bmocoeff(:,ibasis+6:ibasis+9)=-bmocoeff(:,ibasis+6:ibasis+9)
		end if
		ibasis=ibasis+shtype2nbas(shelltype(ishell))
	end do
end if
!Fix orbital coefficients for CFour according to Molden2aim. CFour only use Cartesian type basis function
!Notice that at current stage the GTO recording sequence has not been reordered, which is still identical to Molden sequence
!Also notice that even we do this, the result is still incorrect for e.g. benzene. But if we don't do this, we can't even obtain correct result for test case of Molden2aim
if (icfour==1) then
	ibasis=0
	do ishell=1,nshell
		if (shelltype(ishell)==2) then
			!xx, yy, zz, xy, xz, yz
			amocoeff(:,ibasis+1:ibasis+3)=amocoeff(:,ibasis+1:ibasis+3)*sqrt(3D0) !d(xx,yy,zz)*sqrt(3)
			if (ibeta==1) bmocoeff(:,ibasis+1:ibasis+3)=bmocoeff(:,ibasis+1:ibasis+3)*sqrt(3D0)
		else if (shelltype(ishell)==3) then
			!xxx yyy zzz xyy xxy xxz xzz yzz yyz xyz !Molden sequence
			amocoeff(:,ibasis+1:ibasis+3)=amocoeff(:,ibasis+1:ibasis+3)*sqrt(15D0) !f(xxx,yyy,zzz)*sqrt(15)
			amocoeff(:,ibasis+4:ibasis+9)=amocoeff(:,ibasis+4:ibasis+9)*sqrt(3D0) !f(xyy,xzz,yxx,yzz,zxx,zyy)*sqrt(3)
			if (ibeta==1) then
				bmocoeff(:,ibasis+1:ibasis+3)=bmocoeff(:,ibasis+1:ibasis+3)*sqrt(15D0)
				bmocoeff(:,ibasis+4:ibasis+9)=bmocoeff(:,ibasis+4:ibasis+9)*sqrt(3D0)
			end if
		else if (shelltype(ishell)==4) then
			!xxxx yyyy zzzz xxxy xxxz yyyx yyyz zzzx zzzy xxyy xxzz yyzz xxyz yyxz zzxy !Molden sequence
			amocoeff(:,ibasis+1:ibasis+3)=amocoeff(:,ibasis+1:ibasis+3)*sqrt(105D0) !g(x4,y4,z4)*sqrt(105)
			amocoeff(:,ibasis+4:ibasis+9)=amocoeff(:,ibasis+4:ibasis+9)*sqrt(15D0) !g(x3y,x3z,y3x,y3z,z3x,z3y)*sqrt(15)
			amocoeff(:,ibasis+10:ibasis+12)=amocoeff(:,ibasis+10:ibasis+12)*3D0 !g(x2y2,x2z2,y2z2)*3.0
			amocoeff(:,ibasis+13:ibasis+15)=amocoeff(:,ibasis+13:ibasis+15)*sqrt(3D0) !g(x2yz,y2xz,z2xy)*sqrt(3)
			if (ibeta==1) then
				bmocoeff(:,ibasis+1:ibasis+3)=bmocoeff(:,ibasis+1:ibasis+3)*sqrt(105D0)
				bmocoeff(:,ibasis+4:ibasis+9)=bmocoeff(:,ibasis+4:ibasis+9)*sqrt(15D0)
				bmocoeff(:,ibasis+10:ibasis+12)=bmocoeff(:,ibasis+10:ibasis+12)*3D0
				bmocoeff(:,ibasis+13:ibasis+15)=bmocoeff(:,ibasis+13:ibasis+15)*sqrt(3D0)
			end if
		end if
		ibasis=ibasis+shtype2nbas(shelltype(ishell))
	end do	
end if
!xtb ignore inner core orbitals, therefore the nuclear charge of atoms must be substracted by number of core electrons
if (ixtb==1) then
	do i=1,ncenter
		if (a(i)%index>2.and.a(i)%index<=10) a(i)%charge=a(i)%charge-2
		if (a(i)%index>10.and.a(i)%index<=18) a(i)%charge=a(i)%charge-10
		if (a(i)%index>18.and.a(i)%index<=36) a(i)%charge=a(i)%charge-18
		if (a(i)%index>36.and.a(i)%index<=54) a(i)%charge=a(i)%charge-36
		if (a(i)%index>54.and.a(i)%index<=86) a(i)%charge=a(i)%charge-54
		if (a(i)%index>86) a(i)%charge=a(i)%charge-86
	end do
end if

!Determine wavefunction type
if (ibeta==0) then
	MOtype=0 !Close shell orbital
	wfntype=0 !RHF
	if (any(MOocc/=nint(MOocc))) then
		wfntype=3 !R-post-HF
	else if (any(MOocc==1D0)) then
		wfntype=2 !ROHF
		do imo=1,nmo
			if (MOocc(imo)==1D0) MOtype(imo)=1
		end do
	end if
	if (infomode==0) write(*,"( ' The actual number of orbitals read:',i10)") iMOa
else if (ibeta==1) then
	wfntype=1 !UHF
	if (any(MOocc/=nint(MOocc))) wfntype=4 !U-post-HF
	MOtype(1:nbasis)=1
	MOtype(nbasis+1:nmo)=2
	if (infomode==0) write(*,"( ' The actual number of Alpha/Beta orbitals read:',i10,'  /',i10)") iMOa,iMOb
end if
call updatenelec !Cound the number of electrons

close(10)

!!!!!! All reading have finished, now generate basis information
!Below codes are adapted from readfch

!Backup spherical Gaussian basis information with 5D suffix (of course, may be 7f, 9g... in fact), convert them to cartesian type temporarily, 
!at final stage recover them back, namely get Sbas, Ptot... in spherical basis
if (isphergau==1) then
	allocate(shelltype5D(nshell))
	shelltype5D=shelltype
	where (shelltype<=-2) shelltype=-shelltype !Convert to cartesian type
	nbasis5D=nbasis
	nbasis=0
	do i=1,nshell
		nbasis=nbasis+shtype2nbas(shelltype(i))
	end do
end if

!Allocate space for arrays
nprims=0
do i=1,nshell
	nprims=nprims+shtype2nbas(shelltype(i))*shellcon(i)
end do
allocate(b(nprims),co(nmo,nprims),basshell(nbasis),bascen(nbasis),bastype(nbasis),primstart(nbasis),&
primend(nbasis),primconnorm(nprims),basstart(ncenter),basend(ncenter))

!Fill Cobasa and CObasb
if (isphergau==0) then
	allocate(CObasa(nbasis,nbasis))
	CObasa=transpose(amocoeff)
	if (wfntype==1.or.wfntype==4) then
		allocate(CObasb(nbasis,nbasis))
		CObasb=transpose(bmocoeff)
	end if
else if (isphergau==1) then !Since we have artifically converted spherical shells to cartesian shells, here the orbital coefficients are also correspondingly converted
	allocate(CObasa(nbasis,nbasis),CObasa5D(nbasis5D,nbasis5D))
	CObasa5D=transpose(amocoeff)
	CObasa=0D0
	if (wfntype==1.or.wfntype==4) then
		allocate(CObasb(nbasis,nbasis),CObasb5D(nbasis5D,nbasis5D))
		CObasb5D=transpose(bmocoeff)
		CObasb=0D0
	end if
	!Map 5D coefficient to 6D coefficient. Since the number of spherical basis functions is more than cartesian ones, 
	!therefore Cobasa (6D) will have some orbitals with vacant coefficients, only orbitals (1~nbasis5D) are filled
	ipos5D=1
	ipos6D=1
	do ish=1,nshell
		ishtyp5D=shelltype5D(ish)
		ishtyp6D=shelltype(ish)
		numshorb5D=shtype2nbas(ishtyp5D)
		numshorb6D=shtype2nbas(ishtyp6D)
		if (ishtyp5D>=-1) then !S or P or SP or other cartesian shells, directly copy
			CObasa(ipos6D:ipos6D+numshorb6D-1,1:nbasis5D)=CObasa5D(ipos5D:ipos5D+numshorb5D-1,:)
			if (wfntype==1.or.wfntype==4) CObasb(ipos6D:ipos6D+numshorb6D-1,1:nbasis5D)=CObasb5D(ipos5D:ipos5D+numshorb5D-1,:)			
		else if (ishtyp5D==-2) then
			!5D->6D
			CObasa(ipos6D:ipos6D+5,1:nbasis5D)=matmul(conv5d6d,CObasa5D(ipos5D:ipos5D+4,:))
			if (wfntype==1.or.wfntype==4) CObasb(ipos6D:ipos6D+5,1:nbasis5D)=matmul(conv5d6d,CObasb5D(ipos5D:ipos5D+4,:))
		else if (ishtyp5D==-3) then
			!7F->10F
			CObasa(ipos6D:ipos6D+9,1:nbasis5D)=matmul(conv7f10f,CObasa5D(ipos5D:ipos5D+6,:))
			if (wfntype==1.or.wfntype==4) CObasb(ipos6D:ipos6D+9,1:nbasis5D)=matmul(conv7f10f,CObasb5D(ipos5D:ipos5D+6,:))
		else if (ishtyp5D==-4) then
			!9G->15G
			CObasa(ipos6D:ipos6D+14,1:nbasis5D)=matmul(conv9g15g,CObasa5D(ipos5D:ipos5D+8,:))
			if (wfntype==1.or.wfntype==4) CObasb(ipos6D:ipos6D+14,1:nbasis5D)=matmul(conv9g15g,CObasb5D(ipos5D:ipos5D+8,:))
		else if (ishtyp5D==-5) then
			!11H->21H
			CObasa(ipos6D:ipos6D+20,1:nbasis5D)=matmul(conv11h21h,CObasa5D(ipos5D:ipos5D+10,:))
			if (wfntype==1.or.wfntype==4) CObasb(ipos6D:ipos6D+20,1:nbasis5D)=matmul(conv11h21h,CObasb5D(ipos5D:ipos5D+10,:))
		end if
		ipos5D=ipos5D+numshorb5D
		ipos6D=ipos6D+numshorb6D
	end do
end if

if (infomode==0) write(*,*) "Converting basis function information to GTF information..."
!Distribute exponent, functype to every GTF and generate CO(:,:) from amocoeff/bmocoeff
!Fill: b,basshell,bascen,bastype,co,primstart,primend,primconnorm
k=1 !current position of GTF
iexp=1
ibasis=1 !current position of basis
!Note: Below commented with !!! means the line associated to setting basis information
do i=1,nshell !cycle each basis shell
	b(k:k+shellcon(i)*shtype2nbas(shelltype(i))-1)%center=shell2atom(i)
	basshell(ibasis:ibasis+shtype2nbas(shelltype(i))-1)=i !!! set basis attributed to which shell
	bascen(ibasis:ibasis+shtype2nbas(shelltype(i))-1)=shell2atom(i) !!! set basis attributed to which center
	do j=1,shtype2nbas(shelltype(i)) !cycle each basis function in each basis shell
		b(k:k+shellcon(i)-1)%type=s2f(shelltype(i),j)
		bastype(ibasis)=s2f(shelltype(i),j) !!! set basis type
		primstart(ibasis)=k !!! From where the GTFs attributed to ibasis'th basis
		primend(ibasis)=k+shellcon(i)-1 !!! To where the GTFs attributed to ibasis'th basis
		do l=1,shellcon(i) !cycle each GTF in each basis function
			b(k)%exp=primexp(iexp+l-1)
			tnormgau=normgau(b(k)%type,b(k)%exp)  !!!Normalization coefficient of cartesian GTFs
			temp=concoeff(iexp+l-1)  !!!Contraction coefficient of GTFs
			primconnorm(k)=temp*tnormgau !Combines contraction and normalization coefficient
			do imo=1,nmo
				if (wfntype==0.or.wfntype==2.or.wfntype==3) then
					co(imo,k)=CObasa(ibasis,imo)*temp*tnormgau
! 					if (imo==4) write(*,"(4f18.10)") co(imo,k),CObasa(ibasis,imo),temp,tnormgau
				else if (wfntype==1.or.wfntype==4) then
					if (isphergau==1) then
						if (imo<=nbasis5D) co(imo,k)=CObasa(ibasis,imo)*temp*tnormgau
						if (imo>nbasis5D) co(imo,k)=CObasb(ibasis,imo-nbasis5D)*temp*tnormgau
					else
						if (imo<=nbasis) co(imo,k)=CObasa(ibasis,imo)*temp*tnormgau
						if (imo>nbasis) co(imo,k)=CObasb(ibasis,imo-nbasis)*temp*tnormgau
					end if
				end if
			end do
			k=k+1
		end do
		ibasis=ibasis+1
	end do
	iexp=iexp+shellcon(i)
end do

!Generate overlap matrix and dipole moment integral matrix for Cartesian Gaussian basis functions
if (infomode==0) write(*,*) "Generating overlap matrix..."
allocate(Sbas(nbasis,nbasis))
call genSbas
if (igenDbas==1) then
	if (infomode==0) write(*,*) "Generating electric dipole moment integral matrix..."
	allocate(Dbas(3,nbasis,nbasis))
	call genDbas
end if
if (igenMagbas==1) then
	if (infomode==0) write(*,*) "Generating magnetic dipole moment integral matrix..."
	allocate(Magbas(3,nbasis,nbasis))
	call genMagbas
end if

!Check normalizaiton of basis functions
! do i=1,size(sbas,1)
! 	write(*,"(i10,f12.6)") i,sbas(i,i)
! end do

if (isphergau==1) then
	if (iloadasCart==1) then !For special purpose, keep Cartesian basis functions, e.g. convert spherical .fch/.molden to .47 file
		!Currently nbasis and dimension of all matrix (except for CO) correspond to full Cartesian case, but nmo &
		!and related arrays as well as CO still correspond to spherical harmonic case and thus need to be "expanded", the MO energies are artifically set to 100
		allocate(MOocc5D(nmo),MOene5D(nmo),MOtype5D(nmo),MOsym5D(nmo),CO5D(nmo,nprims))
		MOocc5D=MOocc
		MOene5D=MOene
		MOtype5D=MOtype
		MOsym5D=MOsym
		CO5D=CO
		deallocate(MOocc,MOene,MOtype,MOsym,CO)
		if (wfntype==0.or.wfntype==2.or.wfntype==3) nmo=nbasis !R, RO
		if (wfntype==1.or.wfntype==4) nmo=2*nbasis !U
		allocate(MOocc(nmo),MOene(nmo),MOtype(nmo),MOsym(nmo),CO(nmo,nprims))
		MOocc=0
		MOene=100
		MOsym=" "
		CO=0
		if (wfntype==0.or.wfntype==2.or.wfntype==3) then !R, RO
			MOtype=0
			MOocc(1:nbasis5D)=MOocc5D
			MOene(1:nbasis5D)=MOene5D
			MOtype(1:nbasis5D)=MOtype5D
			MOsym(1:nbasis5D)=MOsym5D
			CO(1:nbasis5D,:)=CO5D
		else !U
			MOtype(:nbasis)=1
			MOtype(nbasis+1:)=2
			MOocc(:nbasis5D)=MOocc5D(:nbasis5D)
			MOocc(nbasis+1:nbasis+nbasis5D)=MOocc5D(nbasis5D+1:)
			MOene(:nbasis5D)=MOene5D(:nbasis5D)
			MOene(nbasis+1:nbasis+nbasis5D)=MOene5D(nbasis5D+1:)
			MOtype(:nbasis5D)=MOtype5D(:nbasis5D)
			MOtype(nbasis+1:nbasis+nbasis5D)=MOtype5D(nbasis5D+1:)
			MOsym(:nbasis5D)=MOsym5D(:nbasis5D)
			MOsym(nbasis+1:nbasis+nbasis5D)=MOsym5D(nbasis5D+1:)
			CO(:nbasis5D,:)=CO5D(:nbasis5D,:)
			CO(nbasis+1:nbasis+nbasis5D,:)=CO5D(nbasis5D+1:,:)
		end if
		isphergau=0
		
	else !Commonly case, transform to spherical harmonic functions
		if (infomode==0) write(*,*) "Back converting basis function information from Cartesian to spherical type..."
		!Map cartesian overlap matrix to spherical harmonic overlap matrix
		allocate(Sbas5D(nbasis5D,nbasis5D))
		if (igenDbas==1) allocate(Dbas5D(3,nbasis5D,nbasis5D))
		if (igenMagbas==1) allocate(Magbas5D(3,nbasis5D,nbasis5D))
		ipos5D=1
		ipos6D=1
		do ish=1,nshell
			ishtyp5D=shelltype5D(ish)
			ishtyp6D=shelltype(ish)
			numshorb5D=shtype2nbas(ishtyp5D)
			numshorb6D=shtype2nbas(ishtyp6D)
			!Now contract columns
			if (ishtyp5D>=-1) sbas(:,ipos5D:ipos5D+numshorb5D-1)=sbas(:,ipos6D:ipos6D+numshorb6D-1) !S, P, SP or other Cartesian shells
			if (ishtyp5D==-2) sbas(:,ipos5D:ipos5D+numshorb5D-1)=matmul(sbas(:,ipos6D:ipos6D+numshorb6D-1),conv5d6d)
			if (ishtyp5D==-3) sbas(:,ipos5D:ipos5D+numshorb5D-1)=matmul(sbas(:,ipos6D:ipos6D+numshorb6D-1),conv7f10f)
			if (ishtyp5D==-4) sbas(:,ipos5D:ipos5D+numshorb5D-1)=matmul(sbas(:,ipos6D:ipos6D+numshorb6D-1),conv9g15g)
			if (ishtyp5D==-5) sbas(:,ipos5D:ipos5D+numshorb5D-1)=matmul(sbas(:,ipos6D:ipos6D+numshorb6D-1),conv11h21h)
			!Now contract rows
			if (ishtyp5D>=-1) sbas(ipos5D:ipos5D+numshorb5D-1,:)=sbas(ipos6D:ipos6D+numshorb6D-1,:) !S, P, SP or other Cartesian shells
			if (ishtyp5D==-2) sbas(ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv5d6dtr,sbas(ipos6D:ipos6D+numshorb6D-1,:))
			if (ishtyp5D==-3) sbas(ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv7f10ftr,sbas(ipos6D:ipos6D+numshorb6D-1,:))
			if (ishtyp5D==-4) sbas(ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv9g15gtr,sbas(ipos6D:ipos6D+numshorb6D-1,:))
			if (ishtyp5D==-5) sbas(ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv11h21htr,sbas(ipos6D:ipos6D+numshorb6D-1,:))
			
			if (igenDbas==1) then
				do idir=1,3
					!Now contract columns of Dbas
					if (ishtyp5D>=-1) Dbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=Dbas(idir,:,ipos6D:ipos6D+numshorb6D-1) !S, P, SP or other Cartesian shells
					if (ishtyp5D==-2) Dbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Dbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv5d6d)
					if (ishtyp5D==-3) Dbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Dbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv7f10f)
					if (ishtyp5D==-4) Dbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Dbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv9g15g)
					if (ishtyp5D==-5) Dbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Dbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv11h21h)
					!Now contract rows of Dbas
					if (ishtyp5D>=-1) Dbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=Dbas(idir,ipos6D:ipos6D+numshorb6D-1,:) !S, P, SP or other Cartesian shells
					if (ishtyp5D==-2) Dbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv5d6dtr,Dbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
					if (ishtyp5D==-3) Dbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv7f10ftr,Dbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
					if (ishtyp5D==-4) Dbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv9g15gtr,Dbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
					if (ishtyp5D==-5) Dbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv11h21htr,Dbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
				end do
			end if
			if (igenMagbas==1) then
				do idir=1,3
					!Now contract columns of Magbas
					if (ishtyp5D>=-1) Magbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=Magbas(idir,:,ipos6D:ipos6D+numshorb6D-1) !S, P, SP or other Cartesian shells
					if (ishtyp5D==-2) Magbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Magbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv5d6d)
					if (ishtyp5D==-3) Magbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Magbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv7f10f)
					if (ishtyp5D==-4) Magbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Magbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv9g15g)
					if (ishtyp5D==-5) Magbas(idir,:,ipos5D:ipos5D+numshorb5D-1)=matmul(Magbas(idir,:,ipos6D:ipos6D+numshorb6D-1),conv11h21h)
					!Now contract rows of Magbas
					if (ishtyp5D>=-1) Magbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=Magbas(idir,ipos6D:ipos6D+numshorb6D-1,:) !S, P, SP or other Cartesian shells
					if (ishtyp5D==-2) Magbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv5d6dtr,Magbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
					if (ishtyp5D==-3) Magbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv7f10ftr,Magbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
					if (ishtyp5D==-4) Magbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv9g15gtr,Magbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
					if (ishtyp5D==-5) Magbas(idir,ipos5D:ipos5D+numshorb5D-1,:)=matmul(conv11h21htr,Magbas(idir,ipos6D:ipos6D+numshorb6D-1,:))
				end do
			end if
			ipos5D=ipos5D+numshorb5D
			ipos6D=ipos6D+numshorb6D
		end do
		Sbas5D=sbas(1:nbasis5D,1:nbasis5D)
		if (igenDbas==1) Dbas5D=Dbas(:,1:nbasis5D,1:nbasis5D)
		if (igenMagbas==1) Magbas5D=Magbas(:,1:nbasis5D,1:nbasis5D)

		!Recover spherical Gaussian basis function information
		nbasis=nbasis5D
		shelltype=shelltype5D
		ibasis=1
		do i=1,nshell
			basshell(ibasis:ibasis+shtype2nbas(shelltype(i))-1)=i
			bascen(ibasis:ibasis+shtype2nbas(shelltype(i))-1)=shell2atom(i)
			do j=1,shtype2nbas(shelltype(i))
				bastype(ibasis)=s2f(shelltype(i),j)
				ibasis=ibasis+1
			end do
		end do
		deallocate(CObasa)
		allocate(CObasa(nbasis,nbasis))
		CObasa=CObasa5D
		if (wfntype==1.or.wfntype==4) then
			deallocate(CObasb)
			allocate(CObasb(nbasis,nbasis))
			CObasb=CObasb5D
		end if
		deallocate(sbas)
		allocate(sbas(nbasis,nbasis))
		sbas=Sbas5D
		if (igenDbas==1) then
			deallocate(Dbas)
			allocate(Dbas(3,nbasis,nbasis))
			Dbas=Dbas5D
		end if
		if (igenMagbas==1) then
			deallocate(Magbas)
			allocate(Magbas(3,nbasis,nbasis))
			Magbas=Magbas5D
		end if
	end if
end if

!Move local shell arrays to the global ones, they will be used in other functions
allocate(shtype(nshell),shcen(nshell),shcon(nshell),primshexp(nprimshell),primshcoeff(nprimshell))
shtype=shelltype
shcen=shell2atom
shcon=shellcon
primshexp=primexp
primshcoeff=concoeff

!Generate basstart and basend
nowcen=0
indcen=0
do ibasis=1,nbasis
	if (bascen(ibasis)/=nowcen) then
		nowcen=bascen(ibasis)
		indcen=indcen+1
		basstart(indcen)=ibasis
		if (indcen/=1) basend(indcen-1)=ibasis-1
	end if
end do
basend(ncenter)=nbasis

!Generate one-particle density matrix for basis functions
if (igenP==1) then
	if (infomode==0) write(*,*) "Generating density matrix..."
	call genP
end if

!Check wavefunction sanity
if (iorca==0) then !For ORCA with angular moment >f, warning has already been shown before"
	devtmp=abs(sum(Sbas*Ptot)-nint(nelec))
	if (devtmp>0.01D0) then
		write(*,"( ' Deviation of Tr(S*P) to the total number of electrons:',f12.6)") devtmp
		write(*,"(/,a)") " Warning: The wavefunction loaded is problematic! That means this Molden input file cannot be well supported by Multiwfn. &
		I strongly suggest you use Molden2AIM program to standardize this file before loading into Multiwfn. Please check Section 5.1 of the manual for detail"
		write(*,"(a)") " If you really want to proceed, press ENTER button, but notice that the result will not be correct"
		read(*,*)
	end if
end if

!Output summary of present wavefunction
if (infomode==0) then
	write(*,*)
	write(*,"(' Total/Alpha/Beta electrons:',3f12.4)") nelec,naelec,nbelec
	write(*,"(' Net charge:',f12.5,'      Expected multiplicity:',i5)") sum(a(:)%charge)-nelec,nint(naelec-nbelec)+1
	write(*,"(' Atoms:',i7,',  Basis functions:',i7,',  GTFs:',i7)") ncenter,nbasis,nprims
	if (wfntype==0) then
		write(*,"(' This is restricted single-determinant wavefunction')")
		write(*,"(' Orbitals from 1 to',i6,' are occupied')") nint(nelec/2)
	else if (wfntype==1) then
		write(*,"(' This is unrestricted single-determinant wavefunction')")
		write(*,"(' Orbitals from ',i6,' to',i6,' are alpha, from',i6,' to',i6,' are occupied')") 1,nbasis,1,nint(naelec)
		write(*,"(' Orbitals from ',i6,' to',i6,' are beta,  from',i6,' to',i6,' are occupied')") nbasis+1,nmo,nbasis+1,nbasis+nint(nbelec)
	else if (wfntype==2) then
		write(*,"(' This is restricted open-shell wavefunction')")
		write(*,"(' Orbitals from',i6,' to',i6,' are doubly occupied')") 1,nint(nbelec)
		write(*,"(' Orbitals from',i6,' to',i6,' are singly occupied')") nint(nbelec)+1,nint(naelec)
	else if (wfntype==3) then
		write(*,"(' This is restricted post-HF wavefunction')")
	else if (wfntype==4) then
		write(*,"(' This is unrestricted open-shell wavefunction')")
		write(*,"(' Orbitals from ',i6,' to',i6,' are alpha, from',i6,' to',i6,' are beta')") 1,nbasis,nbasis+1,nmo
	end if
end if

!Find out index of HOMO, will be used in some cases, only for RHF
if (wfntype==0) then
	do idxHOMO=nmo,1,-1
		if (nint(MOocc(idxHOMO))==2D0) exit
	end do
end if

end subroutine




!---------- Read GAMESS-US output file to get GTF and basis information, the suffix must be "gms"
!GAMESS-US always print LCAO cofficients as Cartesian basis functions, while the number of MOs correspond to spherical harmonic functions (if used)
!infomode=0 means output info, =1 silent
subroutine readgms(name,infomode)
use defvar
use util
implicit real*8 (a-h,o-z)
character(len=*) name
character c80*80,chartmp
integer :: s2f(0:5,21)=0 !Give shell type & orbital index to get functype
real*8,allocatable :: amocoeff(:,:),bmocoeff(:,:)
integer,allocatable :: zcore(:) !The number of core electrons replaced with ECP
real*8,external :: normgau

ifiletype=10
imodwfn=0
s2f(0,1)=1
s2f(1,1:3)=(/ 2,3,4 /)
s2f(2,1:6)=(/ 5,6,7,8,9,10 /)
!---------- The sequence of f functions in Multiwfn (=wfn=wfx) is not identical to GAMESS, so convert here
!11  12  13  14  15  16  17  18  19  20  !Multiwfn sequence
!XXX YYY ZZZ XXY XXZ YYZ XYY XZZ YZZ XYZ
!xxx yyy zzz xxy xxz xyy yyz zzx zzy xyz !GAMESS sequence
s2f(3,1:10)=(/ 11,12,13,14,15,17,16,18,19,20 /)
!---------- The sequence of g functions in Multiwfn (=fch) is not identical to GAMESS(=molden), so convert here
! 21   22   23   24   25   26   27   28   29   30   31   32   33   34   35  !Multiwfn sequence
!ZZZZ YZZZ YYZZ YYYZ YYYY XZZZ XYZZ XYYZ XYYY XXZZ XXYZ XXYY XXXZ XXXY XXXX
!xxxx yyyy zzzz xxxy xxxz yyyx yyyz zzzx zzzy xxyy xxzz yyzz xxyz yyxz zzxy !GAMESS sequence
s2f(4,1:15)=(/ 35,25,21,34,33,29,24,26,22,32,30,23,31,28,27 /)
!---------- The sequence of h functions in Multiwfn (=fch=wfx) is not identical to GAMESS, so convert here
! 36    37    38    39    40    41    42    43    44    45    46  !Multiwfn sequence
!ZZZZZ YZZZZ YYZZZ YYYZZ YYYYZ YYYYY XZZZZ XYZZZ XYYZZ XYYYZ XYYYY
!xxxxx yyyyy zzzzz xxxxy xxxxz xyyyy yyyyz xzzzz yzzzz xxxyy xxxzz !GAMESS sequence
! 47    48    49    50    51    52    53    54    55    56  !Multiwfn sequence
!XXZZZ XXYZZ XXYYZ XXYYY XXXZZ XXXYZ XXXYY XXXXZ XXXXY XXXXX
!xxyyy yyyzz xxzzz yyzzz xxxyz xyyyz xyzzz xxyyz xxyzz xyyzz !GAMESS sequence
s2f(5,1:21)=(/ 56,41,36,55,54,46,40,42,37,53,51, 50,39,47,38,52,45,43,49,48,44 /)

open(10,file=name,access="sequential",status="old")
call loclabel(10,"Firefly Project",ifirefly,maxline=100)
if (ifirefly==1) write(*,*) "Note: This file will be recognized as Firefly output"
if (infomode==0) write(*,*) "Loading various information of the wavefunction"

!!!!! Sanity check
call loclabel(10,"RUNTYP=OPTIMIZE",iopt,maxline=10000)
if (iopt==1) then
	!Although GAMESS-US output final geometry and corresponding wavefunction (labelled by "MOLECULAR ORBITALS"), 
	!the number of orbitals is much smaller than expected, therefore unable to provide enough information
	write(*,"(a)") " Warning: This is an optimization task, only wavefunction corresponding to initial geometry will be loaded"
	write(*,*) "Press ENTER button to continue"
	read(*,*)
end if
!If point group is not C1, only basis set definition of symmetry unique atoms is printed
call loclabel(10,"THE POINT GROUP OF THE MOLECULE IS C1",iC1,maxline=1000)
if (iC1==0) then
    write(*,"(a)") " Error: The point group is not C1, Multiwfn does not support this case"
	write(*,*) "Press ENTER button to exit"
	read(*,*)
    stop
end if

!!!!! Load atom information
call loclabel(10,"TOTAL NUMBER OF ATOMS",ifound)
if (ifirefly==0) read(10,"(47x,i5)") ncenter
if (ifirefly==1) read(10,"(38x,i5)") ncenter
allocate(a(ncenter))
call loclabel(10,"ATOM      ATOMIC",ifound)
read(10,*)
read(10,*)
do iatm=1,ncenter
	read(10,*) c80,a(iatm)%index,a(iatm)%x,a(iatm)%y,a(iatm)%z
end do
a%name=ind2name(a%index)
a%charge=a%index
rewind(10)

!!!!! Load basis set, and build up GTF information
!First time, we count how many shells are there to allocate proper size of allocatable arrays
nshell=0
nprimshell=0
if (ifirefly==0) call loclabel(10,"SHELL TYPE  PRIMITIVE",ifound)
if (ifirefly==1) call loclabel(10,"SHELL TYPE PRIM",ifound)
if (ifound==0) then
	write(*,"(' Error: Unable to find basis set definition section!')")
	write(*,*) "Press ENTER button to exit"
	read(*,*)
	stop
else
	if (infomode==0) write(*,*) "Loading basis set definition..."
end if
read(10,*)
read(10,*)
iaddshell=0
do iatm=1,ncenter
	read(10,*) !Atom name
	read(10,*)
	do while(.true.)
		read(10,"(a)") c80
		if (c80==" ") then !Finished loading last shell
			nshell=nshell+1
			if (iaddshell==1) nshell=nshell+1 !Last shell is L
			iaddshell=0
			read(10,"(a)") c80
			backspace(10)
			if (iachar(c80(2:2))>=65) exit !The second column is letter, indicating a new atom is encountered
		else
			nprimshell=nprimshell+1
			if (index(c80,"L")/=0) then
				nprimshell=nprimshell+1 !sp shell will be separated as s and p
				iaddshell=1
			end if
! 			write(*,*) iatm,a(iatm)%name,nprimshell
		end if
	end do
end do

!Second time, read basis set information actually
allocate(shtype(nshell),shcon(nshell),shcen(nshell))
allocate(primshexp(nprimshell),primshcoeff(nprimshell))
if (ifirefly==0) call loclabel(10,"SHELL TYPE  PRIMITIVE",ifound)
if (ifirefly==1) call loclabel(10,"SHELL TYPE PRIM",ifound)
read(10,*)
read(10,*)
shcon=0
ishell=1
iprimshell=0
do iatm=1,ncenter
	read(10,*) !Atom name
	read(10,*)
! 	write(*,*) iatm,ncenter
	do while(.true.)
		read(10,"(a)") c80
! 		write(*,*) trim(c80)
		if (c80==" ") then !Finished loading last basis shell
			if (shtype(ishell)==-1) then !Separate SP shell as S and P shells
				shtype(ishell)=0 !s
				ishell=ishell+1
				shtype(ishell)=1 !p
				shcon(ishell)=shcon(ishell-1)
				shcen(ishell)=shcen(ishell-1)
				do itmp=1,shcon(ishell)+1 !Backspace and load P contraction coefficient
					backspace(10)
				end do
				do itmp=1,shcon(ishell)
					iprimshell=iprimshell+1
					if (ifirefly==0) then
						read(10,*) inouse,chartmp,inouse,primshexp(iprimshell),rnouse,primshcoeff(iprimshell)
					else
						read(10,"(a)") c80
						read(c80,*) inouse,chartmp,inouse,primshexp(iprimshell)
						read(c80(69:78),*) primshcoeff(iprimshell)
! 						write(*,*) iprimshell,primshcoeff(iprimshell)
					end if
				end do
				read(10,*)
			end if
			ishell=ishell+1
			read(10,"(a)") c80
			backspace(10)
			if (iachar(c80(2:2))>=65) exit !The second column is letter, indicating a new atom is encountered
		else !Loading a new basis shell
			iprimshell=iprimshell+1
			shcon(ishell)=shcon(ishell)+1
			if (ifirefly==0) then
				read(c80,*) inouse,chartmp,inouse,primshexp(iprimshell),primshcoeff(iprimshell)
			else
				read(c80,*) inouse,chartmp,inouse,primshexp(iprimshell)
				read(c80(43:52),*) primshcoeff(iprimshell)
! 				write(*,*) iprimshell,chartmp,primshcoeff(iprimshell)
			end if
			shcen(ishell)=iatm
			if (chartmp=="L") then
				shtype(ishell)=-1
			else if (chartmp=="S") then
				shtype(ishell)=0
			else if (chartmp=="P") then
				shtype(ishell)=1
			else if (chartmp=="D") then
				shtype(ishell)=2
			else if (chartmp=="F") then
				shtype(ishell)=3
			else if (chartmp=="G") then
				shtype(ishell)=4
			else if (chartmp=="H") then
				shtype(ishell)=5
			end if
		end if
	end do
end do

nbasis=0
do ishell=1,nshell
!  	write(*,*) ishell,shtype(ishell),shcon(ishell),shcen(ishell)
	nbasis=nbasis+shtype2nbas(shtype(ishell))
end do
! do i=1,nprimshell
! 	write(*,"(i5,2f20.10)") i,primshexp(i),primshcoeff(i)
! end do

if (infomode==0) write(*,*) "Loading orbitals..."
call loclabel(10,"----- BETA SET -----",ibeta)
call loclabel(10,"TOTAL NUMBER OF MOS IN VARIATION SPACE=",ispher)
if (ispher==0) then !Cartesian functions
	nmoactual=nbasis
else
	read(10,"(40x,i8)") nmoactual !The actual number of MOs (value is for each spin in unrestricted case), less than nbasis
end if
if (ibeta==0) then !Only one set of orbitals
	nmo=nbasis
	allocate(amocoeff(nbasis,nmoactual),MOocc(nmo),MOene(nmo),MOtype(nmo),MOsym(nmo))
	MOocc=0
	MOene=100
	MOsym="?"
	call loclabel(10,"          EIGENVECTORS",ifound)
    if (ifound==0) then
        write(*,"(a)") " Error: Unable to find orbital coefficients field! The NPRINT parameter must be set to default value"
        write(*,*) "Press ENTER button to exit"
        read(*,*)
        stop
    end if
	call readgmsLCAO(10,ifirefly,nbasis,nmoactual,amocoeff,MOene(1:nmoactual),MOsym(1:nmoactual))
else
	nmo=2*nbasis
	allocate(amocoeff(nbasis,nmoactual),bmocoeff(nbasis,nmoactual),MOocc(nmo),MOene(nmo),MOtype(nmo),MOsym(nmo))
	MOocc=0
	MOene=100
	MOsym="?"
	call loclabel(10,"          EIGENVECTORS",ifound)
    if (ifound==0) then
        write(*,"(a)") " Error: Unable to find orbital coefficients field! The NPRINT parameter must be set to default value"
        write(*,*) "Press ENTER button to exit"
        read(*,*)
        stop
    end if
	call readgmsLCAO(10,ifirefly,nbasis,nmoactual,amocoeff,MOene(1:nmoactual),MOsym(1:nmoactual))
	call loclabel(10,"          EIGENVECTORS",ifound,0) !Don't rewind
	call readgmsLCAO(10,ifirefly,nbasis,nmoactual,bmocoeff,MOene(nbasis+1:nbasis+nmoactual),MOsym(nbasis+1:nbasis+nmoactual))
end if

!Determine the number of electrons and load the number of core electrons replaced with ECP
call loclabel(10,"NUMBER OF OCCUPIED ORBITALS (ALPHA)",ifound)
if (ifirefly==0) read(10,"(47x,f5.0)") naelec
if (ifirefly==1) read(10,"(38x,f5.0)") naelec
call loclabel(10,"NUMBER OF OCCUPIED ORBITALS (BETA )",ifound)
if (ifirefly==0) read(10,"(47x,f5.0)") nbelec
if (ifirefly==1) read(10,"(38x,f5.0)") nbelec
call loclabel(10,"THE ECP RUN REMOVES",iECP,maxline=10000) !ECP is used
if (iECP==1) then
	allocate(zcore(ncenter))
	zcore=0
	rewind(10)
	do while(.true.)
		call loclabel(10,"WITH ZCORE",ifound,0,maxline=10000)
		if (ifound==1) then
			read(10,"(a)") c80
! 			write(*,*) trim(c80)
			itmp=index(c80,"ATOM")
			read(c80(itmp+4:),*) iatm
			itmp=index(c80,"ZCORE")
			read(c80(itmp+5:),*) zcore(iatm)
		else
			exit
		end if
	end do
	rewind(10)
	do while(.true.)
		call loclabel(10,"ARE THE SAME AS ATOM",ifound,0,maxline=10000)
		if (ifound==1) then
			read(10,"(a)") c80
			itmp=index(c80,"ATOM")
			read(c80(itmp+4:),*) iatm
			itmp=index(c80,"ATOM",back=.true.)
			read(c80(itmp+4:),*) jatm
			zcore(iatm)=zcore(jatm)
		else
			exit
		end if
	end do
	a%charge=a%charge-zcore !Change "charge" from atom indices to actual number of electrons in ECP calculation
	naelec=naelec-sum(zcore)/2
	nbelec=nbelec-sum(zcore)/2
end if
nelec=naelec+nbelec

!Determine wavefunction type
if (ibeta==0) then
	if (any(MOocc/=nint(MOocc))) then
! 		wfntype=3 !R-post-HF
	else if (naelec/=nbelec) then
		wfntype=2 !ROHF
		MOocc(1:nbelec)=2D0
		MOocc(nbelec+1:naelec)=1D0
		MOtype=0 !Close shell orbital
		MOtype(nbelec+1:naelec)=1
	else !RHF
		wfntype=0
		MOocc(1:naelec)=2D0
		MOtype=0 !Close shell orbital		
	end if
else if (ibeta==1) then
	wfntype=1 !UHF
! 	if (any(MOocc/=nint(MOocc))) wfntype=4 !U-post-HF
	MOtype(1:nbasis)=1
	MOtype(nbasis+1:nmo)=2
	MOocc(1:naelec)=1D0
	MOocc(nbasis+1:nbasis+nbelec)=1D0
end if

close(10)

!!!!!! All reading have finished, now generate basis information
 
!Allocate space for arrays
nprims=0
do i=1,nshell
	nprims=nprims+shtype2nbas(shtype(i))*shcon(i)
end do
allocate(b(nprims),co(nmo,nprims),basshell(nbasis),bascen(nbasis),bastype(nbasis),primstart(nbasis),&
primend(nbasis),primconnorm(nprims),basstart(ncenter),basend(ncenter))

!Fill Cobasa and CObasb, the gap spaces due to difference between Cartesian and spherical harmonic functions are filled by zero
allocate(CObasa(nbasis,nbasis))
CObasa=0
CObasa(:,1:nmoactual)=amocoeff
if (wfntype==1.or.wfntype==4) then
	allocate(CObasb(nbasis,nbasis))
	CObasb=0
	CObasb(:,1:nmoactual)=bmocoeff
end if

if (infomode==0) write(*,*) "Converting basis function information to GTF information..."
!Distribute exponent, functype to every GTF and generate CO(:,:) from amocoeff/bmocoeff
!Fill: b,basshell,bascen,bastype,co,primstart,primend,primconnorm
k=1 !current position of GTF
iexp=1
ibasis=1 !current position of basis
!Note: Below commented with !!! means the line associated to setting basis information
do i=1,nshell !cycle each basis shell
	b(k:k+shcon(i)*shtype2nbas(shtype(i))-1)%center=shcen(i)
	basshell(ibasis:ibasis+shtype2nbas(shtype(i))-1)=i !!! set basis attributed to which shell
	bascen(ibasis:ibasis+shtype2nbas(shtype(i))-1)=shcen(i) !!! set basis attributed to which center
	do j=1,shtype2nbas(shtype(i)) !cycle each basis function in each basis shell
		b(k:k+shcon(i)-1)%type=s2f(shtype(i),j)
		bastype(ibasis)=s2f(shtype(i),j) !!! set basis type
		primstart(ibasis)=k !!! From where the GTFs attributed to ibasis'th basis
		primend(ibasis)=k+shcon(i)-1 !!! To where the GTFs attributed to ibasis'th basis
		do l=1,shcon(i) !cycle each GTF in each basis function
			b(k)%exp=primshexp(iexp+l-1)
			tnormgau=normgau(b(k)%type,b(k)%exp)  !!!Normalization coefficient of cartesian GTFs
			temp=primshcoeff(iexp+l-1)  !!!Contraction coefficient of GTFs
			primconnorm(k)=temp*tnormgau !Combines contraction and normalization coefficient
			do imo=1,nmo
				if (wfntype==0.or.wfntype==2.or.wfntype==3) then
					co(imo,k)=CObasa(ibasis,imo)*temp*tnormgau
				else if (wfntype==1.or.wfntype==4) then
					if (imo<=nbasis) co(imo,k)=CObasa(ibasis,imo)*temp*tnormgau
					if (imo>nbasis) co(imo,k)=CObasb(ibasis,imo-nbasis)*temp*tnormgau
				end if
			end do
			k=k+1
		end do
		ibasis=ibasis+1
	end do
	iexp=iexp+shcon(i)
end do

!Generate basstart and basend
nowcen=0
indcen=0
do ibasis=1,nbasis
	if (bascen(ibasis)/=nowcen) then
		nowcen=bascen(ibasis)
		indcen=indcen+1
		basstart(indcen)=ibasis
		if (indcen/=1) basend(indcen-1)=ibasis-1
	end if
end do
basend(ncenter)=nbasis

!Generate one-particle matrices for basis functions
if (igenP==1) then
	if (infomode==0) write(*,*) "Generating density matrix..."
	call genP
end if
if (infomode==0) write(*,*) "Generating overlap matrix..."
allocate(Sbas(nbasis,nbasis))
call genSbas
if (igenDbas==1) then
	if (infomode==0) write(*,*) "Generating electric dipole moment integral matrix..."
	allocate(Dbas(3,nbasis,nbasis))
	call genDbas
end if
if (igenMagbas==1) then
	if (infomode==0) write(*,*) "Generating magnetic dipole moment integral matrix..."
	allocate(Magbas(3,nbasis,nbasis))
	call genMagbas
end if
 
!Output summary of present wavefunction
if (infomode==0) then
	write(*,*)
	write(*,"(' Total/Alpha/Beta electrons:',3f12.4)") nelec,naelec,nbelec
	write(*,"(' Net charge:',f12.5,'      Expected multiplicity:',i5)") sum(a(:)%charge)-nelec,nint(naelec-nbelec)+1
	write(*,"(' Atoms:',i7,',  Basis functions:',i7,',  GTFs:',i7)") ncenter,nbasis,nprims
	if (wfntype==0) then
		write(*,"(' This is restricted single-determinant wavefunction')")
		write(*,"(' Orbitals from 1 to',i6,' are occupied')") nint(nelec/2)
	else if (wfntype==1) then
		write(*,"(' This is unrestricted single-determinant wavefunction')")
		write(*,"(' Orbitals from ',i6,' to',i6,' are alpha, from',i6,' to',i6,' are occupied')") 1,nbasis,1,nint(naelec)
		write(*,"(' Orbitals from ',i6,' to',i6,' are beta,  from',i6,' to',i6,' are occupied')") nbasis+1,nmo,nbasis+1,nbasis+nint(nbelec)
	else if (wfntype==2) then
		write(*,"(' This is restricted open-shell wavefunction')")
		write(*,"(' Orbitals from',i6,' to',i6,' are doubly occupied')") 1,nint(nbelec)
		write(*,"(' Orbitals from',i6,' to',i6,' are singly occupied')") nint(nbelec)+1,nint(naelec)
	else if (wfntype==3) then
		write(*,"(' This is restricted post-HF wavefunction')")
	else if (wfntype==4) then
		write(*,"(' This is unrestricted open-shell wavefunction')")
		write(*,"(' Orbitals from ',i6,' to',i6,' are alpha, from',i6,' to',i6,' are beta')") 1,nbasis,nbasis+1,nmo
	end if
end if

!Find out index of HOMO, will be used in some cases, only for RHF
if (wfntype==0) then
	do idxHOMO=nmo,1,-1
		if (nint(MOocc(idxHOMO))==2D0) exit
	end do
end if

end subroutine

!----- A routine used to read GAMESS-US LCAO matrix. One should first move pointer to the line containing "EIGENVECTORS"
subroutine readgmsLCAO(fileid,ifirefly,n1,n2,mat,ene,sym)
implicit real*8(a-h,o-z)
integer fileid,ifirefly,n1,n2
real*8 :: mat(n1,n2),ene(n2)
character*4 :: sym(n2)
character c80tmp*80
read(fileid,*)
read(fileid,*)
ncol=5
nt=ceiling(n2/float(ncol))
do i=1,nt !Number of frames
! 	write(*,*) i !Show frame
	read(fileid,*)
	ns=(i-1)*ncol+1
	if (i/=nt) ne=(i-1)*ncol+ncol
	if (i==nt) ne=n2
	read(fileid,*)
! 	read(fileid,"(a)") c80tmp !Used to debug which column has problem
! 	write(*,*) trim(c80tmp)
	read(fileid,"(15x)",advance="no")
	do j=ns,ne
		read(fileid,"(f11.4)",advance="no") ene(j)
	end do
	read(fileid,*)
	read(fileid,*) sym(ns:ne)
	do k=1,n1 !Scan rows in each frame
		read(fileid,"(15x)",advance='no') !Skip marker columns in each row
		do j=ns,ne !Scan elements in each row
! 			write(*,*) i,nt,k,j
			read(fileid,"(f11.6)",advance='no') mat(k,j)
		end do
		read(fileid,*)
	end do
end do
end subroutine









!=======================================================================
!=======================================================================
!!!!!!!!!!!!!!!! Below routines are used to output files !!!!!!!!!!!!!!!
!=======================================================================
!=======================================================================



!!---------- Output current coordinate to pdb file
subroutine outpdb(outpdbname,ifileid)
use defvar
character(len=*) outpdbname
integer i,ifileid
open(ifileid,file=outpdbname,status="replace")
write(ifileid,"('REMARK   Generated by Multiwfn, Totally',i10,' atoms')") ncenter
do i=1,ncenter
	write(ifileid,"(a6,i5,1x,a4,1x,a3, 1x,a1,i4,4x,3f8.3,2f6.2,10x,a2)") &
	"HETATM",i,' '//ind2name_up(a(i)%index)//' ',"MOL",'A',1,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a,1.0,0.0,adjustr(ind2name_up(a(i)%index))
end do
write(ifileid,"('END')")
close(ifileid)
write(*,*) "Exporting pdb file finished!"
end subroutine


!!---------- Output current coordinate and atomic charges to pqr file
subroutine outpqr(outpqrname,ifileid)
use defvar
character(len=*) outpqrname
integer i,ifileid
open(ifileid,file=outpqrname,status="replace")
write(ifileid,"('REMARK   Generated by Multiwfn, Totally',i10,' atoms')") ncenter
do i=1,ncenter
	write(ifileid,"(a6,i5,1x,a4,1x,a3, 1x,a1,i4,4x,3f8.3,f13.8,f9.4,a2)") &
	"HETATM",i,' '//ind2name_up(a(i)%index)//' ',"MOL",'A',1,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a,a(i)%charge,vdwr(a(i)%index)*b2a,adjustr(ind2name_up(a(i)%index))
end do
write(ifileid,"('END')")
close(ifileid)
write(*,*) "Exporting pqr file finished!"
write(*,"(a)") " This file contains atomic charges that originally recorded in your .chg file. The radius column corresponds to Bondi vdW radii"
end subroutine


!!---------- Output current coordinate to xyz file
subroutine outxyz(outxyzname,ifileid)
use defvar
character(len=*) outxyzname
integer i,ifileid
open(ifileid,file=outxyzname,status="replace")
write(ifileid,"(i6)") ncenter
write(ifileid,*) "Generated by Multiwfn"
do i=1,ncenter
	write(ifileid,"(a,3f16.8)") ind2name(a(i)%index),a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a
end do
close(ifileid)
write(*,*) "Exporting xyz file finished!"
end subroutine


!!---------- Output current coordinate to cml file
!iunit=0: In Angstrom, iunit=1: In Bohr
subroutine outcml(outcmlname,ifileid,iunit)
use defvar
character(len=*) outcmlname
character tmpstr1*80,tmpstr2*80,tmpstr3*80,tmpstr4*80
integer i,ifileid,iunit
if (.not.allocated(connmat)) call genconnmat !Generate connectivity matrix

open(ifileid,file=outcmlname,status="replace")
write(ifileid,"(a)") '<molecule>'
write(ifileid,"(a)") ' <atomArray>'
fac=1
if (iunit==0) fac=b2a !Convert to Angstrom

do i=1,ncenter
    write(tmpstr1,*) i
    write(tmpstr2,"(f12.6)") a(i)%x*fac
    write(tmpstr3,"(f12.6)") a(i)%y*fac
    write(tmpstr4,"(f12.6)") a(i)%z*fac
    write(ifileid,"(a)") &
    '  <atom id="a'//trim(adjustl(tmpstr1))//'" elementType="'//trim(adjustl(a(i)%name))//'" x3="'//trim(adjustl(tmpstr2))&
    //'" y3="'//trim(adjustl(tmpstr3))//'" z3="'//trim(adjustl(tmpstr4))//'"/>'
end do
write(ifileid,"(a)") ' </atomArray>'
write(ifileid,"(a)") ' <bondArray>'
do i=1,ncenter
    do j=i+1,ncenter
        if (connmat(i,j)>0) then
            write(tmpstr1,*) i
            write(tmpstr2,*) j
            write(tmpstr3,*) connmat(i,j)
            write(ifileid,"(a)") '  <bond atomRefs2="a'//trim(adjustl(tmpstr1))//' a'//&
            trim(adjustl(tmpstr2))//'" order="'//trim(adjustl(tmpstr3))//'"/>'
        end if
    end do
end do
write(ifileid,"(a)") ' </bondArray>'
write(ifileid,"(a)") '</molecule>'

close(ifileid)
write(*,*) "Exporting cml file finished!"
end subroutine


!!---------- Output current coordinate to chg file
subroutine outchg(outchgname,ifileid)
use defvar
character(len=*) outchgname
integer i,ifileid
open(ifileid,file=outchgname,status="replace")
do i=1,ncenter
	write(ifileid,"(a,4f16.8)") ind2name(a(i)%index),a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a,a(i)%charge
end do
close(ifileid)
write(*,*) "Exporting chg file finished!"
end subroutine


!!---------- Output current coordinate to Gaussian input file
subroutine outgjf(outgjfname,ifileid)
use defvar
character(len=*) outgjfname
character selectyn
selectyn='n'
if (allocated(CObasa).and.wfntype<=2) then
    write(*,"(a)") " Do you also want to write current wavefunction as initial guess into the .gjf file? (y/n)"
    read(*,*) selectyn
end if
open(ifileid,file=outgjfname,status="replace")
if (selectyn=='y') then
    write(ifileid,"(a,/,/,a,/)") "#P B3LYP/6-31G* guess=cards","Generated by Multiwfn"
else
    write(ifileid,"(a,/,/,a,/)") "#P B3LYP/6-31G*","Generated by Multiwfn"
end if
netcharge=nint(sum(a%charge)-nelec)
if (nelec==0) netcharge=0 !nelec==0 means no electron informations, e.g. pdb file
write(ifileid,"(2i2)") netcharge,nint(naelec-nbelec)+1
do i=1,ncenter
	write(ifileid,"(a,1x,3f14.8)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a
end do
if (selectyn=='y') then
    write(ifileid,"(/,'5(E16.9)',/,'-1')")
    do i=1,nbasis
	    if (wfntype==0.or.wfntype==2) write(ifileid,"('! Orbital:',i6,' Occ:',f10.6)") i,MOocc(i)
	    if (wfntype==1) write(ifileid,"('! Alpha orbital:',i6,' Occ:',f10.6)") i,MOocc(i)
	    write(ifileid,"(5E16.9)") (CObasa(j,i),j=1,nbasis)
    end do
    if (wfntype==1) then
	    write(ifileid,"('-1')")
	    do i=1,nbasis
		    write(ifileid,"('! Beta orbital:',i6,' Occ:',f10.6)") i,MOocc(nbasis+i)
		    write(ifileid,"(5E16.9)") (CObasb(j,i),j=1,nbasis)
	    end do
    end if
    write(ifileid,"('0',/)")
end if
write(ifileid,*)  !Two blank line at the end of the file
close(ifileid)
write(*,"(a)") " Exporting Gaussian input file finished! It corresponds to single point task at B3LYP/6-31G* level"
if (selectyn=='y') write(*,"(a)") " Note that you must specify the basis set to the one &
originally used to yield the wavefunction, otherwise Gaussian calculation must be failed"
end subroutine


!!---------- Output current coordinate to GAMESS-US input file
!The level exactly corresponds to the B3LYP-D3(BJ)/6-31G* of Gaussian
subroutine outGAMESSinp(outname,ifileid)
use defvar
character(len=*) outname
character SCFTYPE*3,selectyn
ioutguess=0
if (allocated(CObasa)) then
	write(*,*) "If write initial guess information? (y/n)"
	read(*,*) selectyn
	if (selectyn=='y'.or.selectyn=='Y') ioutguess=1
end if
netcharge=nint(sum(a%charge)-nelec)
if (nelec==0) netcharge=0 !nelec==0 means no electron informations, e.g. pdb file
mult=nint(naelec-nbelec)+1
iopsh=0
if (mult/=1.or.wfntype==1.or.wfntype==4) iopsh=1
SCFTYPE="RHF"
if (iopsh==1) SCFTYPE="UHF"
open(ifileid,file=outname,status="replace")
write(ifileid,"(a,i1,a,i1,a)") " $CONTRL SCFTYP="//SCFTYPE//" MULT=",mult," ICHARG=",netcharge," RUNTYP=ENERGY"
write(ifileid,"(a)") " DFTTYP=B3LYPV1R ISPHER=0 MAXIT=60 NPRINT=-5 $END"
write(ifileid,"(a)") " $BASIS GBASIS=N31 NGAUSS=6 NDFUNC=1 NPFUNC=0 DIFFSP=.F. DIFFS=.F. $END"
write(ifileid,"(a)") " $SYSTEM MWORDS=200 $END"
!  $lmoeda matom(1)=3,4 mcharg(1)=0,0 mmult(1)=1,1 $end
write(ifileid,"(a)") " $SCF DIRSCF=.T. $END"
write(ifileid,"(a)") " $DFT IDCVER=4 $END"
if (ioutguess==1) write(ifileid,"(a,i5,a)") " $GUESS GUESS=MOREAD NORB=",nbasis," $END"
write(ifileid,"(a)") " $DATA"
write(ifileid,"(a)") "Generated by Multiwfn"
write(ifileid,"(a)") "C1"
do i=1,ncenter
	write(ifileid,"(a,1x,f5.1,1x,3f14.8)") a(i)%name,dfloat(a(i)%index),a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a
end do
write(ifileid,"(a)") " $END"
! Write $VEC information
if (ioutguess==1) then
	write(ifileid,*)
	write(ifileid,"(a)") " $VEC"
	ntime=ceiling(nbasis/5D0)
	idxmo=1
	do imo=1,nbasis
		do itime=1,ntime
			if (itime<ntime) then
				write(ifileid,"(i2,i3,5(1PE15.8))") idxmo,itime,CObasa((itime-1)*5+1:itime*5,imo)
			else
				write(ifileid,"(i2,i3,5(1PE15.8))") idxmo,itime,CObasa((itime-1)*5+1:nbasis,imo)
			end if
		end do
		idxmo=idxmo+1
		if (idxmo==100) idxmo=0
	end do
	if (iopsh==1) then
		idxmo=1
		do imo=1,nbasis
			do itime=1,ntime
				if (itime<ntime) then
					write(ifileid,"(i2,i3,5(1PE15.8))") idxmo,itime,CObasb((itime-1)*5+1:itime*5,imo)
				else
					write(ifileid,"(i2,i3,5(1PE15.8))") idxmo,itime,CObasb((itime-1)*5+1:nbasis,imo)
				end if
			end do
			idxmo=idxmo+1
			if (idxmo==100) idxmo=0
		end do
	end if
	write(ifileid,"(a)") " $END"
end if
close(ifileid)
write(*,"(a)") " Exporting GAMESS-US input file finished! It corresponds to single point task at B3LYP-D3(BJ)/6-31G* level"
end subroutine


!!---------- Output current coordinate to ORCA input file
subroutine outORCAinp(outname,ifileid)
use defvar
character(len=*) outname
character keyword*200,c200tmp*200,solvname*30
integer :: itask=1,idiffuse=0
isolv=0
write(*,"(a)") " Note: The generated keywords are at least compatible with ORCA 4.2"
do while(.true.)
    write(*,*)
    if (idiffuse==1) write(*,*) "-2 Toggle adding diffuse functions, current: Yes"
    if (idiffuse==0) write(*,*) "-2 Toggle adding diffuse functions, current: No"
    if (isolv==1) write(*,*) "-1 Toggle employing SMD solvation model, current: Yes, "//trim(solvname)
    if (isolv==0) write(*,*) "-1 Toggle employing SMD solvation model, current: No"
    if (itask==1) write(*,*) "0 Select task, current: Single point"
    if (itask==2) write(*,*) "0 Select task, current: Optimization"
    if (itask==3) write(*,*) "0 Select task, current: Frequency"
    if (itask==4) write(*,*) "0 Select task, current: Optimization + Frequency"
    write(*,*) "1 B97-3c"
    write(*,*) "2 RI-BLYP-D3(BJ)/def2-TZVP"
    write(*,*) "3 RI-B3LYP-D3(BJ)/def2-TZVP(-f)"
    write(*,*) "4 RI-B3LYP-D3(BJ)/def2-TZVP"
    write(*,*) "5 RI-wB97M-V/def2-TZVP"
    write(*,*) "6 RI-PWPB95-D3(BJ)/def2-TZVPP"
    write(*,*) "7 RI-PWPB95-D3(BJ)/def2-QZVPP"
    write(*,*) "8 DLPNO-CCSD(T)/cc-pVTZ with normalPNO and RIJK"
    write(*,*) "9 DLPNO-CCSD(T)/cc-pVTZ with tightPNO"
    write(*,*) "10 CCSD(T)/cc-pVTZ"
    write(*,*) "11 CCSD(T)-F12/cc-pVDZ-F12 with RI"
    write(*,*) "12 CCSD(T)/CBS with help of MP2 (cc-pVTZ->QZ extrapolation)"
    write(*,*) "13 CCSD(T)/CBS (cc-pVTZ->QZ extrapolation)"
    write(*,*) "20 sTD-DFT based on RI-wB97X-D3/def2-SV(P) orbitals"
    write(*,*) "21 TDA-DFT RI-PBE0/def2-SV(P) with riints_disk (much faster than 22)"
    write(*,*) "22 TDDFT RI-PBE0/def2-SV(P)"
    write(*,*) "23 TDDFT RI-wB2GP-PLYP/def2-TZVP"
    write(*,*) "24 EOM-CCSD/cc-pVTZ"
    read(*,*) ilevel
    if (ilevel==0) then
        write(*,*) "1 Single point"
        write(*,*) "2 Optimization"
        write(*,*) "3 Frequency"
        write(*,*) "4 Optimization + Frequency"
        write(*,*) "5 Optimization for transition state + Frequency"
        read(*,*) itask
    else if (ilevel==-1) then
        if (isolv==1) then
            isolv=0
        else
            isolv=1
            write(*,*) "Input name of solvent, e.g. ethanol"
            write(*,*) "If press ENTER button directly, water will be employed as solvent"
            write(*,*) "To customize a solvent for CPCM model, input ""c"""
            read(*,"(a)") solvname
            if (solvname==" ") then
                solvname="water"
            else if (solvname=='c') then
                write(*,*) "Input dielectic constant, e.g. 80"
                read(*,*) diecons
                write(*,*) "Input refractive index, e.g. 1.33"
                write(*,*) "If actual value is known, usually 1.33 is a good approximation"
                read(*,*) refrac
            end if
        end if
    else if (ilevel==-2) then
        if (idiffuse==1) then
            idiffuse=0
        else
            idiffuse=1
        end if
    else
        exit
    end if
end do

if (idiffuse==0) then
    if (ilevel==1) c200tmp="! B97-3c"
    if (ilevel==2) c200tmp="! BLYP D3 def2-TZVP def2/J" !For pure functional, RIJ is used by default even without def2/J
    if (ilevel==3) c200tmp="! B3LYP D3 def2-TZVP(-f) def2/J RIJCOSX"
    if (ilevel==4) c200tmp="! B3LYP D3 def2-TZVP def2/J RIJCOSX"
    if (ilevel==5) c200tmp="! wB97M-V def2-TZVP def2/J RIJCOSX grid4 gridx4"
    if (ilevel==6) c200tmp="! PWPB95 D3 def2-TZVPP def2/J def2-TZVPP/C RIJCOSX grid4 gridx4 tightSCF" !When RIJCOSX or RIJK is used, the MP2 will also use RI by default
    if (ilevel==7) c200tmp="! PWPB95 D3 def2-QZVPP def2/J def2-QZVPP/C RIJCOSX grid4 gridx4 tightSCF"
    if (ilevel==8) c200tmp="! DLPNO-CCSD(T) normalPNO RIJK cc-pVTZ cc-pVTZ/JK cc-pVTZ/C tightSCF"
    if (ilevel==9) c200tmp="! DLPNO-CCSD(T) tightPNO cc-pVTZ cc-pVTZ/C tightSCF"
    if (ilevel==10) c200tmp="! CCSD(T) cc-pVTZ tightSCF"
    if (ilevel==11) c200tmp="! CCSD(T)-F12/RI cc-pVDZ-F12 cc-pVDZ-F12-CABS cc-pVTZ/C"
    if (ilevel==12) c200tmp="! ExtrapolateEP2(3/4,cc,MP2) tightSCF"
    if (ilevel==13) c200tmp="! CCSD(T) Extrapolate(3/4,cc) tightSCF"
    if (ilevel==20) c200tmp="! wB97X-D3 def2-SV(P) def2/J RIJCOSX"
    if (ilevel==21) c200tmp="! PBE0 def2-SV(P) def2/J def2-SVP/C RIJCOSX grid4 gridx4 tightSCF"
    if (ilevel==22) c200tmp="! PBE0 def2-SV(P) def2/J RIJCOSX grid4 gridx4 tightSCF"
    if (ilevel==23) c200tmp="! wB2GP-PLYP def2-TZVP def2/J def2-TZVP/C RIJCOSX grid4 gridx4 tightSCF"
    if (ilevel==24) c200tmp="! EOM-CCSD cc-pVTZ tightSCF"
else
    if (ilevel==1) c200tmp="! B97-3c"
    if (ilevel==2) c200tmp="! BLYP D3 ma-def2-TZVP autoaux"
    if (ilevel==3) c200tmp="! B3LYP D3 ma-def2-TZVP(-f) autoaux RIJCOSX"
    if (ilevel==4) c200tmp="! B3LYP D3 ma-def2-TZVP autoaux RIJCOSX"
    if (ilevel==5) c200tmp="! wB97M-V ma-def2-TZVP autoaux RIJCOSX grid4 gridx4"
    if (ilevel==6) c200tmp="! PWPB95 D3 ma-def2-TZVPP autoaux RIJCOSX grid4 gridx4 tightSCF" !When RIJCOSX or RIJK is used, the MP2 will also use RI by default
    if (ilevel==7) c200tmp="! PWPB95 D3 ma-def2-QZVPP autoaux RIJCOSX grid4 gridx4 tightSCF"
    if (ilevel==8) c200tmp="! DLPNO-CCSD(T) normalPNO RIJK aug-cc-pVTZ aug-cc-pVTZ/JK aug-cc-pVTZ/C tightSCF"
    if (ilevel==9) c200tmp="! DLPNO-CCSD(T) tightPNO aug-cc-pVTZ aug-cc-pVTZ/C tightSCF"
    if (ilevel==10) c200tmp="! CCSD(T) aug-cc-pVTZ tightSCF"
    if (ilevel==11) c200tmp="! CCSD(T)-F12/RI cc-pVDZ-F12 cc-pVDZ-F12-CABS cc-pVTZ/C"
    if (ilevel==12) c200tmp="! ExtrapolateEP2(3/4,aug-cc,MP2) tightSCF"
    if (ilevel==13) c200tmp="! CCSD(T) Extrapolate(3/4,aug-cc) tightSCF"
    if (ilevel==20) c200tmp="! wB97X-D3 ma-def2-SV(P) autoaux RIJCOSX"
    if (ilevel==21) c200tmp="! PBE0 ma-def2-SV(P) autoaux RIJCOSX grid4 gridx4 tightSCF"
    if (ilevel==22) c200tmp="! PBE0 ma-def2-SV(P) autoaux RIJCOSX grid4 gridx4 tightSCF"
    if (ilevel==23) c200tmp="! wB2GP-PLYP ma-def2-TZVP autoaux RIJCOSX grid4 gridx4 tightSCF"
    if (ilevel==24) c200tmp="! EOM-CCSD aug-cc-pVTZ tightSCF"
    
    if (ilevel==1.or.ilevel==11) then
        write(*,*) "Warning: Diffuse functions cannot be added to this level!"
        write(*,*) "Press ENTER button to return"
        read(*,*)
        return
    end if
end if

if (itask==1) then !SP
    keyword=trim(c200tmp)//" noautostart miniprint nopop"
else if (itask==2) then !opt task uses tightSCF by default
    keyword=trim(c200tmp)//" opt noautostart miniprint nopop"
    if (ilevel==22.and.idiffuse==0) keyword=trim(keyword)//" def2-SVP/C" !RI-TDDFT opt must use /C
else if (itask==3) then !freq
    if (index(c200tmp,"tightSCF")==0) then
        keyword=trim(c200tmp)//" freq tightSCF noautostart miniprint nopop"
    else
        keyword=trim(c200tmp)//" freq noautostart miniprint nopop"
    end if
else if (itask==4) then !opt freq
    if (index(c200tmp,"tightSCF")==0) then
        keyword=trim(c200tmp)//" opt freq tightSCF noautostart miniprint nopop"
    else
        keyword=trim(c200tmp)//" opt freq noautostart miniprint nopop"
    end if
    if (ilevel==22.and.idiffuse==0) keyword=trim(keyword)//" def2-SVP/C" !RI-TDDFT opt must use /C
else if (itask==5) then !optTS
    if (index(c200tmp,"tightSCF")==0) then
        keyword=trim(c200tmp)//" optTS freq tightSCF noautostart miniprint nopop"
    else
        keyword=trim(c200tmp)//" optTS freq noautostart miniprint nopop"
    end if
    if (ilevel==22.and.idiffuse==0) keyword=trim(keyword)//" def2-SVP/C" !RI-TDDFT opt must use /C
end if

open(ifileid,file=outname,status="replace")
write(ifileid,"(a)") trim(keyword)
netcharge=nint(sum(a%charge)-nelec)
if (nelec==0) netcharge=0 !nelec==0 means no electron informations, e.g. pdb file
write(ifileid,"(a)") "%maxcore 1000"
write(ifileid,"('%pal nprocs',i3,' end')") nthreads
if (isolv==1) then
    write(ifileid,"(a)") "%cpcm"
    if (solvname=='c') then
        write(ifileid,"(a,f8.2)") "epsilon",diecons
        write(ifileid,"(a,f8.2)") "refrac",refrac
    else
        write(ifileid,"(a)") "smd true"
        write(ifileid,"(a)") 'SMDsolvent "'//trim(solvname)//'"'
    end if
    write(ifileid,"(a)") "end"
end if
if (itask==5) then !Calculate Hessian for optTS
    write(ifileid,"(a)") "%geom Calc_Hess true end"
end if
if (ilevel==20) then
    write(ifileid,"(a)") "%tddft"
    write(ifileid,"(a)") "Mode sTDDFT"
    write(ifileid,"(a)") "Ethresh 7.0"
    write(ifileid,"(a)") "PThresh 1e-4"
    write(ifileid,"(a)") "PTLimit 30"
    write(ifileid,"(a)") "maxcore 6000" !sTDDFT only support serial mode, therefore more memory could be assigned
    write(ifileid,"(a)") "end"
else if (ilevel==21) then
    write(ifileid,"(a)") "%tddft"
    write(ifileid,"(a)") "nroots 10"
    write(ifileid,"(a)") "mode riints_disk"
    write(ifileid,"(a)") "end"
else if (ilevel==22.or.ilevel==23) then
    write(ifileid,"(a)") "%tddft"
    write(ifileid,"(a)") "nroots 10"
    write(ifileid,"(a)") "TDA false"
    write(ifileid,"(a)") "end"
else if (ilevel==24) then
    write(ifileid,"(a)") "%mdci"
    write(ifileid,"(a)") "nroots 3"
    write(ifileid,"(a)") "end"
end if
write(ifileid,"('* xyz',2i4)") netcharge,nint(naelec-nbelec)+1
do i=1,ncenter
	write(ifileid,"(a,1x,3f14.8)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a
end do
write(ifileid,*) "*"
close(ifileid)
write(*,"(a)") " Exporting ORCA input file finished! You should properly check it and modify keywords"
end subroutine


!!---------- Output current coordinate to NWChem input file
subroutine outNWCheminp(outname,ifileid)
use defvar
character(len=*) outname
open(ifileid,file=outname,status="replace")
netcharge=nint(sum(a%charge)-nelec)
if (nelec==0) netcharge=0 !nelec==0 means no electron informations, e.g. pdb file
write(ifileid,"(a,i3)") "charge",netcharge
write(ifileid,"(a)") "GEOMETRY"
do i=1,ncenter
	write(ifileid,"(a,1x,3f14.8)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a
end do
write(ifileid,"(a)") "END"
write(ifileid,"(a)") "BASIS"
write(ifileid,"(a)") "* library 6-31G*"
write(ifileid,"(a)") "END"
write(ifileid,"(a)") "DFT"
write(ifileid,"(a)") "XC B3LYP"
write(ifileid,"(a,i3)") "mult",nint(naelec-nbelec)+1
write(ifileid,"(a)") "END"
write(ifileid,"(a)") "TASK DFT ENERGY"
close(ifileid)
write(*,"(a)") " Exporting NWChem input file finished! It corresponds to single point task at B3LYP/6-31G* level"
end subroutine


!!---------- Output current coordinate to MOPAC input file
subroutine outMOPACinp(outname,ifileid)
use defvar
character(len=*) outname
character multistr*80,chargestr*10
open(ifileid,file=outname,status="replace")
netcharge=nint(sum(a%charge)-nelec)
if (nelec==0) netcharge=0 !nelec==0 means no electron informations, e.g. pdb file
write(chargestr,"(i4)") netcharge
chargestr=adjustl(chargestr)
multival=nint(naelec-nbelec)+1
if (multival==1) then
	write(ifileid,"(a,a)") "PM7 precise charge=",trim(chargestr)
else
	if (multival==2) multistr="doublet"
	if (multival==3) multistr="triplet"
	if (multival==4) multistr="quartet"
	if (multival==5) multistr="quintet"
	if (multival==6) multistr="sextet"
	write(ifileid,"(a,a,1x,a)") "PM7 precise UHF charge=",trim(chargestr),multistr
end if
write(ifileid,"(a)") "molecule"
write(ifileid,"(a)") "All coordinates are Cartesian"
do i=1,ncenter
	write(ifileid,"(a,1x,3f14.8)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a
end do
write(*,"(a)") " Exporting MOPAC input file finished! It corresponds to geometry optimization task at PM7 level"
end subroutine



!!---------- Output current coordinate to PSI input file
subroutine outPSIinp(outname,ifileid)
use defvar
character(len=*) outname
open(ifileid,file=outname,status="replace")
netcharge=nint(sum(a%charge)-nelec)
if (nelec==0) netcharge=0 !nelec==0 means no electron informations, e.g. pdb file
write(ifileid,"(a)") "molecule mol {"
write(ifileid,"(2i4)") netcharge,nint(naelec-nbelec)+1
do i=1,ncenter
	write(ifileid,"(a,1x,3f14.8)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a
end do
write(ifileid,"(a)") "}"
write(ifileid,"(a)") "set basis cc-pVDZ"
write(ifileid,"(a)") "energy('b3lyp')"
close(ifileid)
write(*,"(a)") " Exporting PSI input file finished! It corresponds to single point task at B3LYP/cc-pVDZ level"
end subroutine


!!---------- Output current coordinate to MRCC input file
subroutine outMRCCinp(outname,ifileid)
use defvar
character(len=*) outname
open(ifileid,file=outname,status="replace")
netcharge=nint(sum(a%charge)-nelec)
if (nelec==0) netcharge=0 !nelec==0 means no electron informations, e.g. pdb file
write(ifileid,"(a)") "basis=cc-pVDZ"
write(ifileid,"(a)") "calc=CCSDT(Q)"
write(ifileid,"(a,i2)") "charge=",netcharge
write(ifileid,"(a,i2)") "multi=",nint(naelec-nbelec)+1
write(ifileid,"(a)") "mem=2500MB"
write(ifileid,*)
write(ifileid,"(a)") "geom=xyz"
write(ifileid,"(i5)") ncenter
write(ifileid,*)
do i=1,ncenter
	write(ifileid,"(a,1x,3f14.8)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a
end do
close(ifileid)
write(*,"(a)") " Exporting MRCC input file finished! The file is named ""MINP"" in current folder. It corresponds to single point task at CCSDT(Q)/cc-pVDZ level"
end subroutine


!!---------- Output current coordinate to CFOUR input file
subroutine outCFOURinp(outname,ifileid)
use defvar
character(len=*) outname
open(ifileid,file=outname,status="replace")
netcharge=nint(sum(a%charge)-nelec)
if (nelec==0) netcharge=0 !nelec==0 means no electron informations, e.g. pdb file
write(ifileid,"(a)") "mol"
do i=1,ncenter
	write(ifileid,"(a,1x,3f14.8)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a
end do
write(ifileid,*)
write(ifileid,"(a,i3)") "CHARGE=",netcharge
write(ifileid,"(a,i3)") "MULTIPLICITY=",nint(naelec-nbelec)+1
write(ifileid,"(a)") "FROZEN_CORE=ON"
write(ifileid,"(a)") "*CFOUR(CALC=CCSD(T),BASIS=cc-pVTZ,COORD=CARTESIAN,CC_PROG=NCC,ABCDTYPE=AOBASIS)"
write(ifileid,*)
close(ifileid)
write(*,"(a)") " Exporting CFOUR input file finished! The file is named ""ZMAT"" in current folder. It corresponds to single point task at CCSD(T,FC)/cc-pVTZ level"
end subroutine


!!---------- Output current coordinate to Molpro input file
subroutine outMolproinp(outname,ifileid)
use defvar
character(len=*) outname
open(ifileid,file=outname,status="replace")
netcharge=nint(sum(a%charge)-nelec)
if (nelec==0) netcharge=0 !nelec==0 means no electron informations, e.g. pdb file
write(ifileid,"(a)") "angstrom"
write(ifileid,"(a)") "geometry={"
do i=1,ncenter
	write(ifileid,"(a,',,',f14.8,',',f14.8,',',f14.8)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a
end do
write(ifileid,"(a)") "}"
write(ifileid,"(a)") "basis=vtz"
write(ifileid,"(a)") "{hf"
write(ifileid,"(a,i2,a,i2)") "wf, charge=",netcharge,", spin=",nint(naelec-nbelec)
write(ifileid,"(a)") "}"
write(ifileid,"(a)") "ccsd(T)"
close(ifileid)
write(*,"(a)") " Exporting Molpro input file finished! It corresponds to single point task at CCSD(T)/cc-pVTZ level"
end subroutine


!!---------- Output current coordinate to Molcas input file
subroutine outmolcasinp(outname,ifileid)
use defvar
character(len=*) outname
open(ifileid,file=outname,status="replace")
netcharge=nint(sum(a%charge)-nelec)
if (nelec==0) netcharge=0 !nelec==0 means no electron informations, e.g. pdb file
write(ifileid,"(a)") "&GATEWAY"
write(ifileid,"(a)") "Coord"
write(ifileid,"(i5)") ncenter
write(ifileid,"(a)") "Generated by Multiwfn"
do i=1,ncenter
	write(ifileid,"(a,3f14.8)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a
end do
write(ifileid,"(a)") "basis=cc-pVDZ"
write(ifileid,"(a)") "&SEWARD"
write(ifileid,"(a)") "&SCF"
if (naelec==nbelec) then
	write(ifileid,"(a)") "ksdft=b3lyp"
else
	write(ifileid,"(a)") "uhf;ksdft=b3lyp"
end if
write(ifileid,"(a,i3)") "charge=",netcharge
write(ifileid,"(a,i3)") "zspin=",nint(naelec-nbelec)
close(ifileid)
write(*,"(a)") " Exporting Molcas input file finished! It corresponds to single point task at B3LYP/cc-pVDZ level"
end subroutine


!!---------- Output current coordinate to Dalton input file (.dal and .mol)
!When symmetry is used, the number of atomtypes may not be consistent with the number of elements, such as O3.
!Therefore, for simplicity, we do not use symmetry (i.e. Nosymmetry), and thus assign atomtypes as the number of elements
!Spin multiplicity is not set in .dal, because the rule in Dalton is quite complicated
subroutine outDaltoninp(dalname,molname,ifileid)
use defvar
character(len=*) dalname,molname
character tmpstr*5,c20tmp*20,c20tmp2*20
netcharge=nint(sum(a%charge)-nelec)
if (nelec==0) netcharge=0 !nelec==0 means no electron informations, e.g. pdb file
if (dalname/=" ") then
	open(ifileid,file=dalname,status="replace")
	write(ifileid,"(a)") "**DALTON INPUT"
	write(ifileid,"(a)") ".RUN WAVE FUNCTIONS"
	write(ifileid,"(a)") "**WAVE FUNCTIONS"
	write(ifileid,"(a)") ".DFT"
	write(ifileid,"(a)") " B3LYPg"
	write(ifileid,"(a)") "**END OF INPUT"
	close(ifileid)
end if
! write(*,*) sum(a%charge),nelec,nEDFelec,netcharge

open(ifileid,file=molname,status="replace")
write(ifileid,"(a)") "ATOMBASIS"
write(ifileid,"(a)") "test molecule"
write(ifileid,"(a)") "Generated by Multiwfn"
natmtype=0
do iele=1,nelesupp
	if (any(a%index==iele)) natmtype=natmtype+1
end do
write(c20tmp,"(i5)") natmtype
c20tmp=adjustl(c20tmp)
write(c20tmp2,"(i5)") netcharge
c20tmp2=adjustl(c20tmp2)
write(ifileid,"(a,a,a,a)") "Atomtypes=",trim(c20tmp)," Angstrom Nosymmetry charge=",trim(c20tmp2)
do iele=1,nelesupp
	natmthis=count(a%index==iele)
	if (natmthis>0) then	
		write(c20tmp,"(f4.1)") dfloat(iele)
		c20tmp=adjustl(c20tmp)
		write(c20tmp2,"(i5)") natmthis
		c20tmp2=adjustl(c20tmp2)
		write(ifileid,"(a,a,a,a,a)") "Charge=",trim(c20tmp)," Atoms=",trim(c20tmp2)," Basis=6-31G*"
		itmp=0
		do iatm=1,ncenter
			if (a(iatm)%index/=iele) cycle
			itmp=itmp+1
			write(tmpstr,"(i5)") itmp
			tmpstr=adjustl(tmpstr)
			write(ifileid,"(a,3f14.8)") trim(a(iatm)%name)//trim(tmpstr),a(iatm)%x*b2a,a(iatm)%y*b2a,a(iatm)%z*b2a
		end do
	end if
end do
close(ifileid)
write(*,"(a)") " Exporting Dalton input file finished! It corresponds to single point task at B3LYPg/6-31G* level"
if (naelec/=nbelec) write(*,*) "Electronic configuration in .dal file should be manually set properly"
end subroutine



!!!-------- Output 3D matrix with property to a cube file. fileid must be opened before invoking this routine, and close it after that
!Example of calling: outcube(holegrid,nx,ny,nz,orgx,orgy,orgz,gridvec1,gridvec2,gridvec3,10)
subroutine outcube(matrix,numx,numy,numz,org_x,org_y,org_z,trans1,trans2,trans3,fileid)
use defvar
implicit real*8 (a-h,o-z)
integer numx,numy,numz,fileid
real*8 org_x,org_y,org_z,trans1(3),trans2(3),trans3(3)
real*8 matrix(numx,numy,numz)

write(fileid,"(' Generated by Multiwfn')")
write(fileid,"(' Totally ',i12,' grid points')") numx*numy*numz
if (ncenter>=1) then
	write(fileid,"(i5,3f12.6)") ncenter,org_x,org_y,org_z
else
	write(fileid,"(i5,3f12.6)") 1,org_x,org_y,org_z
end if
write(fileid,"(i5,3f12.6)") numx,trans1
write(fileid,"(i5,3f12.6)") numy,trans2
write(fileid,"(i5,3f12.6)") numz,trans3
if (ncenter>=1) then
	do i=1,ncenter
		write(fileid,"(i5,4f12.6)") a(i)%index,a(i)%charge,a(i)%x,a(i)%y,a(i)%z
	end do
else
	write(*,"(a)") " Note: Current system has no atom, in order to maximize compatibility of the generated .cub file, a hydrogen atom is added to 0,0,0"
	write(*,*)
	write(fileid,"(i5,4f12.6)") 1,1D0,0D0,0D0,0D0
end if
where (abs(matrix)<=1D-99) matrix=0D0 !Diminish too small value, otherwise the symbol "E" cannot be shown by 1PE13.5 format e.g. 9.39376-116, 
!write(*,*) "Exporting cube file, please wait..."
do i=1,numx
	do j=1,numy
		write(fileid,"(6(1PE13.5))",advance="no") matrix(i,j,1:numz)
		write(fileid,*)
	end do
end do
end subroutine


!!!------------- Output grid data to .vti file, which can be visualized by ParaView
subroutine outvti(outvtiname,ifileid)
use defvar
implicit real*8 (a-h,o-z)
character(len=*) outvtiname
character outcmlname*200,selectyn
open(ifileid,file=outvtiname,status="replace")
write(ifileid,"(a)") '<?xml version="1.0"?>'
write(ifileid,"(a)") ' <VTKFile type="ImageData" version="0.1" byte_order="LittleEndian">'
write(ifileid,"(a,6i5,a,3f12.6,a,3f12.6,a)") '  <ImageData WholeExtent="',0,nx-1,0,ny-1,0,nz-1,' " Origin="',orgx,orgy,orgz,' " Spacing="',dx,dy,dz,' ">'
write(ifileid,"(a,6i5,a)") '  <Piece Extent="',0,nx-1,0,ny-1,0,nz-1,' ">'
write(ifileid,"(a)") '  <PointData Scalars="scalars">'
write(ifileid,"(a)") '  <DataArray Name="scalars" type="Float64" NumberOfComponents="1" Format="ascii">'
write(ifileid,"(4E14.6)") (((cubmat(i,j,k),i=1,nx),j=1,ny),k=1,nz)
write(ifileid,"(a)") '    </DataArray>'
write(ifileid,"(a)") '    </PointData>'
write(ifileid,"(a)") '    </Piece>'
write(ifileid,"(a)") '    </ImageData>'
write(ifileid,"(a)") ' </VTKFile>'
close(ifileid)
write(*,*) "Exporting vti file finished!"
write(*,*)
write(*,"(a)") " Do you also want to export .cml file in Bohr, so that you can visualize grid data as well as molecular structure in ParaView? (y/n)"
read(*,*) selectyn
if (selectyn=='y') then
    write(*,*) "Input path of .cml file, e.g. C:\aqours.cml"
    read(*,"(a)") outcmlname
    call outcml(outcmlname,ifileid,1)
end if
end subroutine



!!!--------------------- Output current wavefunction to a .wfn
!If isortatmind==1, then any atom without GTF posited on it will not be output, and the index is filled to assure contiguous
!Orbitals with zero occupiation will not be outputted
!If ioutinfo==1, output information
subroutine outwfn(outwfnname,isortatmind,ioutinfo,ifileid)
use defvar
implicit real*8 (a-h,o-z)
character(len=*) outwfnname
integer isortatmind,ioutinfo,ifileid,indconv(ncenter)
!convGmul2wfn converts the g sequence used internally in Multiwfn (input) to commonly used g sequence in .wfn (the one outputted by Molden2AIM and g09 since B01 )
integer :: convGmul2wfn(35)=(/ (0,i=1,20), 23,29,32,27,22, 28,35,34,26,31, 33,30,25,24,21 /)
integer :: convGwfn2mul(35)=(/ (0,i=1,20), 35,25,21,34,33, 29,24,26,22,32, 30,23,31,28,27 /)

open(ifileid,file=outwfnname,status="replace")
write(ifileid,*) "Generated by Multiwfn"

if (isortatmind==1) then !Find real number of centers
	j=0
	do i=1,ncenter
		if (any(b%center==i)) j=j+1
	end do
	write(ifileid,"('GAUSSIAN',i15,' MOL ORBITALS',i7,' PRIMITIVES',i9,' NUCLEI')") count(MOocc(1:nmo)/=0D0),nprims,j
else
	write(ifileid,"('GAUSSIAN',i15,' MOL ORBITALS',i7,' PRIMITIVES',i9,' NUCLEI')") count(MOocc(1:nmo)/=0D0),nprims,ncenter
end if

j=1
do i=1,ncenter
	if (isortatmind==1) then
		if (all(b%center/=i)) cycle
	end if
	indconv(j)=i !The j actual atom corresponds to the i original atom
	write(ifileid,"(2x,a2,i4,4x,'(CENTRE',i3,')',1x,3f12.8,'  CHARGE =',f5.1)") a(i)%name,j,j,a(i)%x,a(i)%y,a(i)%z,a(i)%charge
	j=j+1
end do

if (isortatmind==1) then !Convert center of GTF to reordered index
	do i=1,j-1 !Cycle the centers with GTF
		where(b%center==indconv(i)) b%center=i
	end do
end if

write(ifileid,"('CENTRE ASSIGNMENTS  ',20i3)") b(1:nprims)%center

!Convert the g sequence to common sequence in .wfn, output them, and then convert back. This make output easier
do i=1,nprims
	if (b(i)%type>=21.and.b(i)%type<=35) b(i)%type=convGmul2wfn(b(i)%type)
end do
write(ifileid,"('TYPE ASSIGNMENTS    ',20i3)") b%type
do i=1,nprims
	if (b(i)%type>=21.and.b(i)%type<=35) b(i)%type=convGwfn2mul(b(i)%type)
end do

write(ifileid,"('EXPONENTS ',5D14.7)") b%exp
imo=0
nzeroocc=0
do i=1,nmo
	if (MOocc(i)==0D0) then
		nzeroocc=nzeroocc+1
		cycle
	end if
	imo=imo+1 !Use imo instead of i, this can make MO index contiguous
	write(ifileid,"('MO',I5,'     MO 0.0        OCC NO = ',f12.7,'  ORB. ENERGY =', f12.6)") imo,MOocc(i),MOene(i)
	write(ifileid,"(5D16.8)") (co(i,j),j=1,nprims)
end do
write(ifileid,"('END DATA',/,' THE  HF ENERGY = ',f19.12,' THE VIRIAL(-V/T)= ',f12.8)") totenergy,virialratio
close(ifileid)
if (nzeroocc>0.and.ioutinfo==1) write(*,"(a,i10,a)") " Note: Found",nzeroocc," zero occupied orbitals and have discarded them"
end subroutine



!!!----------------- Output current wavefunction to a .wfx file
!Orbitals with zero occupiation will not be outputted
!If ioutinfo==1, output information
subroutine outwfx(outwfxname,ioutinfo,ifileid)
use defvar
implicit real*8 (a-h,o-z)
character(len=*) outwfxname
character c10tmp*10
integer ioutinfo,ifileid,indconv(ncenter)
! convGseq is used to convert g used in internal sequence of Multiwfn to .wfx sequence
! PS: spdfh sequence in .wfx is identical to Multiwfn
integer :: convGseq(35)
convGseq(21:35)=(/ 23,29,32,27,22,28,35,34,26,31,33,30,25,24,21 /) !g 21~35

nzeroocc=count(MOocc==0D0)
open(ifileid,file=outwfxname,status="replace")
write(ifileid,"(a)") "<Title>"
write(ifileid,*) "Generated by Multiwfn"
write(ifileid,"(a)") "</Title>"
write(ifileid,"(a)") "<Keywords>"
write(ifileid,"(a)") " GTO"
write(ifileid,"(a)") "</Keywords>"
write(ifileid,"(a)") "<Number of Nuclei>"
write(ifileid,"(i6)") ncenter
write(ifileid,"(a)") "</Number of Nuclei>"
write(ifileid,"(a)") "<Number of Occupied Molecular Orbitals>"
write(ifileid,"(i6)") nmo-nzeroocc
write(ifileid,"(a)") "</Number of Occupied Molecular Orbitals>"
write(ifileid,"(a)") "<Number of Perturbations>"
write(ifileid,"(i6)") 0
write(ifileid,"(a)") "</Number of Perturbations>"
write(ifileid,"(a)") "<Net Charge>"
write(ifileid,"(i6)") sum(a%charge)-nint(nelec)
write(ifileid,"(a)") "</Net Charge>"
write(ifileid,"(a)") "<Number of Electrons>"
write(ifileid,"(i6)") nint(nelec)
write(ifileid,"(a)") "</Number of Electrons>"
write(ifileid,"(a)") "<Number of Alpha Electrons>"
write(ifileid,"(i6)") nint(naelec)
write(ifileid,"(a)") "</Number of Alpha Electrons>"
write(ifileid,"(a)") "<Number of Beta Electrons>"
write(ifileid,"(i6)") nint(nbelec)
write(ifileid,"(a)") "</Number of Beta Electrons>"
write(ifileid,"(a)") "<Electronic Spin Multiplicity>"
write(ifileid,"(i6)") nint(naelec-nbelec)+1
write(ifileid,"(a)") "</Electronic Spin Multiplicity>"
write(ifileid,"(a)") "<Number of Core Electrons>"
write(ifileid,"(i6)") nEDFelec
write(ifileid,"(a)") "</Number of Core Electrons>"
write(ifileid,"(a)") "<Nuclear Names>"
do iatm=1,ncenter
	write(c10tmp,"(i6)") iatm
	write(ifileid,"(1x,a)") trim(a(iatm)%name)//trim(adjustl(c10tmp))
end do
write(ifileid,"(a)") "</Nuclear Names>"
write(ifileid,"(a)") "<Atomic Numbers>"
do iatm=1,ncenter
	write(ifileid,"(i3)") a(iatm)%index
end do
write(ifileid,"(a)") "</Atomic Numbers>"
write(ifileid,"(a)") "<Nuclear Charges>"
do iatm=1,ncenter
	write(ifileid,"(E20.12)") a(iatm)%charge
end do
write(ifileid,"(a)") "</Nuclear Charges>"
write(ifileid,"(a)") "<Nuclear Cartesian Coordinates>"
do iatm=1,ncenter
	write(ifileid,"(3E20.12)") a(iatm)%x,a(iatm)%y,a(iatm)%z
end do
write(ifileid,"(a)") "</Nuclear Cartesian Coordinates>"
write(ifileid,"(a)") "<Number of Primitives>"
write(ifileid,"(i6)") nprims
write(ifileid,"(a)") "</Number of Primitives>"
write(ifileid,"(a)") "<Primitive Centers>"
write(ifileid,"(5i20)") b%center
write(ifileid,"(a)") "</Primitive Centers>"
write(ifileid,"(a)") "<Primitive Types>"
icount=0
do iprim=1,nprims
	if (b(iprim)%type>=21.and.b(iprim)%type<=35) then
		write(ifileid,"(i20)",advance="no") convGseq(b(iprim)%type)
	else
		write(ifileid,"(i20)",advance="no") b(iprim)%type
	end if
	icount=icount+1
	if (icount==5.or.iprim==nprims) then
		write(ifileid,*)
		icount=0
	end if
end do
write(ifileid,"(a)") "</Primitive Types>"
write(ifileid,"(a)") "<Primitive Exponents>"
write(ifileid,"(5E20.12)") b%exp
write(ifileid,"(a)") "</Primitive Exponents>"
if (allocated(b_EDF)) then
	write(ifileid,"(a)") "<Additional Electron Density Function (EDF)>"
	write(ifileid,"(a)") "<Number of EDF Primitives>"
	write(ifileid,"(i6)") nEDFprims
	write(ifileid,"(a)") "</Number of EDF Primitives>"
	write(ifileid,"(a)") "<EDF Primitive Centers>"
	write(ifileid,"(5i20)") b_EDF%center
	write(ifileid,"(a)") "</EDF Primitive Centers>"
	write(ifileid,"(a)") "<EDF Primitive Types>"
	write(ifileid,"(5i20)") b_EDF%type
	write(ifileid,"(a)") "</EDF Primitive Types>"
	write(ifileid,"(a)") "<EDF Primitive Exponents>"
	write(ifileid,"(5E20.12)") b_EDF%exp
	write(ifileid,"(a)") "</EDF Primitive Exponents>"
	write(ifileid,"(a)") "<EDF Primitive Coefficients>"
	write(ifileid,"(5E20.12)") CO_EDF
	write(ifileid,"(a)") "</EDF Primitive Coefficients>"
	write(ifileid,"(a)") "</Additional Electron Density Function (EDF)>"
end if

write(ifileid,"(a)") "<Molecular Orbital Occupation Numbers>"
do imo=1,nmo
	if (MOocc(imo)/=0D0) write(ifileid,"(E20.12)") MOocc(imo)
end do
write(ifileid,"(a)") "</Molecular Orbital Occupation Numbers>"

write(ifileid,"(a)") "<Molecular Orbital Energies>"
do imo=1,nmo
	if (MOocc(imo)/=0D0) write(ifileid,"(E20.12)") MOene(imo)
end do
write(ifileid,"(a)") "</Molecular Orbital Energies>"

write(ifileid,"(a)") "<Molecular Orbital Spin Types>"
do imo=1,nmo
	if (MOocc(imo)/=0D0) then
		if (MOtype(imo)==0) write(ifileid,"(a)") " Alpha and Beta"
		if (MOtype(imo)==1) write(ifileid,"(a)") " Alpha"
		if (MOtype(imo)==2) write(ifileid,"(a)") " Beta"
	end if
end do
write(ifileid,"(a)") "</Molecular Orbital Spin Types>"

write(ifileid,"(a)") "<Molecular Orbital Primitive Coefficients>"
i=0
nzeroocc=0
do imo=1,nmo
	if (MOocc(imo)==0D0) cycle
	i=i+1 !Use i instead of imo, this can make MO index contiguous
	write(ifileid,"(a)") "<MO Number>"
	write(ifileid,"(i6)") i
	write(ifileid,"(a)") "</MO Number>"
	write(ifileid,"(4E20.12)") CO(imo,:)
end do
write(ifileid,"(a)") "</Molecular Orbital Primitive Coefficients>"

write(ifileid,"(a)") "<Energy = T + Vne + Vee + Vnn>"
write(ifileid,"(E20.12)") totenergy
write(ifileid,"(a)") "</Energy = T + Vne + Vee + Vnn>"

write(ifileid,"(a)") "<Virial Ratio (-V/T)>"
write(ifileid,"(E20.12)") virialratio
write(ifileid,"(a)") "</Virial Ratio (-V/T)>"

close(ifileid)
if (nzeroocc>0.and.ioutinfo==1) write(*,"(a,i10,a)") " Note: Found",nzeroocc," zero occupied orbitals and have discarded them"
end subroutine



!!!------------------------- Output current wavefunction to Molden input file
subroutine outmolden(outname,ifileid)
use defvar
use util
implicit real*8 (a-h,o-z)
character(len=*) outname
integer ifileid
character symbol

open(ifileid,file=outname,status="replace")
write(ifileid,"(a)") "[Molden Format]"
write(ifileid,"(a)") "[Atoms] AU"
do i=1,ncenter
	write(ifileid,"(a,i7,i4,3f14.7)") a(i)%name,i,a(i)%index,a(i)%x,a(i)%y,a(i)%z
end do
write(ifileid,"(a)") "[GTO]"
do iatm=1,ncenter
	write(ifileid,"(2i6)") iatm,0
	do ish=1,nshell
		if (shcen(ish)==iatm) then
			symbol=shtype2name(shtype(ish))
			call struc2lc(symbol)
			write(ifileid,"(a,i4,' 1.0')") symbol,shcon(ish)
			if (ish==0) then
				istart=0
			else
				istart=sum(shcon(1:ish-1))
			end if
			do ipsh=istart+1,istart+shcon(ish)
				write(ifileid,"(2(1PE16.8))") primshexp(ipsh),primshcoeff(ipsh)
			end do
		end if
	end do
	write(ifileid,*)
end do
write(ifileid,*) 

if (any(shtype==-2).and.any(shtype==-3)) then !Default is 6d10f
	write(ifileid,"('[5D]')") !5d7f
else if (any(shtype==-2)) then
	write(ifileid,"('[5D10F]')") !5d10f
else if (any(shtype==-3)) then
	write(ifileid,"('[7F]')") !=6d7f
end if
if (any(shtype==-4)) write(ifileid,"('[9G]')") !Default is Cartesian G
if (any(shtype==-5)) write(ifileid,"('[11H]')") !Default is Cartesian H
write(ifileid,"(a)") "[MO]"
if (wfntype==0.or.wfntype==2.or.wfntype==3) then !Close shell
! 	write(*,*) nmo
	do imo=1,nmo
		write(ifileid,"('Ene=',f16.8)") MOene(imo)
		write(ifileid,"('Spin= Alpha')")
		write(ifileid,"('Occup=',f10.6)") MOocc(imo)
		do ibas=1,nbasis
			write(ifileid,"(i6,f18.10)") ibas,CObasa(ibas,imo)
		end do
	end do
else !Open shell
	do isep=nmo,1,-1
		if (MOtype(isep)==1) exit
	end do
	do imo=1,isep !Alpha part
		write(ifileid,"('Ene=',f16.8)") MOene(imo)
		write(ifileid,"('Spin= Alpha')")
		write(ifileid,"('Occup=',f10.6)") MOocc(imo)
		do ibas=1,nbasis
			write(ifileid,"(i6,f18.10)") ibas,CObasa(ibas,imo)
		end do
	end do
	do imo=isep+1,nmo !Beta part
		write(ifileid,"('Ene=',f16.8)") MOene(imo)
		write(ifileid,"('Spin= Beta')")
		write(ifileid,"('Occup=',f10.6)") MOocc(imo)
		do ibas=1,nbasis
			write(ifileid,"(i6,f18.10)") ibas,CObasb(ibas,imo-isep)
		end do
	end do
end if

close(ifileid)
end subroutine




!!!--------------- Output current wavefunction to mkl file (old Molekel input file), then orca_2mkl can convert it to .gbw
!The format is exactly identical to the .mkl file produced by orca_2mkl (e.g. orca_2mkl test can generate test.mkl from test.gbw)
subroutine outmkl(outname,ifileid)
use defvar
use util
implicit real*8 (a-h,o-z)
character(len=*) outname
integer ifileid
character symbol,writeformat*20
character*4 irrep(nmo)

open(ifileid,file=outname,status="replace")
write(ifileid,"(a)") "$MKL"
write(ifileid,"(a)") "#"
write(ifileid,"(a)") "# MKL format file produced by Multiwfn"
write(ifileid,"(a)") "#"
write(ifileid,"(a)") "$CHAR_MULT"
write(ifileid,"(2i3)") nint(sum(a(:)%charge)-naelec-nbelec),nint(naelec-nbelec+1)
write(ifileid,"(a)") "$END"
write(ifileid,*)

write(ifileid,"(a)") "$COORD"
do i=1,ncenter
	write(ifileid,"(i4,1x,3f11.6)") a(i)%index,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a
end do
write(ifileid,"(a)") "$END"
write(ifileid,*)

write(ifileid,"(a)") "$BASIS"
do iatm=1,ncenter
	do ish=1,nshell
		if (shcen(ish)==iatm) then
			symbol=shtype2name(shtype(ish))
			write(ifileid,"(i2,1x,a,f4.1)") shtype2nbas(shtype(ish)),symbol,1D0
			if (ish==0) then
				istart=0
			else
				istart=sum(shcon(1:ish-1))
			end if
			do ipsh=istart+1,istart+shcon(ish)
				write(ifileid,"(f19.9,f17.9)") primshexp(ipsh),primshcoeff(ipsh)
			end do
		end if
	end do
	if (iatm/=ncenter) write(ifileid,"(a)") "$$"
end do
write(ifileid,"(a)") "$END"
write(ifileid,*)

nframe=ceiling(nbasis/5D0)
!if (allocated(MOsym)) then
!    irrep=MOsym
!else
    irrep=" a1g" !orca_2mkl convention
!end if
write(ifileid,"(a)") "$COEFF_ALPHA"
do iframe=1,nframe
    if (iframe/=nframe) then
        ncol=5
    else
        ncol=nbasis-(nframe-1)*5
    end if
    ibeg=(iframe-1)*5+1
    iend=(iframe-1)*5+ncol
    write(writeformat,"('(',i1,'(a,1x))')") ncol
    write(ifileid,writeformat) irrep(ibeg:iend)
    write(writeformat,"('(',i1,'(f13.7,1x))')") ncol
    write(ifileid,writeformat) MOene(ibeg:iend)
    write(writeformat,"('(',i1,'(f12.7,1x))')") ncol
	do ibas=1,nbasis
        write(ifileid,writeformat) CObasa(ibas,ibeg:iend)
	end do
end do
write(ifileid,"(a)") " $END"
write(ifileid,*)

write(ifileid,"(a)") "$OCC_ALPHA"
write(ifileid,"(5f12.7)") MOocc(1:nbasis)
write(ifileid,"(a)") " $END"
    
if (wfntype==1.or.wfntype==4) then !Open shell
    write(ifileid,*)
    write(ifileid,"(a)") "$COEFF_BETA"
    do iframe=1,nframe
        if (iframe/=nframe) then
            ncol=5
        else
            ncol=nbasis-(nframe-1)*5
        end if
        ibeg=(iframe-1)*5+1
        iend=(iframe-1)*5+ncol
        write(writeformat,"('(',i1,'(a,1x))')") ncol
        write(ifileid,writeformat) irrep(ibeg+nbasis:iend+nbasis)
        write(writeformat,"('(',i1,'(f13.7,1x))')") ncol
        write(ifileid,writeformat) MOene(ibeg+nbasis:iend+nbasis)
        write(writeformat,"('(',i1,'(f12.7,1x))')") ncol
	    do ibas=1,nbasis
            write(ifileid,writeformat) CObasb(ibas,ibeg:iend)
	    end do
    end do
    write(ifileid,"(a)") " $END"
    write(ifileid,*)

    write(ifileid,"(a)") "$OCC_BETA"
    write(ifileid,"(5f12.7)") MOocc(nbasis+1:nmo)
    write(ifileid,"(a)") " $END"
end if

close(ifileid)
end subroutine



!!!------------------------- Output current wavefunction to .fch file
!informode=1: print prompt message, =0: do not print
subroutine outfch(outname,ifileid,infomode)
use defvar
use util
implicit real*8 (a-h,o-z)
character(len=*) outname
integer ifileid,infomode
open(ifileid,file=outname,status="replace")
write(ifileid,"(a)") "Generated by Multiwfn"
if (wfntype==0.or.wfntype==3) write(ifileid,"(a10,a30,a30)") "SP        ","RB3LYP                        ","                      6-31G(d)"
if (wfntype==1.or.wfntype==4) write(ifileid,"(a10,a30,a30)") "SP        ","UB3LYP                        ","                      6-31G(d)"
if (wfntype==2)               write(ifileid,"(a10,a30,a30)") "SP        ","ROB3LYP                       ","                      6-31G(d)"
write(ifileid,"(A40,3X,A1,5X,I12)") "Number of atoms                         ","I",ncenter
write(ifileid,"(A40,3X,A1,5X,I12)") "Charge                                  ","I",nint(sum(a%charge)-nelec)
write(ifileid,"(A40,3X,A1,5X,I12)") "Multiplicity                            ","I",nint(naelec-nbelec)+1
write(ifileid,"(A40,3X,A1,5X,I12)") "Number of electrons                     ","I",nint(nelec)
write(ifileid,"(A40,3X,A1,5X,I12)") "Number of alpha electrons               ","I",nint(naelec)
write(ifileid,"(A40,3X,A1,5X,I12)") "Number of beta electrons                ","I",nint(nbelec)
write(ifileid,"(A40,3X,A1,5X,I12)") "Number of basis functions               ","I",nbasis
write(ifileid,"(A40,3X,A1,5X,I12)") "Number of independent functions         ","I",nbasis
write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Atomic numbers                          ","I",ncenter
write(ifileid,"(6I12)") a(:)%index
write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Nuclear charges                         ","R",ncenter
write(ifileid,"(5(1PE16.8))") a(:)%charge
write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Current cartesian coordinates           ","R",ncenter*3
write(ifileid,"(5(1PE16.8))") (a(i)%x,a(i)%y,a(i)%z,i=1,ncenter)
!Basis function definition
write(ifileid,"(A40,3X,A1,5X,I12)") "Number of contracted shells             ","I",nshell
write(ifileid,"(A40,3X,A1,5X,I12)") "Number of primitive shells              ","I",nprimshell
write(ifileid,"(A40,3X,A1,5X,I12)") "Highest angular momentum                ","I",maxval(abs(shtype))
write(ifileid,"(A40,3X,A1,5X,I12)") "Largest degree of contraction           ","I",maxval(abs(shcon))
write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Shell types                             ","I",nshell
write(ifileid,"(6I12)") shtype(:)
write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Number of primitives per shell          ","I",nshell
write(ifileid,"(6I12)") shcon(:)
write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Shell to atom map                       ","I",nshell
write(ifileid,"(6I12)") shcen(:)
write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Primitive exponents                     ","R",nprimshell
write(ifileid,"(5(1PE16.8))") primshexp(:)
write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Contraction coefficients                ","R",nprimshell
write(ifileid,"(5(1PE16.8))") primshcoeff(:)
write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Coordinates of each shell               ","R",nshell*3
write(ifileid,"(5(1PE16.8))") (a(shcen(i))%x,a(shcen(i))%y,a(shcen(i))%z,i=1,nshell)
write(ifileid,"(A40,3X,A1,5X,1PE22.15)") "Virial Ratio                            ","R",virialratio
write(ifileid,"(A40,3X,A1,5X,1PE22.15)") "Total Energy                            ","R",totenergy
!Orbital informaiton
write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Alpha Orbital Energies                  ","R",nbasis
write(ifileid,"(5(1PE16.8))") MOene(1:nbasis)
if (wfntype==1.or.wfntype==4) then
	write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Beta Orbital Energies                   ","R",nbasis
	write(ifileid,"(5(1PE16.8))") MOene(nbasis+1:2*nbasis)
end if
write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Alpha MO coefficients                   ","R",nbasis*nbasis
write(ifileid,"(5(1PE16.8))") ((CObasa(ibasis,imo),ibasis=1,nbasis),imo=1,nbasis)
if (wfntype==1.or.wfntype==4) then
	write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Beta MO coefficients                    ","R",nbasis*nbasis
	write(ifileid,"(5(1PE16.8))") ((CObasb(ibasis,imo),ibasis=1,nbasis),imo=1,nbasis)
end if
write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Total SCF Density                       ","R",nbasis*(nbasis+1)/2
write(ifileid,"(5(1PE16.8))") ((Ptot(i,j),j=1,i),i=1,nbasis)
if (wfntype==1.or.wfntype==4) then
	write(ifileid,"(A40,3X,A1,3X,'N=',I12)") "Spin SCF Density                        ","R",nbasis*(nbasis+1)/2
	write(ifileid,"(5(1PE16.8))") ((Palpha(i,j)-Pbeta(i,j),j=1,i),i=1,nbasis)
end if
close(ifileid)
if (infomode==1) then
	if (wfntype==0.or.wfntype==2.or.wfntype==3) then
		write(*,"(a)") " Exporting .fch file finished! The ""Total SCF Density"" field in this file corresponds to density matrix at current level"
	else
		write(*,"(a)") " Exporting .fch file finished! The ""Total SCF Density"" and ""Spin SCF Density"" fields in this file correspond &
		to total density matrix and difference between alpha and beta density matrices at current level, respectively"
	end if
end if
end subroutine


!!!---------- Output current wavefunction to .47 file, which is input file of NBO program
!Notice that although "UPPER" keyword is used here, in fact the matrix recorded in .47 is lower-triangular matrix (i.e. NBO's rule is confusing!)
subroutine out47(outname,ifileid)
use defvar
use util
implicit real*8 (a-h,o-z)
character(len=*) outname
character(80) c80tmp1,c80tmp2
real*8 primshcoefftmp(nprimshell)
real*8,allocatable :: halfmat(:)
real*8,external :: normgau
integer bastype2NBO(50),nptr(nshell)
integer ifileid
bastype2NBO(1 )=1   !s
bastype2NBO(2 )=101 !x
bastype2NBO(3 )=102 !y
bastype2NBO(4 )=103 !z
bastype2NBO(5 )=201 !xx
bastype2NBO(8 )=202 !xy
bastype2NBO(9 )=203 !xz
bastype2NBO(6 )=204 !yy
bastype2NBO(10)=205 !yz
bastype2NBO(7 )=206 !zz
bastype2NBO(11)=301 !xxx
bastype2NBO(14)=302 !xxy
bastype2NBO(15)=303 !xxz
bastype2NBO(17)=304 !xyy
bastype2NBO(20)=305 !xyz
bastype2NBO(18)=306 !xzz
bastype2NBO(12)=307 !yyy
bastype2NBO(16)=308 !yyz
bastype2NBO(19)=309 !yzz
bastype2NBO(13)=310 !zzz
!Below g sequence comes from line 47384 in NBO_5 src
bastype2NBO(35)=401 !XXXX
bastype2NBO(34)=402 !XXXY
bastype2NBO(33)=403 !XXXZ
bastype2NBO(32)=404 !XXYY
bastype2NBO(31)=405 !XXYZ
bastype2NBO(30)=406 !XXZZ
bastype2NBO(29)=407 !XYYY
bastype2NBO(28)=408 !XYYZ
bastype2NBO(27)=409 !XYZZ
bastype2NBO(26)=410 !XZZZ
bastype2NBO(25)=411 !YYYY
bastype2NBO(24)=412 !YYYZ
bastype2NBO(23)=413 !YYZZ
bastype2NBO(22)=414 !YZZZ
bastype2NBO(21)=415 !ZZZZ

if (any(shtype<0)) then
	write(*,*) "Error: This function only works when all basis functions are Cartesian type!"
	write(*,"(a)") " Hint: If you set ""iloadasCart"" in settings.ini to 1, then all spherical harmonic type of &
	basis functions will be converted to Cartesian type when loading input file, and then this function will be usable"
	write(*,*) "Press ENTER button to return"
	read(*,*)
	return
end if

open(ifileid,file=outname,status="replace")
write(c80tmp1,*) ncenter
write(c80tmp2,*) nbasis
if (wfntype==0.or.wfntype==3) then !Closed-shell
	write(10,"(' $GENNBO NATOMS=',a,' NBAS=',a,' UPPER BODM $END')") trim(adjustl(c80tmp1)),trim(adjustl(c80tmp2))
else !Open-shell
	write(10,"(' $GENNBO NATOMS=',a,' NBAS=',a,' UPPER BODM OPEN $END')") trim(adjustl(c80tmp1)),trim(adjustl(c80tmp2))
end if
write(10,*) "$NBO $END"
write(10,*) "$COORD"
write(10,*) "Generated by Multiwfn"
do iatm=1,ncenter
	write(10,"(2i6,3f12.6)") a(iatm)%index,int(a(iatm)%charge),a(iatm)%x*b2a,a(iatm)%y*b2a,a(iatm)%z*b2a
end do
write(10,*) "$END"

!Basis function information
write(10,*) "$BASIS"
write(10,"(' CENTER =')")
write(10,"(10i6)") bascen(:)
write(10,"(' LABEL =')")
write(10,"(10i6)") bastype2NBO(bastype(:))
write(10,"(' $END')")

!Shell information
write(10,*) "$CONTRACT"
write(10,"(' NSHELL =',i6)") nshell
write(10,"('   NEXP =',i6)") nprimshell
write(10,"('  NCOMP =')")
write(10,"(10i6)") shtype2nbas(shtype(:))
write(10,"('  NPRIM =')")
write(10,"(10i6)") shcon(:)
nptr(1)=1
do ish=2,nshell
	nptr(ish)=nptr(ish-1)+shcon(ish-1)
end do
write(10,"('   NPTR =')")
write(10,"(10i6)") nptr(:)
write(10,"('    EXP =')")
write(10,"(4E16.7)") primshexp(:)
!In standard .47 and .37 file, the shell contraction coefficients include normalization coefficients
!For d, f, g, the normalization coefficients are for XX/YY/ZZ, XXX/YYY/ZZZ, XXXX/YYYY/ZZZZ, respectively
do iang=0,maxval(abs(shtype))
	primshcoefftmp=0
	if (iang==0) then
		write(10,"('     CS =')")
		do ish=1,nshell
			if (shtype(ish)==0) then
				do icon=1,shcon(ish)
					iprimshnow=nptr(ish)+icon-1
					primshcoefftmp(iprimshnow)=primshcoeff(iprimshnow)*normgau(1,primshexp(iprimshnow))
				end do
			end if
		end do
	else if (iang==1) then
		write(10,"('     CP =')")
		do ish=1,nshell
			if (shtype(ish)==1) then
				do icon=1,shcon(ish)
					iprimshnow=nptr(ish)+icon-1
					primshcoefftmp(iprimshnow)=primshcoeff(iprimshnow)*normgau(2,primshexp(iprimshnow))
				end do
			end if
		end do
	else if (iang==2) then
		write(10,"('     CD =')")
		do ish=1,nshell
			if (shtype(ish)==2) then
				do icon=1,shcon(ish)
					iprimshnow=nptr(ish)+icon-1
					primshcoefftmp(iprimshnow)=primshcoeff(iprimshnow)*normgau(5,primshexp(iprimshnow))
				end do
			end if
		end do
	else if (iang==3) then
		write(10,"('     CF =')")
		do ish=1,nshell
			if (shtype(ish)==3) then
				do icon=1,shcon(ish)
					iprimshnow=nptr(ish)+icon-1
					primshcoefftmp(iprimshnow)=primshcoeff(iprimshnow)*normgau(11,primshexp(iprimshnow))
				end do
			end if
		end do
	else if (iang==4) then
		write(10,"('     CG =')")
		do ish=1,nshell
			if (shtype(ish)==4) then
				do icon=1,shcon(ish)
					iprimshnow=nptr(ish)+icon-1
					primshcoefftmp(iprimshnow)=primshcoeff(iprimshnow)*normgau(21,primshexp(iprimshnow))
				end do
			end if
		end do
	end if
	write(10,"(4E16.7)") primshcoefftmp(:)
end do
write(10,"(' $END')")

allocate(halfmat(nbasis*(nbasis+1)/2))
!Overlap matrix
write(10,"(' $OVERLAP')")
call mat2arr(Sbas,halfmat,2)
write(10,"(5E15.7)") halfmat(:)
write(10,"(' $END')")
!Density matrix
write(10,"(' $DENSITY')")
if (wfntype==0.or.wfntype==3) then !Closed-shell
	call mat2arr(Ptot,halfmat,2)
	write(10,"(5E15.7)") halfmat(:)
else !Open-shell
	call mat2arr(Palpha,halfmat,2)
	write(10,"(5E15.7)") halfmat(:)
	call mat2arr(Pbeta,halfmat,2)
	write(10,"(5E15.7)") halfmat(:)
end if
write(10,"(' $END')")

!LCAOMO matrix. Note that if "iloadasCart" is set to 1, when loading .molden and .fch where spherical harmonic basis functions are presented, &
!they will be converted to Cartesian type and retain this status. In this case some highest MOs have zero coefficients, but NBO can still work normally
! if (nmo==nbasis.or.nmo==2*nbasis) then !The input file must only contain Cartesian basis functions
write(10,"(' $LCAOMO')")
if (wfntype==0.or.wfntype==2.or.wfntype==3) then !R or RO
	do imo=1,nmo
		write(10,"(5E15.7)") CObasa(:,imo)
	end do
	if (wfntype==2) then !RO, output orbitals twice
		do imo=1,nmo
			write(10,"(5E15.7)") CObasa(:,imo)
		end do
	end if
else !U
	do imo=1,nbasis
		write(10,"(5E15.7)") CObasa(:,imo)
	end do
	do imo=1,nbasis
		write(10,"(5E15.7)") CObasb(:,imo)
	end do
end if
write(10,"(' $END')")

!Dipole matrix
if (allocated(Dbas)) then
	write(10,"(' $DIPOLE')")
	call mat2arr(Dbas(1,:,:),halfmat,2)
	write(10,"(5E15.7)") halfmat(:)*(-b2a) !Must be converted from Bohr to Angstrom
	call mat2arr(Dbas(2,:,:),halfmat,2)
	write(10,"(5E15.7)") halfmat(:)*(-b2a)
	call mat2arr(Dbas(3,:,:),halfmat,2)
	write(10,"(5E15.7)") halfmat(:)*(-b2a)
	write(10,"(' $END')")
else
	write(*,"(a)") " Note: If you want to write dipole moment matrix into .47 file so that you can use ""DIPOLE"" keyword in NBO, &
	you should set ""igenDbas"" in settings.ini to 1 and reload input file, so that dipole moment matrix can be generated."
	write(*,*)
end if

close(ifileid)
write(*,*) "Exporting .47 file finished!"
end subroutine










!=======================================================================
!=======================================================================
!!!!!!!! Load parameters in settings.ini when boot up multiwfn !!!!!!!!!
!=======================================================================
!=======================================================================
subroutine loadsetting
use defvar
use util
implicit real*8 (a-h,o-z)
character c80tmp*80,c200tmp*200,settingpath*80
!Set default color of atomic spheres
atm3Dclr(:,1)=0.85D0
atm3Dclr(:,2)=0.6D0
atm3Dclr(:,3)=0.5D0
atm3Dclr(0,:)=(/0.3D0,  0.8D0,  0.85D0 /) !Bq
atm3Dclr(1,:)=(/0.95D0, 0.95D0, 0.95D0/) !H
atm3Dclr(5,:)=(/0.95D0, 0.7D0,  0.7D0 /) !B
atm3Dclr(6,:)=(/0.85D0, 0.85D0, 0.55D0/) !C
atm3Dclr(7,:)=(/0.5D0,  0.5D0,  1.0D0 /) !N
atm3Dclr(8,:)=(/1.0D0,  0.2D0,  0.2D0 /) !O
atm3Dclr(9,:)=(/0.6D0,  0.9D0,  0.9D0 /) !F
atm3Dclr(15,:)=(/0.9D0, 0.4D0,  0.0D0 /) !P
atm3Dclr(16,:)=(/0.9D0, 0.7D0,  0.1D0 /) !S
atm3Dclr(17,:)=(/0.1D0, 0.9D0,  0.1D0 /) !Cl

inquire(file="settings.ini",exist=alive)
if (alive==.true.) then
	settingpath="settings.ini"
else if (alive==.false.) then
	call getenv("Multiwfnpath",c80tmp)
	if (isys==1) then
		settingpath=trim(c80tmp)//"\settings.ini"
	else if (isys==2) then
		settingpath=trim(c80tmp)//"/settings.ini"
	end if
	inquire(file=settingpath,exist=alive)
	if (alive==.false.) then
		write(*,"(a)") " Warning: ""settings.ini"" was found neither in current folder nor in the path defined by ""Multiwfnpath"" &
		environment variable. Now using default settings instead"
		write(*,*)
		return
	end if
end if

open(20,file=settingpath,status="old")
! Below are the parameters can affect calculation results
call loclabel(20,'iuserfunc=',ifound)
if (ifound==1) read(20,*) c80tmp,iuserfunc
call loclabel(20,'refxyz=',ifound)
if (ifound==1) read(20,*) c80tmp,refx,refy,refz
call loclabel(20,'iDFTxcsel=',ifound)
if (ifound==1) read(20,*) c80tmp,iDFTxcsel
call loclabel(20,'iKEDsel=',ifound)
if (ifound==1) read(20,*) c80tmp,iKEDsel
call loclabel(20,'uservar=',ifound)
if (ifound==1) read(20,*) c80tmp,uservar
call loclabel(20,'paircorrtype=',ifound)
if (ifound==1) read(20,*) c80tmp,paircorrtype
call loclabel(20,'pairfunctype=',ifound)
if (ifound==1) read(20,*) c80tmp,pairfunctype
call loclabel(20,'iautointgrid=',ifound)
if (ifound==1) read(20,*) c80tmp,iautointgrid
call loclabel(20,'radpot=',ifound)
if (ifound==1) read(20,*) c80tmp,radpot
call loclabel(20,'sphpot=',ifound)
if (ifound==1) read(20,*) c80tmp,sphpot
call loclabel(20,'radcut=',ifound)
if (ifound==1) read(20,*) c80tmp,radcut
call loclabel(20,'expcutoff=',ifound)
if (ifound==1) read(20,*) c80tmp,expcutoff
call loclabel(20,'espprecutoff=',ifound)
if (ifound==1) read(20,*) c80tmp,espprecutoff
call loclabel(20,'RDG_maxrho=',ifound)
if (ifound==1) read(20,*) c80tmp,RDG_maxrho
call loclabel(20,'RDGprodens_maxrho=',ifound)
if (ifound==1) read(20,*) c80tmp,RDGprodens_maxrho
call loclabel(20,'ELF_addminimal=',ifound)
if (ifound==1) read(20,*) c80tmp,ELF_addminimal
call loclabel(20,'ELFLOL_type=',ifound)
if (ifound==1) read(20,*) c80tmp,ELFLOL_type
call loclabel(20,'iALIEdecomp=',ifound)
if (ifound==1) read(20,*) c80tmp,iALIEdecomp
call loclabel(20,'srcfuncmode=',ifound)
if (ifound==1) read(20,*) c80tmp,srcfuncmode
call loclabel(20,'atomdenscut=',ifound)
if (ifound==1) read(20,*) c80tmp,atomdenscut
call loclabel(20,'aug1D=',ifound)
if (ifound==1) read(20,*) c80tmp,aug1D
call loclabel(20,'aug2D=',ifound)
if (ifound==1) read(20,*) c80tmp,aug2D
call loclabel(20,'aug3D=',ifound)
if (ifound==1) read(20,*) c80tmp,aug3D
call loclabel(20,'num1Dpoints=',ifound)
if (ifound==1) read(20,*) c80tmp,num1Dpoints
call loclabel(20,'nprevorbgrid=',ifound)
if (ifound==1) read(20,*) c80tmp,nprevorbgrid
call loclabel(20,'bndordthres=',ifound)
if (ifound==1) read(20,*) c80tmp,bndordthres
call loclabel(20,'compthres=',ifound)
if (ifound==1) read(20,*) c80tmp,compthres
call loclabel(20,'compthresCDA=',ifound)
if (ifound==1) read(20,*) c80tmp,compthresCDA
call loclabel(20,'ispheratm=',ifound)
if (ifound==1) read(20,*) c80tmp,ispheratm
call loclabel(20,'laplfac=',ifound)
if (ifound==1) read(20,*) c80tmp,laplfac
call loclabel(20,'ipolarpara=',ifound)
if (ifound==1) read(20,*) c80tmp,ipolarpara
call loclabel(20,'ishowchgtrans=',ifound)
if (ifound==1) read(20,*) c80tmp,ishowchgtrans
call loclabel(20,'SpherIVgroup=',ifound)
if (ifound==1) read(20,*) c80tmp,SpherIVgroup
call loclabel(20,'MCvolmethod=',ifound)
if (ifound==1) read(20,*) c80tmp,MCvolmethod
call loclabel(20,'readEDF=',ifound)
if (ifound==1) read(20,*) c80tmp,readEDF
call loclabel(20,'isupplyEDF=',ifound)
if (ifound==1) read(20,*) c80tmp,isupplyEDF
call loclabel(20,'idelvirorb=',ifound)
if (ifound==1) read(20,*) c80tmp,idelvirorb
call loclabel(20,'ifchprog=',ifound)
if (ifound==1) read(20,*) c80tmp,ifchprog
call loclabel(20,'ishowptESP=',ifound)
if (ifound==1) read(20,*) c80tmp,ishowptESP
call loclabel(20,'imolsurparmode=',ifound)
if (ifound==1) read(20,*) c80tmp,imolsurparmode
call loclabel(20,'steric_addminimal=',ifound)
if (ifound==1) read(20,*) c80tmp,steric_addminimal
call loclabel(20,'steric_potcutrho=',ifound)
if (ifound==1) read(20,*) c80tmp,steric_potcutrho
call loclabel(20,'steric_potcons=',ifound)
if (ifound==1) read(20,*) c80tmp,steric_potcons
call loclabel(20,'NICSnptlim=',ifound)
if (ifound==1) read(20,*) c80tmp,NICSnptlim
call loclabel(20,'iplaneextdata=',ifound)
if (ifound==1) read(20,*) c80tmp,iplaneextdata
call loclabel(20,'igenP=',ifound)
if (ifound==1) read(20,*) c80tmp,igenP
call loclabel(20,'igenDbas=',ifound)
if (ifound==1) read(20,*) c80tmp,igenDbas
call loclabel(20,'igenMagbas=',ifound)
if (ifound==1) read(20,*) c80tmp,igenMagbas
call loclabel(20,'iloadasCart=',ifound)
if (ifound==1) read(20,*) c80tmp,iloadasCart
call loclabel(20,'maxloadexc=',ifound)
if (ifound==1) read(20,*) c80tmp,maxloadexc
call loclabel(20,'iprintLMOorder=',ifound)
if (ifound==1) read(20,*) c80tmp,iprintLMOorder

!Below are the parameters involved in plotting
call loclabel(20,'plotwinsize3D=',ifound)
if (ifound==1) read(20,*) c80tmp,plotwinsize3D
call loclabel(20,'imodlayout=',ifound)
if (ifound==1) read(20,*) c80tmp,imodlayout
call loclabel(20,'symbolsize=',ifound)
if (ifound==1) read(20,*) c80tmp,symbolsize
call loclabel(20,'pleatmlabsize=',ifound)
if (ifound==1) read(20,*) c80tmp,pleatmlabsize
call loclabel(20,'disshowlabel=',ifound)
if (ifound==1) read(20,*) c80tmp,disshowlabel
call loclabel(20,'iatom_on_plane_far=',ifound)
if (ifound==1) read(20,*) c80tmp,iatom_on_plane_far
call loclabel(20,'iatmlabtype=',ifound)
if (ifound==1) read(20,*) c80tmp,iatmlabtype
call loclabel(20,'iatmlabtype3D=',ifound)
if (ifound==1) read(20,*) c80tmp,iatmlabtype3D
call loclabel(20,'graphformat=',ifound)
if (ifound==1) read(20,*) c80tmp,graphformat
call loclabel(20,'graph1Dsize=',ifound)
if (ifound==1) read(20,*) c80tmp,graph1Dwidth,graph1Dheight
call loclabel(20,'graph2Dsize=',ifound)
if (ifound==1) read(20,*) c80tmp,graph2Dwidth,graph2Dheight
call loclabel(20,'graph3Dsize=',ifound)
if (ifound==1) read(20,*) c80tmp,graph3Dwidth,graph3Dheight
call loclabel(20,'numdigxyz=',ifound)
if (ifound==1) read(20,*) c80tmp,numdigx,numdigy,numdigz
call loclabel(20,'numdiglinexy=',ifound)
if (ifound==1) read(20,*) c80tmp,numdiglinex,numdigliney
call loclabel(20,'numdigctr=',ifound)
if (ifound==1) read(20,*) c80tmp,numdigctr
call loclabel(20,'fillcoloritpxy=',ifound)
if (ifound==1) read(20,*) c80tmp,fillcoloritpx,fillcoloritpy
call loclabel(20,'itransparent=',ifound)
if (ifound==1) read(20,*) c80tmp,itransparent
call loclabel(20,'isurfstyle=',ifound)
if (ifound==1) read(20,*) c80tmp,isurfstyle
call loclabel(20,'bondRGB=',ifound)
if (ifound==1) read(20,*) c80tmp,bondclrR,bondclrG,bondclrB
call loclabel(20,'atmlabRGB=',ifound)
if (ifound==1) read(20,*) c80tmp,atmlabclrR,atmlabclrG,atmlabclrB
call loclabel(20,'CP_RGB=',ifound)
if (ifound==1) read(20,*) c80tmp,CP3n3RGB,CP3n1RGB,CP3p1RGB,CP3p3RGB
call loclabel(20,'atmcolorfile=',ifound) !Set atom 3D color either according to external file or default setting
if (ifound==1) then
	read(20,*) c80tmp,c200tmp
	inquire(file=c200tmp,exist=alive)
	if (index(c200tmp,"none")==0.and.alive) then
		write(*,"(' Note: Loading atom color settings from ',a)") trim(c200tmp)
		open(21,file=c200tmp,status="old")
		do iele=0,nelesupp
			read(21,*) inouse,atm3Dclr(iele,:)
		end do
		close(21)
	end if
end if
!Below are parameters about system
call loclabel(20,'nthreads=',ifound)
if (ifound==1) read(20,*) c80tmp,nthreads
call loclabel(20,'ompstacksize=',ifound)
if (ifound==1) read(20,*) c80tmp,ompstacksize
call loclabel(20,'gaupath=',ifound)
if (ifound==1) read(20,*) c200tmp,gaupath
call loclabel(20,'cubegenpath=',ifound)
if (ifound==1) read(20,*) c200tmp,cubegenpath
call loclabel(20,'cubegendenstype=',ifound)
if (ifound==1) read(20,*) c200tmp,cubegendenstype
call loclabel(20,'formchkpath=',ifound)
if (ifound==1) read(20,*) c200tmp,formchkpath
call loclabel(20,'orca_2mklpath=',ifound)
if (ifound==1) read(20,*) c200tmp,orca_2mklpath
call loclabel(20,'isilent=',ifound)
if (ifound==1) read(20,*) c80tmp,isilent
call loclabel(20,'outmedinfo=',ifound)
if (ifound==1) read(20,*) c80tmp,outmedinfo
call loclabel(20,'iwfntmptype=',ifound)
if (ifound==1) read(20,*) c80tmp,iwfntmptype
call loclabel(20,'ispecial=',ifound)
if (ifound==1) read(20,*) c80tmp,ispecial
!The last opened file name
call loclabel(20,'lastfile=',ifound)
if (ifound==1) read(20,"(10x,a)") lastfile
close(20)
end subroutine