

!=========================================================================================
!=========================================================================================
!============= Loading opened Gaussian+NBO or GENNBO output file (fileid=10) =============
!=========================================================================================
!=========================================================================================
!Usual using steps:
!use NAOmod
!open(10,file=filename,status="old")
!call checkNPA(ifound);if (ifound==0) return
!call loadNAOinfo
!call loadNAOatminfo  !optional
!
!call checkDMNAO(ifound);if (ifound==0) return
!call loadDMNAO
!
!call checkNAOMO(ifound);if (ifound==0) return
!call loadNAOMO
!
!call checkAONAO(ifound);if (ifound==0) return
!call loadAONAO
!close(10)


!------ Check if natural population analysis information is presented
!ifound=0: NPA information is not found   ifound=1: NPA information is found
subroutine checkNPA(ifound)
use NAOmod
use util
integer ifound
call loclabel(10,"NATURAL POPULATIONS",ifound)
if (ifound==0) then
    close(10)
	write(*,"(a)") " Error: Cannot found natural population analysis information in the input file!"
    write(*,*) "Press ENTER button to return"
    read(*,*)
    return
end if
end subroutine



!------ Load detailed information about NAOs
subroutine loadNAOinfo
use NAOmod
use util
implicit real*8 (a-h,o-z)
character :: c80tmp*80,c80tmp2*80

!Set iopshNAO
call loclabel(10,"*******         Alpha spin orbitals         *******",iopshNAO,1)
if (iopshNAO==0) write(*,*) "This is a closed shell calculation"
if (iopshNAO==1) write(*,*) "This is an open shell calculation"

!Set numNAO and numNAOcen
call loclabel(10,"NATURAL POPULATIONS")
read(10,*);read(10,*);read(10,*);read(10,*)
ilastspc=0
do while(.true.) !Find how many centers and how many NAOs. We need to carefully check where is ending
	read(10,"(a)") c80tmp
	if (c80tmp==' '.or.index(c80tmp,"low occupancy")/=0.or.index(c80tmp,"Population inversion found")/=0.or.index(c80tmp,"effective core potential")/=0) then
		if (ilastspc==1) then
			numNAOcen=iatm
			numNAO=iNAO
			exit
		end if
		ilastspc=1 !last line is space
	else
		read(c80tmp,*) iNAO,c80tmp2,iatm
		ilastspc=0
	end if
end do
write(*,"(' The number of atoms:',i10)") numNAOcen
write(*,"(' The number of NAOs: ',i10)") numNAO

call dealloNAONBO(1)
allocate(NAOinit(numNAOcen),NAOend(numNAOcen),NAOcen(numNAO),NAOcenname(numNAO))
allocate(NAOset(numNAO,0:2),NAOtype(numNAO),NAOshell(numNAO),NAOocc(numNAO,0:2),NAOene(numNAO,0:2))

!Load initial and ending index of NAOs corresponding to various atoms
call loclabel(10,"NATURAL POPULATIONS")
read(10,*);read(10,*);read(10,*);read(10,*)
ilastspc=1
do while(.true.)
	read(10,"(a)") c80tmp
	if (c80tmp/=' ') then
		read(c80tmp,*) iNAO,c80tmp2,iatm
		NAOcen(iNAO)=iatm
		if (ilastspc==1) NAOinit(iatm)=iNAO
		ilastspc=0
	else
		NAOend(iatm)=iNAO
		if (iatm==numNAOcen) exit
		ilastspc=1
	end if
end do

!do i=1,numNAOcen
!    write(*,*) i,NAOinit(i),NAOend(i)
!end do

!Load NAO information deriving from total density matrix
call loclabel(10,"NATURAL POPULATIONS")
read(10,*);read(10,*);read(10,*);read(10,*)
ispin=0
do iatm=1,numNAOcen
	do iNAO=NAOinit(iatm),NAOend(iatm)
		read(10,"(a)") c80tmp
		if (index(c80tmp,"Cor")/=0) then
			NAOset(iNAO,ispin)="Cor"
		else if (index(c80tmp,"Val")/=0) then
			NAOset(iNAO,ispin)="Val"
		else if (index(c80tmp,"Ryd")/=0) then
			NAOset(iNAO,ispin)="Ryd"
		end if
        !This code is even compatible with complicate case:   63   Th  1  f(0)   Val( 5f)     0.08546       0.08559
		read(c80tmp,*) c80tmp2,NAOcenname(iNAO),c80tmp2,NAOtype(iNAO)
        !For total electron part, load orbital energies for closed shell case, while for open shell case the NAO energies will be loaded later
        i2=index(c80tmp,')',back=.true.)
        if (iopshNAO==0) then
            read(c80tmp(i2+1:),*) NAOocc(iNAO,ispin),NAOene(iNAO,ispin)
        else
		    read(c80tmp(i2+1:),*) NAOocc(iNAO,ispin)
            NAOene(iNAO,ispin)=0
        end if
		NAOshell(iNAO)=c80tmp2(1:2)
        !mimic output of NPA output of NBO code
        !write(*,"(i6,a5,i5,2x,a7,a3,'( ',a,')',f12.5,f14.5)") &
        !iNAO,NAOcenname(iNAO),NAOcen(iNAO),NAOtype(iNAO),NAOset(iNAO,ispin),NAOshell(iNAO),NAOocc(iNAO,ispin),NAOene(iNAO,ispin)
	end do
	read(10,*)
end do

if (iopshNAO==1) then
    !Load NAOset, NAOocc and NAOene of alpha spin
    do ispin=1,2
        call loclabel(10,"NATURAL POPULATIONS",ifound,0)
        read(10,*);read(10,*);read(10,*);read(10,*)
        do iatm=1,numNAOcen
	        do iNAO=NAOinit(iatm),NAOend(iatm)
		        read(10,"(a)") c80tmp
		        if (index(c80tmp,"Cor")/=0) then
			        NAOset(iNAO,ispin)="Cor"
		        else if (index(c80tmp,"Val")/=0) then
			        NAOset(iNAO,ispin)="Val"
		        else if (index(c80tmp,"Ryd")/=0) then
			        NAOset(iNAO,ispin)="Ryd"
		        end if
                read(c80tmp(i2+1:),*) NAOocc(iNAO,ispin),NAOene(iNAO,ispin)
                !write(*,"(i6,a5,i5,2x,a7,a3,'( ',a,')',f12.5,f14.5)") &
                !iNAO,NAOcenname(iNAO),NAOcen(iNAO),NAOtype(iNAO),NAOset(iNAO,ispin),NAOshell(iNAO),NAOocc(iNAO,ispin),NAOene(iNAO,ispin)
            end do
	        read(10,*)
        end do
    end do
end if
end subroutine




!------ Load atom information from NAO information part
!ncenter will be set to numNAOcen, element and index property of "a" array will be filled
subroutine loadNAOatminfo
use NAOmod
use util
use defvar
implicit real*8 (a-h,o-z)
character :: c80tmp*80,c80tmp2*80

ncenter=numNAOcen
if (allocated(a)) deallocate(a)
allocate(a(ncenter))

call loclabel(10,"NATURAL POPULATIONS")
read(10,*);read(10,*);read(10,*);read(10,*)
do while(.true.)
	read(10,"(a)") c80tmp
	if (c80tmp/=' ') then
		read(c80tmp,*) inao,c80tmp2,iatm
		do iele=1,nelesupp
			if (c80tmp2(1:2)==ind2name(iele)) then
				a(iatm)%name=c80tmp2(1:2)
				a(iatm)%index=iele
				exit
			end if
			if (iele==nelesupp) write(*,"(a)") " Warning: Detected unrecognizable element name "//c80tmp2(1:2)//" !"
		end do
	else
		if (iatm==ncenter) exit
	end if
end do
end subroutine




!------ Check if DMNAO matrix is presented
!ifound=0: DMNAO is not found   ifound=1: DMNAO is found
subroutine checkDMNAO(ifound)
use NAOmod
use util
integer ifound
call loclabel(10,"NAO density matrix:",ifound)
if (ifound==0) then
    close(10)
	write(*,"(a)") " Error: Cannot found density matrix in NAO basis in the input file! You must use ""DMNAO"" keyword in the NBO"
    write(*,*) "Press ENTER button to return"
    read(*,*)
    return
end if
end subroutine




!------ Load density matrix in NAO basis
subroutine loadDMNAO
use util
use NAOmod
implicit real*8 (a-h,o-z)
character c80tmp*80

call dealloNAONBO(2)
allocate(DMNAO(numNAO,numNAO))
call loclabel(10,"NAO density matrix:")
read(10,*);read(10,*);read(10,*)
read(10,"(a)") c80tmp
nskipcol=index(c80tmp,"- -")
backspace(10);backspace(10);backspace(10);backspace(10)
call readmatgau(10,DMNAO,0,"f8.4 ",nskipcol,8,3) !For open-shell case, the firstly printed density matrix is for alpha
call loclabel(10," Alpha spin orbitals ",iopenshell)
if (iopenshell==1) then
	allocate(DMNAOa(numNAO,numNAO),DMNAOb(numNAO,numNAO))
    DMNAOa=DMNAO
	write(*,*) "This is an open-shell calculation"
	call loclabel(10,"*******         Beta  spin orbitals         *******",ifound)
	call loclabel(10,"NAO density matrix:",ifound,0)
	call readmatgau(10,DMNAOb,0,"f8.4 ",nskipcol,8,3)
	DMNAO=DMNAOa+DMNAOb
end if
!call showmatgau(DMNAO,"Density matrix in NAO basis ",0,"f14.8",6)
end subroutine




!------ Check if NAOMO matrix is presented
!ifound=0: NAOMO is not found   ifound=1: NAOMO is found
subroutine checkNAOMO(ifound)
use NAOmod
use util
integer ifound
call loclabel(10,"MOs in the NAO basis:",ifound)
if (ifound==0) then
    close(10)
	write(*,"(a)") " Error: Cannot found coefficient matrix of NAOs in MOs from the input file, you should use ""NAOMO"" keyword in NBO module"
    write(*,*) "Press ENTER button to return"
	read(*,*)
	return
end if
end subroutine




!------ Load NAOMO matrix
!numorb: The number of actual MOs (equals to NBsUse of Gaussian) 
subroutine loadNAOMO(numorb)
use util
use NAOmod
implicit real*8 (a-h,o-z)
character c80tmp*80
integer numorb

call dealloNAONBO(3)
allocate(NAOMO(numNAO,numorb))

call loclabel(10,"MOs in the NAO basis:")
!Check the columns that should be skipped during matrix reading, then return to title line
read(10,*);read(10,*);read(10,*)
read(10,"(a)") c80tmp
nskipcol=index(c80tmp,"- -")
backspace(10);backspace(10);backspace(10);backspace(10)
call readmatgau(10,NAOMO,0,"f8.4 ",nskipcol,8,3)

if (iopshNAO==1) then !Also load beta part
    allocate(NAOMOb(numNAO,numorb))
    call loclabel(10,"MOs in the NAO basis:",ifound,0)
    call readmatgau(10,NAOMOb,0,"f8.4 ",nskipcol,8,3)
end if

!call showmatgau(NAOMO,"MOs in the NAO basis",0,"f14.8",6)

end subroutine




!------ Check if AONAO matrix is presented
!ifound=0: AONAO is not found   ifound=1: AONAO is found
subroutine checkAONAO(ifound)
use NAOmod
use util
integer ifound
call loclabel(10,"NAOs in the AO basis:",ifound)
if (ifound==0) then
	close(10)
	write(*,"(a)") " Error: Cannot found coefficient matrix of NAOs in AO basis in the input file! &
    You must use ""AONAO"" keyword in NBO program!"
    write(*,*) "Press ENTER button to return"
	read(*,*)
	return
end if
end subroutine




!----- Load AONAO matrix
!numbas: Should be number of basis function before elimination of lineary dependency
!Since NAO orbitals is always generated by total density matrix, NBO only print it once even for open shell system
subroutine loadAONAO(numbas)
use defvar
use util
use NAOmod
implicit real*8 (a-h,o-z)
character c80tmp*80
integer numbas

call dealloNAONBO(4)
allocate(AONAO(numbas,numNAO))
write(*,"(a)") " Loading transformation matrix between original basis and NAO from input file..." 
!Check columns that should be skipped during matrix reading, then return to title line
read(10,*);read(10,*);read(10,*)
read(10,"(a)") c80tmp
nskipcol=index(c80tmp,"- -")
backspace(10);backspace(10);backspace(10);backspace(10)
!Note: AONAO matrix in NBO output may be any number of columns (so that to ensure long data can be fully recorded), 
!so we must try to determine the actual number of rows and then use correct format to load it
!I assume that at least 5 columns and at most 8 columns
read(10,*)
read(10,*)
read(10,"(a)") c80tmp
if8col=index(c80tmp,'8')
if7col=index(c80tmp,'7')
if6col=index(c80tmp,'6')
if5col=index(c80tmp,'5')
backspace(10)
backspace(10)
backspace(10)
if (if8col/=0) then !8 columns
    call readmatgau(10,AONAO,0,"f8.4 ",nskipcol,8,3)
else if (if7col/=0) then !7 columns
	call readmatgau(10,AONAO,0,"f9.4 ",nskipcol,7,3)
else if (if6col/=0) then !6 columns
	call readmatgau(10,AONAO,0,"f10.4",nskipcol,6,3)
else if (if5col/=0) then !5 columns
	call readmatgau(10,AONAO,0,"f11.4",nskipcol,5,3)
end if
end subroutine




!----- Deallocate all arrays involved in NAO/NBO analysis
!itype=0: Deallocate all NAO/NBO related arrays
!itype=1: Deallocate NAO information
!itype=2: Deallocate DMNAO matrix
!itype=3: Deallocate NAOMO matrix
!itype=4: Deallocate AONAO matrix
subroutine dealloNAONBO(itype)
use NAOmod
if (itype==0.or.itype==1) then
    if (allocated(NAOinit   )) deallocate(NAOinit   )
    if (allocated(NAOend    )) deallocate(NAOend    )
    if (allocated(NAOcen    )) deallocate(NAOcen    )
    if (allocated(NAOcenname)) deallocate(NAOcenname)
    if (allocated(NAOset    )) deallocate(NAOset    )
    if (allocated(NAOshell  )) deallocate(NAOshell  )
    if (allocated(NAOtype   )) deallocate(NAOtype   )
    if (allocated(NAOocc    )) deallocate(NAOocc    )
    if (allocated(NAOene    )) deallocate(NAOene    )
end if
if (itype==0.or.itype==2) then
    if (allocated(DMNAO     )) deallocate(DMNAO     )
    if (allocated(DMNAOa    )) deallocate(DMNAOa    )
    if (allocated(DMNAOb    )) deallocate(DMNAOb    )
end if
if (itype==0.or.itype==3) then
    if (allocated(NAOMO     )) deallocate(NAOMO     )
    if (allocated(NAOMOb    )) deallocate(NAOMOb    )
end if
if (itype==0.or.itype==4) then
    if (allocated(AONAO     )) deallocate(AONAO     )
end if
end subroutine