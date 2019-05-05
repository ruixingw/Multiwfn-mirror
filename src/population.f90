!----------- Interface of various population analyses methods
subroutine population_main
use defvar
use util
implicit real*8 (a-h,o-z)
character c2000tmp*2000
character :: MPApath*200=" "

if (ifragcontri==1) then
	write(*,*) "Population analysis function could not be used combining with self-defined fragment"
else
	do while(.true.)
		imodwfnold=imodwfn
		write(*,*)
		write(*,*) "                ============== Population analysis =============="
		write(*,*) "-2 Calculate interaction energy between fragments based on atomic charges"
		if (.not.allocated(frag1)) then
			write(*,*) "-1 Define fragment"
		else
			write(*,"(a,i5)") " -1 Redefine fragment, current number of atoms:",size(frag1)
		end if
		write(*,*) "0 Return"
		write(*,*) "1 Hirshfeld atom population"
		write(*,*) "2 Voronoi deformation density (VDD) atom population"
		!Not available because integration error of below two methods by means of Becke integration are too large
	! 		write(*,*) "3 Integrate electron density in voronoi cell"
	! 		write(*,*) "4 Adjusted method 3 by Rousseau et al."
		if (allocated(CObasa)) then
			write(*,*) "5 Mulliken atom & basis function population analysis"
			write(*,*) "6 Lowdin atom & basis function population analysis"
			write(*,*) "7 Modified Mulliken atom population defined by Ros & Schuit (SCPA)"
			write(*,*) "8 Modified Mulliken atom population defined by Stout & Politzer"
			write(*,*) "9 Modified Mulliken atom population defined by Bickelhaupt"
		end if
		write(*,*) "10 Becke atomic charge with atomic dipole moment correction"
		write(*,*) "11 Atomic dipole corrected Hirshfeld atomic charge (ADCH) (recommended)"
		write(*,*) "12 CHELPG ESP fitting atomic charge"
		write(*,*) "13 Merz-Kollmann (MK) ESP fitting atomic charge"
		write(*,*) "14 AIM atomic charge"
		write(*,*) "15 Hirshfeld-I atom population"
		write(*,*) "16 CM5 atomic charge"
		write(*,*) "17 Electronegativity Equalization Method (EEM) atomic charge"
		write(*,*) "18 Restrained ElectroStatic Potential (RESP) atomic charge"
		read(*,*) ipopsel
		
		if (ipopsel==0) then
			if (allocated(frag1)) deallocate(frag1)
			return
		else if (ipopsel==-2) then
			if (ifiletype/=4) then
				write(*,*) "Error: You must use .chg file for this function!"
				write(*,*) "Press ENTER button to continue"
				read(*,*)
				cycle
			end if
			call coulint_atmchg
		else if (ipopsel==-1) then
			if (allocated(frag1)) then
				write(*,*) "Atoms in current fragment:"
				write(*,"(13i6)") frag1
				write(*,"(a)") " Input 0 to keep unchanged, or redefine fragment, e.g. 1,3-6,8,10-11 means the atoms 1,3,4,5,6,8,10,11 will constitute the fragment"
			else
				write(*,"(a)") " Input atomic indices to define the fragment. e.g. 1,3-6,8,10-11 means the atoms 1,3,4,5,6,8,10,11 will constitute the fragment"
			end if
			read(*,"(a)") c2000tmp
			if (c2000tmp(1:1)/='0') then
				if (allocated(frag1)) deallocate(frag1)
				call str2arr(c2000tmp,nfragatm)
				allocate(frag1(nfragatm))
				call str2arr(c2000tmp,nfragatm,frag1)
				if (any(frag1>ncenter)) then
					write(*,*) "Error: Some atomic indices exceeded valid range! Please define again"
					write(*,*)
					deallocate(frag1)
				end if
				write(*,*) "Done!"
			end if
		else if (ipopsel==1) then
			write(*,*) "Citation: Theor. Chim. Acta. (Berl), 44, 129-138 (1977)"
			call spacecharge(1)
		else if (ipopsel==2) then
			write(*,*) "Citation: Organomet., 15, 2923-2931"
			write(*,*) "Citation: J. Comput. Chem., 25, 189-210 (2004)"
			call spacecharge(2)
		else if (ipopsel==3) then
			call spacecharge(3)
		else if (ipopsel==4) then
			write(*,*) "Citation: J. Mol. Struct.(Theochem), 538, 235-238 (2001)"
			call spacecharge(4)
		else if (ipopsel==5) then
			do while(.true.)
				write(*,*)
				write(*,*) "              ---------- Mulliken population analysis ----------"
				if (MPApath==" ") then
					write(*,*) "-1 Choose output destination, current: Screen"
				else
					write(*,"(a)") " -1 Choose output destination, current: "//trim(MPApath)
				end if
				write(*,*) "0 Return"
				write(*,*) "1 Output Mulliken population and atomic charges"
				write(*,*) "2 Output gross atomic population matrix and decompose it"
				write(*,*) "3 Output gross basis function population matrix and decompose it"
				write(*,*) "4 Output orbital contributions to atomic populations to atmpopdcp.txt"
				read(*,*) ipopsel2
				if (ipopsel2==0) then
					exit
				else if (ipopsel2==-1) then
					write(*,*) "Input the path for printing population analysis result, e.g. C:\ACG.txt"
					write(*,*) "Note: If press ENTER button directly, result will be printed on screen"
					read(*,"(a)") MPApath
				else
					call MPA(ipopsel2,MPApath)
				end if
			end do
		else if (ipopsel==6) then
			write(*,*) "Performing Lowdin orthogonalization, please wait..."
			call symmortho
			write(*,*) "Input the path for printing population analysis result, e.g. C:\ACG.txt"
			write(*,*) "Note: If press ENTER button directly, result will be printed on screen"
			read(*,"(a)") MPApath
			call MPA(1,MPApath)
			call dealloall
			call readinfile(firstfilename,1) !Current wavefunction has been altered, recover the initial state
		else if (ipopsel==7) then
			call MMPA(1)
		else if (ipopsel==8) then
			call MMPA(2)
		else if (ipopsel==9) then
			call Bickelhaupt
		else if (ipopsel==10) then
			call spacecharge(5)
		else if (ipopsel==11) then
			write(*,"(a)") " Citation of ADCH: Tian Lu, Feiwu Chen, Atomic dipole moment corrected Hirshfeld population method, J. Theor. Comput. Chem., 11, 163 (2011)"
			write(*,*)
			call spacecharge(6)
		else if (ipopsel==12) then
			call fitESP(2)
		else if (ipopsel==13) then
			call fitESP(1)
		else if (ipopsel==14) then
			write(*,"(a)") " NOTE: AIM charges cannot be calculated in present module but can be calculated in basin analysis module, &
			please check the example given in Section 4.17.1 of the manual on how to do this"
		else if (ipopsel==15) then
			if (iautointgrid==1) then
				radpot=30
				sphpot=170
				if (any(a%index>18)) radpot=40
				if (any(a%index>36)) radpot=50
				if (any(a%index>54)) radpot=60
			end if
			call Hirshfeld_I_wrapper(1)
		else if (ipopsel==16) then
			call spacecharge(7)
		else if (ipopsel==17) then
			call EEM
		else if (ipopsel==18) then
			call RESP
		end if
		if (imodwfnold==1.and.(ipopsel==1.or.ipopsel==2.or.ipopsel==6.or.ipopsel==11)) then !1,2,6,11 are the methods need to reload the initial wavefunction
			write(*,"(a)") " Note: The wavefunction file has been reloaded, your previous modifications on occupation number will be ignored"
		end if
	end do
end if
end subroutine


!!---------- Calculate Coulomb interaction between two fragment based on atomic charges in .chg file
subroutine coulint_atmchg
use defvar
use util
character c2000tmp*2000
do while(.true.)
	write(*,*) "Input atom list for fragment 1, e.g. 1,4,6-9"
	write(*,*) "Input 0 can exit"
	read(*,"(a)") c2000tmp
	if (c2000tmp(1:1)=='0') exit
	call str2arr(c2000tmp,nfrag1)
	if (allocated(frag1)) deallocate(frag1)
	allocate(frag1(nfrag1))
	call str2arr(c2000tmp,nfrag1,frag1)
	write(*,*) "Input atom list for fragment 2, e.g. 1,4,6-9"
	read(*,"(a)") c2000tmp
	call str2arr(c2000tmp,nfrag2)
	if (allocated(frag2)) deallocate(frag2)
	allocate(frag2(nfrag2))
	call str2arr(c2000tmp,nfrag2,frag2)
	eleint=0
	do iatmidx=1,nfrag1
		iatm=frag1(iatmidx)
		do jatmidx=1,nfrag2
			jatm=frag2(jatmidx)
			eleint=eleint+a(iatm)%charge*a(jatm)%charge/distmat(iatm,jatm)
		end do
	end do
	write(*,"(' Electrostatic interaction energy:',f12.2,' kJ/mol',f12.2,' kcal/mol',/)") eleint*au2kJ,eleint*au2kcal
end do
end subroutine



!!---------- Modified Mulliken population analysis defined by Bickelhaupt
subroutine Bickelhaupt
use defvar
use util
implicit real*8 (a-h,o-z)
character selectyn,chgfilename*200
real*8 spinpop(ncenter)
real*8,target :: atmeletot(ncenter),atmelea(ncenter)
real*8,pointer :: tmpmat(:,:),tmpele(:)
do itime=1,2
	if (itime==1) then
		tmpele=>atmeletot
		tmpmat=>Ptot
	else if (itime==2) then
		tmpele=>atmelea
		tmpmat=>Palpha
	end if
	tmpele=0D0
	do i=1,ncenter
		do j=basstart(i),basend(i)
			cross=0D0
			do k=1,nbasis !This method equalvalent to use diagonal element of density matrix to partition nondiagonal element of P*S matrix
				if (k/=j) then
					if (tmpmat(j,j)+tmpmat(k,k)/=0D0) then
						cross=cross+tmpmat(j,j)/(tmpmat(j,j)+tmpmat(k,k))*tmpmat(j,k)*Sbas(j,k)
					else
						cross=cross+tmpmat(j,k)*Sbas(j,k)/2D0  !Use equivalent partition, when denominator is zero
					end if
				end if
			end do
			tmpele(i)=tmpele(i)+tmpmat(j,j)+2*cross !Plus electrons localized in basis and partitioned cross term
		end do
	end do
	if (wfntype==0.or.wfntype==3) exit
end do

if (wfntype==0.or.wfntype==3) then
	do iatm=1,ncenter
		write(*,"(' Atom',i6,'(',a2,')','  Population:',f10.5,'  Atomic charge:',f10.5)") iatm,a(iatm)%name,atmeletot(iatm),a(iatm)%charge-atmeletot(iatm)
	end do
	write(*,"(' Total net charge:',f10.5)") sum(a(:)%charge)-sum(atmeletot(:))
else if (wfntype==1.or.wfntype==2.or.wfntype==4) then
	write(*,*) "    Atom      Alpha pop.   Beta pop.    Spin pop.     Atomic charge"
	totbetapop=0D0
	do iatm=1,ncenter
		betapop=atmeletot(iatm)-atmelea(iatm)
		totbetapop=totbetapop+betapop
		spinpop(iatm)=atmelea(iatm)-betapop
		write(*,"(i6,'(',a2,')',3f13.5,f16.5)") iatm,a(iatm)%name,atmelea(iatm),betapop,spinpop(iatm),a(iatm)%charge-atmeletot(iatm)
	end do
	write(*,"(' Total net charge:',f10.5,'      Total spin electrons:',f10.5)") sum(a(:)%charge)-sum(atmeletot(:)),sum(atmelea(:))-totbetapop
end if

!Show fragment information
if (allocated(frag1)) then
	write(*,"(/,' Fragment charge:',f12.6)") sum(a(frag1)%charge-atmeletot(frag1))
	if (wfntype==1.or.wfntype==2.or.wfntype==4) write(*,"(' Fragment spin population:',f12.6)") sum(spinpop(frag1))
end if

call path2filename(firstfilename,chgfilename)
write(*,*)
write(*,"(a)") " If output atom with charges to "//trim(chgfilename)//".chg in current folder? (y/n)"
read(*,*) selectyn
if (selectyn=="y".or.selectyn=="Y") then
	open(10,file=trim(chgfilename)//".chg",status="replace")
	do i=1,ncenter
		write(10,"(a4,4f12.6)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a,a(i)%charge-atmeletot(i)
	end do
	close(10)
	write(*,"(a)") " Result have been saved to "//trim(chgfilename)//".chg in current folder"
	write(*,"(a)") " Columns 1 to 5 are name,X,Y,Z,charge, respectively. The ith row corresponds to the ith atom. The unit is Angstrom"
end if
end subroutine



!!---------- Modified Mulliken population analysis
! isel=1 :Defined by Ros & Schuit (SCPA)"
! isel=2 :Defined by Stout & Politzer
subroutine MMPA(isel)
use defvar
use util
implicit real*8 (a-h,o-z)
integer isel
character selectyn,chgfilename*200
real*8,target :: atmelea(ncenter),atmeleb(ncenter)
real*8,pointer :: tmpmat(:,:),tmpele(:)
atmelea=0D0
atmeleb=0D0
do itime=1,2 !1=Alpha part or total electron, 2=Beta part
	do imo=1,nbasis
		if (itime==1) then
			irealmo=imo
			tmpele=>atmelea
			tmpmat=>CObasa
		else if (itime==2) then
			if (wfntype==1.or.wfntype==4) then
				irealmo=imo+nbasis
				tmpmat=>CObasb
			else !RO
				irealmo=imo
				tmpmat=>CObasa
			end if
			tmpele=>atmeleb
		end if
		if (MOocc(irealmo)==0D0) cycle
		if (isel==1) allbassqr=sum(tmpmat(:,imo)**2)
		do i=1,ncenter
			atmbassqr=sum(tmpmat(basstart(i):basend(i),imo)**2)
			if (isel==1) then !SCPA
				if (wfntype==2) then !RO
					if (MOocc(irealmo)==2D0.or.(MOtype(irealmo)==1.and.itime==1)) tmpele(i)=tmpele(i)+atmbassqr/allbassqr
				else
					tmpele(i)=tmpele(i)+MOocc(irealmo)*atmbassqr/allbassqr
				end if
			else if (isel==2) then !Stout & Politzer
				cross=0D0
				do ii=basstart(i),basend(i)
					do jj=1,nbasis
						denomin=tmpmat(ii,imo)**2+tmpmat(jj,imo)**2
						if (jj/=ii.and.denomin>=1D-120) cross=cross+tmpmat(ii,imo)**2/denomin*tmpmat(ii,imo)*tmpmat(jj,imo)*Sbas(ii,jj)
					end do
				end do
				if (wfntype==2) then !RO
					if (MOocc(irealmo)==2D0.or.(MOtype(irealmo)==1.and.itime==1)) tmpele(i)=tmpele(i)+(atmbassqr+2*cross)
				else
					tmpele(i)=tmpele(i)+MOocc(irealmo)*(atmbassqr+2*cross)
				end if
			end if
		end do
	end do
	if (wfntype==0.or.wfntype==3) exit
end do
if (wfntype==0.or.wfntype==3) then
	do iatm=1,ncenter
		write(*,"(' Atom',i6,'(',a2,')','  Population:',f10.5,'  Atomic charge:',f10.5)") iatm,a(iatm)%name,atmelea(iatm),a(iatm)%charge-atmelea(iatm)
	end do
	write(*,"(' Total net charge:',f10.5)") sum(a(:)%charge)-sum(atmelea(:))
	if (allocated(frag1)) write(*,"(/,' Fragment charge:',f12.6)") sum(a(frag1)%charge-atmelea(frag1))
else if (wfntype==1.or.wfntype==2.or.wfntype==4) then
	write(*,*) "    Atom      Alpha pop.   Beta pop.    Spin pop.     Atomic charge"
	do iatm=1,ncenter
		write(*,"(i6,'(',a2,')',3f13.5,f16.5)") iatm,a(iatm)%name,atmelea(iatm),atmeleb(iatm),atmelea(iatm)-atmeleb(iatm),a(iatm)%charge-atmelea(iatm)-atmeleb(iatm)
	end do
	write(*,"(' Total net charge:',f10.5,'      Total spin electrons:',f10.5)") sum(a(:)%charge)-sum(atmelea(:))-sum(atmeleb(:)),sum(atmelea(:))-sum(atmeleb(:))
	if (allocated(frag1)) then
		write(*,"(/,' Fragment charge:',f12.6)") sum(a(frag1)%charge-atmelea(frag1)-atmeleb(frag1))
		write(*,"(' Fragment spin population:',f12.6)") sum(atmelea(frag1)-atmeleb(frag1))
	end if
end if

call path2filename(firstfilename,chgfilename)
write(*,*)
write(*,"(a)") " If output atom coordinates with charges to "//trim(chgfilename)//".chg in current folder? (y/n)"
read(*,*) selectyn
if (selectyn=="y".or.selectyn=="Y") then
	open(10,file=trim(chgfilename)//".chg",status="replace")
	do i=1,ncenter
		if (wfntype==0.or.wfntype==3) write(10,"(a4,4f12.6)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a,a(i)%charge-atmelea(i)
		if (wfntype==1.or.wfntype==2.or.wfntype==4) write(10,"(a4,4f12.6)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a,a(i)%charge-atmelea(i)-atmeleb(i)
	end do
	close(10)
	write(*,"(a)") " Result have been saved to "//trim(chgfilename)//".chg in current folder"
	write(*,"(a)") " Columns 1 to 5 are name,X,Y,Z,charge respectively, unit is Angstrom"
end if
end subroutine




!!--------- Mulliken/Lowdin population analysis & decompose to orbital contributions
! isel=1 Output Mulliken/Lowdin population
! isel=2 Output gross atomic population matrix and decompose it to orbital contributions
! isel=3 Output gross basis function population matrix and decompose it to orbital contributions
! isel=4 Decompose atomic population to orbital contributions
!Note: If doing Lowdin population, density matrix and overlap matrix should be transformed first before invoking this routine
subroutine MPA(isel,MPApath)
use defvar
use util
implicit real*8 (a-h,o-z)
integer isel
real*8 MOcenmat(ncenter,nbasis),groatmmat(ncenter+1,ncenter),atmele(ncenter),charge(ncenter),spinpop(ncenter)
real*8,pointer :: ptmat(:,:)
real*8,allocatable :: tmpmat(:,:),basmata(:,:),angorbpop(:,:),angorbpopa(:,:),angorbpopb(:,:)
character selectyn,corbnum*6,cOcc*12,chgfilename*200
character(LEN=*) MPApath

if (MPApath==" ") then
	ides=6
else if (isel/=4) then
	open(10,file=trim(MPApath),status="replace")
	ides=10
end if

if (isel==1.or.isel==2.or.isel==4) then
	allocate(tmpmat(nbasis,nbasis),basmata(nbasis,nbasis)) !basmata stores basis gross population of alpha part
	do itime=1,3 !Total elec, alpha elec, beta elec
		if (itime==1) then
			tmpmat=Ptot*Sbas
		else if (itime==2) then
			tmpmat=Palpha*Sbas
			basmata=tmpmat !Backup
		else if (itime==3) then
			tmpmat=Pbeta*Sbas
		end if
		!Calculate gross atomic population matrix
		do i=1,ncenter
			do j=1,ncenter
				accum=0D0
				do ii=basstart(i),basend(i)
					do jj=basstart(j),basend(j)
						accum=accum+tmpmat(ii,jj)
					end do
				end do
				groatmmat(i,j)=accum
			end do
		end do
		do i=1,ncenter !Stored atom populations to the last row
			groatmmat(ncenter+1,i)=sum(groatmmat(1:ncenter,i))
		end do
		totelec=0D0
		
		if (isel==1) then !Contract gross atomic population matrix and output population in each basis function/shell/atom
			if (wfntype==0.or.wfntype==3) then !Notice that only perform once (itime=1)
				allocate(angorbpop(ncenter,0:5)) !Record the population number in each angular moment orbitals, up to H
				angorbpop=0D0
				write(ides,*) "Population of basis functions:"
				write(ides,"('  Basis Type    Atom    Shell   Population')")
				do ibas=1,nbasis
					write(ides,"(i6,3x,a,i5,a,i5,f13.5)") ibas,GTFtype2name(bastype(ibas)),bascen(ibas),'('//a(bascen(ibas))%name//')',basshell(ibas),sum(tmpmat(ibas,:))
				end do
				write(ides,*)
				write(ides,*) "Population of shells of basis functions:"
				do ish=1,nshell
					shellpop=0D0
					do ibas=1,nbasis
						if (basshell(ibas)==ish) then
							iatm=bascen(ibas) !Which atom this shell attribute to
							shellpop=shellpop+sum(tmpmat(ibas,:))
						end if
					end do
					write(ides,"(' Shell',i6,' Type: ',a,'    in atom',i5,'(',a,') :',f9.5)") ish,shtype2name(shtype(ish)),iatm,a(iatm)%name,shellpop
					iangtmp=abs(shtype(ish))
					angorbpop(iatm,iangtmp)=angorbpop(iatm,iangtmp)+shellpop
				end do
				write(ides,*)
				write(ides,*) "Population of each type of angular moment orbitals:"
				do iatm=1,ncenter
					write(ides,"(' Atom',i6,'(',a2,')',' s:',f7.4,' p:',f7.4,' d:',f7.4,' f:',f7.4,' g:',f7.4,' h:',f7.4)") iatm,a(iatm)%name,angorbpop(iatm,:)
				end do
				write(ides,"(' Sum  s:',f9.4,' p:',f9.4,' d:',f9.4,' f:',f9.4,' g:',f9.4,' h:',f9.4)") &
				sum(angorbpop(:,0)),sum(angorbpop(:,1)),sum(angorbpop(:,2)),sum(angorbpop(:,3)),sum(angorbpop(:,4)),sum(angorbpop(:,5))
				write(ides,*)
				write(ides,*) "Population of atoms:"
				do iatm=1,ncenter
					charge(iatm)=a(iatm)%charge-groatmmat(ncenter+1,iatm)
					write(ides,"(' Atom',i6,'(',a2,')','    Population:',f11.5,'    Net charge:',f11.5)") iatm,a(iatm)%name,groatmmat(ncenter+1,iatm),charge(iatm)
				end do
				write(ides,"(' Total net charge:',f10.5)") sum(a(:)%charge)-sum(groatmmat(ncenter+1,:))
			else if ((wfntype==1.or.wfntype==2.or.wfntype==4).and.itime==3) then !For unrestrict wfn, at last "itime" cycle print result
				allocate(angorbpopa(ncenter,0:5),angorbpopb(ncenter,0:5))
				angorbpopa=0D0
				angorbpopb=0D0
				write(ides,*) "Population of basis functions:"
				write(ides,"('  Basis Type    Atom    Shell   Alpha pop.   Beta pop.  Total pop.   Spin pop.')")
				do ibas=1,nbasis !Note: Currently, tmpmat stores basis gross population of beta part, basmata stores alpha part
					baspopa=sum(basmata(ibas,:))
					baspopb=sum(tmpmat(ibas,:))
					write(ides,"(i6,3x,a,i5,a,i5,1x,4f12.5)") ibas,GTFtype2name(bastype(ibas)),bascen(ibas),&
					'('//a(bascen(ibas))%name//')',basshell(ibas),baspopa,baspopb,baspopa+baspopb,baspopa-baspopb
				end do
				write(ides,*)
				write(ides,*) "Population of shells:"
				write(ides,*) "Shell  Type     Atom     Alpha pop.  Beta pop.   Total pop.  Spin pop."
				do ish=1,nshell
					shellpopa=0D0
					shellpopb=0D0
					do ibas=1,nbasis
						if (basshell(ibas)==ish) then
							iatm=bascen(ibas) !Which atom this shell attribute to
							shellpopa=shellpopa+sum(basmata(ibas,:))
							shellpopb=shellpopb+sum(tmpmat(ibas,:))
						end if
					end do
					write(ides,"(i5,5x,a,i7,'(',a,')' ,4f12.5)") ish,shtype2name(shtype(ish)),iatm,a(iatm)%name,shellpopa,shellpopb,shellpopa+shellpopb,shellpopa-shellpopb
					iangtmp=abs(shtype(ish))
					angorbpopa(iatm,iangtmp)=angorbpopa(iatm,iangtmp)+shellpopa
					angorbpopb(iatm,iangtmp)=angorbpopb(iatm,iangtmp)+shellpopb
				end do
				write(ides,*)
				write(ides,*) "Population of each type of angular moment atomic orbitals:"
				write(ides,*) "    Atom    Type   Alpha pop.   Beta pop.    Total pop.   Spin pop."
				do iatm=1,ncenter
					if (angorbpopa(iatm,0)/=0D0.or.angorbpopb(iatm,0)/=0D0) write(ides,"(i6,'(',a2,')    s',4f13.5)") &
					iatm,a(iatm)%name,angorbpopa(iatm,0),angorbpopb(iatm,0),angorbpopa(iatm,0)+angorbpopb(iatm,0),angorbpopa(iatm,0)-angorbpopb(iatm,0)
					if (angorbpopa(iatm,1)/=0D0.or.angorbpopb(iatm,1)/=0D0) write(ides,"('              p',4f13.5)") &
					angorbpopa(iatm,1),angorbpopb(iatm,1),angorbpopa(iatm,1)+angorbpopb(iatm,1),angorbpopa(iatm,1)-angorbpopb(iatm,1)
					if (angorbpopa(iatm,2)/=0D0.or.angorbpopb(iatm,2)/=0D0) write(ides,"('              d',4f13.5)") &
					angorbpopa(iatm,2),angorbpopb(iatm,2),angorbpopa(iatm,2)+angorbpopb(iatm,2),angorbpopa(iatm,2)-angorbpopb(iatm,2)
					if (angorbpopa(iatm,3)/=0D0.or.angorbpopb(iatm,3)/=0D0) write(ides,"('              f',4f13.5)") &
					angorbpopa(iatm,3),angorbpopb(iatm,3),angorbpopa(iatm,3)+angorbpopb(iatm,3),angorbpopa(iatm,3)-angorbpopb(iatm,3)
					if (angorbpopa(iatm,4)/=0D0.or.angorbpopb(iatm,4)/=0D0) write(ides,"('              g',4f13.5)") &
					angorbpopa(iatm,4),angorbpopb(iatm,4),angorbpopa(iatm,4)+angorbpopb(iatm,4),angorbpopa(iatm,4)-angorbpopb(iatm,4)
					if (angorbpopa(iatm,5)/=0D0.or.angorbpopb(iatm,5)/=0D0) write(ides,"('              h',4f13.5)") &
					angorbpopa(iatm,5),angorbpopb(iatm,5),angorbpopa(iatm,5)+angorbpopb(iatm,5),angorbpopa(iatm,5)-angorbpopb(iatm,5)
				end do
				write(ides,*)
				if (sum(angorbpopa(:,0))/=0D0.or.sum(angorbpopb(:,0))/=0D0) write(ides,"('     Total    s',4f13.5)") &
				sum(angorbpopa(:,0)),sum(angorbpopb(:,0)),sum(angorbpopa(:,0))+sum(angorbpopb(:,0)),sum(angorbpopa(:,0))-sum(angorbpopb(:,0))
				if (sum(angorbpopa(:,1))/=0D0.or.sum(angorbpopb(:,1))/=0D0) write(ides,"('              p',4f13.5)") &
				sum(angorbpopa(:,1)),sum(angorbpopb(:,1)),sum(angorbpopa(:,1))+sum(angorbpopb(:,1)),sum(angorbpopa(:,1))-sum(angorbpopb(:,1))
				if (sum(angorbpopa(:,2))/=0D0.or.sum(angorbpopb(:,2))/=0D0) write(ides,"('              d',4f13.5)") &
				sum(angorbpopa(:,2)),sum(angorbpopb(:,2)),sum(angorbpopa(:,2))+sum(angorbpopb(:,2)),sum(angorbpopa(:,2))-sum(angorbpopb(:,2))
				if (sum(angorbpopa(:,3))/=0D0.or.sum(angorbpopb(:,3))/=0D0) write(ides,"('              f',4f13.5)") &
				sum(angorbpopa(:,3)),sum(angorbpopb(:,3)),sum(angorbpopa(:,3))+sum(angorbpopb(:,3)),sum(angorbpopa(:,3))-sum(angorbpopb(:,3))
				if (sum(angorbpopa(:,4))/=0D0.or.sum(angorbpopb(:,4))/=0D0) write(ides,"('              g',4f13.5)") &
				sum(angorbpopa(:,4)),sum(angorbpopb(:,4)),sum(angorbpopa(:,4))+sum(angorbpopb(:,4)),sum(angorbpopa(:,4))-sum(angorbpopb(:,4))
				if (sum(angorbpopa(:,5))/=0D0.or.sum(angorbpopb(:,5))/=0D0) write(ides,"('              h',4f13.5)") &
				sum(angorbpopa(:,5)),sum(angorbpopb(:,5)),sum(angorbpopa(:,5))+sum(angorbpopb(:,5)),sum(angorbpopa(:,5))-sum(angorbpopb(:,5))
				write(ides,*)
				write(ides,*) "Population of atoms:"
				write(ides,*) "    Atom      Alpha pop.   Beta pop.    Spin pop.     Atomic charge"
				totspinpop=0D0
				do iatm=1,ncenter
					alphaele=atmele(iatm)
					betaele=groatmmat(ncenter+1,iatm)
					charge(iatm)=a(iatm)%charge-(alphaele+betaele)
					spinpop(iatm)=alphaele-betaele
					write(ides,"(i6,'(',a2,')',3f13.5,f16.5)") iatm,a(iatm)%name,alphaele,betaele,spinpop(iatm),charge(iatm)
					totspinpop=totspinpop+alphaele-betaele
				end do
				write(ides,"(' Total net charge:',f10.5,'      Total spin electrons:',f10.5)") sum(charge),totspinpop
			end if
			if (itime==2) atmele(:)=groatmmat(ncenter+1,:) !Store alpha occupation of each atom to a temporary array
			
		else if (isel==2) then !Output gross atomic population matrix
			if (itime==1) call showmatgau(groatmmat,"Total gross atomic population matrix",0,"f14.8",ides)
			if (itime==2) call showmatgau(groatmmat,"Alpha gross atomic population matrix",0,"f14.8",ides)
			if (itime==3) call showmatgau(groatmmat,"Beta gross atomic population matrix",0,"f14.8",ides)
			write(ides,*)
		end if
		
		if (wfntype==0.or.wfntype==3) exit !RHF or ROHF, don't continue to process alpha & beta respectively
	end do
	
	!Show fragment information or some prompts
	if (isel==1) then
		write(*,*)
		if (allocated(frag1)) then
			write(ides,"(' Fragment charge:',f12.6)") sum(charge(frag1))
			if (wfntype==1.or.wfntype==2.or.wfntype==4) write(ides,"(' Fragment spin population:',f12.6)") sum(spinpop(frag1))
			write(*,*)
		end if
	else if (isel==2) then
		write(ides,*) "The last row is the sum of corresponding column elements (atomic population)"
		write(*,*)
	end if
	if (MPApath/=" ".and.isel/=4) then
		close(10)
		write(*,"(' Done! Data have been outputted to ',a,/)") trim(MPApath) 
	end if
	
	!Decompose to orbital contributions
	selectyn='n'
	if (isel==2) then
		write(*,"(a)") " Decompose gross atomic population matrix to orbital contributions and write to groatmdcp.txt in current folder? (y/n)"
		read(*,*) selectyn
	else if (isel==4) then
		selectyn='y'
	end if
	if (selectyn=='y'.or.selectyn=='Y') then
		if (isel==2) then
			open(10,file="groatmdcp.txt",status="replace")
			write(10,*) "The last row is the sum of corresponding column elements"
			write(10,*)
		else if (isel==4) then
			open(10,file="atmpopdcp.txt",status="replace")
			write(10,*) "(i,j) elements is contribution to the ith atoms from the jth orbital"
			write(10,*)
		end if
		do itime=1,2
			MOcenmat=0D0
			if (itime==1) ptmat=>CObasa
			if (itime==2) ptmat=>CObasb
			do imo=1,nbasis
				if (itime==1) irealmo=imo
				if (itime==2) irealmo=imo+nbasis
				write(corbnum,"(i6)") imo
				write(cOcc,"(f12.8)") MOocc(irealmo)
				if (MOocc(irealmo)==0D0) cycle
				!Construct gross atomic population matrix
				do i=1,ncenter
					do j=1,ncenter
						accum=0D0
						do ii=basstart(i),basend(i)
							do jj=basstart(j),basend(j)
								accum=accum+MOocc(irealmo)*ptmat(ii,imo)*ptmat(jj,imo)*Sbas(ii,jj)
							end do
						end do
						groatmmat(i,j)=accum
					end do
				end do
				do i=1,ncenter
					groatmmat(ncenter+1,i)=sum(groatmmat(1:ncenter,i))
				end do
				if (isel==2) then
					if (wfntype==0.or.wfntype==2.or.wfntype==3) then
						call showmatgau(groatmmat,"Orbital"//corbnum//"  Occ:"//cOcc,0,"f14.8",10)
					else if (itime==1.and.(wfntype==1.or.wfntype==4)) then
						call showmatgau(groatmmat,"Alpha Orbital"//corbnum//"  Occ:"//cOcc,0,"f14.8",10)
					else if (itime==2.and.(wfntype==1.or.wfntype==4)) then
						call showmatgau(groatmmat,"Beta Orbital"//corbnum//"  Occ:"//cOcc,0,"f14.8",10)
					end if
					write(10,*)
				else if (isel==4) then
					MOcenmat(:,imo)=groatmmat(ncenter+1,:)
				end if
			end do
			if (isel==4) then
				if (wfntype==0.or.wfntype==2) then
					call showmatgau(MOcenmat(:,1:nint(naelec)),"",0,"f14.8",10)
				else if (wfntype==1) then
					if (itime==1) call showmatgau(MOcenmat(:,1:nint(naelec)),"Alpha part",0,"f14.8",10)
					if (itime==2) call showmatgau(MOcenmat(:,1:nint(nbelec)),"Beta part",0,"f14.8",10)
				else if (wfntype==3) then
					call showmatgau(MOcenmat,"",0,"f14.8",10)
				else if (wfntype==4) then
					if (itime==1) call showmatgau(MOcenmat,"Alpha part",0,"f14.8",10)
					if (itime==2) call showmatgau(MOcenmat,"Beta part",0,"f14.8",10)
				end if
				write(10,*)
			end if
			if (wfntype==0.or.wfntype==2.or.wfntype==3) exit !ROHF needn't to separate to alpha and beta
		end do
		close(10)
		if (isel==2) then
			write(*,"(a)") " Done! The matrices have been outputted to groatmdcp.txt in current folder"
		else if (isel==4) then
			write(*,"(a)") " Done! The result have been outputted to atmpopdcp.txt in current folder"
		end if
	end if
	
	!If calculating atomic charges, asking user if output it
	if (isel==1) then
		call path2filename(firstfilename,chgfilename)
		write(*,"(a)") " If output atoms with charges to "//trim(chgfilename)//".chg in current folder? (y/n)"
		read(*,*) selectyn
		if (selectyn=="y".or.selectyn=="Y") then
			open(10,file=trim(chgfilename)//".chg",status="replace")
			do i=1,ncenter
				if (wfntype==0.or.wfntype==3) write(10,"(a4,4f12.6)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a,charge(i)
				if (wfntype==1.or.wfntype==2.or.wfntype==4) write(10,"(a4,4f12.6)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a,charge(i)
			end do
			close(10)
			write(*,"(a)") " Result have been saved to "//trim(chgfilename)//".chg in current folder"
			write(*,"(a)") " Columns 1 to 5 are name,X,Y,Z,charge respectively, unit is Angstrom"
		end if
	end if
		
else if (isel==3) then
	write(ides,*) "The (i,j) element corresponds to ¡Æ[imo] Occ(imo)*C(i,imo)*C(j,imo)*S(i,j)"
	write(ides,*)
	call showmatgau(Ptot*Sbas,"Total gross basis function population matrix",0,"f14.8",ides)
	write(ides,*)
	if (wfntype==1.or.wfntype==2.or.wfntype==4) then
		call showmatgau(Palpha*Sbas,"Alpha gross basis function population matrix",0,"f14.8",ides)
		write(ides,*)
		call showmatgau(Pbeta*Sbas,"Beta gross basis function population matrix",0,"f14.8",ides)
		write(ides,*)
	end if
	if (MPApath/=" ") then
		close(10)
		write(*,"(' Done! Data have been outputted to ',a)") trim(MPApath)
		write(*,*)
	end if
	write(*,"(a)") " Decompose gross basis function population matrix to orbital contributions and write to grobasdcp.txt in current folder? (y/n)"
	read(*,*) selectyn
	if (selectyn=='y'.or.selectyn=='Y') then
		open(10,file="grobasdcp.txt",status="replace")
		write(10,*) "Notes:"
		write(10,*) "The (i,j) element means C(i,imo)*C(j,imo)*S(i,j)"
		write(10,*) "The last row is the sum of corresponding column elements"
		write(10,*)
		allocate(tmpmat(nbasis+1,nbasis))
		do itime=1,2
			if (itime==1) ptmat=>CObasa
			if (itime==2) ptmat=>CObasb
			do imo=1,nbasis
				if (itime==1) irealmo=imo
				if (itime==2) irealmo=imo+nbasis
				write(corbnum,"(i6)") imo
				write(cOcc,"(f12.8)") MOocc(irealmo)
				if (MOocc(irealmo)==0D0) cycle

				do ii=1,nbasis
					do jj=1,nbasis
						tmpmat(ii,jj)=MOocc(irealmo)*ptmat(ii,imo)*ptmat(jj,imo)*Sbas(ii,jj)
					end do
				end do
				do j=1,nbasis
					tmpmat(nbasis+1,j)=sum(tmpmat(1:nbasis,j))
				end do
				if (wfntype==0.or.wfntype==2.or.wfntype==3) then
					call showmatgau(tmpmat,"Orbital"//corbnum//"  Occ:"//cOcc,0,"f14.8",10)
				else if (itime==1.and.(wfntype==1.or.wfntype==4)) then
					call showmatgau(tmpmat,"Alpha Orbital"//corbnum//"  Occ:"//cOcc,0,"f14.8",10)
				else if (itime==2.and.(wfntype==1.or.wfntype==4)) then
					call showmatgau(tmpmat,"Beta Orbital"//corbnum//"  Occ:"//cOcc,0,"f14.8",10)
				end if
				write(10,*)
			end do
			if (wfntype==0.or.wfntype==2.or.wfntype==3) exit
		end do
		close(10)
		write(*,*) "Done!"
	end if
end if
end subroutine




!!-------------- Calculate charge based on space partition method
!1=Hirshfeld, 2=VDD, 3=Integrate electron density in voronoi cell
!4=Adjusted method 3 by Rousseau et al., 5= Becke with/without ADC, 6= ADCH, 7= CM5
subroutine spacecharge(chgtype)
use defvar
use function
use util
implicit real*8(a-h,o-z)
integer chgtype
real*8 molrho(radpot*sphpot),promol(radpot*sphpot),tmpdens(radpot*sphpot),beckeweigrid(radpot*sphpot),selfdens(radpot*sphpot)
type(content) gridatm(radpot*sphpot),gridatmorg(radpot*sphpot)
real*8 atmdipx(ncenter),atmdipy(ncenter),atmdipz(ncenter),charge(ncenter)
real*8 :: covr_becke(0:nelesupp) !covalent radii used for Becke population
character selectyn,chgfilename*200
integer :: nbeckeiter=3

if (chgtype==5) then !Select atomic radii for Becke population
	covr_becke=covr_TianLu
	iraddefine=2
	do while(.true.)
		write(*,*) "-1 Return"
		write(*,*) "0 Start calculation of Becke charge!"
		if (iraddefine==0) write(*,*) "1 Select the definition of atomic radii, current: Custom"
		if (iraddefine==1) write(*,*) "1 Select the definition of atomic radii, current: CSD"
		if (iraddefine==2) write(*,*) "1 Select the definition of atomic radii, current: Modified CSD"
		if (iraddefine==3) write(*,*) "1 Select the definition of atomic radii, current: Pyykko"
		if (iraddefine==4) write(*,*) "1 Select the definition of atomic radii, current: Suresh"
		if (iraddefine==5) write(*,*) "1 Select the definition of atomic radii, current: Hugo"
		write(*,"(a,i2)") " 2 Set the number of iterations for defining Becke atomic space, current:",nbeckeiter
		write(*,*) "10 Read radii from external file"
		write(*,*) "11 Modify current radii by manual input"
		write(*,*) "12 Print current radii list"
		read(*,*) isel
		if (isel==-1) then
			return
		else if (isel==0) then
			exit
		else if (isel==1) then
			write(*,*) "1 Use CSD radii (Dalton Trans., 2008, 2832-2838)"
			write(*,*) "2 Use the modified version of CSD radii defined by Tian Lu (Recommended)"
			write(*,*) "3 Use Pyykko radii (Chem. Eur.-J., 15, 186-197)"
			write(*,*) "4 Use Suresh radii (J. Phys. Chem. A, 105, 5940-5944)"
			write(*,*) "5 Use Hugo radii (Chem. Phys. Lett., 480, 127-131)"
			read(*,*) iselrad
			if (iselrad==1) then
				covr_becke=covr
				iraddefine=1
			else if (iselrad==2) then
				covr_becke=covr_TianLu
				iraddefine=2
			else if (iselrad==3) then
				covr_becke=covr_pyy
				iraddefine=3
			else if (iselrad==4) then
				covr_becke=covr_Suresh
				iraddefine=4
			else if (iselrad==5) then
				covr_becke=radii_hugo
				iraddefine=5
			end if
		else if (isel==2) then
			write(*,*) "Input a number, e.g. 3"
			read(*,*) nbeckeiter
		else if (isel==10) then
			iraddefine=0
			write(*,"(a)") " About the file format:"
			write(*,"(a)") " The first line should be the number of elements you want to modify, followed by element indices and radii (in Angstrom), for example:"
			write(*,"(a)") " 4"
			write(*,"(a)") " 1 0.35"
			write(*,"(a)") " 4 1.2"
			write(*,"(a)") " 5 1.12"
			write(*,"(a)") " 14 1.63"
			write(*,*)
			write(*,*) "Input filename"
			read(*,"(a)") radfilename
			inquire(file=radfilename,exist=alive)
			if (alive.eqv..true.) then
				open(10,file=radfilename,status="old")
				read(10,*) nmodrad
				do irad=1,nmodrad
					read(10,*) indtmp,radtmp
					covr_becke(indtmp)=radtmp/b2a
				end do
				close(10)
				write(*,*) "Done!"
			else
				write(*,*) "Error: File cannot be found"
			end if
		else if (isel==11) then
			iraddefine=0
			write(*,*) "Input element index and radius (in Angstrom), e.g. 5,0.84"
			read(*,*) indtmp,radtmp
			covr_becke(indtmp)=radtmp/b2a
			write(*,*) "Done!"
		else if (isel==12) then
			do irad=0,nelesupp
				write(*,"(' Element:',i5,'(',a,')   Radius:',f8.3,' Angstrom')") irad,ind2name(irad),covr_becke(irad)*b2a
			end do
			write(*,*)
		end if
	end do
end if

!Generate quadrature point and weighs by combination of Gauss-Chebyshev and Lebedev grids
call gen1cintgrid(gridatmorg,iradcut)

!***** 1=Hirshfeld, 2=VDD, 6=ADCH, 7=CM5
if (chgtype==1.or.chgtype==2.or.chgtype==6.or.chgtype==7) then
	write(*,*) "This task requests atomic densities, please select how to obtain them"
	write(*,*) "1 Use build-in sphericalized atomic densities in free-states (more convenient)"
	write(*,"(a)") " 2 Provide wavefunction file of involved elements by yourself or invoke Gaussian to automatically calculate them"
	read(*,*) iatmdensmode
	if (iatmdensmode==2) call setpromol !In this routine reload first molecule at the end
	write(*,"(' Radial grids:',i5,'    Angular grids:',i5,'   Total:',i10)") radpot,sphpot,radpot*sphpot
	write(*,*) "Calculating, please wait..."
	write(*,*)
	call walltime(nwalltime1)
	do iatm=1,ncenter !Cycle each atom to calculate their charges and dipole
		call delvirorb(0) !For faster calculation, remove virtual MOs in whole system, will not affect result
		atmx=a(iatm)%x
		atmy=a(iatm)%y
		atmz=a(iatm)%z
		gridatm%value=gridatmorg%value !Weight in this grid point
		gridatm%x=gridatmorg%x+atmx !Move quadrature point to actual position in molecule
		gridatm%y=gridatmorg%y+atmy
		gridatm%z=gridatmorg%z+atmz
		!Calculate molecular density first
		!$OMP parallel do shared(molrho) private(i) num_threads(nthreads)
		do i=1,radpot*sphpot
			molrho(i)=fdens(gridatm(i)%x,gridatm(i)%y,gridatm(i)%z)
		end do
		!$OMP end parallel do
		!Calc free atomic density to obtain promolecule density
		promol=0D0
		if (iatmdensmode==1) then
			do jatm=1,ncenter
				!$OMP parallel do shared(tmpdens) private(ipt) num_threads(nthreads)
				do ipt=1,radpot*sphpot
					tmpdens(ipt)=calcatmdens(jatm,gridatm(ipt)%x,gridatm(ipt)%y,gridatm(ipt)%z,0)
				end do
				!$OMP end parallel do
				promol=promol+tmpdens
				if (jatm==iatm) selfdens=tmpdens
			end do
		else if (iatmdensmode==2) then
			do jatm=1,ncenter
				call dealloall
				call readwfn(custommapname(jatm),1)
				!$OMP parallel do shared(tmpdens) private(ipt) num_threads(nthreads)
				do ipt=1,radpot*sphpot
					tmpdens(ipt)=fdens(gridatm(ipt)%x,gridatm(ipt)%y,gridatm(ipt)%z)
				end do
				!$OMP end parallel do
				promol=promol+tmpdens
				if (jatm==iatm) selfdens=tmpdens
			end do
			call dealloall
			call readinfile(firstfilename,1) !Retrieve to first loaded file(whole molecule) to calc real rho again
		end if
		!Now we have needed data in hand, calculate atomic charges and atomic dipole moments
		tmpcharge=0D0
		dipx=0D0
		dipy=0D0
		dipz=0D0
		if (chgtype==1.or.chgtype==6.or.chgtype==7) then !Hirshfeld, ADCH charge, CM5 charge
			do i=1,radpot*sphpot
				if (promol(i)/=0D0) then
					tmpv=selfdens(i)/promol(i)*molrho(i)*gridatm(i)%value
					tmpcharge=tmpcharge-tmpv
					dipx=dipx-(gridatm(i)%x-atmx)*tmpv
					dipy=dipy-(gridatm(i)%y-atmy)*tmpv
					dipz=dipz-(gridatm(i)%z-atmz)*tmpv
				end if
			end do
			if (nEDFelec==0) charge(iatm)=a(iatm)%charge+tmpcharge
			if (nEDFelec>0) charge(iatm)=a(iatm)%index+tmpcharge !EDF is provided
		else if (chgtype==2) then !VDD charge
			do i=1,radpot*sphpot !Cycle each grid point of iatm, if the distance between the grid point and other atom is shorter than iatm, weight=0
				vddwei=1D0
				discen2=(gridatm(i)%x-atmx)**2+(gridatm(i)%y-atmy)**2+(gridatm(i)%z-atmz)**2 !Distance between this grid and current center atom
				do jatm=1,ncenter_org !Note: Current wfn is atomic wfn, so use _org suffix
					if (jatm==iatm) cycle
					disother2=(gridatm(i)%x-a_org(jatm)%x)**2+(gridatm(i)%y-a_org(jatm)%y)**2+(gridatm(i)%z-a_org(jatm)%z)**2
					if (disother2<discen2) then
						vddwei=0D0 !Using this weight is equivalent to using Voronoi cell
						exit
					end if
				end do
				tmpv=vddwei*(molrho(i)-promol(i))*gridatm(i)%value
				tmpcharge=tmpcharge-tmpv
				dipx=dipx-(gridatm(i)%x-atmx)*tmpv
				dipy=dipy-(gridatm(i)%y-atmy)*tmpv
				dipz=dipz-(gridatm(i)%z-atmz)*tmpv
			end do
			charge(iatm)=tmpcharge
		end if
		atmdipx(iatm)=dipx
		atmdipy(iatm)=dipy
		atmdipz(iatm)=dipz
		if (chgtype==1.or.chgtype==6.or.chgtype==7) write(*,"(' Hirshfeld charge of atom ',i5,'(',a2,')',' is',f12.6)") iatm,a_org(iatm)%name,charge(iatm)
		if (chgtype==2) write(*,"(' VDD charge of atom ',i5,'(',a2,')',' is',f12.6)") iatm,a_org(iatm)%name,charge(iatm)
	end do
	
!***** 3=Integrate electron density in Voronoi cell, 4=Adjusted method 3 by Rousseau et al
else if (chgtype==3.or.chgtype==4) then
	write(*,"(' Radial grids:',i5,'    Angular grids:',i5,'   Total:',i10)") radpot,sphpot,radpot*sphpot
	write(*,*) "Calculating, please wait..."
	write(*,*)
	call walltime(nwalltime1)
	if (chgtype==4) then !vdW radius From J.Mol.Stru.(Theo.) 538,235-238 is not identical to original definition
		vdwr(1)=0.68D0/b2a
		!B,C,N,O,F
		vdwr(5)=1.46D0/b2a
		vdwr(6)=1.46D0/b2a
		vdwr(7)=1.39D0/b2a
		vdwr(8)=1.35D0/b2a
		vdwr(9)=1.29D0/b2a
		!P S Cl
		vdwr(15)=1.78D0/b2a
		vdwr(16)=1.74D0/b2a
		vdwr(17)=1.69D0/b2a
	end if
	do iatm=1,ncenter
		tmpcharge=0D0
		dipx=0D0
		dipy=0D0
		dipz=0D0
		atmx=a(iatm)%x
		atmy=a(iatm)%y
		atmz=a(iatm)%z
		gridatm%value=gridatmorg%value
		gridatm%x=gridatmorg%x+atmx !Move quadrature point with center of current atom
		gridatm%y=gridatmorg%y+atmy
		gridatm%z=gridatmorg%z+atmz
		do i=1,radpot*sphpot
			vorwei=1.0D0
			discen2=(gridatm(i)%x-atmx)**2+(gridatm(i)%y-atmy)**2+(gridatm(i)%z-atmz)**2 !Distance between this grid and current center atom
			do jatm=1,ncenter !Determine the boundary of cell
				if (jatm==iatm) cycle
				disother2=(gridatm(i)%x-a(jatm)%x)**2+(gridatm(i)%y-a(jatm)%y)**2+(gridatm(i)%z-a(jatm)%z)**2
				if (chgtype==3) then
					if (disother2<discen2) then
						vorwei=0.0D0 !Use this weights equivalent to use voronoi cell
						exit
					end if
				else if (chgtype==4) then !Adjusted voronoi
					vdwra=vdwr(a(iatm)%index)
					vdwrb=vdwr(a(jatm)%index)
					RAB=distmat(iatm,jatm)
					rhoval=(RAB**2+discen2-disother2)/2.0D0/RAB
					rhoa=vdwra/(vdwra+vdwrb)*RAB
					if (rhoval>rhoa) then
						vorwei=0.0D0
						exit
					end if
				end if
			end do
			if (vorwei/=0.0D0) then
				tmpv=vorwei*fdens(gridatm(i)%x,gridatm(i)%y,gridatm(i)%z)*gridatm(i)%value
				tmpcharge=tmpcharge-tmpv
				dipx=dipx-(gridatm(i)%x-atmx)*tmpv
				dipy=dipy-(gridatm(i)%y-atmy)*tmpv
				dipz=dipz-(gridatm(i)%z-atmz)*tmpv
			end if
		end do
		charge(iatm)=tmpcharge+a(iatm)%charge
		atmdipx(iatm)=dipx
		atmdipy(iatm)=dipy
		atmdipz(iatm)=dipz
		write(*,"(' The charge of atom ',i5,'(',a2,')',' is',f12.6)") iatm,a(iatm)%name,charge(iatm)
	end do
	
!***** Becke population
else if (chgtype==5) then
	write(*,"(' Radial grids:',i5,'    Angular grids:',i5,'   Total:',i10)") radpot,sphpot,radpot*sphpot
	write(*,*) "Calculating, please wait..."
	write(*,*)
	call walltime(nwalltime1)
	do iatm=1,ncenter !Cycle each atom to calculate their charges and dipole
		gridatm%value=gridatmorg%value !Weight in this grid point
		gridatm%x=gridatmorg%x+a(iatm)%x !Move quadrature point to actual position in molecule
		gridatm%y=gridatmorg%y+a(iatm)%y
		gridatm%z=gridatmorg%z+a(iatm)%z
		!$OMP parallel do shared(tmpdens) private(i) num_threads(nthreads)
	    do i=1,radpot*sphpot !Calc molecular density first
		    tmpdens(i)=fdens(gridatm(i)%x,gridatm(i)%y,gridatm(i)%z)
	    end do
		!$OMP end parallel do
		call gen1cbeckewei(iatm,iradcut,gridatm,beckeweigrid)
		tmpcharge=0D0
		dipx=0D0
		dipy=0D0
		dipz=0D0
		do i=1+iradcut*sphpot,radpot*sphpot
			tmpv=tmpdens(i)*beckeweigrid(i)*gridatm(i)%value
			tmpcharge=tmpcharge-tmpv
			dipx=dipx-(gridatm(i)%x-a(iatm)%x)*tmpv
			dipy=dipy-(gridatm(i)%y-a(iatm)%y)*tmpv
			dipz=dipz-(gridatm(i)%z-a(iatm)%z)*tmpv
		end do
		if (nEDFelec==0) charge(iatm)=a(iatm)%charge+tmpcharge
		if (nEDFelec>0) charge(iatm)=a(iatm)%index+tmpcharge !EDF is provided
		atmdipx(iatm)=dipx
		atmdipy(iatm)=dipy
		atmdipz(iatm)=dipz
		write(*,"(' Becke charge of atom ',i5,'(',a2,')',' is',f12.6)") iatm,a(iatm)%name,charge(iatm)
	end do
end if

write(*,"(' Summing up all charges:',f15.8)") sum(charge)
write(*,*)
xmoldip=0.0D0
ymoldip=0.0D0
zmoldip=0.0D0
do i=1,ncenter
	xmoldip=xmoldip+a(i)%x*charge(i)
	ymoldip=ymoldip+a(i)%y*charge(i)
	zmoldip=zmoldip+a(i)%z*charge(i)
end do
totdip=dsqrt(xmoldip**2+ymoldip**2+zmoldip**2)
write(*,"(' Total dipole moment from atomic charges:',f12.6,' a.u.')") totdip
write(*,"(' X/Y/Z of dipole from atomic charge:',3f12.6,' a.u.')") xmoldip,ymoldip,zmoldip

if (chgtype==5.or.chgtype==6) then
	write(*,*)
	write(*,*) "Atomic dipole moments (a.u.):"
	do iatm=1,ncenter
		write(*,"(' Atom ',i5,'(',a2,')',' in X/Y/Z:',3f11.6,' Norm:',f11.6)") &
		iatm,a(iatm)%name,atmdipx(iatm),atmdipy(iatm),atmdipz(iatm),dsqrt(atmdipx(iatm)**2+atmdipy(iatm)**2+atmdipz(iatm)**2)
	end do
	totatmdip=dsqrt(sum(atmdipx)**2+sum(atmdipy)**2+sum(atmdipz)**2)
	write(*,"(' Total atomic dipole moment:',f12.6,' a.u.')") totatmdip
	write(*,"(' X/Y/Z of total atomic dipole:',3f12.6,' a.u.')") sum(atmdipx),sum(atmdipy),sum(atmdipz)
	corrdipx=xmoldip+sum(atmdipx) !Corresponding to actual molecular dipole moment derived from molecular density
	corrdipy=ymoldip+sum(atmdipy)
	corrdipz=zmoldip+sum(atmdipz)
	realdip=dsqrt(corrdipx**2+corrdipy**2+corrdipz**2)
	if (chgtype==5) call doADC(atmdipx,atmdipy,atmdipz,charge,realdip,5) !Becke with ADC
	if (chgtype==6) call doADC(atmdipx,atmdipy,atmdipz,charge,realdip,6) !ADCH
else if (chgtype==7) then
	call doCM5(charge)
end if

!Show fragment charge
if (allocated(frag1)) write(*,"(/,' Fragment charge:',f12.6)") sum(charge(frag1))

write(*,*)
call walltime(nwalltime2)
write(*,"(' Calculation took up',i8,' seconds wall clock time')")  nwalltime2-nwalltime1

call path2filename(firstfilename,chgfilename)
write(*,*)
write(*,"(a)") " If output atoms with charges to "//trim(chgfilename)//".chg in current folder? (y/n)"
read(*,*) selectyn
if (selectyn=="y".or.selectyn=="Y") then
	open(10,file=trim(chgfilename)//".chg",status="replace")
	do i=1,ncenter
		write(10,"(a4,4f12.6)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a,charge(i)
	end do
	close(10)
	write(*,"(a)") " Result have been saved to "//trim(chgfilename)//".chg in current folder"
	write(*,"(a)") " Columns 1 to 5 are name,X,Y,Z,charge respectively, unit is Angstrom"
end if
end subroutine


!!------ Calculate atomic dipole moment corrected charge based on existing atomic charge (charge) and atomic dipole moments (dipx/y/z)
!This routine is previously specific for ADCH, but can be extended to any other types of atomic charges
!The "charge" is inputted Hirshfeld charge, finally it is replaced by ADC charge 
!chgtype 5= Becke with/without ADC, 6= ADCH
subroutine doADC(dipx,dipy,dipz,charge,realdip,chgtype)
use defvar
use util
implicit real*8 (a-h,o-z)
integer chgtype
real*8 gammamat(3,3),mat(3,3),avgr(3,1),avgrr(3,3),r(3,1),dip(3,1),tmp(1,1),eigval(3),eigvecmat(3,3)
real*8 w(ncenter),chargecorr(ncenter),charge(ncenter)
real*8 dipx(ncenter),dipy(ncenter),dipz(ncenter),realdip

write(*,*)
write(*,*) "Now calculating atomic dipole moment corrected charge..."
write(*,*)
chargecorr=charge

do i=1,ncenter
	if (ishowchgtrans==1) write(*,"('Atom: 'i4,a)") i,a(i)%name !ishowchgtrans==1 means output detail of charge transferation process during atomic dipole moment correction
	!Initialize variables
	totq=0.0D0
	tottmpdipx=0.0D0
	tottmpdipy=0.0D0
	tottmpdipz=0.0D0
	avgr=0.0D0
	avgrr=0.0D0
	dip(1,1)=dipx(i)
	dip(2,1)=dipy(i)
	dip(3,1)=dipz(i)

	!Calculate weight of every atom
	do j=1,ncenter
		r(1,1)=a(j)%x-a(i)%x
		r(2,1)=a(j)%y-a(i)%y
		r(3,1)=a(j)%z-a(i)%z
		r2=r(1,1)**2+r(2,1)**2+r(3,1)**2
		distij=dsqrt(r2)
		
		!Use modified Becke weight function with vdW radii criterion
		rmaxdist=vdwr(a(i)%index)+vdwr(a(j)%index)
		tr=distij/(rmaxdist/2.0D0)-1 !Transform variable so that it can in 0~rmaxdist range
		tr=1.5*tr-0.5*tr**3
		tr=1.5*tr-0.5*tr**3
		w(j)=0.5*(1-(1.5*tr-0.5*tr**3))
		if (distij>rmaxdist) w(j)=0.0D0

		avgr=avgr+w(j)*r
		avgrr=avgrr+w(j)*matmul(r,transpose(r))
	end do

	wtot=sum(w)
	avgr=avgr/wtot !Now avgr is <r> column vector
	avgrr=avgrr/wtot !Now avgrr is <r r^T> matrix
	gammamat=avgrr-matmul(avgr,transpose(avgr))
! 	call showmatgau(gammamat,form="f12.6")
	call Diagmat(gammamat,eigvecmat,eigval,500,1D-10)
	if (outmedinfo==1) write(*,"(i5,a,3f14.10)") i,a(i)%name,eigval !Test eigenvalue of gamma matrix
	
	!Idea of the treatment:
	!The gammamat is a symmetric matrix, however some of its eigenvalues may be too close to zero to get reasonable inversed matrix,
	!therefore we diagonalize it, get its eigenvalues and eigenvectors, then all following steps are operated in new local coordinate.
	!In the new local coordinate the inverse of the gammamat is imply a diagonal matrix, whose elements are inverse of eigenvalues, hence in this case
	!the eigenvalues can be manually added by a minor value so that is inverse it not extremely large (now we simply omit very small eigenvalues)
	!position or dipole moment vectors in the new local coordinate and in old (Cartesian) coordinate can be transformed via the eigvecmat
	mat=0D0
! 	tmpmin=1D-5
! 	addtmp=tmpmin*(maxval(eigval)+tmpmin)
	do ii=1,3
		if (abs(eigval(ii))>1D-5) mat(ii,ii)=1/eigval(ii) !Ignore ADC for component less than 1E-4 to avoid numerical unstablity. This treatment is more meaningful
! 		mat(ii,ii)=1D0/(eigval(ii)+addtmp) !Original ADC implementation, still has numerical unstability problem in rare case and may worse result
	end do
	
	!Use transform matrix to transform r in old coordinate to r' in new coordinate, and transform P to P'
	!r=matmul(eigvecmat,r'), so r'=matmul(eigvecmat^(-1),r), because eigvecmat is unitary matrix, r'=matmul(transepose(eigvecmat),r)
	avgr=matmul(transpose(eigvecmat),avgr)
	dip=matmul(transpose(eigvecmat),dip)
	
! 	ishowchgtrans=1
! 	if (i==10.or.i==12) then
! 		ishowchgtrans=1
! 		write(*,"(f20.10)") addtmp
! 		write(*,"(3f20.10)") mat(1,1),mat(2,2),mat(3,3)
! 		call showmatgau(eigvecmat,form="f12.6")
! 		write(*,"(3f14.10)") avgr
! 		write(*,"(3f14.10)") dip
! 	end if

	!All values need have been calculated, now calculate final result
	do j=1,ncenter
		r(1,1)=a(j)%x-a(i)%x
		r(2,1)=a(j)%y-a(i)%y
		r(3,1)=a(j)%z-a(i)%z
		r=matmul(transpose(eigvecmat),r) ! Get r(i,j) vector in new coordinate
		tmp=w(j)/wtot*matmul(matmul(transpose(r-avgr),mat) ,dip) !delta q, namely the charge which atom i gives atom j
		chargecorr(j)=chargecorr(j)+tmp(1,1) !Charge after corrected
		if (ishowchgtrans==1) write(*,"(' Give atom ',i4,a4,f15.10,'  Weight',2f15.12)") j,a(j)%name,tmp(1,1),w(j)
		totq=totq+tmp(1,1)
		tottmpdipx=tottmpdipx+(a(j)%x-a(i)%x)*tmp(1,1)
		tottmpdipy=tottmpdipy+(a(j)%y-a(i)%y)*tmp(1,1)
		tottmpdipz=tottmpdipz+(a(j)%z-a(i)%z)*tmp(1,1)
	end do
	
	if (ishowchgtrans==1) write(*,*)
end do

write(*,*) "   ======= Summary of atomic dipole moment corrected (ADC) charges ======="
do i=1,ncenter
	write(*,"(' Atom: ',i4,a,'  Corrected charge:',f12.6,'  Before:',f12.6)") i,a(i)%name,chargecorr(i),charge(i)
end do
write(*,"(' Summing up all corrected charges:',f12.7)") sum(chargecorr)
if (chgtype==5) write(*,"(a)") " Note: The values shown after ""Corrected charge"" are atomic dipole moment corrected Becke charges, the ones after ""Before"" are normal Becke charges"
if (chgtype==6) write(*,"(a)") " Note: The values shown after ""Corrected charge"" are ADCH charges, the ones after ""Before"" are Hirshfeld charges"
ADCdipx=sum(a%x*chargecorr)
ADCdipy=sum(a%y*chargecorr)
ADCdipz=sum(a%z*chargecorr)
ADCdip=sqrt(ADCdipx**2+ADCdipy**2+ADCdipz**2)
write(*,*)
write(*,"(' Total dipole from ADC charges (a.u.)',f11.7,'  Error:',f11.7)") ADCdip,abs(ADCdip-realdip)
write(*,"(' X/Y/Z of dipole moment from the charge (a.u.)',3f11.7)") ADCdipx,ADCdipy,ADCdipz
charge=chargecorr !Overlay charge array, then return to Hirshfeld module and output result to .chg file
end subroutine


!!--------- Calculate CM5 charge based on Hirshfeld charge
subroutine doCM5(charge)
use defvar
implicit real*8 (a-h,o-z)
real*8 charge(ncenter),CMcharge(ncenter),radius(118),Dparm(118)
alpha=2.474D0
Dparm=0D0
Dparm(1)=0.0056D0
Dparm(2)=-0.1543D0
Dparm(4)=0.0333D0
Dparm(5)=-0.1030D0
Dparm(6)=-0.0446D0
Dparm(7)=-0.1072D0
Dparm(8)=-0.0802D0
Dparm(9)=-0.0629D0
Dparm(10)=-0.1088D0
Dparm(11)=0.0184D0
Dparm(13)=-0.0726D0
Dparm(14)=-0.0790D0
Dparm(15)=-0.0756D0
Dparm(16)=-0.0565D0
Dparm(17)=-0.0444D0
Dparm(18)=-0.0767D0
Dparm(19)=0.0130D0
Dparm(31)=-0.0512D0
Dparm(32)=-0.0557D0
Dparm(33)=-0.0533D0
Dparm(34)=-0.0399D0
Dparm(35)=-0.0313D0
Dparm(36)=-0.0541D0
Dparm(37)=0.0092D0
Dparm(49)=-0.0361D0
Dparm(50)=-0.0393D0
Dparm(51)=-0.0376D0
Dparm(52)=-0.0281D0
Dparm(53)=-0.0220D0
Dparm(54)=-0.0381D0
Dparm(55)=0.0065D0
Dparm(81)=-0.0255D0
Dparm(82)=-0.0277D0
Dparm(83)=-0.0265D0
Dparm(84)=-0.0198D0
Dparm(85)=-0.0155D0
Dparm(86)=-0.0269D0
Dparm(87)=0.0046D0
Dparm(113)=-0.0179D0
Dparm(114)=-0.0195D0
Dparm(115)=-0.0187D0
Dparm(116)=-0.0140D0
Dparm(117)=-0.0110D0
Dparm(118)=-0.0189D0
!As shown in CM5 paper, the covalent radii used in CM5 equation are tabulated in CRC book 91th, where they are obtained as follows:
!For Z=1~96, the radii are the average of CSD radii (For Fe, Mn, Co the low-spin is used) and Pyykko radii
!For Z=97~118, the radii are Pyykko radii
radius(1:96)=(covr(1:96)+covr_pyy(1:96))/2D0
radius(97:118)=covr_pyy(97:118)
radius=radius*b2a !Because the radii have already been converted to Bohr, so we convert them back to Angstrom

if (ishowchgtrans==1) write(*,"(/,a)") " Details of CM5 charge correction:"

do iatm=1,ncenter
	CMcorr=0
	iZ=a(iatm)%index
	do jatm=1,ncenter
		if (iatm==jatm) cycle
		jZ=a(jatm)%index
		Bval=exp( -alpha*(distmat(iatm,jatm)*b2a-radius(iZ)-radius(jZ)) )
		if (iZ==1.and.jZ==6) then
			Tval=0.0502D0
		else if (iZ==6.and.jZ==1) then
			Tval=-0.0502D0
		else if (iZ==1.and.jZ==7) then
			Tval=0.1747D0
		else if (iZ==7.and.jZ==1) then
			Tval=-0.1747D0
		else if (iZ==1.and.jZ==8) then
			Tval=0.1671D0
		else if (iZ==8.and.jZ==1) then
			Tval=-0.1671D0
		else if (iZ==6.and.jZ==7) then
			Tval=0.0556D0
		else if (iZ==7.and.jZ==6) then
			Tval=-0.0556D0
		else if (iZ==6.and.jZ==8) then
			Tval=0.0234D0
		else if (iZ==8.and.jZ==6) then
			Tval=-0.0234D0
		else if (iZ==7.and.jZ==8) then
			Tval=-0.0346D0
		else if (iZ==8.and.jZ==7) then
			Tval=0.0346D0
		else
			Tval=Dparm(iZ)-Dparm(jZ)
		end if
		CMcorr=CMcorr+Tval*Bval
		if (ishowchgtrans==1) then
			write(*,"(i4,a,i4,a,'  B_term:',f10.5,'  T_term:',f10.5,'  Corr. charge:',f10.5)") iatm,a(iatm)%name,jatm,a(jatm)%name,Bval,Tval,Tval*Bval
		end if
	end do
	CMcharge(iatm)=charge(iatm)+CMcorr
end do
write(*,*)
write(*,*) "                    ======= Summary of CM5 charges ======="
do i=1,ncenter
	write(*,"(' Atom: ',i4,a,'  CM5 charge:',f12.6,'  Hirshfeld charge:',f12.6)") i,a(i)%name,CMcharge(i),charge(i)
end do
write(*,"(' Summing up all CM5 charges:',f15.8)") sum(CMcharge)
CM5dipx=sum(a%x*CMcharge)
CM5dipy=sum(a%y*CMcharge)
CM5dipz=sum(a%z*CMcharge)
CM5dip=sqrt(CM5dipx**2+CM5dipy**2+CM5dipz**2)
write(*,*)
write(*,"(' Total dipole moment from CM5 charges',f12.7,' a.u.')") CM5dip
write(*,"(' X/Y/Z of dipole moment from CM5 charges',3f10.5, ' a.u.')") CM5dipx,CM5dipy,CM5dipz
charge=CMcharge
end subroutine











!!============================ ESP charge ============================!!
!!============================ ESP charge ============================!!
!!============================ ESP charge ============================!!
!!============================ ESP charge ============================!!
!!============================ ESP charge ============================!!


!!------------ Calculate Restrained ElectroStatic Potential (RESP) charge
!I currently assume that the number of fitting centers is equal to actual number of atoms (nfitcen=ncenter)
subroutine RESP
use util
use defvar
use function
implicit real*8 (a-h,o-z)
character selectyn,c80*80,molfilepath*200,gauoutfilepath*200,eqvconsfilepath*200,outchgfilepath*200,c2000tmp*2000,c200tmp*200
real*8 :: hyper_a=0.0005D0,hyper_a_1=0.0005D0,hyper_a_2=0.001D0,hyper_b=0.1D0 !Hyperbolic restraint parameters
integer :: ideterbond=1,igridtype=1,iradiisel=1,iESPtype=1
real*8 tmpmat(1,1)
!Charge constraint
character chgconsfilepath*200
integer nchgcons !The number of charge constraint terms
integer,allocatable :: chgconsnatm(:) !The i index is number of atoms in charge constraint i
integer,allocatable :: chgconsatm(:,:) !chgconsatm(1:chgconsnatm(i),i) is the atom indices of charge constraint i
real*8,allocatable :: chgconsval(:) !Value of charge constraint
!Conformation information
integer nconf
character*200,allocatable :: conffilepath(:) !File path of each conformation
real*8,allocatable :: confweight(:)
real*8,allocatable :: fitcen(:,:,:) !x/y/z, atom index, conformer index
!About distribution of fitting points
integer :: nMKlayer=4
real*8 :: espfitvdwr(nelesupp),sclvdwlayer(100)=(/1.4D0,1.6D0,1.8D0,2.0D0,(0D0,i=5,100)/)
real*8 :: fitspc=0.566917796573677D0 !0.3/b2a, spacing between grid for CHELPG
real*8 :: extdis=5.29123276802099D0 !2.8/b2a, extend 2.8 Angstrom to each side for CHELPG
real*8 :: MKptdens=1.68017136515525D0 !6D0*b2a**2, 6.0 Angstrom**2 point density per for MK. Multiply by b2a**2 to convert to Bohr**2
!Arrays used in ESP fitting
integer :: maxESPpt !The maximum of number of fitting points among all conformers (used to allocate ESPpt and ESPptval)
integer,allocatable :: nESPpt(:) !The actual number of fitting points of each conformer
real*8,allocatable :: ESPptval(:,:) !ESP values, conformer index
real*8,allocatable :: ESPpt(:,:,:) !x/y/z, fitting point index, conformer index
real*8,allocatable :: fitcenvdwr(:) !vdW radius of each fitting center
integer :: MKatmlist(ncenter) !Record index of atoms used to construct MK fitting points
real*8 atmchg(ncenter) !Final result
real*8 atmchg_stage1(ncenter) !Record stage 1 result of standard RESP
!eqvlist records equivalence relationship. neqvlist is the number of equivalence constraints, eqvlistlen is the number of atoms in each equivalence constraint
!If e.g. eqvlist(1:eqvlistlen(3),3) contains 5,6, that means atoms 5 and 6 should be contrainted to be eequivalent.
!If eqvlistlen(i) is 1, that means no equivalence constraint is imposed to atom i
integer neqvlist,eqvlistlen(ncenter),eqvlist(10,ncenter) !Size of 10 and ncenter are sufficiently high
!Arrays used for constructing standard RESP type of constraint 
real*8 bondedmat(ncenter,ncenter) !1/0 means the two atoms are bonded / not bonded
integer H_list(5) !Temporarily record index
integer nCHlist,CHlist(ncenter) !Record index of sp3 carbons, methyl carbons, and hydrogens attached to them, they are atoms to be fitted in RESP stage 2. nCHlist is actual length
integer neqvlist_H,eqvlistlen_H(ncenter),eqvlist_H(10,ncenter) !Constraint hydrogens in -CH3, =CH2, -CH2- to be equivalent

!By default, only one conformer
nconf=1
allocate(confweight(nconf),conffilepath(nconf))
confweight(1)=1
conffilepath(1)=firstfilename

ifloadconflist=0
iloadgau=0
ieqvcons=2
ichgcons=0 !=0/1 apply/disable charge constraint

! ichgcons=1
! chgconsfilepath="C:\Users\Sobereva\Desktop\RESP\methanol\chgcons.txt"
! gauoutfilepath="C:\Users\Sobereva\Desktop\RESP\largemol\test.out"

maincyc: do while(.true.) !Main loop

do while(.true.) !Interface loop
	write(*,*)
	write(*,*) "             ------------ Calculation of RESP charges ------------"
	if (ifloadconflist==0) write(*,*) "-1 Load list of conformer and weights from external file"
	if (ifloadconflist==1) write(*,"(a,i4,a)") "-1 Reload list of conformers from external file, current:",nconf," conformers"
	write(*,*) "0 Return"
	write(*,*) "1 Start standard two-stage RESP fitting calculation"
	write(*,*) "2 Start one-stage ESP fitting calculation with all your settings"
	if (iloadgau==0) then
		if (igridtype==1) write(*,*) "3 Set method and parameters for distributing fitting points, current: MK"
		if (igridtype==2) write(*,*) "3 Set method and parameters for distributing fitting points, current: CHELPG"
	end if
	write(*,*) "4 Set hyperbolic penalty parameters"
	if (ieqvcons==0) write(*,*) "5 Set equivalence constraint in one-stage fitting, current: No constraint"
	if (ieqvcons==1) write(*,*) "5 Set equivalence constraint in one-stage fitting, current: Customized"
	if (ieqvcons==2) write(*,*) "5 Set equivalence constraint in one-stage fitting, current: H in CH2 and CH3"
	if (ichgcons==0) write(*,*) "6 Set charge constraint in one-stage fitting, current: No constraint"
	if (ichgcons==1) write(*,*) "6 Set charge constraint in one-stage fitting, current: Customized"
	if (ideterbond==1) write(*,*) "7 Set the way of determining connectivity, current: Guess from bond length"
	if (ideterbond==2) write(*,*) "7 Set the way of determining connectivity, current: Load from .mol"
	if (iloadgau==0) write(*,"(a)") " 8 Toggle if loading fitting points and ESP values from Gaussian output file of pop=MK/CHELPG task with IOp(6/33=2) during the calculation, current: No"
	if (iloadgau==1) write(*,"(a)") " 8 Toggle if loading fitting points and ESP values from Gaussian output file of pop=MK/CHELPG task with IOp(6/33=2) during the calculation, current: Yes"
	if (iradiisel==1) write(*,*) "10 Choose the atomic radii used in fitting, current: Automatic"
	if (iradiisel==2) write(*,*) "10 Choose the atomic radii used in fitting, current: Scaled UFF"
	if (iradiisel==3) write(*,*) "10 Choose the atomic radii used in fitting, current: Customized"
	if (iESPtype==1) write(*,*) "11 Choose ESP type, current: Nuclear + Electronic"
	if (iESPtype==2) write(*,*) "11 Choose ESP type, current: Electronic"
	if (iESPtype==3) write(*,*) "11 Choose ESP type, current: Transition electronic"
	read(*,*) isel
	
	if (isel==-1) then
		write(*,*) "Input path of the file containing conformer list, e.g. C:\conflist.txt"
		do while(.true.)
			read(*,"(a)") c2000tmp
			inquire(file=c2000tmp,exist=alive)
			if (alive) exit
			write(*,*) "Cannot find the file, input again"
		end do
		open(10,file=c2000tmp,status="old")
		nconf=totlinenum(10,1)
		write(*,"(' There are',i5,' conformers')") nconf
		deallocate(conffilepath,confweight)
		allocate(conffilepath(nconf),confweight(nconf))
		rewind(10)
		do iconf=1,nconf
			read(10,"(a)") c200tmp
			isep=index(trim(c200tmp),' ',back=.true.)
			read(c200tmp(:isep-1),"(a)") conffilepath(iconf)
			read(c200tmp(isep+1:),*) confweight(iconf)
			inquire(file=conffilepath(iconf),exist=alive)
			if (.not.alive) then
				write(*,"(/,a)") " Error: Cannot find "//trim(conffilepath(iconf))
				write(*,*) "Press ENTER button to cancel the loading"
				read(*,*)
				nconf=1
				close(10)
				cycle maincyc
			end if
		end do
		close(10)
		totwei=sum(confweight)
		write(*,"(' Sum of weights:',f12.6)") totwei
		if (abs(totwei-1)>0.001) write(*,*) "Warning: The sum of weights deviates from 1.0 evidently!"
		ifloadconflist=1
	else if (isel==0) then
		Return
	else if (isel==1.or.isel==2) then
		exit
	else if (isel==3) then
		write(*,*) "Use which kind of fitting points?"
		write(*,*) "1 MK grid"
		write(*,*) "2 CHELPG grid"
		read(*,*) igridtype
		if (igridtype==1) then
			do while(.true.)
				write(*,*)
				write(*,*) "0 Finished!"
				write(*,"(' 1 Set number of points per Angstrom^2, current:',f10.3)") MKptdens/b2a**2 !Temporary convert to Angstrom**2 for convention
				write(*,"(' 2 Set number of layers per atom, current:',i4)") nMKlayer
				write(*,"(' 3 Set the value times van der Waals radius in each layer')")
				read(*,*) isel2
				if (isel2==0) then
					exit
				else if (isel2==1) then
					write(*,*) "Input a number (in per Angstrom^2), e.g. 6.0"
					read(*,*) MKptdens
					MKptdens=MKptdens*b2a**2
				else if (isel2==2) then
					write(*,*) "Input a value, e.g. 5"
					read(*,*) nMKlayer
				else if (isel2==3) then
					write(*,*) "Current values:"
					do ilayer=1,nMKlayer
						write(*,"(' Layer',i3,':',f8.4)") ilayer,sclvdwlayer(ilayer)
					end do
					write(*,*)
					do ilayer=1,nMKlayer
						write(*,"(a,i3,', e.g. 1.5')") " Input value for layer",ilayer
						read(*,*) sclvdwlayer(ilayer)
					end do
				end if
			end do
		else if (igridtype==2) then
			do while(.true.)
				write(*,*)
				write(*,*) "0 Finished!"
				write(*,"(' 1 Set grid spacing, current:',f7.3,' Bohr (',f7.3,' Angstrom)')") fitspc,fitspc*b2a
				write(*,"(' 2 Set box extension, current:',f7.3,' Bohr (',f7.3,' Angstrom)')") extdis,extdis*b2a
				read(*,*) isel2
				if (isel2==0) then
					exit
				else if (isel2==1) then
					write(*,*) "Input a value in Bohr, e.g. 0.5"
					read(*,*) fitspc
				else if (isel2==2) then
					write(*,*) "Input a value in Bohr, e.g. 6.5"
					read(*,*) extdis
				end if
			end do
		end if
		
	else if (isel==4) then
		do while(.true.)
			write(*,*)
			write(*,*) "0 Return"
			write(*,"(' 1 Set tightness parameter (b), current:',f8.5)") hyper_b
			write(*,"(' 2 Set restraint strength (a) for one-stage fitting, current:',f8.5)") hyper_a
			write(*,"(' 3 Set restraint strength in stage 1 of standard RESP, current:',f8.5)") hyper_a_1
			write(*,"(' 4 Set restraint strength in stage 2 of standard RESP, current:',f8.5)") hyper_a_2
			read(*,*) isel2
			if (isel2==0) then
				exit
			else if (isel2==1) then
				write(*,*) "Input a value, e.g. 0.1"
				read(*,*) hyper_b
			else if (isel2==2) then
				write(*,*) "Input a value, e.g. 0.0005"
				read(*,*) hyper_a
			else if (isel2==3) then
				write(*,*) "Input a value, e.g. 0.0005"
				read(*,*) hyper_a_1
			else if (isel2==4) then
				write(*,*) "Input a value, e.g. 0.002"
				read(*,*) hyper_a_2
			end if
			write(*,*) "Done!"
		end do
		
	else if (isel==5) then
		write(*,*) "0 No equivalence constraint will be imposed"
		write(*,*) "1 Load equivalence constraint setting from external plain text file"
		write(*,*) "2 H in each =CH2, -CH2-, CH3 are constrainted to be equivalent"
		read(*,*) ieqvcons
		if (ieqvcons==1) then
			write(*,*) "Input path of the plain text file, e.g. C:\eqvcons.txt"
			do while(.true.)
				read(*,"(a)") eqvconsfilepath
				inquire(file=eqvconsfilepath,exist=alive)
				if (alive) exit
				write(*,*) "Cannot find the file, input again"
			end do
			write(*,*) "OK, equivalence constraint will be loaded from it during calculation"
		end if
		
	else if (isel==6) then
		write(*,*) "0 No charge constraint will be imposed" 
		write(*,*) "1 Load charge constraint setting from external plain text file"
		read(*,*) ichgcons
		if (ichgcons==1) then
			write(*,*) "Input path of the plain text file, e.g. C:\chgcons.txt"
			do while(.true.)
				read(*,"(a)") chgconsfilepath
				inquire(file=chgconsfilepath,exist=alive)
				if (alive) exit
				write(*,*) "Cannot find the file, input again"
			end do
			write(*,*) "OK, charge constraint will be loaded from it during calculation"
		end if
		
	else if (isel==7) then
		write(*,*) "1 Guess connectivity based on atomic covalent radii and interatomic distance"
		write(*,*) "2 Load connectivity from a .mol file"
		read(*,*) ideterbond
		if (ideterbond==2) then
			write(*,*) "Input file path of the .mol file, e.g. C:\tsushima_yoshiko.mol"
			do while(.true.)
				read(*,"(a)") molfilepath
				inquire(file=molfilepath,exist=alive)
				if (alive) exit
				write(*,*) "Cannot find the file, input again"
			end do
			write(*,*) "OK, the connectivity will be loaded from this file during calculation"
		end if
		
	else if (isel==8) then
		if (iloadgau==1) then
			iloadgau=0
		else if (iloadgau==0) then
			iloadgau=1
			write(*,"(a)") " OK, ESP fitting points with ESP values will be directly loaded from specified Gaussian output file during the calculation"
		end if
		
	else if (isel==10) then
		write(*,*) "Please choose the way of determining atomic radii:"
		write(*,"(a)") " 1 Automatic (The radii actually used will depend on the way of distributing fitting points)"
		write(*,"(a)") " 2 UFF radii scaled by 1/1.2, defined for entire periodic table"
		write(*,"(a)") " 3 Customized (The radii will be loaded from external file during fitting)"
		read(*,*) iradiisel
		
	else if (isel==11) then
		write(*,*) "Choose ESP type:"
		write(*,*) "1 Nuclear + Electronic"
		write(*,*) "2 Electronic only"
		write(*,*) "3 Transition electronic (Mainly used for deriving TrEsp charges)"
		read(*,*) iESPtype
		if (iESPtype==3) then
			hyper_a=0
			write(*,*) "Note: Restraint strength (a) for one-stage fitting has been set to zero"
			if (ieqvcons/=0) then
				ieqvcons=0
				write(*,*) "Note: Atom equivalence constraint has been removed"
			end if
		end if
	end if
end do !end interface cycle


nfitcen=ncenter
allocate(fitcen(3,nfitcen,nconf),fitcenvdwr(nfitcen),nESPpt(nconf))
!Generate fitting centers, fitting points and calculate ESP value using internal code
if (iloadgau==0) then
	!Set and check sanity of vdW radii used in fitting
	espfitvdwr=-1
	if (iradiisel==1) then
		call setESPfitvdwr(igridtype,espfitvdwr)
	else if (iradiisel==2) then !Scaled UFF
		call setESPfitvdwr(0,espfitvdwr)
	else if (iradiisel==3) then !Customize radii
		call setESPfitvdwr(-1,espfitvdwr)
	end if
	do iatm=1,ncenter
		fitcenvdwr(iatm)=espfitvdwr(a(iatm)%index) !vdW radius for each fitting center
	end do
	
	!Cycle conformers. nconf may be 1
	do iconf=1,nconf
		if (ifloadconflist==1) then
			write(*,"(a,i5)") " Generating fitting points and calculate ESP for conformer",iconf
			call dealloall
			call readinfile(conffilepath(iconf),1)
		end if
		!Generate information of fitting centers
		do iatm=1,ncenter
			fitcen(1,iatm,iconf)=a(iatm)%x
			fitcen(2,iatm,iconf)=a(iatm)%y
			fitcen(3,iatm,iconf)=a(iatm)%z
		end do
		
		if (igridtype==1) then !MK
			nMKatoms=ncenter !All atoms are used to construct MK fitting points
			forall(i=1:ncenter) MKatmlist(i)=i
			call setMKpt(1,nfitcen,fitcen(:,:,iconf),fitcenvdwr,nptthis,tmpmat,sclvdwlayer,MKptdens,nMKlayer,nMKatoms,MKatmlist) !Returned number of points is upper limit
			if (iconf==1) then !At the first conformer, allocate large enough size for ESPptval and ESPpt
				maxESPpt=nptthis*2
				allocate(ESPptval(maxESPpt,nconf),ESPpt(3,maxESPpt,nconf))
			end if
			call setMKpt(2,nfitcen,fitcen(:,:,iconf),fitcenvdwr,nptthis,ESPpt(:,:,iconf),sclvdwlayer,MKptdens,nMKlayer,nMKatoms,MKatmlist)
		else if (igridtype==2) then !CHELPG
			call setCHELPGpt(1,nfitcen,fitcen(:,:,iconf),fitcenvdwr,nptthis,tmpmat,extdis,fitspc) !Return actual number of points
			if (iconf==1) then !At the first conformer, allocate large enough size for ESPptval and ESPpt
				maxESPpt=nptthis*2
				allocate(ESPptval(maxESPpt,nconf),ESPpt(3,maxESPpt,nconf))
			end if
			call setCHELPGpt(2,nfitcen,fitcen(:,:,iconf),fitcenvdwr,nptthis,ESPpt(:,:,iconf),extdis,fitspc)
		end if
		nESPpt(iconf)=nptthis

		!Generate ESP value of fitting points
		if (nconf==1) then !Show prompts
			call fitESP_calcESP(1,iESPtype,nptthis,ESPpt(:,1:nptthis,iconf),ESPptval(1:nptthis,iconf),conffilepath(iconf))
		else !Don't show prompts
			call fitESP_calcESP(0,iESPtype,nptthis,ESPpt(:,1:nptthis,iconf),ESPptval(1:nptthis,iconf),conffilepath(iconf))
		end if
		
!  		do ipt=1,nESPpt(iconf)
!  			write(11,"(i6,3f12.6,f16.10)") ipt,ESPpt(:,ipt,iconf),ESPptval(ipt,iconf)
!  		end do
!  		pause
	end do
	
	if (ifloadconflist==1) then
		write(*,*) "Reloading the file when Multiwfn boots up..."
		call dealloall
		call readinfile(firstfilename,1)
	end if
	
!Reading ESP and coordinates of fitting points from Gaussian Iop(6/33=2) output. File containing geometry must be loaded to provide atom coordinates
else
	if (nconf==1) then
		write(*,"(a)") " Input file path of the Gaussian pop=MK/CHELPG task with IOp(6/33=2) keyword, e.g. C:\tsushima_yoshiko.out. &
		Note that the geometry used in the Gaussian calculation must be exactly identical to current geometry"
		do while(.true.)
			read(*,"(a)") gauoutfilepath
			inquire(file=gauoutfilepath,exist=alive)
			if (alive) exit
			write(*,*) "Error: Cannot find the file, input again"
		end do
		write(*,"(a)") " Loading ESP data from "//trim(gauoutfilepath)
		iconf=1
		call loadgauESP_num(gauoutfilepath,nfitcen,nptthis) !Get number of points
		nESPpt(iconf)=nptthis
		maxESPpt=nptthis
		allocate(ESPptval(nptthis,iconf),ESPpt(3,nptthis,iconf))
		call loadgauESP(gauoutfilepath,nfitcen,fitcen(:,:,iconf),nptthis,ESPpt(:,:,iconf),ESPptval(:,iconf))
	else
		do iconf=1,nconf
			write(*,"(a)") " Loading ESP data from "//trim(conffilepath(iconf))
			call loadgauESP_num(conffilepath(iconf),nfitcen,nptthis) !Get number of points
			if (iconf==1) then !At the first conformer, allocate large enough size for ESPptval and ESPpt
				maxESPpt=nptthis*2
				allocate(ESPptval(maxESPpt,nconf),ESPpt(3,maxESPpt,nconf))
			end if
			nESPpt(iconf)=nptthis
			call loadgauESP(conffilepath(iconf),nfitcen,fitcen(:,:,iconf),nptthis,ESPpt(:,1:nptthis,iconf),ESPptval(1:nptthis,iconf))
		end do
	end if
end if
write(*,*)


!For standard RESP or one-stage fitting with eqv. cons. on H in -CH3, -CH2-, =CH2, we need to identify bonding and generate relevant arrays
if (isel==1.or.ieqvcons==2) then
	!Generate bonding matrix, used to identify -CH3, -CH2-, =CH2 groups
	bondedmat=0
	if (ideterbond==1) then !Guess bonding relationship
		do i=1,ncenter
			do j=i+1,ncenter
				if ( distmat(i,j)<( covr(a(i)%index)+covr(a(j)%index) )*bondcrit ) bondedmat(i,j)=1
				bondedmat(j,i)=bondedmat(i,j)
			end do
		end do
	else !Generate bonding matrix based on loaded connectivity matrix in specified .mol file
		call readmolconn(molfilepath)
		do i=1,ncenter
			do j=i+1,ncenter
				if (connmat(i,j)>0) bondedmat(i,j)=1
				bondedmat(j,i)=bondedmat(i,j)
			end do
		end do
	end if
	
	!Identify active C and H in stage 2 of standard RESP fitting, meantime generate equivalence list considering hydrogens in each -CH3, -CH2-, =CH2 is equivalent
	neqvlist_H=0
	eqvlist_H=0
	nCHlist=0
	do iatm=1,ncenter
		if (a(iatm)%index==6) then !Carbon
			nbond=sum(bondedmat(iatm,:))
			natt_H=0
			natt_C=0
			do jatm=1,ncenter
				if (bondedmat(iatm,jatm)==1) then
					if (a(jatm)%index==1) then
						natt_H=natt_H+1
						H_list(natt_H)=jatm
					else if (a(jatm)%index==6) then
						natt_C=natt_C+1
					end if
				end if
			end do
			!sp3 or methylene carbon, its charge should be fitted at stage 2 of standard RESP
			if (nbond==4.or.(natt_H==2.and.natt_C==1)) then
				nCHlist=nCHlist+1
				CHlist(nCHlist)=iatm
				neqvlist_H=neqvlist_H+1
				eqvlistlen_H(neqvlist_H)=1
				eqvlist_H(1,neqvlist_H)=iatm
				if (natt_H>0) then !Hydrogens attached to such carbon should have equivalent constraint
					neqvlist_H=neqvlist_H+1
					eqvlistlen_H(neqvlist_H)=natt_H
					do iatt_H=1,natt_H
						nCHlist=nCHlist+1
						CHlist(nCHlist)=H_list(iatt_H)
						eqvlist_H(iatt_H,neqvlist_H)=H_list(iatt_H)
					end do
				end if
			end if
		end if 
	end do
	!If there are atoms have not appeared in the equivalence constraint list, add them to a slot
	do icen=1,nfitcen
		if (all(eqvlist_H/=icen)) then
			neqvlist_H=neqvlist_H+1
			eqvlistlen_H(neqvlist_H)=1
			eqvlist_H(1,neqvlist_H)=icen
		end if
	end do
end if

!By default, equivalence constraint is not employed, therefore each atom occupies a slot
neqvlist=nfitcen
eqvlistlen=1
forall(i=1:neqvlist) eqvlist(1,i)=i


!------ Start charge fitting now!!!
if (isel==2) then !One-stage ESP fitting

	!Setting up charge constraint
	if (ichgcons==1) then !Load setting from plain text file
		write(*,"(' Loading charge constraint setting from ',a)") trim(chgconsfilepath)
		open(10,file=chgconsfilepath,status="old")
		nchgcons=totlinenum(10,1) !The number of charge constraint terms
		allocate(chgconsnatm(nchgcons),chgconsval(nchgcons),chgconsatm(nfitcen,nchgcons))
		rewind(10)
		do icons=1,nchgcons
			read(10,"(a)") c2000tmp
			isep=index(trim(c2000tmp),' ',back=.true.)
			read(c2000tmp(isep+1:),*) chgconsval(icons)
			call str2arr(c2000tmp(:isep-1),ntmp)
			chgconsnatm(icons)=ntmp
			call str2arr(c2000tmp(:isep-1),ntmp,chgconsatm(1:ntmp,icons))
			write(*,"(' Charge constraint',i4,':',i4,' atoms, charge:',f12.6)") icons,ntmp,chgconsval(icons)
		end do
		close(10)
	else !No charge constraint
		nchgcons=0
		allocate(chgconsnatm(nchgcons),chgconsval(nchgcons),chgconsatm(nfitcen,nchgcons))
	end if
	
	!Setting up equivalence constraint
	if (ieqvcons==1) then !Load setting from plain text file
		eqvlist=0
		write(*,"(' Loading equivalence constraint setting from ',a)") trim(eqvconsfilepath)
		open(10,file=eqvconsfilepath,status="old")
		neqvlist=totlinenum(10,1)
		rewind(10)
		do ieqv=1,neqvlist
			read(10,"(a)") c2000tmp
			call str2arr(c2000tmp,ntmp)
			eqvlistlen(ieqv)=ntmp
			call str2arr(c2000tmp,ntmp,eqvlist(1:ntmp,ieqv))
		end do
		close(10)
		!If there are atoms have not appeared in the equivalence constraint list, add them to a slot
		do icen=1,nfitcen
			if (all(eqvlist/=icen)) then
				neqvlist=neqvlist+1
				eqvlistlen(neqvlist)=1
				eqvlist(1,neqvlist)=icen
			end if
		end do
	else if (ieqvcons==2) then !Impose equivalence constraint on hydrogens in each CH2 and CH3 group
		neqvlist=neqvlist_H
		eqvlistlen=eqvlistlen_H
		eqvlist=eqvlist_H
	end if
	
	call showeqvcons(neqvlist,eqvlistlen,eqvlist)
	write(*,*)
	write(*,*) "One-stage restrainted ESP fitting iteration has started"
	call RESPiter(hyper_a,hyper_b,nconf,confweight,nfitcen,fitcen,maxESPpt,nESPpt,ESPpt,ESPptval,&
	atmchg,nchgcons,chgconsnatm,chgconsatm,chgconsval,neqvlist,eqvlistlen,eqvlist,iESPtype)

else if (isel==1) then !Standard RESP fitting

	write(*,*) "**** Stage 1: RESP fitting under weak hyperbolic penalty"
	write(*,*) "No charge constraint and equivalence constraint are imposed in this stage"
	nchgcons=0
	allocate(chgconsnatm(nchgcons),chgconsval(nchgcons),chgconsatm(nfitcen,nchgcons))
	call RESPiter(hyper_a_1,hyper_b,nconf,confweight,nfitcen,fitcen,maxESPpt,nESPpt,ESPpt,ESPptval,&
	atmchg_stage1,nchgcons,chgconsnatm,chgconsatm,chgconsval,neqvlist,eqvlistlen,eqvlist,iESPtype)
	
	write(*,*)
	write(*,*) "**** Stage 2: RESP fitting under strong hyperbolic penalty"
	if (nCHlist>0) then
		neqvlist=neqvlist_H
		eqvlistlen=eqvlistlen_H
		eqvlist=eqvlist_H
		! Equivalence constraint: Hydrogens in -CH3, =CH2, -CH2-
		call showeqvcons(neqvlist,eqvlistlen,eqvlist)
		write(*,*) "Fitting objects: sp3 carbons, methyl carbons and hydrogens attached to them"
		write(*,*) "Indices of these atoms:"
		do i=1,nCHlist
			write(*,"(i5,a)",advance='no') CHlist(i),a(CHlist(i))%name
			if (mod(i,10)==0) write(*,*)
		end do
		
		deallocate(chgconsatm,chgconsnatm,chgconsval)
		nchgcons=nfitcen-nCHlist
		allocate(chgconsnatm(nchgcons),chgconsval(nchgcons),chgconsatm(nfitcen,nchgcons))
		chgconsnatm=1
		icons=0
		do icen=1,nfitcen
			if (all(CHlist(1:nCHlist)/=icen)) then !Charge of this atom should be fixed
				icons=icons+1
				chgconsval(icons)=atmchg_stage1(icen)
				chgconsatm(1,icons)=icen
			end if
		end do
		!If current folder has chgcons_stage2.txt, then export constrainted charges in the RESP stage 2 to this file
		inquire(file="chgcons_stage2.txt",exist=alive)
		if (alive) then
			open(10,file="chgcons_stage2.txt",status="replace")
			do idx=1,icons
				write(10,"(i6,f12.6)") chgconsatm(1,idx),chgconsval(idx)
			end do
			close(10)
			write(*,"(/,a)") " The charges that kept fixed at stage 2 of RESP fitting &
			has been written to chgcons_stage2.txt in current folder"
		end if
		
		write(*,*)
		call RESPiter(hyper_a_2,hyper_b,nconf,confweight,nfitcen,fitcen,maxESPpt,nESPpt,ESPpt,ESPptval,&
		atmchg,nchgcons,chgconsnatm,chgconsatm,chgconsval,neqvlist,eqvlistlen,eqvlist,iESPtype)
	else
		write(*,*) "Stage 2 of standard RESP fitting is skipped since no atom needs to be fitted"
		atmchg=atmchg_stage1
	end if
	
end if


!Output summary
write(*,*)
write(*,*) "  Center      Charge"
do icen=1,nfitcen
	write(*,"(i6,'(',a,')',f12.6)") icen,ind2name(a(icen)%index),atmchg(icen)
end do
write(*,"(' Sum of charges:',f12.6)") sum(atmchg)

!Calculate RMSE and RRMSE
weiRMSE=0;weiRRMSE=0
do iconf=1,nconf
	RMSE=0D0
	do ipt=1,nESPpt(iconf)
		atmchgesp=0D0
		do icen=1,nfitcen
			dis=dsqrt( sum((ESPpt(:,ipt,iconf)-fitcen(:,icen,iconf))**2) )
			atmchgesp=atmchgesp+atmchg(icen)/dis
		end do
		RMSE=RMSE+(ESPptval(ipt,iconf)-atmchgesp)**2
	end do
	RRMSE=dsqrt(RMSE/sum(ESPptval(1:nESPpt(iconf),iconf)**2))
	RMSE=dsqrt(RMSE/nESPpt(iconf))
	if (nconf>1) then
		write(*,"(' Conformer:',i5,'   RMSE:',f12.6,'   RRMSE:',f12.6)") iconf,RMSE,RRMSE
		weiRMSE=weiRMSE+RMSE*confweight(iconf)
		weiRRMSE=weiRRMSE+RRMSE*confweight(iconf)
	else
		write(*,"(' RMSE:',f12.6,'   RRMSE:',f12.6)") RMSE,RRMSE
	end if
end do
if (nconf>1) write(*,"(' Weighted RMSE:',f12.6,'   Weighted RRMSE',f12.6)") weiRMSE,weiRRMSE

!Show fragment charge
if (allocated(frag1)) write(*,"(/,' Fragment charge:',f12.6)") sum(atmchg(frag1))

!Export .chg file
call path2filename(firstfilename,outchgfilepath)
write(*,"(/,a)") " If exporting atom coordinates with RESP charges to "//trim(outchgfilepath)//".chg in current folder? (y/n)"
read(*,*) selectyn
if (selectyn=='y'.or.selectyn=="Y") then
	open(10,file=trim(outchgfilepath)//".chg",status="replace")
	do i=1,ncenter
		write(10,"(a4,4f12.6)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a,atmchg(i)
	end do
	close(10)
	write(*,"(a)") " Result have been saved to "//trim(outchgfilepath)//".chg in current folder"
	write(*,"(a)") " Columns from 1 to 5 are name,X,Y,Z,charge respectively, unit is Angstrom"
end if

deallocate(fitcen,fitcenvdwr,nESPpt,ESPptval,ESPpt,chgconsatm,chgconsnatm,chgconsval)

end do maincyc

end subroutine


!!------ Perform RESP iteration
subroutine RESPiter(hyper_a,hyper_b,nconf,confweight,nfitcen,fitcen,maxESPpt,nESPpt,ESPpt,ESPptval,&
atmchg,nchgcons,chgconsnatm,chgconsatm,chgconsval,neqvlist,eqvlistlen,eqvlist,iESPtype)
use defvar
use util
implicit real*8 (a-h,o-z)
real*8 hyper_a,hyper_b
integer nconf,nfitcen,maxESPpt,nchgcons,neqvlist,iESPtype
integer nESPpt(nconf)
real*8 confweight(nconf),fitcen(3,nfitcen,nconf),ESPpt(3,maxESPpt,nconf),ESPptval(maxESPpt,nconf),atmchg(nfitcen),chgconsval(nchgcons)
integer chgconsnatm(nchgcons),chgconsatm(ncenter,nchgcons),eqvlistlen(ncenter),eqvlist(10,ncenter)
real*8,allocatable :: Bvec(:),Amat(:,:),Amat_bk(:,:),Amat_tmp(:,:),Amatinv(:,:),qvec(:),qvec_old(:)
real*8,allocatable :: Bveceqv(:),Amateqv(:,:),Amateqvinv(:,:),qveceqv(:) !The counterpart of Amat,Bvec,qvec when considering equivalence contraint
integer :: maxiter=30
real*8 :: convcrit=0.000001D0

if (hyper_a==0) then
	write(*,*) "Since restraint strength was set to zero, no iteration will be carried out"
else
	write(*,"(' Convergence criterion:',f11.8)") convcrit
	write(*,"(' Hyperbolic restraint strength (a):',f9.6,'    Tightness (b):',f9.6)") hyper_a,hyper_b
end if

! write(*,*) nESPpt,nconf
! write(*,*) neqvlist,nchgcons,nfitcen
! write(*,*) "eqv"
! do ieqv=1,neqvlist
! 	write(*,*) ieqv,eqvlist(1:eqvlistlen(ieqv),ieqv)
! end do
! write(*,*) "chgcons"
! do icons=1,nchgcons
! 	write(*,*) icons,chgconsatm(1:chgconsnatm(icons),icons),chgconsval(icons)
! end do
! write(*,*) "confweight"
! do iconf=1,nconf
! 	write(*,*) iconf,confweight(iconf)
! end do
! pause

matdim=nfitcen+nchgcons+1
allocate(Bvec(matdim),Amat(matdim,matdim),Amat_bk(matdim,matdim),Amatinv(matdim,matdim),qvec(matdim),qvec_old(matdim))

!Forming Amat
Amat=0D0
do icen=1,nfitcen
	do jcen=icen,nfitcen
		do iconf=1,nconf
			tmp=0
			do ipt=1,nESPpt(iconf)
				dis1=dsqrt( sum((ESPpt(:,ipt,iconf)-fitcen(:,icen,iconf))**2) )
				dis2=dsqrt( sum((ESPpt(:,ipt,iconf)-fitcen(:,jcen,iconf))**2) )
				tmp=tmp+1D0/dis1/dis2
			end do
			Amat(icen,jcen)=Amat(icen,jcen)+tmp*confweight(iconf)
		end do
		Amat(jcen,icen)=Amat(icen,jcen)
	end do
end do
Amat(nfitcen+1,:nfitcen)=1D0
Amat(:nfitcen,nfitcen+1)=1D0
do icons=1,nchgcons
	do idx=1,chgconsnatm(icons)
		icen=chgconsatm(idx,icons)
		Amat(icen,nfitcen+1+icons)=1
		Amat(nfitcen+1+icons,icen)=1
	end do
end do
Amat_bk=Amat !Backup, its diagonal terms will be used during RESP calculation

!Forming Bvec
Bvec=0D0
do icen=1,nfitcen
	do iconf=1,nconf
		tmp=0
		do ipt=1,nESPpt(iconf)
			dis=dsqrt( sum((ESPpt(:,ipt,iconf)-fitcen(:,icen,iconf))**2) )
			tmp=tmp+ESPptval(ipt,iconf)/dis
		end do
		Bvec(icen)=Bvec(icen)+tmp*confweight(iconf)
	end do
end do
if (iESPtype==1) then !Take nuclei into account
	Bvec(nfitcen+1)=sum(a(:)%charge)-nelec
else if (iESPtype==2) then !Do not take nuclei into account
	Bvec(nfitcen+1)=-nelec
else if (iESPtype==3) then !Electronic transition density
	Bvec(nfitcen+1)=0
end if
do icons=1,nchgcons
	Bvec(nfitcen+1+icons)=chgconsval(icons)
end do

matdimeqv=neqvlist+nchgcons+1
allocate(Amateqv(matdimeqv,matdimeqv),Amateqvinv(matdimeqv,matdimeqv),Bveceqv(matdimeqv),qveceqv(matdimeqv))
allocate(Amat_tmp(matdimeqv,matdim)) !Used to temporarily store Amat with contracted rows due to eqv. cons.
qvec=0

do iter=1,maxiter
	qvec_old=qvec
	
	!Only diagonal terms have an additional term due to hyperbolic restraint
	do icen=1,nfitcen
		if (a(icen)%index==1) cycle !Penalty function is only applied to non-hydrogen atoms
		Amat(icen,icen)=Amat_bk(icen,icen)+ hyper_a/dsqrt(qvec(icen)**2+hyper_b**2)
	end do
	
	!Construct matrices with consideration of equivalence constraint
	Amat_tmp=0
	Amateqv=0
	Bveceqv=0
	!Contract row of Amat to form Amat_tmp and meantime contract Bvec to form Bveceqv
	do ieqv=1,neqvlist
		do idx=1,eqvlistlen(ieqv)
			irow=eqvlist(idx,ieqv)
			Amat_tmp(ieqv,:)=Amat_tmp(ieqv,:)+Amat(irow,:)
			Bveceqv(ieqv)=Bveceqv(ieqv)+Bvec(irow)
		end do
	end do
	do idx=1,nchgcons+1
		Amat_tmp(neqvlist+idx,:)=Amat(nfitcen+idx,:)
		Bveceqv(neqvlist+idx)=Bvec(nfitcen+idx)
	end do
	!Contract column of Amat_tmp to form Amateqv
	do ieqv=1,neqvlist
		do idx=1,eqvlistlen(ieqv)
			icol=eqvlist(idx,ieqv)
			Amateqv(:,ieqv)=Amateqv(:,ieqv)+Amat_tmp(:,icol)
		end do
	end do
	do idx=1,nchgcons+1
		Amateqv(:,neqvlist+idx)=Amat_tmp(:,nfitcen+idx)
	end do
	
! 	call showmatgau(Amat,"Amat",form="f12.6")
! 	call showmatgau(Amateqv,"Amatvec",form="f12.6")
 	!write(*,"(f12.6)") Bvec
! 	write(*,*)
! 	write(*,"(f12.6)") Bveceqv
! 	pause
	
	!Evaluate and update charges
	Amateqvinv=invmat(Amateqv,matdimeqv)
	qveceqv=matmul(Amateqvinv,Bveceqv)
	do ieqv=1,neqvlist
		do idx=1,eqvlistlen(ieqv)
			qvec(eqvlist(idx,ieqv))=qveceqv(ieqv)
		end do
	end do
	
	if (hyper_a==0) exit
	deltamax=maxval(abs(qvec(1:nfitcen)-qvec_old(1:nfitcen)))
	write(*,"(' Iter:',i4,'   Maximum charge variation:',f16.10)") iter,deltamax
	if (deltamax<convcrit) exit
	
end do
if (iter==maxiter+1) then
	write(*,*) "Error: Convergence failed!"
	write(*,*) "Press ENTER button to return"
	read(*,*)
	return
else if (hyper_a/=0) then
	write(*,*) "Successfully converged!"
end if

atmchg=qvec(1:nfitcen)
end subroutine

!!----- Show atom equivalence constraint in ESP fitting
subroutine showeqvcons(neqvlist,eqvlistlen,eqvlist)
use defvar
implicit real*8 (a-h,o-z)
integer neqvlist,eqvlistlen(ncenter),eqvlist(10,ncenter)
if (any(eqvlistlen(1:neqvlist)>1)) then
	write(*,*) "Atoms equivalence constraint imposed in this fitting stage:"
	icons=0
	do ieqv=1,neqvlist
		if (eqvlistlen(ieqv)>1) then
			icons=icons+1
			write(*,"(' Constraint',i4,':')",advance='no') icons
			do idx=1,eqvlistlen(ieqv)
				iatm=eqvlist(idx,ieqv)
				write(*,"(i5,'(',a,')')",advance='no') iatm,a(iatm)%name
			end do
			write(*,*)
		end if
	end do
else
	write(*,*) "No atoms equivalence constraint is imposed in this fitting stage"
end if
end subroutine






!!------------ Calculate MK and CHELPG charges
! igridtype=1:MK   igridtype=2:CHELPG
! This module does not have wealth of features as RESP routine, but it is very clear and have these special points:
! (1) Support additional fitting center (2) Support external fitting points (3) Fitting points could be exported
! (4) Can calculate TrEsp (transition charge from electrostatic potential), see manual
subroutine fitESP(igridtype)
use util
use defvar
use function
implicit real*8 (a-h,o-z)
integer igridtype
character*200 addcenfile,extptfile,chgfile,gauoutfilepath,outchgfilepath
character selectyn,c80tmp*80,c2000tmp*2000
integer :: iESPtype=1,ioutfitptval=0,iradiisel=1
!About distribution of fitting points
integer :: nMKlayer=4
integer :: MKatmlist(ncenter) !Record index of atoms used to construct MK fitting points
real*8 :: espfitvdwr(nelesupp),sclvdwlayer(100)=(/1.4D0,1.6D0,1.8D0,2.0D0,(0D0,i=5,100)/)
real*8 :: fitspc=0.566917796573677D0 !0.3/b2a, spacing between grid for CHELPG
real*8 :: extdis=5.29123276802099D0 !2.8/b2a, extend 2.8 Angstrom to each side for CHELPG
real*8 :: MKptdens=1.68017136515525D0 !6D0*b2a**2, 6.0 Angstrom**2 point density per for MK. Multiply by b2a**2 to convert to Bohr**2
!Arrays used in ESP fitting
real*8,allocatable :: ESPptval(:),ESPpt(:,:),fitcen(:,:) !x/y/z,index
real*8,allocatable :: Bvec(:),Amat(:,:),Amatinv(:,:),qvec(:)
real*8,allocatable :: fitcenvdwr(:)
real*8,allocatable :: cenchg(:) !Final result
real*8,allocatable :: ESPerr(:)

iaddcen=0 !If give additional centers
naddcen=0
iuseextpt=0 !If use external points
iskipespcalc=0 !If read ESP from external file directly rather than calculate here
iloadgau=0
iloadchg=0
nMKatoms=ncenter !The number of atoms used to construct MK fitting points
forall(i=1:ncenter) MKatmlist(i)=i

10 do while(.true.)
	write(*,*)
	if (igridtype==1) write(*,*) "              ------------ Calculation of MK charges ------------"
	if (igridtype==2) write(*,*) "            ------------ Calculation of CHELPG charges ------------"
	if (iloadchg==0) write(*,*) "-3 Toggle using atomic charges in external file, current: No"
	if (iloadchg==1) write(*,*) "-3 Toggle using atomic charges in external file, current: Yes"
	if (iaddcen==0) write(*,*) "-2 Toggle loading additional fitting centers from external file, current: No"
	if (iaddcen==1) write(*,*) "-2 Toggle loading additional fitting centers from external file, current: Yes"
	if (iuseextpt==0) write(*,*) "-1 Toggle using fitting points in external file, current: No"
	if (iuseextpt==1) write(*,*) "-1 Toggle using fitting points in external file, current: Yes"
	write(*,*) "0 Return"
	write(*,*) "1 Start calculation!"
	if (iuseextpt==0) then
		if (igridtype==1) then
			write(*,"(' 2 Set number of points per Angstrom^2, current:',f10.3)") MKptdens/b2a**2 !Temporary convert to Angstrom**2 for convention
			write(*,"(' 3 Set number and scale factors of layers of MK fitting points')")
			if (nMKatoms==ncenter) then
				write(*,*) "4 Choose the atoms used for generating fitting points, current: All atoms"
			else
				write(*,"(a,i5,a)") " 4 Choose the atoms used for generating fitting points, current:",nMKatoms," atoms"
			end if
		else if (igridtype==2) then
			write(*,"(' 2 Set grid spacing, current:',f7.3,' Bohr (',f7.3,' Angstrom)')") fitspc,fitspc*b2a
			write(*,"(' 3 Set box extension, current:',f7.3,' Bohr (',f7.3,' Angstrom)')") extdis,extdis*b2a
		end if
	end if
	if (iESPtype==1) write(*,*) "5 Choose ESP type, current: Nuclear + Electronic"
	if (iESPtype==2) write(*,*) "5 Choose ESP type, current: Electronic"
	if (iESPtype==3) write(*,*) "5 Choose ESP type, current: Transition electronic"
	if (ioutfitptval==0) write(*,*) "6 Toggle if exporting fitting points with ESP after the task, current: No"
	if (ioutfitptval==1) write(*,*) "6 Toggle if exporting fitting points with ESP after the task, current: Yes"
	!This feature is hidden to user. Its default status is "No", if you want to use it, choose it and then input an output file of e.g. pop=MK/CHELPG task with IOp(6/33=2)
! 	if (iloadgau==0) write(*,"(a)") " 7 Toggle if reading fitting points and ESP values from Gaussian output file of ESP fitting task with IOp(6/33=2) keyword, current: No"
! 	if (iloadgau==1) write(*,"(a)") " 7 Toggle if reading fitting points and ESP values from Gaussian output file of ESP fitting task with IOp(6/33=2) keyword, current: Yes"
	if (iradiisel==1) write(*,*) "10 Choose the atomic radii used in fitting, current: Automatic"
	if (iradiisel==2) write(*,*) "10 Choose the atomic radii used in fitting, current: Scaled UFF"
	if (iradiisel==3) write(*,*) "10 Choose the atomic radii used in fitting, current: Customized"
	read(*,*) isel
	
	if (isel==-3) then
		if (iloadchg==1) then
			iloadchg=0
		else
			write(*,*) "Input the path of the .chg file containing atomic charges, e.g. C:\H2O.chg"
			do while(.true.)
				read(*,"(a)") chgfile
				inquire(file=chgfile,exist=alive)
				if (alive) exit
				write(*,*) "Cannot find the file, input again"
			end do
			iloadchg=1
			write(*,"(a)") " OK, the atomic charges contained in this file will be directly used and no ESP fitting charges will be yielded"
		end if
	else if (isel==-2) then
		if (iaddcen==1) then
			iaddcen=0
		else
			write(*,"(a)") " Input the path of the file recording coordinates of additional fitting centers, e.g. C:\ll_sunshine\Riko.txt"
			do while(.true.)
				read(*,"(a)") addcenfile
				inquire(file=addcenfile,exist=alive)
				if (alive) exit
				write(*,*) "Cannot find the file, input again"
			end do
			iaddcen=1
			write(*,*) "Done!"
		end if
	else if (isel==-1) then
		if (iuseextpt==1) then
			iuseextpt=0
		else
			write(*,"(a)") " Input the path of the file recording coordinates of ESP fitting points, e.g. C:\ll_sunshine\You.txt"
			do while(.true.)
				read(*,"(a)") extptfile
				inquire(file=extptfile,exist=alive)
				if (alive) exit
				write(*,*) "Cannot find the file, input again"
			end do
			iuseextpt=1
			write(*,*) "OK, the points recorded in this file will be used as fitting points"
		end if
	else if (isel==0) then
		Return
	else if (isel==1) then
		exit
	else if (isel==2) then
		if (igridtype==1) then
			write(*,*) "Input a number (in per Angstrom^2), e.g. 6.0"
			read(*,*) MKptdens
			MKptdens=MKptdens*b2a**2 !Convert to per Bohr^2
		else if (igridtype==2) then
			write(*,*) "Input a value in Bohr, e.g. 0.5"
			read(*,*) fitspc
		end if
	else if (isel==3) then
		if (igridtype==1) then
			write(*,*) "Current layers of MK fitting points:"
			do ilayer=1,nMKlayer
				write(*,"(' Layer',i3,':',f8.4)") ilayer,sclvdwlayer(ilayer)
			end do
			nMKlayer=0
			write(*,*)
			do while(.true.)
				nMKlayer=nMKlayer+1
				write(*,"(' Input scale factor (w.r.t atomic vdW radius) for layer',i4)") nMKlayer
				write(*,*) "If input ""q"", the setting will be finished"
				if (sclvdwlayer(nMKlayer)/=0) write(*,"(' If press ENTER directly, current value',f8.4,' will be retained')")
				read(*,"(a)") c80tmp
				if (index(c80tmp,'q')/=0) then
					sclvdwlayer(nMKlayer:)=0
					nMKlayer=nMKlayer-1
					exit
				end if
				if (c80tmp/=" ") read(c80tmp,*) sclvdwlayer(nMKlayer)
			end do
			write(*,*) "Current layer of MK fitting points:"
			do ilayer=1,nMKlayer
				write(*,"(' Layer',i3,'  Scale factor:',f8.4)") ilayer,sclvdwlayer(ilayer)
			end do
		else if (igridtype==2) then
			write(*,*) "Input a value in Bohr, e.g. 6.5"
			read(*,*) extdis
		end if
	else if (isel==4) then
		write(*,*) "Input the indices of the atoms used to construct MK fitting points"
		write(*,*) "e.g. 1-5,8,10-12"
		read(*,"(a)") c2000tmp
		call str2arr(c2000tmp,nMKatoms,MKatmlist)
		write(*,*) "Done!"
	else if (isel==5) then
		write(*,*) "Choose ESP type:"
		write(*,*) "1 Nuclear + Electronic"
		write(*,*) "2 Electronic only"
		write(*,*) "3 Transition electronic (Mainly used for deriving TrEsp charges)"
		read(*,*) iESPtype
	else if (isel==6) then
		if (ioutfitptval==1) then
			ioutfitptval=0
		else if (ioutfitptval==0) then
			ioutfitptval=1
		end if
	else if (isel==7) then
		if (iloadgau==1) then
			iloadgau=0
		else if (iloadgau==0) then
			iloadgau=1
			write(*,"(a)") " Input file path of the Gaussian pop=MK/CHELPG task with IOp(6/33=2) keyword, e.g. C:\tsushima_yoshiko.out. &
			Note that the geometry used in the Gaussian calculation must be exactly identical to current geometry"
			do while(.true.)
				read(*,"(a)") gauoutfilepath
				inquire(file=gauoutfilepath,exist=alive)
				if (alive) exit
				write(*,*) "Cannot find the file, input again"
			end do
			write(*,"(a)") " OK, ESP fitting points with ESP values will be directly loaded from this file during calculation"
		end if
	else if (isel==10) then
		write(*,*) "Please choose the way of determining atomic radii:"
		write(*,"(a)") " 1 Automatic (The radii actually used will depend on the way of distributing fitting points)"
		write(*,"(a)") " 2 UFF radii scaled by 1/1.2, defined for entire periodic table"
		write(*,"(a)") " 3 Customized (The radii will be loaded from external file during fitting)"
		read(*,*) iradiisel
	end if
end do


!Generate fitting centers, fitting points and calculate ESP value using internal code
if (iloadgau==0) then
	!Set and check sanity of vdW radii used in fitting
	espfitvdwr=-1
	if (iradiisel==1) then
		call setESPfitvdwr(igridtype,espfitvdwr)
	else if (iradiisel==2) then !Scaled UFF
		call setESPfitvdwr(0,espfitvdwr)
	else if (iradiisel==3) then !Customize radii
		call setESPfitvdwr(-1,espfitvdwr)
	end if

	!Set total number of fitting centers
	naddcen=0
	if (iaddcen==1) then !Load additional fitting centers
		open(10,file=addcenfile,status="old")
		read(10,*) naddcen
	end if
	nfitcen=ncenter+naddcen
	allocate(fitcen(3,nfitcen),fitcenvdwr(nfitcen))
	!Generate positions and vdW radii of fitting centers
	do iatm=1,ncenter
		fitcen(1,iatm)=a(iatm)%x
		fitcen(2,iatm)=a(iatm)%y
		fitcen(3,iatm)=a(iatm)%z
		fitcenvdwr(iatm)=espfitvdwr(a(iatm)%index) !vdW radius for each fitting center
	end do
	if (iaddcen==1) then
		do icen=ncenter+1,ncenter+naddcen
			read(10,*) fitcen(:,icen)
! 			write(*,"(' Please input radius for extra fitting center',i4,' (Bohr), e.g. 0.4')") icen
! 			read(*,*) fitcenvdwr(icen)
			fitcenvdwr(icen)=0 !The important thing is reproducing ESP on molecular surface, therefore extra point should not affect construction of fitting points
		end do
		close(10)
	end if

	!Generate position of fitting points
	write(*,*)
	if (iuseextpt==0) then !Count number and generate coordinates of fitting points
		if (igridtype==1) then !MK
			allocate(ESPpt(3,0)) !Temporarily assign a minimal length
			call setMKpt(1,nfitcen,fitcen,fitcenvdwr,nESPpt,ESPpt,sclvdwlayer,MKptdens,nMKlayer,nMKatoms,MKatmlist) !The returned nESPpt is upper limit
			deallocate(ESPpt);allocate(ESPptval(nESPpt),ESPpt(3,nESPpt))
			call setMKpt(2,nfitcen,fitcen,fitcenvdwr,nESPpt,ESPpt,sclvdwlayer,MKptdens,nMKlayer,nMKatoms,MKatmlist)
		else if (igridtype==2) then !CHELPG
			allocate(ESPpt(3,0)) !Temporarily assign a minimal length
			call setCHELPGpt(1,nfitcen,fitcen,fitcenvdwr,nESPpt,ESPpt,extdis,fitspc) !Return actual nESPpt
			deallocate(ESPpt);allocate(ESPptval(nESPpt),ESPpt(3,nESPpt))
			call setCHELPGpt(2,nfitcen,fitcen,fitcenvdwr,nESPpt,ESPpt,extdis,fitspc)
		end if
	else if (iuseextpt==1) then !Directly use fitting points in external file. The ESP values may or may not be provided in the file
		open(10,file=extptfile,status="old")
		read(10,*) nESPpt
		if (nESPpt<0) then
			iskipespcalc=1 !If the number of fitting points is negative, that means the fourth column records ESP value and needn't to be recalculated
			write(*,*) "ESP value of all fitting points are read from external file directly"
		end if
		nESPpt=abs(nESPpt)
		write(*,"(' Total number of fitting points used:',i10)") nESPpt
		allocate(ESPptval(nESPpt),ESPpt(3,nESPpt))
		do i=1,nESPpt
			if (iskipespcalc==0) read(10,*) ESPpt(:,i)
			if (iskipespcalc==1) read(10,*) ESPpt(:,i),ESPptval(i)
		end do
		close(10)
	end if

	!Generate ESP value of fitting points
	if (iskipespcalc==0) call fitESP_calcESP(1,iESPtype,nESPpt,ESPpt,ESPptval,filename)

else !Reading ESP and coordinates of fitting points from Gaussian Iop(6/33=2) output. File containing geometry must be loaded to provide atom coordinates
	write(*,"(a)") " Loading ESP data from "//trim(gauoutfilepath)
	call loadgauESP_num(gauoutfilepath,nfitcen,nESPpt)
	allocate(fitcen(3,nfitcen),ESPptval(nESPpt),ESPpt(3,nESPpt))
	call loadgauESP(gauoutfilepath,nfitcen,fitcen,nESPpt,ESPpt,ESPptval)
end if

allocate(cenchg(nfitcen))
if (iloadchg==0) then
	!Calculate ESP fitting charges. See original paper of MK for detail of algorithem
	matdim=nfitcen+1
	allocate(Bvec(matdim),Amat(matdim,matdim),Amatinv(matdim,matdim),qvec(matdim))
	!Forming Amat
	Amat=0D0
	do icen=1,nfitcen
		do jcen=icen,nfitcen
			do ipt=1,nESPpt
				dis1=dsqrt( sum((ESPpt(:,ipt)-fitcen(:,icen))**2) )
				dis2=dsqrt( sum((ESPpt(:,ipt)-fitcen(:,jcen))**2) )
				Amat(icen,jcen)=Amat(icen,jcen)+1D0/dis1/dis2
			end do
			Amat(jcen,icen)=Amat(icen,jcen)
		end do
	end do
	Amat(matdim,:nfitcen)=1D0
	Amat(:nfitcen,matdim)=1D0
	!Forming Bvec
	Bvec=0D0
	do icen=1,nfitcen
		do ipt=1,nESPpt
			dis=dsqrt( sum((ESPpt(:,ipt)-fitcen(:,icen))**2) )
			Bvec(icen)=Bvec(icen)+ESPptval(ipt)/dis
		end do
	end do
	if (iESPtype==1) then !Take nuclei into account
		Bvec(matdim)=sum(a(:)%charge)-nelec !Net charge of the system
	else if (iESPtype==2) then !Do not take nuclei into account
		Bvec(matdim)=-nelec
	else if (iESPtype==3) then !Electronic transition density
		Bvec(matdim)=0
	end if
	Amatinv=invmat(Amat,matdim)
	qvec=matmul(Amatinv,Bvec)
	cenchg=qvec(1:nfitcen)
	deallocate(Bvec,Amat,Amatinv,qvec)
else if (iloadchg==1) then
	!Directly load charges of fitting centers from external files
	write(*,"(a)") " Loading charges of fitting centers from "//trim(chgfile)
	open(10,file=chgfile,status="old")
	do icen=1,nfitcen
		read(10,*) c80tmp,tmp1,tmp2,tmp3,cenchg(icen)
	end do
	close(10)
end if

!Output summary
write(*,*)
write(*,*) "  Center      Charge"
do i=1,ncenter
	write(*,"(i6,'(',a,')',f12.6)") i,ind2name(a(i)%index),cenchg(i)
end do
do i=ncenter+1,ncenter+naddcen
	write(*,"(i6,4x,f12.6)") i,cenchg(i)
end do
write(*,"(' Sum of charges:',f12.6)") sum(cenchg(1:nfitcen))

!Calculate RMSE and RRMSE
if (allocated(ESPerr)) deallocate(ESPerr)
allocate(ESPerr(nESPpt))
RMSE=0D0
do ipt=1,nESPpt
	atmchgesp=0D0
	do icen=1,nfitcen
		dis=dsqrt( sum((ESPpt(:,ipt)-fitcen(:,icen))**2) )
		atmchgesp=atmchgesp+cenchg(icen)/dis
	end do
	ESPerr(ipt)=abs(ESPptval(ipt)-atmchgesp)
	RMSE=RMSE+(ESPptval(ipt)-atmchgesp)**2
end do
RRMSE=dsqrt(RMSE/sum(ESPptval(1:nESPpt)**2))
RMSE=dsqrt(RMSE/nESPpt)
write(*,"(' RMSE:',f12.6,'   RRMSE:',f12.6)") RMSE,RRMSE

!Show fragment charge
if (allocated(frag1)) write(*,"(/,' Fragment charge:',f12.6)") sum(cenchg(frag1))
write(*,*)

!Exporting fitting points with ESP values
if (ioutfitptval==1) then
	do while(.true.)
		write(*,*) "0 Continue"
		write(*,*) "1 Export fitting points with ESP value to ESPfitpt.txt in current folder"
		write(*,*) "2 Export fitting points with ESP value to ESPfitpt.pqr in current folder"
		write(*,"(a)") " 3 Export fitting points with ESP reproduction error to ESPerr.pqr in current folder"
		read(*,*) ides
		if (ides==0) then
			exit
		else if (ides==1) then
			open(10,file="ESPfitpt.txt",status="replace")
			write(10,*) nESPpt
			do ipt=1,nESPpt
				write(10,"(3f12.6,f14.8)") ESPpt(:,ipt),ESPptval(ipt)
			end do
			write(*,*) "Done! Fitting points have been exported to ESPfitpt.txt in current folder"
			write(*,"(a)") " All units are in a.u. The first line shows the number of fitting points, &
			the first three columns are X,Y,Z coordinates, the last column corresponds to ESP value"
			close(10)
		else if (ides==2) then
			open(10,file="ESPfitpt.pqr",status="replace")
			do ipt=1,nESPpt
				write(10,"(a6,i5,1x,a4,1x,a3, 1x,a1,i4,4x,3f8.3,f13.8,f9.3,a2)") &
					"HETATM",ipt,' '//"O "//' ',"MOL",'A',1,ESPpt(:,ipt)*b2a,ESPptval(ipt)*au2kcal,0.1D0,"O "
			end do
			write(*,"(a)") " Done! Fitting points have been exported to ESPfitpt.pqr in current folder. &
			The ""charge"" column in this file corresponds to ESP value in kcal/mol. The radius column is meaningless"
			close(10)
		else if (ides==3) then
			open(10,file="ESPerr.pqr",status="replace")
			do ipt=1,nESPpt
				write(10,"(a6,i5,1x,a4,1x,a3, 1x,a1,i4,4x,3f8.3,f13.8,f9.3,a2)") &
					"HETATM",ipt,' '//"O "//' ',"MOL",'A',1,ESPpt(:,ipt)*b2a,ESPerr(ipt)*au2kcal,0.1D0,"O "
			end do
			write(*,"(a)") " Done! Fitting points have been exported to ESPerr.pqr in current folder. &
			The ""charge"" column in this file corresponds to absolute different (in kcal/mol) between the exactly evaluated ESP &
			and that evaluated based on atomic charges. The radius column is meaningless"
			close(10)
		end if
		write(*,*)
	end do
end if


!Export .chg file. Additional centers are denoted as Bq
call path2filename(firstfilename,outchgfilepath)
write(*,"(a)") " If output atom coordinates with charges to "//trim(outchgfilepath)//".chg in current folder? (y/n)"
read(*,*) selectyn
if (selectyn=='y'.or.selectyn=="Y") then
	open(10,file=trim(outchgfilepath)//".chg",status="replace")
	do icen=1,nfitcen
		if (icen<=ncenter) then
			write(10,"(a,2x,4f12.6)") a(icen)%name,fitcen(:,icen)*b2a,cenchg(icen)
		else
			write(10,"(a,2x,4f12.6)") "X ",fitcen(:,icen)*b2a,cenchg(icen)
		end if
	end do
	close(10)
	write(*,"(a)") " Result have been saved to "//trim(outchgfilepath)//".chg in current folder"
	write(*,"(a)") " Columns from 1 to 5 are name,X,Y,Z,charge respectively, unit is Angstrom"
end if

deallocate(ESPpt,ESPptval,fitcen)
if (allocated(fitcenvdwr)) deallocate(fitcenvdwr)
deallocate(cenchg)
goto 10 !Return to ESP fitting interface
end subroutine


!!------- Set MK fitting points for present system
!imode=1: Used to get upper limit of number of fitting points (nESPpt), the nESPpt and ESPpt that passed in could be any length
!imode=2: Fill coordinate of ESP fitting points to ESPpt array, in this case the passed-in ESPpt must have enough size
!MKatmlist records the actual atoms used to constructing fitting points, nMKatoms is its length
subroutine setMKpt(imode,nfitcen,fitcen,fitcenvdwr,nESPpt,ESPpt,sclvdwlayer,MKptdens,nMKlayer,nMKatoms,MKatmlist)
use defvar
implicit real*8 (a-h,o-z)
integer nfitcen,nESPpt,nMKlayer,MKatmlist(nMKatoms)
real*8 fitcen(3,nfitcen),fitcenvdwr(nfitcen),ESPpt(3,nESPpt),sclvdwlayer(100),MKptdens
real*8,allocatable :: origsphpt(:,:)
if (imode==1) then !Count how many possible ESP points in total, the number is upper limit because some points will be pruned
	nESPpt=0
	do idx=1,nMKatoms !Note that only actual atoms may be used to construct fitting points
		icen=MKatmlist(idx)
		do ilayer=1,nMKlayer
			numsphpt=nint(4D0*pi*(fitcenvdwr(icen)*sclvdwlayer(ilayer))**2 *MKptdens)
			nESPpt=nESPpt+numsphpt
		end do
	end do
else
	cutinnerscl=minval(sclvdwlayer(1:nMKlayer)) !If distance between a ESP point and any atom is smaller than this, the point will be discarded
	maxsphpt=nint(4D0*pi*(maxval(fitcenvdwr)*maxval(sclvdwlayer))**2 *MKptdens) !Find maximal possible number of points in unit sphere to allocate temporary origsphpt
	allocate(origsphpt(3,maxsphpt))
	iESPpt=0
	do idx=1,nMKatoms
		icen=MKatmlist(idx)
		do ilayer=1,nMKlayer
			radius=fitcenvdwr(icen)*sclvdwlayer(ilayer)
			numsphpt=nint(4D0*pi*radius**2 *MKptdens)
			call unitspherept(origsphpt,numsphpt) !Input expected number of point in unit sphere, return actual number of points
			origsphpt(:,1:numsphpt)=origsphpt(:,1:numsphpt)*radius
			do idir=1,3
				origsphpt(idir,1:numsphpt)=origsphpt(idir,1:numsphpt)+fitcen(idir,icen) !Move unit sphere to atomic center
			end do
			!Prune out the fitting points lying inside the intermost shell
			do ipt=1,numsphpt
				tmpx=origsphpt(1,ipt)
				tmpy=origsphpt(2,ipt)
				tmpz=origsphpt(3,ipt)
				iok=1
				do icen2=1,ncenter
					if (icen2==icen) cycle
					disptcensq=(fitcen(1,icen2)-tmpx)**2+(fitcen(2,icen2)-tmpy)**2+(fitcen(3,icen2)-tmpz)**2 !Distance between point and center
					if (disptcensq<(fitcenvdwr(icen2)*cutinnerscl)**2) then !Less than vdW RADIUS*cutinner of atom icen2, it should be ommitted
						iok=0
						exit
					end if
				end do
				if (iok==1) then
					iESPpt=iESPpt+1
					ESPpt(1,iESPpt)=tmpx
					ESPpt(2,iESPpt)=tmpy
					ESPpt(3,iESPpt)=tmpz
				end if
			end do
		end do
	end do
	nESPpt=iESPpt
	write(*,"(' Number of MK fitting points used:',i10)") nESPpt
end if
end subroutine

!!------- Set CHELPG fitting points for present system
!imode=1: Used to get number of fitting piints (nESPpt), the nESPpt and ESPpt that passed in could be any length
!imode=2: Fill coordinate of ESP fitting points to ESPpt array, in this case the passed-in ESPpt must have enough size
subroutine setCHELPGpt(imode,nfitcen,fitcen,fitcenvdwr,nESPpt,ESPpt,extdis,fitspc)
use defvar
implicit real*8 (a-h,o-z)
integer nfitcen,nESPpt
real*8 fitcen(3,nfitcen),fitcenvdwr(nfitcen),disptcen(nfitcen)
real*8 ESPpt(3,nESPpt)
xlow=minval(fitcen(1,:))-extdis
xhigh=maxval(fitcen(1,:))+extdis
ylow=minval(fitcen(2,:))-extdis
yhigh=maxval(fitcen(2,:))+extdis
zlow=minval(fitcen(3,:))-extdis
zhigh=maxval(fitcen(3,:))+extdis
xlen=xhigh-xlow
ylen=yhigh-ylow
zlen=zhigh-zlow
nxfit=int(xlen/fitspc)+1
nyfit=int(ylen/fitspc)+1
nzfit=int(zlen/fitspc)+1
if (imode==1) then
	nESPpt=0
	do ix=0,nxfit
		do iy=0,nyfit
			do iz=0,nzfit
				tmpx=xlow+ix*fitspc
				tmpy=ylow+iy*fitspc
				tmpz=zlow+iz*fitspc
				do icen=1,nfitcen
					disptcen(icen)=dsqrt( (fitcen(1,icen)-tmpx)**2+(fitcen(2,icen)-tmpy)**2+(fitcen(3,icen)-tmpz)**2 )
					if (disptcen(icen)<=fitcenvdwr(icen)) exit
					if (icen==nfitcen.and.any(disptcen<=extdis)) nESPpt=nESPpt+1
				end do
			end do
		end do
	end do
	write(*,"(' Number of CHELPG fitting points used:',i10)") nESPpt
else
	iESPpt=0
	do ix=0,nxfit
		do iy=0,nyfit
			do iz=0,nzfit
				tmpx=xlow+ix*fitspc
				tmpy=ylow+iy*fitspc
				tmpz=zlow+iz*fitspc
				do icen=1,nfitcen
					disptcen(icen)=dsqrt( (fitcen(1,icen)-tmpx)**2+(fitcen(2,icen)-tmpy)**2+(fitcen(3,icen)-tmpz)**2 )
					if (disptcen(icen)<=fitcenvdwr(icen)) exit
					if (icen==nfitcen.and.any(disptcen<=extdis)) then
						iESPpt=iESPpt+1
						ESPpt(1,iESPpt)=tmpx
						ESPpt(2,iESPpt)=tmpy
						ESPpt(3,iESPpt)=tmpz
					end if
				end do
			end do
		end do
	end do
end if
end subroutine

!!------ Set vdW radius and check sanity and complete vdW radius table for all involved elements
!iradtype=-1: Load radii from external file
!iradtype=0: Use UFF radii scaled by 1/1.2
!iradtype=1: Use radius specific for MK
!iradtype=2: Use radius specific for CHELPG
!espfitvdwr is in Bohr
subroutine setESPfitvdwr(iradtype,espfitvdwr)
use defvar
use util
implicit real*8 (a-h,o-z)
integer iradtype
real*8 espfitvdwr(nelesupp)
character c80*80,c200tmp*200
if (iradtype>0) then
	if (iradtype==1) then !For MK, copied from GetvdW routine (utilam)
		espfitvdwr(1:17)=(/1.20d0,1.20d0,&
		1.37d0,1.45d0,1.45d0,1.50d0,1.50d0,1.40d0,1.35d0,1.30d0,&
		! 1.57d0,1.36d0,1.24d0,1.17d0,1.80d0,1.75d0,1.70d0/) !From Gaussian code
		1.57d0,1.65d0,1.65d0,1.8d0,1.80d0,1.75d0,1.70d0/) !Modified according to my chemical intuition with consulting Bondi and UFF radii
		espfitvdwr(1:17)=espfitvdwr(1:17)/b2a !to Bohr
	else if (iradtype==2) then !For CHELPG
		espfitvdwr(1:2)=1.45D0 !vdW radius copied from GetvdW routine (utilam), some of them are given in CHELPG original paper
		espfitvdwr(3:6)=1.5D0
		espfitvdwr(7:10)=1.7D0
		espfitvdwr(11:18)=2D0
		espfitvdwr(1:18)=espfitvdwr(1:18)/b2a !to Bohr
	end if
	write(*,*) "Atomic radii used:"
	do ielem=1,nelesupp
		if (any(a%index==ielem).and.espfitvdwr(ielem)/=-1D0) write(*,"(' Element:',a,'     vdW radius (Angstrom):',f6.3)") ind2name(ielem),espfitvdwr(ielem)*b2a
	end do
	do iatm=1,ncenter
		idxtmp=a(iatm)%index
		if (espfitvdwr(idxtmp)==-1D0) then
			write(*,"(' vdW radius used in fitting for ',a,' is missing, input the radius (Angstrom)')") ind2name(idxtmp)
			write(*,"(a)") " Hint: If press ENTER directly, corresponding UFF radii scaled by 1/1.2 will be used, this is a usually reasonable choice"
			read(*,"(a)") c80
			if (c80==" ") then
				espfitvdwr(idxtmp)=vdwr_UFF(idxtmp)/1.2D0
				write(*,"(' Radius of ',a,' has been set to',f8.4,' Angstrom')") ind2name(idxtmp),espfitvdwr(idxtmp)*b2a
			else
				read(c80,*) tmpval
				espfitvdwr(idxtmp)=tmpval/b2a
			end if
		end if
	end do
	
else if (iradtype==0) then
	do iatm=1,ncenter
		idxtmp=a(iatm)%index
		if (espfitvdwr(idxtmp)==-1D0) then
			espfitvdwr(idxtmp)=vdwr_UFF(idxtmp)/1.2D0
			write(*,"(' Radius of ',a,' has been set to',f8.4,' Angstrom')") ind2name(idxtmp),espfitvdwr(idxtmp)*b2a
		end if
	end do

else if (iradtype==-1) then
2737	write(*,*) "Input the path of the file containing element radii, e.g. C:\elerad.txt"
	do while(.true.)
		read(*,"(a)") c200tmp
		inquire(file=c200tmp,exist=alive)
		if (alive) exit
		write(*,*) "Cannot find the file, input again"
	end do
	open(10,file=c200tmp,status="old")
	do iatm=1,ncenter
		idxtmp=a(iatm)%index
		if (espfitvdwr(idxtmp)==-1D0) then
			call loclabel(10,ind2name(idxtmp),ifound)
			if (ifound==1) then
				read(10,*) c80,espfitvdwr(idxtmp)
				espfitvdwr(idxtmp)=espfitvdwr(idxtmp)/b2a
				write(*,"(' Radius of ',a,' has been set to',f8.4,' Angstrom')") ind2name(idxtmp),espfitvdwr(idxtmp)*b2a
			else
				write(*,"(' Error: Unable to find radius of element ',a,'!')") ind2name(idxtmp)
				close(10)
				goto 2737
			end if
		end if
	end do
	close(10)
end if
end subroutine


!!-------- Load the number of ESP fitting centers and fitting points from Gaussian pop=MK/CHELPG
subroutine loadgauESP_num(gauoutfilepath,nfitcen,nESPpt)
use util
character c80*80,gauoutfilepath*200
open(10,file=gauoutfilepath,status="old")
call loclabel(10,"NAtoms")
read(10,*) c80,nfitcen
call loclabel(10,"points will be used for")
read(10,*) nESPpt
write(*,"(' Total number of fitting points used:',i10)") nESPpt
close(10)
end subroutine
!!-------- Load ESP fitting centers, fitting points with ESP values from Gaussian pop=MK/CHELPG with IOp(6/33=2) output file
! Using these data, the result will be exactly identical to those outputted by Gaussian
! Note: For Gaussian pop=MK task, even if IOp(6/42=6) has been specified to make the density of fitting point identical to the default value of Multiwfn,
! the result of Multiwfn and Gaussian is different, because after my careful visual comparison, I found that Gaussian automatically eliminate fitting points
! in highly aggregated region. While for single atom, the number of fitting points used by Gaussian and Multiwfn are completely identical.
subroutine loadgauESP(gauoutfilepath,nfitcen,fitcen,nESPpt,ESPpt,ESPptval)
use util
use defvar
implicit real*8 (a-h,o-z)
character gauoutfilepath*200
real*8 fitcen(3,nfitcen),ESPpt(3,nESPpt),ESPptval(nESPpt)
open(10,file=gauoutfilepath,status="old")
call loclabel(10,"Atomic Center    1 is at")
do icen=1,nfitcen
	read(10,"(32x,3f10.6)") fitcen(:,icen)
end do
fitcen=fitcen/b2a
call loclabel(10,"ESP Fit Center",ifound,0)
do ipt=1,nESPpt
	read(10,"(32x,3f10.6)") ESPpt(:,ipt)
end do
ESPpt=ESPpt/b2a
call loclabel(10," Fit ",ifound,0)
do ipt=1,nESPpt
	read(10,"(14x,f10.6)") ESPptval(ipt)
end do
close(10)
end subroutine

!!---------- Calculating ESP at fitting points during calculation of ESP fitting charges
!iESPtype=1: Take nuclear charge into account  /=1: Ignore nuclear contribution
!ishowprompt=1: Show prompts  =0: Do not show
subroutine fitESP_calcESP(ishowprompt,iESPtype,nESPpt,ESPpt,ESPptval,calcfilepath)
use defvar
use function
use util
implicit real*8 (a-h,o-z)
integer ishowprompt,nESPpt,iESPtype
real*8 ESPpt(3,nESPpt),ESPptval(nESPpt)
character c400tmp*400,filename_tmp*200
character(len=*) calcfilepath

call walltime(iwalltime1)
if (ishowprompt==1) write(*,*) "Calculating ESP at fitting points, please wait..."
!If possible, use cubegen to calculate ESP to reduce computational time
alive=.false.
if (cubegenpath/=" ".and.ifiletype==1) then
	inquire(file=cubegenpath,exist=alive)
	if (alive==.false..and.ishowprompt==1) then
		write(*,"(a)") " Note: Albeit current file type is fch/fchk/chk and ""cubegenpath"" parameter in settings.ini has been defined, &
		the cubegen cannot be found, therefore electrostatic potential will still be calculated using internal code of Multiwfn"
	end if
end if
if (alive.and.ifiletype==1) then !Use cubegen to calculate ESP
	if (ishowprompt==1) write(*,"(a)") " Since the input file type is fch/fchk/chk and ""cubegenpath"" parameter in settings.ini has been properly defined, &
	now Multiwfn directly invokes cubegen to calculate electrostatic potential"
	
	!Generate cubegen input file
	open(10,file="cubegenpt.txt",status="replace")
	do ipt=1,nESPpt
		write(10,"(3f16.8)") ESPpt(:,ipt)*b2a
	end do
	close(10)
	ncubegenthreads=1 !Parallel implementation prior to G16 is buggy, so test here
	if (index(cubegenpath,"G16")/=0.or.index(cubegenpath,"g16")/=0) ncubegenthreads=nthreads
	
	filename_tmp=calcfilepath
	if (index(filename,".chk")/=0) call chk2fch(filename_tmp)
	write(c400tmp,"(a,i5,a)") trim(cubegenpath),ncubegenthreads," potential="//trim(cubegendenstype)//" "//&
	""""//trim(filename_tmp)//""""//" ESPresult.cub -5 h < cubegenpt.txt > nouseout"
	write(*,"(a)") " Running: "//trim(c400tmp)
	call system(c400tmp)
	if (index(filename,".chk")/=0) call delfch(filename_tmp)
	
	!Load ESP data from cubegen resulting file
	open(10,file="ESPresult.cub",status="old")
	do iskip=1,6+ncenter
		read(10,*)
	end do
	do ipt=1,nESPpt
		read(10,*) xtmp,ytmp,ztmp,ESPptval(ipt)
		if (iESPtype==2.or.iESPtype==3) ESPptval(ipt)=ESPptval(ipt)-nucesp(xtmp/b2a,ytmp/b2a,ztmp/b2a) !Remove nuclear contribution
	end do
	close(10)
	!Delete intermediate files
	if (isys==1) then
		call system("del cubegenpt.txt ESPresult.cub nouseout /Q")
	else
		call system("rm cubegenpt.txt ESPresult.cub nouseout -f")
	end if
else !Use internal code to evaluate ESP
	itmp=1
	do ipt=1,nESPpt
		if (ipt>=itmp*300.or.ipt==nESPpt) then
			call showprog(ipt,nESPpt)
			itmp=itmp+1
		end if
		if (iESPtype==1) then !Take nuclear charge into account
			ESPptval(ipt)=totesp(ESPpt(1,ipt),ESPpt(2,ipt),ESPpt(3,ipt))
		else if (iESPtype==2.or.iESPtype==3) then !Do not take nuclear charge into account
			ESPptval(ipt)=eleesp(ESPpt(1,ipt),ESPpt(2,ipt),ESPpt(3,ipt))
		end if
	end do
end if
call walltime(iwalltime2)
if (ishowprompt==1) write(*,"(' Calculation of ESP took up wall clock time',i10,'s')") iwalltime2-iwalltime1
end subroutine

!!--------- Generate numpt points scattered evenly in an unit sphere, used by for obtaining MK charge
! Input argument numpt is the expected number of points, while the return value is actual number
! ptcrd store coordinates of the points 
subroutine unitspherept(ptcrd,numpt)
implicit real*8 (a-h,o-z)
real*8 ptcrd(3,numpt)
integer numpt
pi=3.141592653589793D0
!The average number of equator points in all XY layes is numequ*2/pi, and there are numvert=numequ/2 layers
!Solve (numequ*2/pi)*numequ/2=numpt one can get numequ=sqrt(numpt*pi)
numequ=int(sqrt(numpt*pi)) !Maximal number of point in each XY layer
numvert=numequ/2
ipt=0
do ivert=0,numvert
	angz=dfloat(ivert)/numvert*pi
	scalexy=sin(angz)
	z=cos(angz)
	numxy=int(numequ*scalexy)
	if (numxy==0) numxy=1
	do ihori=1,numxy
		ipt=ipt+1
		if (ipt>numpt) then
			numpt=ipt-1
			return
		end if
		angxy=2D0*pi*ihori/numxy
		ptcrd(1,ipt)=cos(angxy)*scalexy
		ptcrd(2,ipt)=sin(angxy)*scalexy
		ptcrd(3,ipt)=z
	end do
end do
numpt=ipt
end subroutine











!!============================ Hirshfeld-I ============================!!
!!============================ Hirshfeld-I ============================!!
!!============================ Hirshfeld-I ============================!!
!!============================ Hirshfeld-I ============================!!
!!============================ Hirshfeld-I ============================!!
!Wrapper of Hirshfeld-I module to automatically set radpot and sphpot to proper values
!itype=1: Normal population analysis =2: Only used to generate proper atomic space (i.e. Don't do unnecessary things)
subroutine Hirshfeld_I_wrapper(itype)
use defvar
implicit real*8 (a-h,o-z)
radpotold=radpot
sphpotold=sphpot
if (iautointgrid==1) then
	radpot=30
	sphpot=170
	if (any(a%index>18)) radpot=40
	if (any(a%index>36)) radpot=50
	if (any(a%index>54)) radpot=60
end if
call Hirshfeld_I(itype)
if (iautointgrid==1) then
	radpot=radpotold
	sphpot=sphpotold
end if
end subroutine

!!--------- Calculate Hirshfeld-I charge and yield final atomic radial density
!I've compared this module with hipart, this module is faster than hipart, and the accuracy under default setting is at least never lower than hipart
subroutine Hirshfeld_I(itype)
use defvar
use function
use util
implicit real*8 (a-h,o-z)
integer itype
type(content) gridatm(radpot*sphpot),gridatmorg(radpot*sphpot)
real*8 molrho(radpot*sphpot),promol(radpot*sphpot),tmpdens(radpot*sphpot),selfdens(radpot*sphpot),molrhoall(ncenter,radpot*sphpot)
real*8 charge(ncenter),lastcharge(ncenter) !Atomic charge of current iter. and last iter.
real*8 radrholow(200),radrhohigh(200)
character sep,c80tmp*80,chgfilename*200,selectyn
character*2 :: statname(-4:4)=(/ "-4","-3","-2","-1","_0","+1","+2","+3","+4" /)
integer :: maxcyc=50,ioutmedchg=0
real*8 :: crit=0.0002D0
real*8,external :: fdens_rad
!Used for mode 2. e.g. atmstatgrid(iatm,igrid,jatm,-1) means density of jatm with -1 charge state at igrid around iatm
real*8,allocatable :: atmstatgrid(:,:,:,:)
ntotpot=radpot*sphpot

!Mode 1 use very low memory but expensive, because most data is computed every iteration
!Mode 2 use large memory but fast, because most data is only computed once at initial stage
!The result of the two modes differ with each other marginally, probably because in mode 1 radial density is related to max(npthigh,nptlow), which is not involved in mode 2
!In principle, result of mode 2 is slightly better
imode=2

!Ignore jatm contribution to iatm centered grids if distance between iatm and jatm is larger than 1.5 times of sum of their vdwr
!This can decrease lots of time for large system, the lose of accuracy can be ignored (error is ~0.0001 per atom)
ignorefar=1

write(*,*)
if (itype==1) write(*,*) "     =============== Iterative Hirshfeld (Hirshfeld-I) ==============="
if (itype==2) write(*,*) "     ============== Generate Hirshfeld-I atomic weights =============="
do while(.true.)
	if (imode==1) write(*,*) "-2 Switch algorithm, current: Slow & low memory requirement"
	if (imode==2) write(*,*) "-2 Switch algorithm, current: Fast & large memory requirement"
	if (itype==1) then
		if (ioutmedchg==0) write(*,*) "-1 Switch if output intermediate results, current: No"
		if (ioutmedchg==1) write(*,*) "-1 Switch if output intermediate results, current: Yes"
		write(*,*) "0 Return"
	end if
	write(*,*) "1 Start calculation!"
	write(*,"(a,i4)") " 2 Set the maximum number of iterations, current:",maxcyc
	write(*,"(a,f10.6)") " 3 Set convergence criterion of atomic charges, current:",crit
	read(*,*) isel
	if (isel==-2) then
		if (imode==1) then
			imode=2
		else
			imode=1
			crit=0.001 !mode 1 is more time-consuming, use loose criterion
		end if
	else if (isel==-1) then
		if (ioutmedchg==1) then
			ioutmedchg=0
		else
			ioutmedchg=1
		end if
	else if (isel==0) then
		return
	else if (isel==1) then
		exit
	else if (isel==2) then
		write(*,*) "Input maximum number of iterations, e.g. 30"
		read(*,*) maxcyc
	else if (isel==3) then
		write(*,*) "Input convergence criterion of atomic charges, e.g. 0.001"
		read(*,*) crit
	end if
end do

!Generate all needed .rad files
call genatmradfile

!====== Start calculation ======!
call walltime(iwalltime1)
CALL CPU_TIME(time_begin)

!Currently all atoms share the same radial points
nradpt=200
itmp=0
do irad=nradpt,1,-1
	radx=cos(irad*pi/(nradpt+1))
	itmp=itmp+1
	atmradpos(itmp)=(1+radx)/(1-radx)
end do

!Generate single center integration grid
call gen1cintgrid(gridatmorg,iradcut)
write(*,*)
write(*,"(' Radial grids:',i4,'  Angular grids:',i5,'  Total:',i7,'  After pruning:',i7)") radpot,sphpot,radpot*sphpot,radpot*sphpot-iradcut*sphpot

!Calculate molecular density
write(*,*) "Calculating density of actual molecule for all grids..."
!$OMP parallel do shared(molrhoall) private(iatm,ipt,gridatm) num_threads(nthreads)
do iatm=1,ncenter
	gridatm%x=gridatmorg%x+a(iatm)%x !Move quadrature point to actual position in molecule
	gridatm%y=gridatmorg%y+a(iatm)%y
	gridatm%z=gridatmorg%z+a(iatm)%z
	do ipt=1+iradcut*sphpot,ntotpot
		molrhoall(iatm,ipt)=fdens(gridatm(ipt)%x,gridatm(ipt)%y,gridatm(ipt)%z)
	end do
end do
!$OMP end parallel do

if (allocated(atmradnpt)) deallocate(atmradnpt)
if (allocated(atmradrho)) deallocate(atmradrho)
allocate(atmradnpt(ncenter),atmradrho(ncenter,200))
sep='/' !Separation symbol of directory
if (isys==1) sep='\'

!Calculate contribution of all atoms in every state to each atomic centered grids
if (imode==2) then
	allocate(atmstatgrid(ncenter,ntotpot,ncenter,-2:2))
	atmstatgrid=0
	write(*,*) "Calculating atomic density contribution to grids..."
	do iatm=1,ncenter !The center of grids
		write(*,"(' Progress:',i5,' /',i5)") iatm,ncenter
		gridatm%value=gridatmorg%value !Weight in this grid point
		gridatm%x=gridatmorg%x+a(iatm)%x !Move quadrature point to actual position in molecule
		gridatm%y=gridatmorg%y+a(iatm)%y
		gridatm%z=gridatmorg%z+a(iatm)%z
		do istat=-2,2 !Charge state
			do jatm=1,ncenter
				if (ignorefar==1) then
					atmdist=dsqrt( (a(iatm)%x-a(jatm)%x)**2+(a(iatm)%y-a(jatm)%y)**2+(a(iatm)%z-a(jatm)%z)**2 )
					if (atmdist>(vdwr(iatm)+vdwr(jatm))*1.5D0) cycle
				end if
				if (a(jatm)%index==1.and.istat==1) cycle !H+ doesn't contains electron and cannot compute density
				c80tmp="atmrad"//sep//trim(a(jatm)%name)//statname(istat)//".rad"
				inquire(file=c80tmp,exist=alive)
				if (alive==.false.) cycle
				open(10,file=c80tmp,status="old")
				read(10,*) atmradnpt(jatm)
				do ipt=1,atmradnpt(jatm)
					read(10,*) rnouse,atmradrho(jatm,ipt)
				end do
				close(10)
				do ipt=1+iradcut*sphpot,ntotpot
					atmstatgrid(iatm,ipt,jatm,istat)=fdens_rad(jatm,gridatm(ipt)%x,gridatm(ipt)%y,gridatm(ipt)%z)
				end do
			end do
		end do
	end do
end if

!Set atomic initial radial density as neutral state, which is loaded from corresponding .rad file
atmradrho=0
do iatm=1,ncenter
	open(10,file="atmrad"//sep//trim(a(iatm)%name)//"_0.rad",status="old")
	read(10,*) atmradnpt(iatm)
	do ipt=1,atmradnpt(iatm)
		read(10,*) rnouse,atmradrho(iatm,ipt)
	end do
	close(10)
end do

write(*,*)
write(*,*) "Performing Hirshfeld-I iteration to refine atomic spaces..."
lastcharge=0
!Cycle each atom to calculate their charges
do icyc=1,maxcyc
	if (ioutmedchg==1) write(*,*)
	if (icyc==1) then
		write(*,"(' Cycle',i5)") icyc
	else
		write(*,"(' Cycle',i5,'   Maximum change:',f10.6)") icyc,varmax
	end if
	
	do iatm=1,ncenter
		gridatm%value=gridatmorg%value !Weight in this grid point
		gridatm%x=gridatmorg%x+a(iatm)%x !Move quadrature point to actual position in molecule
		gridatm%y=gridatmorg%y+a(iatm)%y
		gridatm%z=gridatmorg%z+a(iatm)%z
		
		!Molecular density
		molrho=molrhoall(iatm,:)
		
		!Calculate promolecular and proatomic density 
		promol=0D0
		do jatm=1,ncenter
			if (ignorefar==1) then
				atmdist=dsqrt( (a(iatm)%x-a(jatm)%x)**2+(a(iatm)%y-a(jatm)%y)**2+(a(iatm)%z-a(jatm)%z)**2 )
				if (atmdist>(vdwr(iatm)+vdwr(jatm))*1.5D0) cycle
			end if
			if (imode==1) then
				!$OMP parallel do shared(tmpdens) private(ipt) num_threads(nthreads)
				do ipt=1+iradcut*sphpot,ntotpot
					tmpdens(ipt)=fdens_rad(jatm,gridatm(ipt)%x,gridatm(ipt)%y,gridatm(ipt)%z)
				end do
				!$OMP end parallel do
			else if (imode==2) then
				if (icyc==1) then
					tmpdens=atmstatgrid(iatm,:,jatm,0)
				else
					ichglow=floor(lastcharge(jatm))	
					ichghigh=ceiling(lastcharge(jatm))
					tmpdens=(lastcharge(jatm)-ichglow)*atmstatgrid(iatm,:,jatm,ichghigh) + (ichghigh-lastcharge(jatm))*atmstatgrid(iatm,:,jatm,ichglow)
				end if
			end if
			promol=promol+tmpdens
			if (jatm==iatm) selfdens=tmpdens
		end do
		
		!Calculate atomic charge
		electmp=0D0
		do ipt=1+iradcut*sphpot,ntotpot
			if (promol(ipt)/=0D0) electmp=electmp+selfdens(ipt)/promol(ipt)*molrho(ipt)*gridatm(ipt)%value
		end do
		if (nEDFelec==0) charge(iatm)=a(iatm)%charge-electmp
		if (nEDFelec>0) charge(iatm)=a(iatm)%index-electmp !Assume EDF information provides inner-core electrons for all atoms using ECP
		if (ioutmedchg==1) write(*,"(' Charge of atom',i5,'(',a2,')',': ',f12.6,'  Delta:',f12.6)") &
		iatm,a(iatm)%name,charge(iatm),charge(iatm)-lastcharge(iatm)
	end do
	
	!Check convergence
	varmax=maxval(abs(charge-lastcharge))
	if (varmax<crit) then
		if (itype==1) then
			write(*,"(a,f10.6)") " All atomic charges have converged to criterion of",crit
			write(*,"(' Sum of all charges:',f14.8)") sum(charge)
			!Normalize to get rid of integration inaccuracy
			totnumelec=sum(a%charge-charge)
			facnorm=nelec/totnumelec
			do iatm=1,ncenter
				charge(iatm)=a(iatm)%charge-facnorm*(a(iatm)%charge-charge(iatm))
			end do
			write(*,*)
			write(*,*) "Final atomic charges, after normalization to actual number of electrons"
			do iatm=1,ncenter
				write(*,"(' Atom',i5,'(',a2,')',': ',f12.6)") iatm,a(iatm)%name,charge(iatm)
			end do
			exit
		else
			write(*,*) "Hirshfeld-I atomic spaces converged successfully!"
			write(*,*)
			return
		end if
	else
		if (icyc==maxcyc) then
			write(*,"(/,' Convergence failed within',i4,' cycles!')") maxcyc
			exit
		end if
	end if
	
	!Update atomic radial density by means of interpolation of adjacent charge state
	do iatm=1,ncenter
		!Read radial density of lower limit state
		ichglow=floor(charge(iatm))
		radrholow=0
		c80tmp="atmrad"//sep//trim(a(iatm)%name)//statname(ichglow)//".rad"
		inquire(file=c80tmp,exist=alive)
		if (alive==.false.) then
			write(*,"(' Error: ',a,' was not prepared!')") trim(c80tmp)
			return
		end if
		open(10,file=c80tmp,status="old")
		read(10,*) nptlow
		do ipt=1,nptlow
			read(10,*) rnouse,radrholow(ipt)
		end do
		close(10)
		!Read radial density of upper limit state
		ichghigh=ceiling(charge(iatm))
		radrhohigh=0
		c80tmp="atmrad"//sep//trim(a(iatm)%name)//statname(ichghigh)//".rad"
		inquire(file=c80tmp,exist=alive)
		if (alive==.false.) then
			write(*,"(' Error: ',a,' was not prepared!')") trim(c80tmp)
			return
		end if
		open(10,file=c80tmp,status="old")
		read(10,*) npthigh
		do ipt=1,npthigh
			read(10,*) rnouse,radrhohigh(ipt)
		end do
		close(10)
		!Update current radial density
		atmradrho(iatm,:)=(charge(iatm)-ichglow)*radrhohigh(:) + (ichghigh-charge(iatm))*radrholow(:)
		atmradnpt(iatm)=max(npthigh,nptlow)
	end do
	
	lastcharge=charge
end do

if (allocated(frag1)) write(*,"(/,' Fragment charge:',f12.6)") sum(charge(frag1))
CALL CPU_TIME(time_end)
call walltime(iwalltime2)
write(*,"(' Calculation took up CPU time',f12.2,'s, wall clock time',i10,'s')") time_end-time_begin,iwalltime2-iwalltime1

call path2filename(firstfilename,chgfilename)
write(*,*)
write(*,"(a)") " If output atoms with charges to "//trim(chgfilename)//".chg in current folder? (y/n)"
read(*,*) selectyn
if (selectyn=="y".or.selectyn=="Y") then
	open(10,file=trim(chgfilename)//".chg",status="replace")
	do i=1,ncenter
		write(10,"(a4,4f12.6)") a(i)%name,a(i)%x*b2a,a(i)%y*b2a,a(i)%z*b2a,charge(i)
	end do
	close(10)
	write(*,"(a)") " Result have been saved to "//trim(chgfilename)//".chg in current folder"
	write(*,"(a)") " Columns 1 to 5 are name,X,Y,Z,charge respectively, unit is Angstrom"
end if
end subroutine


!!------- Generate atomic radial density files at different states, used for such as Hirshfeld-I
!"atmrad" in current folder is used as working directory
!-2,-1,0,+1,+2 charge states of each element will be calculated to produce atomic .wfn file by Gaussian, predefined ground state multiplicity is used
!After that, radial density file (.rad) is generated for each state of each element
!If atomic wfn file is already existed, calculation will be skipped
!Radial distance values are the same as built-in atomic density, i.e. those in atmraddens.f90
subroutine genatmradfile
use defvar
use util
implicit real*8 (a-h,o-z)
character c80tmp*80,c200tmp*200,calclevel*80,radname*200,sep
character*2 :: statname(-3:3)=(/ "-3","-2","-1","_0","+1","+2","+3" /)
integer :: chgmulti(nelesupp,-3:3)=0 !Ground state multiplicity of each charge state of each element. If value=0, means undefined

!Define chgmulti for elements for possible states
!H,Li,Na,K,Rb,Cs
chgmulti(1,0)=2
chgmulti(1,1)=1
chgmulti(1,-1)=1
chgmulti(3,:)=chgmulti(1,:)
chgmulti(11,:)=chgmulti(1,:)
chgmulti(19,:)=chgmulti(1,:)
chgmulti(37,:)=chgmulti(1,:)
chgmulti(55,:)=chgmulti(1,:)
!He,Ne,Ar,Kr,Xe,Rn
chgmulti(2,0)=1
chgmulti(2,1)=2
chgmulti(2,-1)=2
chgmulti(10,:)=chgmulti(2,:)
chgmulti(18,:)=chgmulti(2,:)
chgmulti(36,:)=chgmulti(2,:)
chgmulti(54,:)=chgmulti(2,:)
chgmulti(86,:)=chgmulti(2,:)
!Be,Mg,Ca,Sr,Ba
chgmulti(4,0)=1
chgmulti(4,1)=2
chgmulti(4,2)=1
chgmulti(4,-1)=2
chgmulti(12,:)=chgmulti(4,:)
chgmulti(20,:)=chgmulti(4,:)
chgmulti(38,:)=chgmulti(4,:)
chgmulti(56,:)=chgmulti(4,:)
!B,Al,Ga,In,Tl
chgmulti(5,0)=2
chgmulti(5,1)=1
chgmulti(5,2)=2
chgmulti(5,-1)=3
chgmulti(5,-2)=4
chgmulti(13,:)=chgmulti(5,:)
chgmulti(31,:)=chgmulti(5,:)
chgmulti(49,:)=chgmulti(5,:)
chgmulti(81,:)=chgmulti(5,:)
!C,Si,Ge,Sn,Pb
chgmulti(6,0)=3
chgmulti(6,1)=2
chgmulti(6,2)=1
chgmulti(6,-1)=4
chgmulti(6,-2)=3
chgmulti(14,:)=chgmulti(6,:)
chgmulti(32,:)=chgmulti(6,:)
chgmulti(50,:)=chgmulti(6,:)
chgmulti(82,:)=chgmulti(6,:)
!N,P,As,Sb,Bi
chgmulti(7,0)=4
chgmulti(7,1)=3
chgmulti(7,2)=2
chgmulti(7,-1)=3
chgmulti(7,-2)=2
chgmulti(15,:)=chgmulti(7,:)
chgmulti(33,:)=chgmulti(7,:)
chgmulti(51,:)=chgmulti(7,:)
chgmulti(83,:)=chgmulti(7,:)
!O,S,Se,Te,Po
chgmulti(8,0)=3
chgmulti(8,1)=4
chgmulti(8,2)=3
chgmulti(8,-1)=2
chgmulti(8,-2)=1
chgmulti(16,:)=chgmulti(8,:)
chgmulti(34,:)=chgmulti(8,:)
chgmulti(52,:)=chgmulti(8,:)
chgmulti(84,:)=chgmulti(8,:)
!F,Cl,Br,I,At
chgmulti(9,0)=2
chgmulti(9,1)=3
chgmulti(9,2)=4
chgmulti(9,-1)=1
chgmulti(17,:)=chgmulti(9,:)
chgmulti(35,:)=chgmulti(9,:)
chgmulti(53,:)=chgmulti(9,:)
chgmulti(85,:)=chgmulti(9,:)
!Spin multiplicity of transition metal for each state is determined by chemical intuition as well as a few single point energy data
!For simplicity, I assume that later elements in each row has identical configuration, of course this is incorrect but not too bad
!Sc (3d1,4s2)
chgmulti(21,0)=2
chgmulti(21,1)=3
chgmulti(21,2)=2
chgmulti(21,-1)=3
chgmulti(39,:)=chgmulti(21,:) !Y
chgmulti(57,:)=chgmulti(21,:) !La
!Ti (3d2,4s2)
chgmulti(22,0)=3
chgmulti(22,1)=4
chgmulti(22,2)=3
chgmulti(22,-1)=4
chgmulti(40,:)=chgmulti(22,:) !Zr
chgmulti(72,:)=chgmulti(22,:) !Hf
!V  (3d3,4s2)
chgmulti(23,0)=4
chgmulti(23,1)=5
chgmulti(23,2)=4
chgmulti(23,-1)=5
chgmulti(41,:)=chgmulti(23,:) !Nb
chgmulti(73,:)=chgmulti(23,:) !Ta
!Cr (3d5,4s1)
chgmulti(24,0)=7
chgmulti(24,1)=6
chgmulti(24,2)=5
chgmulti(24,-1)=6
chgmulti(42,:)=chgmulti(24,:) !Mo
chgmulti(74,:)=chgmulti(24,:) !W
!Mn (3d5,4s2)
chgmulti(25,0)=6
chgmulti(25,1)=7
chgmulti(25,2)=6
chgmulti(25,-1)=5
chgmulti(43,:)=chgmulti(25,:) !Tc
chgmulti(75,:)=chgmulti(25,:) !Re
!Fe (3d6,4s2)
chgmulti(26,0)=5
chgmulti(26,1)=6
chgmulti(26,2)=7
chgmulti(26,-1)=4
chgmulti(44,:)=chgmulti(26,:) !Ru
chgmulti(76,:)=chgmulti(26,:) !Os
!Co (3d7,4s2)
chgmulti(27,0)=4
chgmulti(27,1)=5
chgmulti(27,2)=4
chgmulti(27,-1)=3
chgmulti(45,:)=chgmulti(27,:) !Rh
chgmulti(77,:)=chgmulti(27,:) !Ir
!Ni (3d8,4s2)
chgmulti(28,0)=3
chgmulti(28,1)=4
chgmulti(28,2)=3
chgmulti(28,-1)=2
chgmulti(46,:)=chgmulti(28,:) !Pd
chgmulti(78,:)=chgmulti(28,:) !Pt
!Cu (3d10,4s1)
chgmulti(29,0)=2
chgmulti(29,1)=1
chgmulti(29,2)=2
chgmulti(29,-1)=1
chgmulti(47,:)=chgmulti(29,:) !Ag
chgmulti(79,:)=chgmulti(29,:) !Au
!Zn (3d10,4s2)
chgmulti(30,0)=1
chgmulti(30,1)=2
chgmulti(30,2)=1
chgmulti(30,-1)=2
chgmulti(48,:)=chgmulti(30,:) !Cd
chgmulti(80,:)=chgmulti(30,:) !Hg

sep='/' !Separation symbol of directory
if (isys==1) sep='\'
calclevel=" "
! calclevel="B3LYP/def2SVP"

!Cycle each charge state of each atom. Each element is only calculated once. If the file is existing, don't recalculate again
do iatm=1,ncenter
	iele=a(iatm)%index
	do istat=-3,3
		if (chgmulti(iele,istat)==0) cycle !Undefined state
		radname="atmrad"//sep//trim(a(iatm)%name)//statname(istat)//".wfn"
		inquire(file=radname,exist=alive)
		if (alive) cycle
		
		!Check Gaussian path
		inquire(file=gaupath,exist=alive)
		if (.not.alive) then
			write(*,*) "Could not find Gaussian path defined in ""gaupath"" variable in settings.ini"
			if (isys==1) write(*,*) "Input the path of Gaussian executable file, e.g. ""D:\study\g09w\g09.exe"""
			if (isys==2) write(*,*) "Input the path of Gaussian executable file, e.g. ""/sob/g09/g09"""
			do while(.true.)
				read(*,"(a)") gaupath
				inquire(file=gaupath,exist=alive)
				if (alive) exit
				write(*,*) "Could not find Gaussian executable file, input again"
			end do
		end if
		
		!Input calculation level
		if (calclevel==" ") then
			write(*,*) "Some atomic .wfn files are not found in ""atmrad"" folder in current directory"
			write(*,"(a)") " Now input the level for calculating these .wfn files, e.g. B3LYP/def2SVP"
			write(*,"(a)") " You can also add other keywords at the same time, e.g. M062X/6-311G(2df,2p) scf=xqc int=ultrafine"
			read(*,"(a)") calclevel
		end if
		
		!Generate .gjf file 
		inquire(directory="atmrad",exist=alive)
		if (alive==.false.) call system("mkdir atmrad")
		c200tmp="atmrad"//sep//trim(a(iatm)%name)//statname(istat)//".gjf"
		open(10,file=c200tmp,status="replace")
		write(10,"(a)") "# "//trim(calclevel)//" out=wfn"
		write(10,*)
		write(10,"(a)") trim(a(iatm)%name)//statname(istat)
		write(10,*)
		write(10,"(2i3)") istat,chgmulti(iele,istat)
		write(10,"(a)") a(iatm)%name
		write(10,*)
		c200tmp="atmrad"//sep//trim(a(iatm)%name)//statname(istat)//".wfn"
		write(10,"(a)") trim(c200tmp)
		write(10,*)
		write(10,*)
		close(10)
		
		!Start calculation
		c80tmp="atmrad"//sep//trim(a(iatm)%name)//statname(istat)
		write(*,*) "Running: "//trim(gaupath)//' "'//trim(c80tmp)//'.gjf" "'//trim(c80tmp)//'"'
		call system(trim(Gaupath)//' "'//trim(c80tmp)//'.gjf" "'//trim(c80tmp)//'"')
		
		!Check if Gaussian task was successfully finished
		if (isys==1) then
			inquire(file=trim(c80tmp)//".out",exist=alive)
		else
			inquire(file=trim(c80tmp)//".log",exist=alive)
		end if
		if (alive) then
			if (isys==1) then
				open(10,file=trim(c80tmp)//".out",status="old")
			else
				open(10,file=trim(c80tmp)//".log",status="old")
			end if
			call loclabel(10,"Normal termination",igaunormal)
			close(10)
			if (igaunormal==0) then
				write(*,"(a)") " Gaussian running may be failed! Please manually check Gaussian input and output files in atmrad folder"
				write(*,*) "Press ENTER button to continue"
				read(*,*)
			end if
		else
			write(*,"(a)") " Gaussian running may be failed! Please manually check Gaussian input and output files in atmrad folder"
			write(*,*) "Press ENTER button to continue"
			read(*,*)
		end if
	end do
end do

!All element wfn files have been generated, now calculate corresponding radial density file (.rad)
!Existing .rad file will not be re-calculated
write(*,*)
write(*,*) "Generating atomic radial density from atomic wfn file..."
do iatm=1,ncenter
	iele=a_org(iatm)%index
	do istat=-3,3
		if (chgmulti(iele,istat)==0) cycle !Undefined state
		c80tmp="atmrad"//sep//trim(a_org(iatm)%name)//statname(istat)
		inquire(file=trim(c80tmp)//".rad",exist=alive)
		if (alive) cycle
		inquire(file=trim(c80tmp)//".wfn",exist=alive)
		if (alive==.false.) then
			write(*,"(' Error: ',a,' was not found!')") trim(c80tmp)//".wfn"
			write(*,*) "If you want to skip, press ENTER directly"
			read(*,*)
			cycle
		end if
		write(*,"(' Converting ',a,' to ',a)") trim(c80tmp)//".wfn",trim(c80tmp)//".rad"
		call atmwfn2atmrad(trim(c80tmp)//".wfn",trim(c80tmp)//".rad")
	end do
end do

!Recover to the firstly loaded file
call dealloall
call readinfile(firstfilename,1)
end subroutine


!!----- Generate atomic radial density from atomic wfn file
!The code is adapted from sphatmraddens
subroutine atmwfn2atmrad(infile,outfile)
use defvar
use function
implicit real*8 (a-h,o-z)
character(len=*) infile,outfile
real*8,allocatable :: potx(:),poty(:),potz(:),potw(:),radpos(:),sphavgval(:)
call dealloall
call readinfile(infile,1)
truncrho=1D-8
rlow=0D0
rhigh=12
nsphpt=974
nradpt=200 !Totally 200 radial points, but the number of point is truncated at truncrho (because the interpolation routine doesn't work well for very low value)
allocate(potx(nsphpt),poty(nsphpt),potz(nsphpt),potw(nsphpt),radpos(nradpt),sphavgval(nradpt))
sphavgval=0
call Lebedevgen(nsphpt,potx,poty,potz,potw)
!$OMP PARALLEL DO SHARED(sphavgval,radpos) PRIVATE(irad,radx,radr,isph,rnowx,rnowy,rnowz) schedule(dynamic) NUM_THREADS(nthreads)
do irad=1,nradpt
	radx=cos(irad*pi/(nradpt+1))
	radr=(1+radx)/(1-radx) !Becke transform
	radpos(irad)=radr
	do isph=1,nsphpt
		rnowx=potx(isph)*radr
		rnowy=poty(isph)*radr
		rnowz=potz(isph)*radr
		sphavgval(irad)=sphavgval(irad)+fdens(rnowx,rnowy,rnowz)*potw(isph)
	end do
end do
!$OMP END PARALLEL DO
open(10,file=outfile,status="replace")
write(10,*) count(sphavgval>truncrho)
do irad=nradpt,1,-1
	if (sphavgval(irad)>truncrho) write(10,"(f20.12,E18.10)") radpos(irad),sphavgval(irad)
end do
close(10)
end subroutine


!!---- Calculate density at a point for iatm based on loaded atomic radial density
real*8 function fdens_rad(iatm,x,y,z)
use defvar
use util
integer iatm,npt
real*8 x,y,z,r,rnouse
npt=atmradnpt(iatm)
r=dsqrt((a(iatm)%x-x)**2+(a(iatm)%y-y)**2+(a(iatm)%z-z)**2)
call lagintpol(atmradpos(1:npt),atmradrho(iatm,1:npt),npt,r,fdens_rad,rnouse,rnouse,1)
end function








!!----------------------------------------
!!--------- Calculate EEM charge ---------
!!----------------------------------------
subroutine EEM
use defvar
use util
integer,parameter :: maxBO=3 !Maximum of possible bond order
character c200tmp*200
real*8 EEMmat(ncenter+1,ncenter+1),EEMarr(ncenter+1),qarr(ncenter+1)
real*8 kappa,Aparm(nelesupp,maxBO),Bparm(nelesupp,maxBO) !If parameter is -1, means undefined parameter
real*8 :: chgnet=0

if (ifiletype/=11) then
	write(*,"(a)") " Error: MDL Molfile (.mol) file must be used as input file, since it contains atomic connectivity information!"
	write(*,*) "Press Enter to return"
	read(*,*)
	return
end if

iparmset=2
call genEEMparm(iparmset,kappa,Aparm,Bparm)
isel2=-10
	
EEMcyc: do while(.true.)
	write(*,*)
	write(*,*) "           ------ Electronegativity Equalization Method (EEM) ------"
	write(*,*) "-1 Return"
	write(*,*) "0 Start calculation"
	write(*,*) "1 Choose EEM parameters"
	write(*,"(a,f4.1)") " 2 Set net charge, current:",chgnet
	read(*,*) isel
	if (isel==-1) then
		return
	else if (isel==1) then
		do while(.true.)
			if (isel2/=-1) then
				write(*,*)
				write(*,*) "Present EEM parameters:"
				write(*,"(' kappa',f12.6)") kappa
				do iele=1,nelesupp
					do imulti=1,maxBO
						if (Aparm(iele,imulti)/=-1) write(*,"(1x,a,'  Multiplicity:',i2,'    A:',f9.5, '    B:',f9.5)") ind2name(iele),imulti,Aparm(iele,imulti),Bparm(iele,imulti)
					end do
				end do
			end if
			write(*,*)
			write(*,*) "-2 Return"
			write(*,*) "-1 Export present parameters to external file"
			write(*,*) "0 Load parameters from external file"
			write(*,*) "1 Use parameters fitted to HF/STO-3G Mulliken charge, IJMS, 8, 572 (2007)"
			write(*,*) "2 Use parameters fitted to B3LYP/6-31G* CHELPG charge, JCC, 30, 1174 (2009)"
			write(*,*) "3 Use parameters fitted to HF/6-31G* CHELPG charge, JCC, 30, 1174 (2009)"
			write(*,*) "4 Use parameters fitted to B3LYP/6-311G* NPA charge, J Cheminform, 8, 57(2016)"
			read(*,*) isel2
			if (isel2==-2) then
				exit
			else if (isel2==-1) then
				open(10,file="EEMparm.txt",status="replace")
				write(10,"(f12.6)") kappa
				do iele=1,nelesupp
					do imulti=1,maxBO
						if (Aparm(iele,imulti)/=-1) write(10,"(1x,a,i3,2f9.5)") ind2name(iele),imulti,Aparm(iele,imulti),Bparm(iele,imulti)
					end do
				end do
				close(10)
				write(*,*) "Parameters have been exported to EEMparm.txt in current folder"
			else if (isel2==0) then
				write(*,*) "Input path of parameter file, e.g. C:\aqours.txt"
				do while(.true.)
					read(*,"(a)") c200tmp
					inquire(file=c200tmp,exist=alive)
					if (alive) exit
					write(*,*) "Cannot find the file, input again"
				end do
				Aparm=-1
				Bparm=-1
				open(10,file=c200tmp,status="old")
				read(10,*) kappa
				nload=0
				do while(.true.)
					read(10,*,iostat=ierror) c200tmp,imulti,tmpA,tmpB
					if (ierror/=0) exit
					call lc2uc(c200tmp(1:1)) !Convert to upper case
					call uc2lc(c200tmp(2:2)) !Convert to lower case
					do iele=1,nelesupp
						if (c200tmp(1:2)==ind2name(iele)) exit
					end do
					Aparm(iele,imulti)=tmpA
					Bparm(iele,imulti)=tmpB
					nload=nload+1
				end do
				write(*,"(' Loaded',i5,' entries')") nload
				close(10)
			else
				call genEEMparm(isel2,kappa,Aparm,Bparm)
			end if
		end do
	else if (isel==2) then
		write(*,*) "Input net charge of the system, e.g. -1"
		read(*,*) chgnet
	else if (isel==0) then
		write(*,*) "Calculating..."
		write(*,*)
		!Construct EEM array
		EEMarr(ncenter+1)=chgnet
		do iatm=1,ncenter
			imulti=maxval(connmat(iatm,:))
			if (imulti>maxBO) then
				write(*,"(' Error: Multiplicity of atom',i5,' (',i2,') exceeded upper limit (',i2,')!')") iatm,imulti,maxBO
                write(*,"(a)") " The present EEM parameters do not support such bonding status, or connectivity in your input file is wrong"
				cycle EEMcyc
			end if
			tmpval=Aparm(a(iatm)%index,imulti)
			if (tmpval==-1) then
				write(*,"(' Error: Parameter for atom',i5,'(',a,') is unavailable!')") iatm,a(iatm)%name
				cycle EEMcyc
			else
				EEMarr(iatm)=-tmpval
			end if
		end do

		!Construct EEM matrix
		EEMmat=0
		EEMmat(ncenter+1,1:ncenter)=1
		EEMmat(1:ncenter,ncenter+1)=-1
		do i=1,ncenter
			imulti=maxval(connmat(i,:))
			do j=1,ncenter
				if (i==j) then
					EEMmat(i,j)=Bparm(a(i)%index,imulti)
				else
					EEMmat(i,j)=kappa/(distmat(i,j)*b2a)
				end if
			end do
		end do
		
		!Solve EEM equation
		qarr=matmul(invmat(EEMmat,ncenter+1),EEMarr)
		do iatm=1,ncenter
			write(*,"(' EEM charge of atom',i5,'(',a,'):',f12.6)") iatm,a(iatm)%name,qarr(iatm)
		end do
		write(*,"(' Electronegativity:',f12.6)") qarr(ncenter+1)
	end if

end do EEMcyc
end subroutine

!---- Generate EEM parameters
subroutine genEEMparm(iset,kappa,Aparm,Bparm)
use defvar
integer,parameter :: maxBO=3 !Maximum of bond order
real*8 kappa,Aparm(nelesupp,maxBO),Bparm(nelesupp,maxBO)
Aparm=-1
Bparm=-1
if (iset==1) then !Parameters fitted to Mulliken charge at HF/STO-3G, Int. J. Mol. Sci., 8, 572-582 (2007)
	write(*,"(a)") " Parameters have been set to those fitted to Mulliken charges at HF/STO-3G, see Int. J. Mol. Sci., 8, 572-582 (2007)"
	kappa=0.44D0
	Aparm(1,1)= 2.396D0  !H
	Bparm(1,1)= 0.959D0
	Aparm(6,1)= 2.459D0  !C,multi=1
	Bparm(6,1)= 0.611D0
	Aparm(7,1)= 2.597D0  !N,multi=1
	Bparm(7,1)= 0.790D0
	Aparm(8,1)= 2.625D0  !O,multi=1
	Bparm(8,1)= 0.858D0
	Aparm(16,1)= 2.407D0  !S,multi=1
	Bparm(16,1)= 0.491D0
	Aparm(6,2)= 2.464D0  !C,multi=2
	Bparm(6,2)= 0.565D0
	Aparm(7,2)= 2.554D0  !N,multi=2
	Bparm(7,2)= 0.611D0
	Aparm(8,2)= 2.580D0  !O,multi=2
	Bparm(8,2)= 0.691D0
else if (iset==2) then !Parameters fitted to CHELPG charges at B3LYP/6-31G*, J. Comput. Chem., 30, 1174 (2009)
	write(*,"(a)") " Parameters have been set to those fitted to CHELPG charges at B3LYP/6-31G*, see J. Comput. Chem., 30, 1174 (2009)"
	kappa=0.302D0
	Aparm(35,1)= 2.659D0  !Br,multi=1
	Bparm(35,1)= 1.802D0
	Aparm(6,1)= 2.482D0  !C,multi=1
	Bparm(6,1)= 0.464D0
	Aparm(17,1)= 2.519D0  !Cl,multi=1
	Bparm(17,1)= 1.450D0
	Aparm(9,1)= 3.577D0  !F,multi=1
	Bparm(9,1)= 3.419D0
	Aparm(1,1)= 2.385D0  !H,multi=1
	Bparm(1,1)= 0.737D0
	Aparm(7,1)= 2.595D0  !N,multi=1
	Bparm(7,1)= 0.468D0
	Aparm(8,1)= 2.825D0  !O,multi=1
	Bparm(8,1)= 0.844D0
	Aparm(16,1)= 2.452D0  !S,multi=1
	Bparm(16,1)= 0.362D0
	Aparm(30,1)= 2.298D0  !Zn,multi=1
	Bparm(30,1)= 0.420D0
	Aparm(6,2)= 2.464D0  !C,multi=2
	Bparm(6,2)= 0.392D0
	Aparm(7,2)= 2.556D0  !N,multi=2
	Bparm(7,2)= 0.377D0
	Aparm(8,2)= 2.789D0  !O,multi=2
	Bparm(8,2)= 0.834D0
else if (iset==3) then !Parameters fitted to CHELPG charges at HF/6-31G*, J. Comput. Chem., 30, 1174 (2009)
	write(*,"(a)") " Parameters have been set to those fitted to CHELPG charges at HF/6-31G*, see J. Comput. Chem., 30, 1174 (2009)"
	kappa=0.227D0
	Aparm(35,1)= 2.615D0  !Br,multi=1
	Bparm(35,1)= 1.436D0
	Aparm(6,1)= 2.481D0  !C,multi=1
	Bparm(6,1)= 0.373D0
	Aparm(17,1)= 2.517D0  !Cl,multi=1
	Bparm(17,1)= 1.043D0
	Aparm(9,1)= 3.991D0  !F,multi=1
	Bparm(9,1)= 3.594D0
	Aparm(1,1)= 2.357D0  !H,multi=1
	Bparm(1,1)= 0.688D0
	Aparm(7,1)= 2.585D0  !N,multi=1
	Bparm(7,1)= 0.329D0
	Aparm(8,1)= 2.870D0  !O,multi=1
	Bparm(8,1)= 0.717D0
	Aparm(16,1)= 2.450D0  !S,multi=1
	Bparm(16,1)= 0.269D0
	Aparm(30,1)= 2.185D0  !Zn,multi=1
	Bparm(30,1)= 0.375D0
	Aparm(6,2)= 2.475D0  !C,multi=2
	Bparm(6,2)= 0.292D0
	Aparm(7,2)= 2.556D0  !N,multi=2
	Bparm(7,2)= 0.288D0
	Aparm(8,2)= 2.757D0  !O,multi=2
	Bparm(8,2)= 0.621D0
else if (iset==4) then !Parameters fitted to NPA charges at B3LYP/6-311G*, see J. Cheminform., 8, 57 (2016)
!The data were taken from "13321_2016_171_MOESM5_ESM.rar Additional file 5" of supplmental material
!13321_2016_171_MOESM5_ESM\neemp\ideal_q1\set3_DE_RMSD_B3LYP_6311Gd_NPA_cross_ideal\output_set3_DE_RMSD_B3LYP_6311Gd_NPA_cross_ideal_all
	write(*,"(a)") " Parameters have been set to those fitted to NPA charges at B3LYP/6-311G*, they were extracted from SI of J. Cheminform., 8, 57 (2016)"
	kappa=0.4024D0
	Aparm(1,1)= 2.4598D0  !H,multi=1
	Bparm(1,1)= 0.9120D0
	Aparm(6,1)= 2.5957D0  !C,multi=1
	Bparm(6,1)= 0.5083D0
	Aparm(6,2)= 2.6029D0  !C,multi=2
	Bparm(6,2)= 0.5021D0
	Aparm(6,3)= 2.5326D0  !C,multi=3
	Bparm(6,3)= 0.5932D0
	Aparm(7,1)= 2.7802D0  !N,multi=1
	Bparm(7,1)= 0.7060D0
	Aparm(7,2)= 2.7141D0  !N,multi=2
	Bparm(7,2)= 0.5366D0
	Aparm(7,3)= 2.6391D0  !N,multi=3
	Bparm(7,3)= 0.5171D0
	Aparm(8,1)= 2.9496D0  !O,multi=1
	Bparm(8,1)= 0.8264D0
	Aparm(8,2)= 2.8595D0  !O,multi=2
	Bparm(8,2)= 0.6589D0
	Aparm(9,1)= 2.9165D0  !F,multi=1
	Bparm(9,1)= 0.8427D0
	Aparm(15,2)= 2.1712D0  !P,multi=2
	Bparm(15,2)= 0.4802D0
	Aparm(16,1)= 2.5234D0  !S,multi=1
	Bparm(16,1)= 0.3726D0
	Aparm(16,2)= 2.5334D0  !S,multi=2
	Bparm(16,2)= 0.3519D0
	Aparm(17,1)= 2.5625D0  !Cl,multi=1
	Bparm(17,1)= 0.9863D0
	Aparm(35,1)= 2.4772D0  !Br,multi=1
	Bparm(35,1)= 1.2131D0
end if
end subroutine