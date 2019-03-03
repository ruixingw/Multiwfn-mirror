!!------------------------------------------------------
!! ----------- Orbital localization analysis ----------- 
!!------------------------------------------------------
!The final wavefunction can be exported as .fch, I don't select .molden because it doesn't record atomic charge, this will be problematic when ECP is used
subroutine orbloc
use defvar
use util
use function
implicit real*8 (a-h,o-z)
integer :: maxcyc=80,ireload=1,idoene=0,idocore=1,imethod=1,domark(4),iorbcomp=1,iPMexp=2,ilmocen=0
real*8 :: arrayi(nbasis),arrayj(nbasis),crit=1D-4,tmpbasarr(nbasis),tmpprimarr(nprims),bastot(nbasis)
real*8,pointer :: Cmat(:,:)
real*8,allocatable :: FLMOA(:,:),FLMOB(:,:),Xmat(:,:),Xmatinv(:,:),SC(:,:)
real*8 :: orbcomp(ncenter,nbasis) !Used in printing major orbital character
integer :: istatarr(nbasis),iatmarr(ncenter,nbasis) !Used in printing major orbital character
real*8 :: irj(3),iri(3),jrj(3) !Used for Boys localization, store dipole moment integral between two orbitals
real*8 :: crit1c=0.9D0,crit2c=0.85D0
character c200tmp*200,typestr*4
!Below for calculating LMO centers
real*8,allocatable :: orbvalpt(:,:)
real*8 beckeweigrid(radpot*sphpot),lmoposx(nmo),lmoposy(nmo),lmoposz(nmo)
type(content) gridatm(radpot*sphpot),gridatmorg(radpot*sphpot)

if (wfntype==2.or.wfntype==3.or.wfntype==4) then
	write(*,*) "Error: This function only works for restricted or unrestricted SCF wavefunction!"
	write(*,*) "Press ENTER button to return"
	read(*,*)
	return
end if
if (.not.allocated(CObasa)) then
	write(*,*) "Error: Basis function information was not provided by your input file!"
	write(*,*) "Press ENTER button to return"
	read(*,*)
	return
end if

do while(.true.)
	write(*,*)
	write(*,*) "                ======== Orbital localization analysis ========"
	if (iorbcomp==1) write(*,*) "-10 If identifying major character of LMOs, current: Yes"
	if (iorbcomp==0) write(*,*) "-10 If identifying major character of LMOs, current: No"
	if (ilmocen==1) write(*,*) "-8 If adding center of LMOs as Bq atoms, current: Yes"
	if (ilmocen==0) write(*,*) "-8 If adding center of LMOs as Bq atoms, current: No"
	!By default exponent of 2 is used for PM. exponent of 4 can also be used by option -7. However, according to my test, 
	!p=4 converges slower than p=2 for both Mulliken and Lowdin populations, and the localization is not as substantial, idea as p=2, 
	!so I finally decide not to expose this option to users
! 	if (imethod==1.or.imethod==2) write(*,"(a,i3)") " -7 Set exponent of Pipek-Mezey method, current:",iPMexp
	if (imethod==1) write(*,*) "-6 Set localization method, current: Pipek-Mezey with Mulliken population"
	if (imethod==2) write(*,*) "-6 Set localization method, current: Pipek-Mezey with Lowdin population"
	if (imethod==10) write(*,*) "-6 Set localization method, current: Foster-Boys"
	if (idocore==1) write(*,*) "-5 If also localizing core orbitals, current: Yes"
	if (idocore==0) write(*,*) "-5 If also localizing core orbitals, current: No"
	if (idoene==1) write(*,*) "-4 If calculating and printing orbital energies, current: Yes"
	if (idoene==0) write(*,*) "-4 If calculating and printing orbital energies, current: No"
	if (ireload==1) write(*,*) "-3 If reloading newly generated .fch file, current: Yes"
	if (ireload==0) write(*,*) "-3 If reloading newly generated .fch file, current: No"
	write(*,"(a,f12.8)") " -2 Set criterion of convergence, current:",crit
	write(*,"(a,i4)") " -1 Set maximum cycles, current:",maxcyc
	write(*,*) "0 Return"
	write(*,*) "1 Localizing occupied orbitals only"
	write(*,*) "2 Localizing both occupied and unoccupied orbitals separately"
	read(*,*) isel
	if (isel==0) then
		return
	else if (isel==-1) then
		write(*,*) "Input maximum cycles, e.g. 30"
		read(*,*) maxcyc
	else if (isel==-2) then
		write(*,*) "Input criterion of convergence, e.g. 0.0001"
		read(*,*) crit
	else if (isel==-3) then
		if (ireload==1) then
			ireload=0
		else if (ireload==0) then
			ireload=1
		end if
	else if (isel==-4) then
		if (idoene==1) then
			idoene=0
		else if (idoene==0) then
			call loadFock47(idoene)
		end if
	else if (isel==-5) then
		if (idocore==1) then
			idocore=0
		else if (idocore==0) then
			idocore=1
		end if
	else if (isel==-6) then
		write(*,*) "Select orbital localization method"
		write(*,"(a)") " Hint: 2 is the fastest, but not suitable when diffuse functions are presented. &
		10 is the lowest, but fully compatible with diffuse functions"
		write(*,*) "1 Pipek-Mezey based on Mulliken type of population"
		write(*,*) "2 Pipek-Mezey based on Lowdin type of population"
		write(*,*) "10 Foster-Boys"
		read(*,*) imethod
		if (imethod==10.and.igenDbas==0) then !Haven't calculated dipole moment integral matrix, so reload the input file and calculate it
			igenDbas=1
			write(*,*) "Reloading input file and meantime generating dipole moment integral matrix..."
			write(*,*)
			call dealloall
			call readinfile(firstfilename,1)
		end if
	else if (isel==-7) then
		write(*,*) "Input exponent of Pipek-Mezey method. 2 and 4 is allowed"
		write(*,"(a)") " Note: Original paper of PM method use exponent of 2, while 4 is shown to give LMO with more localized character"
		read(*,*) iPMexp
		if (iPMexp/=2.and.iPMexp/=4) then
			write(*,*) "Input error! The value should be either 2 or 4!"
			write(*,*) "Press ENTER button to continue"
			read(*,*)
			iPMexp=2
		end if
	else if (isel==-8) then
		if (ilmocen==0) then
			if (ireload==0) then
				write(*,"(a)") " Error: To use this function, you must first switch the option ""If reloading newly generated .fch file"" to ""Yes"""
				write(*,*) "Press ENTER to continue"
				read(*,*)
				cycle
			end if
			ilmocen=1
		else if (ilmocen==1) then
			ilmocen=0
		end if
	else if (isel==-10) then
		if (iorbcomp==1) then
			iorbcomp=0
		else if (iorbcomp==0) then
			iorbcomp=1
			write(*,*) "Input threshold for determining single and two center LMOs"
			write(*,"(a)") " e.g. Input 0.9,0.85 means using 90% and 85% for the former and latter, respectively"
			read(*,*) crit1c,crit2c
		end if
	else if (isel==1.or.isel==2) then
		exit
	end if
end do

call walltime(iwalltime1)
CALL CPU_TIME(time_begin)

domark=0
if (wfntype==0) then
	domark(1)=1
	if (isel==2) domark(2)=1
else if (wfntype==1) then
	domark(1)=1
	domark(3)=1
	if (isel==2) domark=1
end if

if (idocore==0) then
	call getninnerele(ninnerele,0) !Count the number of inner electrons
	write(*,"(' Note: Lowest',i5,' orbitals are regarded as core orbitals and will not be localized')") ninnerele/2
end if

!Preparation work for specific method
if (imethod==1) then !PM with Mulliken
	allocate(SC(nbasis,nbasis))
else if (imethod==2) then !PM with lowdin
	write(*,*) "Performing Lowdin orthonormalization..."
	allocate(Xmat(nbasis,nbasis),Xmatinv(nbasis,nbasis))
	call symmorthomat(Sbas,Xmat,Xmatinv)
	CObasa=matmul(Xmat,CObasa)
	if (allocated(CObasb)) CObasb=matmul(Xmat,CObasb)
	Sbas_org=Sbas
	Sbas=0
	forall (i=1:nbasis) Sbas(i,i)=1
end if

!Carry out localization
!Alpha-occ,Alpha-vir,Beta-occ,Beta-vir
do itime=1,4
	if (domark(itime)==0) cycle
	if (itime<=2) then
		Cmat=>CObasa
	else
		Cmat=>CObasb
	end if
	if (itime==1) then
		nmobeg=1
		if (idocore==0) nmobeg=ninnerele/2+1
		nmoend=naelec
	else if (itime==2) then
		nmobeg=naelec+1
		nmoend=nbasis
	else if (itime==3) then
		nmobeg=1
		if (idocore==0) nmobeg=ninnerele/2+1
		nmoend=nbelec
	else if (itime==4) then
		nmobeg=nbelec+1
		nmoend=nbasis
	end if
	if (imethod==1) SC=matmul(Sbas,Cmat) !Generate intermediate matrix SC for PM-Mulliken for lowering cost from N^4 to N^3
	
	if (wfntype==0) then
		if (itime==1) write(*,"(/,a)") " Localizing occupied orbitals..."
		if (itime==2) write(*,"(/,a)") " Localizing unoccupied orbitals..."
	else if (wfntype==1) then
		if (itime==1) write(*,"(/,a)") " Localizing alpha occupied orbitals..."
		if (itime==2) write(*,"(/,a)") " Localizing alpha unoccupied orbitals..."
		if (itime==3) write(*,"(/,a)") " Localizing beta occupied orbitals..."
		if (itime==4) write(*,"(/,a)") " Localizing beta unoccupied orbitals..."
	end if

	Pvalold=0
	do icyc=1,maxcyc
		do imo=nmobeg,nmoend-1 !Cycle each orbital pair
			do jmo=imo+1,nmoend
				if (imethod==1) then !Pipek-Mezey with Mulliken population
					Aval=0
					Bval=0
					do iatm=1,ncenter
						istart=basstart(iatm)
						iend=basend(iatm)
						Qij=0.5D0*sum(Cmat(istart:iend,jmo)*SC(istart:iend,imo)+Cmat(istart:iend,imo)*SC(istart:iend,jmo))
						Qii=sum(Cmat(istart:iend,imo)*SC(istart:iend,imo))
						Qjj=sum(Cmat(istart:iend,jmo)*SC(istart:iend,jmo))
						if (iPMexp==2) then
							Aval=Aval+( Qij**2-(Qii-Qjj)**2/4D0 )
							Bval=Bval+( Qij*(Qii-Qjj) )
						else if (iPMexp==4) then
							Aval=Aval-Qii**4-Qjj**4+6*(Qii**2+Qjj**2)*Qij**2+Qii**3*Qjj+Qjj**3*Qii
							Bval=Bval+4*Qij*(Qii**3-Qjj**3)
						end if
					end do
				else if (imethod==2) then !Pipek-Mezey with Lowdin population
					Aval=0
					Bval=0
					do iatm=1,ncenter
						istart=basstart(iatm)
						iend=basend(iatm)
						Qij=sum(Cmat(istart:iend,imo)*Cmat(istart:iend,jmo))
						Qii=sum(Cmat(istart:iend,imo)*Cmat(istart:iend,imo))
						Qjj=sum(Cmat(istart:iend,jmo)*Cmat(istart:iend,jmo))
						if (iPMexp==2) then
							Aval=Aval+( Qij**2-(Qii-Qjj)**2/4D0 )
							Bval=Bval+( Qij*(Qii-Qjj) )
						else if (iPMexp==4) then
							Aval=Aval-Qii**4-Qjj**4+6*(Qii**2+Qjj**2)*Qij**2+Qii**3*Qjj+Qjj**3*Qii
							Bval=Bval+4*Qij*(Qii**3-Qjj**3)
						end if
					end do
				else if (imethod==10) then !Boys
					call boysdipint(iri,jrj,irj,imo,jmo,Cmat)
					Aval=sum(irj**2)-sum((iri-jrj)**2)/4
					Bval=sum(irj*(iri-jrj))
				end if
				if (Aval**2+Bval**2<1D-12) cycle
				gamma=sign(1D0,Bval)*acos(-Aval/dsqrt(Aval**2+Bval**2))/4D0
				arrayi=cos(gamma)*Cmat(:,imo)+sin(gamma)*Cmat(:,jmo)
				arrayj=-sin(gamma)*Cmat(:,imo)+cos(gamma)*Cmat(:,jmo)
				Cmat(:,imo)=arrayi
				Cmat(:,jmo)=arrayj
				if (imethod==1) then !For PM-Mulliken, also correspondingly update intermediate matrix
					arrayi=cos(gamma)*SC(:,imo)+sin(gamma)*SC(:,jmo)
					arrayj=-sin(gamma)*SC(:,imo)+cos(gamma)*SC(:,jmo)
					SC(:,imo)=arrayi
					SC(:,jmo)=arrayj
				end if
			end do
		end do
		Pval=0
		do imo=nmobeg,nmoend
			do iatm=1,ncenter
				Pval=Pval+Qval(Cmat,imo,imo,iatm)**iPMexp
			end do
		end do
		deltaPval=Pval-Pvalold
		write(*,"(' Cycle:',i5,'  P:',f16.8,'  Delta P:',f16.8)") icyc,Pval,deltaPval
		if (abs(deltaPval)<crit) exit
		Pvalold=Pval
	end do
	if (icyc==maxcyc+1) then
		write(*,*) "Warning: Convergence failed!"
	else
		write(*,"(a)") " Successfully converged!"
	end if
end do

if (imethod==2) then !PM with lowdin. Back convert CObas from orthonormal basis to original basis
	CObasa=matmul(Xmatinv,CObasa)
	if (allocated(CObasb)) CObasb=matmul(Xmatinv,CObasb)
	Sbas=Sbas_org
end if

CALL CPU_TIME(time_end)
call walltime(iwalltime2)
write(*,"(/,' Calculation took up CPU time',f12.2,'s, wall clock time',i10,'s',/)") time_end-time_begin,iwalltime2-iwalltime1


!Print orbital energies, sort orbitals according to energies
if (idoene==1) then
	nmobeg=1
	if (idocore==0) nmobeg=ninnerele/2+1
	!Do Alpha part or closed-shell orbitals
	allocate(FLMOA(nbasis,nbasis))
	FLMOA=matmul(matmul(transpose(CObasa),FmatA),CObasa)
	if (isel==1) nmoend=naelec
	if (isel==2) nmoend=nbasis
	do iorb=nmobeg,nmoend
		MOene(iorb)=FLMOA(iorb,iorb)
	end do
	do iorb=nmobeg,nmoend
		do jorb=iorb+1,nmoend
			if (MOene(iorb)>MOene(jorb)) then	
				tmpbasarr=CObasa(:,iorb)
				CObasa(:,iorb)=CObasa(:,jorb)
				CObasa(:,jorb)=tmpbasarr
				tmpprimarr=CO(iorb,:)
				CO(iorb,:)=CO(jorb,:)
				CO(jorb,:)=tmpprimarr
				tmpene=MOene(iorb)
				MOene(iorb)=MOene(jorb)
				MOene(jorb)=tmpene
			end if
		end do
	end do
	write(*,*) "Energies of localized orbitals:"
	do iorb=nmobeg,nmoend
		typestr="A+B"
		if (wfntype==1)	typestr="A"
		write(*,"(i6,'   Energy:',f13.7,' a.u.',f13.4,' eV   Type: ',a,'  Occ:',f4.1)") &
		iorb,MOene(iorb),MOene(iorb)*au2eV,typestr,MOocc(iorb)
	end do
	!Do beta part
	if (wfntype==1) then
		allocate(FLMOB(nbasis,nbasis))
		FLMOB=matmul(matmul(transpose(CObasb),FmatB),CObasb)
		if (isel==1) nmoend=nbelec
		if (isel==2) nmoend=nbasis
		do iorb=nmobeg,nmoend
			MOene(nbasis+iorb)=FLMOB(iorb,iorb)
		end do
		do iorb=nmobeg,nmoend
			do jorb=iorb+1,nmoend
				if (MOene(nbasis+iorb)>MOene(nbasis+jorb)) then	
					tmpbasarr=CObasb(:,iorb)
					CObasb(:,iorb)=CObasb(:,jorb)
					CObasb(:,jorb)=tmpbasarr
					tmpprimarr=CO(nbasis+iorb,:)
					CO(nbasis+iorb,:)=CO(nbasis+jorb,:)
					CO(nbasis+jorb,:)=tmpprimarr
					tmpene=MOene(nbasis+iorb)
					MOene(nbasis+iorb)=MOene(nbasis+jorb)
					MOene(nbasis+jorb)=tmpene
				end if
			end do
		end do
		typestr="B"
		do iorb=nmobeg,nmoend
			write(*,"(i6,'   Energy:',f13.7,' a.u.',f13.4,' eV   Type: ',a,'  Occ:',f4.1)") &
			iorb+nbasis,MOene(iorb+nbasis),MOene(iorb+nbasis)*au2eV,typestr,MOocc(iorb+nbasis)
		end do
	end if
	if (idocore==0)  write(*,*) "Energies of core orbitals are not updated since they were not localized"
	if (isel==1) write(*,*) "Energies of unoccupied orbitals are not updated since they were not localized"
	
	!Second-order perturbation analysis between occupied and virtual orbitals (like NBO E2), this only works when both of them have been localized
	!This part is commented since it don't print any useful result, because it is easy to proved that Fock element between occupied and virtual LMOs are exactly zero
! 	if (isel==1) then
! 		write(*,*) " Note: E(2) analysis is skipped since virtual orbitals were not localized"
! 	else if (isel==2) then
! 		write(*,*) "Second-order perturbation theory analysis of interaction energy:"
! 		!Regenerated Fock matrix in LMO, since they have been sorted
! 		FLMOA=matmul(matmul(transpose(CObasa),FmatA),CObasa)
! 		call showmatgau(FLMOA)
! 		read(*,*)
! 		coeff=2
! 		do iocc=1,naelec
! 			do ivir=naelec+1,nbasis
! 				E2val=-coeff* FLMOA(iocc,ivir)**2/(MOene(ivir)-MOene(iocc))
! 				qCT=coeff* ( FLMOA(iocc,ivir)/(MOene(ivir)-MOene(iocc)) )**2
! 				if (abs(E2val*au2kcal)>0.2D0) then
! 					write(*,"(' Donor:',i5,'  -  Acceptor:',i5,'   E(2):',f7.2,' kcal/mol   q_CT:',f10.5)") iocc,ivir,E2val*au2kcal,qCT
! 					write(*,"(3f16.10)") FLMOA(iocc,ivir),MOene(ivir)-MOene(iocc)
! 				end if
! 			end do
! 		end do
! 	end if
end if


!Calculate orbital composition and print major character of LMOs
if (iorbcomp==1) then
	if (isel==1) write(*,"(a)") " Note: Mulliken method is used to derive orbital compositions &
	for the LMOs. Result is unreliable when diffuse functions are presented"
	if (isel==2) write(*,"(a)") " Note: Mulliken and SCPA methods are used to derive orbital compositions &
	for occupied and virtual LMOs, respectively. Result is unreliable when diffuse functions are presented)"
	!Alpha-occ,Alpha-vir,Beta-occ,Beta-vir
	do itime=1,4
		if (domark(itime)==0) cycle
		if (itime<=2) then
			Cmat=>CObasa
		else
			Cmat=>CObasb
		end if
		if (itime==1) then
			ibeg=1
			if (idocore==0) ibeg=ninnerele/2+1
			iend=naelec
		else if (itime==2) then
			ibeg=naelec+1
			iend=nbasis
		else if (itime==3) then
			ibeg=1
			if (idocore==0) ibeg=ninnerele/2+1
			iend=nbelec
		else if (itime==4) then
			ibeg=nbelec+1
			iend=nbasis
		end if
		
		!Calculate orbital composition and then sort from large to small
		do iorb=ibeg,iend
			if (itime==1.or.itime==3) then !For occupied LMOs, use Mulliken
				do ibas=1,nbasis
					bascross=0D0
					do jbas=1,nbasis
						if (jbas==ibas) cycle
						bascross=bascross+Cmat(ibas,iorb)*Cmat(jbas,iorb)*Sbas(ibas,jbas)
					end do
					bastot(ibas)=Cmat(ibas,iorb)**2+bascross
				end do
				do iatm=1,ncenter
					orbcomp(iatm,iorb)=sum(bastot(basstart(iatm):basend(iatm)))
				end do
			else !For virtual LMOs, use SCPA. Since it guarantees that the result is within 0~100%
 				do iatm=1,ncenter
 					orbcomp(iatm,iorb)=sum(Cmat(basstart(iatm):basend(iatm),iorb)**2)
 				end do
 				orbcomp(:,iorb)=orbcomp(:,iorb)/sum(orbcomp(:,iorb))
			end if
			
			forall(iatm=1:ncenter) iatmarr(iatm,iorb)=iatm
			call sort(orbcomp(:,iorb),"val",iatmarr(:,iorb)) !Sort atomic contributions from small to large
			call invarr(orbcomp(:,iorb),iatmarr(:,iorb)) !Then become from large to small
		end do
		
		if (wfntype==0) then
			if (itime==1) write(*,"(/,a)") " Major character of occupied LMOs:"
			if (itime==2) write(*,"(/,a)") " Major character of unoccupied LMOs:"
		else if (wfntype==1) then
			if (itime==1) write(*,"(/,a)") " Major character of alpha occupied LMOs:"
			if (itime==2) write(*,"(/,a)") " Major character of alpha unoccupied LMOs:"
			if (itime==3) write(*,"(/,a)") " Major character of beta occupied LMOs:"
			if (itime==4) write(*,"(/,a)") " Major character of beta unoccupied LMOs:"
		end if
		istatarr=0 !If =1, then the character of the LMO has been identified and printed
		write(*,"(' Almost single center LMOs: (An atom has contribution >',f5.1,'%)')") crit1c*100
		itmp=0
		do iorb=ibeg,iend
			if (orbcomp(1,iorb)>crit1c) then
				write(*,"(i5,':',i4,'(',a,')',f5.1,'%    ')",advance='no') iorb,iatmarr(1,iorb),a(iatmarr(1,iorb))%name,orbcomp(1,iorb)*100
				itmp=itmp+1
				if (mod(itmp,3)==0) write(*,*)
				istatarr(iorb)=1
			end if
		end do
		if (mod(itmp,3)/=0) write(*,*)
		if (itmp==0) write(*,*) "None!"
		write(*,*)
		
		if (all(istatarr(ibeg:iend)==1)) cycle
		write(*,"(' Almost two center LMOs: (Sum of two largest contributions >',f5.1,'%)')") crit2c*100
		itmp=0
		do iorb=ibeg,iend
			if (istatarr(iorb)==1) cycle
			if (orbcomp(1,iorb)+orbcomp(2,iorb)>crit2c) then
				write(*,"(i5,':',i4,'(',a,')',f5.1,'%',i4,'(',a,')',f5.1,'%     ')",advance='no') &
				iorb,iatmarr(1,iorb),a(iatmarr(1,iorb))%name,orbcomp(1,iorb)*100,&
				iatmarr(2,iorb),a(iatmarr(2,iorb))%name,orbcomp(2,iorb)*100
				itmp=itmp+1
				if (mod(itmp,2)==0) write(*,*)
				istatarr(iorb)=1
			end if
		end do
		if (mod(itmp,2)/=0) write(*,*)
		if (itmp==0) write(*,*) "None!"
		write(*,*)
		
		if (all(istatarr(ibeg:iend)==1)) cycle
		write(*,*) "More delocalized LMOs: (Three largest contributions are printed)"
		do iorb=ibeg,iend
			if (istatarr(iorb)==1) cycle
			write(*,"(i5,':',i5,'(',a,')',f5.1,'%',i5,'(',a,')',f5.1,'%',i5,'(',a,')',f5.1,'%')") &
			iorb,iatmarr(1,iorb),a(iatmarr(1,iorb))%name,orbcomp(1,iorb)*100,&
			iatmarr(2,iorb),a(iatmarr(2,iorb))%name,orbcomp(2,iorb)*100,&
			iatmarr(3,iorb),a(iatmarr(3,iorb))%name,orbcomp(3,iorb)*100
		end do
		write(*,*)
	end do
end if

igenDbas_old=igenDbas
if (ilmocen==1) igenDbas=1

write(*,*) "Exporting localized orbitals to new.fch in current folder"
call outfch("new.fch",10,1)

if (ireload==1) then
	call dealloall
	write(*,*) "Loading new.fch..."
	call readfch("new.fch",1)
	write(*,"(a)") " Loading finished, now you can use main function 0 to visualize them as isosurface"

	!Adding center of LMOs as Bq atoms
	if (ilmocen==1) then
		igenDbas=igenDbas_old
		write(*,*)
		write(*,*) "Calculating center of LMOs and meanwhile adding them as Bq atoms..."
		
		!Backup a to a_tmp, then add Bq to a_tmp during calculation, and finally copy a_tmp to a
		if (isel==1) then
			ncenter_new=ncenter+nint(naelec)
			if (wfntype==1) ncenter_new=ncenter_new+nint(nbelec)
		else if (isel==2) then
			ncenter_new=ncenter+nbasis
			if (wfntype==1) ncenter_new=ncenter_new+nbasis
		end if
		if (idocore==0) then
			if (wfntype==0) ncenter_new=ncenter_new-ninnerele/2
			if (wfntype==1) ncenter_new=ncenter_new-ninnerele
		end if
		allocate(a_tmp(ncenter_new))
		a_tmp(1:ncenter)=a
		itmp=ncenter
		
		!Calculate LMO center and add Bq
		if (iautointgrid==1) then !This setting is good balance between cost and accuracy
			radpotold=radpot
			sphpotold=sphpot
			radcutold=radcut
			radpot=20
			sphpot=170
			radcut=18 !Enlarge radcut, because for Rydberg orbital, the default radcut 10 Bohr is not sufficient
			write(*,"(a)") " Note: The default integration grid in general should be sufficient. If you want to change, &
			set ""iautointgrid"" in settings.ini to 0, and set ""radpot"" and ""sphpot"" to expected values"
		end if
		
		write(*,"(' Radial points:',i5,'    Angular points:',i5,'   Total:',i10,' per center')") radpot,sphpot,radpot*sphpot
		call gen1cintgrid(gridatmorg,iradcut)
		
		lmoposx=0;lmoposy=0;lmoposz=0
		allocate(orbvalpt(nmo,radpot*sphpot))
		
		open(10,file="LMOcen.txt",status="replace")
		if (wfntype==0) ntime=1
		if (wfntype==1) ntime=2
		do itime=1,ntime !=1: total or alpha, =2: beta
			if (itime==1) then
				ibeg=1
				if (idocore==0) ibeg=ninnerele/2+1
				iend=nint(naelec)
				if (isel==2) iend=nbasis
			else if (itime==2) then
				ibeg=nbasis+1
				if (idocore==0) ibeg=nbasis+ninnerele/2+1
				iend=nbasis+nint(nbelec)
				if (isel==2) iend=2*nbasis
			end if
			do iatm=1,ncenter
				!write(*,"(' Processing center',i6,'(',a2,')   /',i6)") iatm,a(iatm)%name,ncenter
				gridatm%x=gridatmorg%x+a(iatm)%x !Move quadrature point to actual position in molecule
				gridatm%y=gridatmorg%y+a(iatm)%y
				gridatm%z=gridatmorg%z+a(iatm)%z
				!$OMP parallel do shared(orbvalpt) private(i) num_threads(nthreads)
				do i=1+iradcut*sphpot,radpot*sphpot
					call orbderv(1,ibeg,iend,gridatm(i)%x,gridatm(i)%y,gridatm(i)%z,orbvalpt(:,i))
				end do
				!$OMP end parallel do
		
				call gen1cbeckewei(iatm,iradcut,gridatm,beckeweigrid)
				do ipt=1+iradcut*sphpot,radpot*sphpot
					totwei=gridatmorg(ipt)%value*beckeweigrid(ipt)
					do iorb=ibeg,iend
						orbsqr=orbvalpt(iorb,ipt)**2
						lmoposx(iorb)=lmoposx(iorb)+gridatm(ipt)%x*orbsqr*totwei
						lmoposy(iorb)=lmoposy(iorb)+gridatm(ipt)%y*orbsqr*totwei
						lmoposz(iorb)=lmoposz(iorb)+gridatm(ipt)%z*orbsqr*totwei
					end do
				end do
			end do
			do iorb=ibeg,iend
				xcen=lmoposx(iorb)
				ycen=lmoposy(iorb)
				zcen=lmoposz(iorb)
				itmp=itmp+1
				a_tmp(itmp)%x=xcen;a_tmp(itmp)%y=ycen;a_tmp(itmp)%z=zcen
				a_tmp(itmp)%index=0
				a_tmp(itmp)%charge=0
				a_tmp(itmp)%name="Bq"
				if (itime==1) then
					if (wfntype==0) write(10,"(' LMO',i6,' corresponds to Bq',i6,', X,Y,Z:',3f10.4,' Bohr')") iorb,itmp,xcen,ycen,zcen
					if (wfntype==1) write(10,"(' Alpha LMO',i6,': Bq',i6,', X,Y,Z:',3f10.4,' Bohr')") iorb,itmp,xcen,ycen,zcen
				else
					write(10,"(' Beta LMO ',i6,': Bq',i6,', X,Y,Z:',3f10.4,' Bohr')") iorb-nbasis,itmp,xcen,ycen,zcen
				end if
			end do
		end do
		deallocate(orbvalpt)
		close(10)
		if (iautointgrid==1) then
			radpot=radpotold
			sphpot=sphpotold
			radcut=radcutold
		end if
		
		ncenter=ncenter_new
		deallocate(a)
		allocate(a(ncenter))
		a=a_tmp
		deallocate(a_tmp)
		write(*,"(/,a)") " The Bq atoms in current system now correspond to center of LMOs. The LMO center coordinates, &
		correspondence between LMO indices and Bq indices have been exported to LMOcen.txt in current folder"
		write(*,"(a)") " Note: Since these Bq atoms do not have corresponding basis functions, &
		the present wavefunction should not be subjected to wavefunction analyses, otherwise Multiwfn may crash"
		write(*,*)
	end if
end if
end subroutine



!------ Calculate Q value used for determining localization convergence
real*8 function Qval(Cmat,imo,jmo,iatm)
use defvar
real*8 Cmat(nbasis,nbasis)
integer imo,jmo,iatm,ibas
Qval=0
do ibas=basstart(iatm),basend(iatm)
	Qval=Qval+sum( (Cmat(:,imo)*Cmat(ibas,jmo)+Cmat(ibas,imo)*Cmat(:,jmo)) *Sbas(ibas,:) )
end do
Qval=Qval/2D0
end function


!------ Calculate dipole moment integral between orbitals that involved in Boys localization
subroutine boysdipint(iri,jrj,irj,imo,jmo,Cmat)
use defvar
implicit real*8 (a-h,o-z)
integer imo,jmo
real*8 :: iri(3),jrj(3),irj(3),iripriv(3),jrjpriv(3),irjpriv(3),Cmat(nbasis,nbasis)
iri=0
jrj=0
irj=0
!$OMP parallel shared(iri,jrj,irj) private(iripriv,jrjpriv,irjpriv) num_threads(nthreads)
iripriv=0
jrjpriv=0
irjpriv=0
!$OMP do schedule(DYNAMIC)
do ibas=1,nbasis
	do jbas=1,nbasis
		iripriv=iripriv+Dbas(:,ibas,jbas)*Cmat(ibas,imo)*Cmat(jbas,imo)
		jrjpriv=jrjpriv+Dbas(:,ibas,jbas)*Cmat(ibas,jmo)*Cmat(jbas,jmo)
		irjpriv=irjpriv+Dbas(:,ibas,jbas)*Cmat(ibas,imo)*Cmat(jbas,jmo)
	end do
end do
!$OMP END DO
!$OMP CRITICAL
iri=iri+iripriv
jrj=jrj+jrjpriv
irj=irj+irjpriv
!$OMP END CRITICAL
!$OMP END PARALLEL
end subroutine