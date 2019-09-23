!Index of routines:
!orbcomp_main: Main interface of various orbital composition analyses
!orballcomp_MMPA: Interface for calculating basis function, shell and atom contribution via Mulliken, Stout-Politzer, SCPA
!orbfragcomp_MMPA: Interface for calculating fragment contribution via Mulliken, Stout-Politzer, SCPA
!orbatmcomp_space: Interface for calculating atom contribution via fuzzy partition methods
!orballcomp_NAO: Interface for calculating atom contribution via NAOMO method
!gen_orbatmcomp_MMPA: Returning all atom contributions to one orbitals via Mulliken and SCPA
!gen_allorbbascomp_SCPA: Returning all basis function contributions to all orbitals via SCPA
!gen_allorbatmcomp_SCPA: Returning all atom contributions to all orbitals via SCPA
    
    
    
    

!!--------- Interface of various orbital composition analyses
subroutine orbcomp_main
use defvar
implicit real*8 (a-h,o-z)

do while(.true.)
	write(*,"(/,a,/)") " If present module is used in your work, citing this paper along with Multiwfn original paper is highly recommended: &
	Tian Lu, Feiwu Chen, Calculation of Molecular Orbital Composition, Acta Chim. Sinica, 69, 2393-2406 (2011)"
	write(*,*) "      ================ Orbital composition analysis ==============="
	write(*,*) "-10 Return"
	if (allocated(CObasa)) then
		write(*,*) "-2 Define fragment 2 (for option 4,5)"
		write(*,*) "-1 Define fragment 1 (for option 1~6)"
		write(*,*) "1 Orbital composition analysis with Mulliken partition"
		write(*,*) "2 Orbital composition analysis with Stout-Politzer partition"
		write(*,*) "3 Orbital composition analysis with Ros-Schuit (SCPA) partition"
		write(*,*) "4 Print frag. 1 & inter-fragment compositions in all orbitals (Mulliken)"
		write(*,*) "5 Print frag. 1 & inter-fragment compositions in all orbitals (Stout-Politzer)"
		write(*,*) "6 Print frag. 1 compositions in all orbitals (SCPA)"
	end if
	write(*,*) "7 Orbital composition analysis by natural atomic orbital (NAO) method"
	if (allocated(b)) write(*,*) "8 Calculate atom and fragment contributions by Hirshfeld method"
	if (allocated(b)) write(*,*) "9 Calculate atom and fragment contributions by Becke method"
	if (allocated(b)) write(*,*) "10 Calculate atom and fragment contributions by Hirshfeld-I method"
	if (allocated(CObasa)) write(*,*) "100 Evaluate oxidation state by LOBA method"
	read(*,*) icompana

	if (icompana==-10) then
		if (allocated(frag1)) deallocate(frag1)
		if (allocated(frag2)) deallocate(frag2)
		exit
	else if (icompana==-1) then
		call deffrag(1)
	else if (icompana==-2) then
		call deffrag(2)
	else if (icompana==1.or.icompana==2.or.icompana==3) then
		if (icompana==1) call orballcomp_MMPA(1)
		if (icompana==2) call orballcomp_MMPA(2)
		if (icompana==3) call orballcomp_MMPA(3)
	else if (icompana==4.or.icompana==5.or.icompana==6) then
		if (.not.allocated(frag1)) then
			write(*,*) "Please use function -1 to define fragment #1"
			write(*,*)
			cycle
		end if
		if (.not.allocated(frag2).and.(icompana==4.or.icompana==5)) &
		write(*,*) "Note: Fragment 2 was not be defined"
		write(*,*)
		if (icompana==4) call orbfragcomp_MMPA(1)
		if (icompana==5) call orbfragcomp_MMPA(2)
		if (icompana==6) call orbfragcomp_MMPA(3)
	else if (icompana==7) then
		call orballcomp_NAO
	else if (icompana==8) then
		call orbatmcomp_space(1)
	else if (icompana==9) then
		call orbatmcomp_space(2)
	else if (icompana==10) then
		call Hirshfeld_I(2)
		call orbatmcomp_space(3)
	else if (icompana==100) then
		call LOBA
	end if
end do
end subroutine



!!-------------- Define fragment 1 & 2 and store to global array frag1 and frag2. "isel" denote define which fragment
!If the fragment is empty, then will not allcoate frag1/2 when exit
subroutine deffrag(isel)
use defvar
use util
implicit real*8 (a-h,o-z)
!fragtmp stores temporary basis index before exit setting interface, by default they are all zero,
!if basis function 4,89,32 are in this fragment, then value 4,89,32 will be in uncertain three slots of fragtmp
integer fragtmp(nbasis)
integer termtmp(nbasis) !Store each time read serial number
integer vectmp(nbasis) !used by "inv" command
integer atmsellist(ncenter),bassellist(nbasis)
character c200*200,lchar
fragtmp=0
if (isel==1.and.allocated(frag1)) then
	fragtmp(1:size(frag1))=frag1(:)
	deallocate(frag1)
else if (isel==2.and.allocated(frag2)) then
	fragtmp(1:size(frag2))=frag2(:)
	deallocate(frag2)
end if
10 write(*,*) "Commands and examples:"
write(*,*) "q: Save fragment and exit"
write(*,*) "clean: Clean current fragment"
write(*,*) "list: List basis functions in current fragment"
write(*,*) "all: List all basis functions of current system"
write(*,*) "addall: Add all basis functions to fragment"
write(*,*) "inv: Invert selection, viz. delete existing basis functions add all other ones"
write(*,*) "help: Print these help information again"
write(*,*) "cond: Add basis functions satisfying given conditions to fragment"
write(*,*) "a 2,5-8,12: Add all basis functions in atoms 2,5,6,7,8,12 to fragment"
write(*,*) "s 2,5-8,12: Add all basis functions in shells 2,5,6,7,8,12 to fragment"
write(*,*) "b 2,5-8,12: Add basis functions 2,5,6,7,8,12 to fragment"
write(*,*) "l s,d: Add basis functions of s and d angular moments to fragment"
write(*,*) "da 5,7,11-13: Delete all basis functions in atoms 5,7,11,12,13 from fragment"
write(*,*) "db 5,7,11-13: Delete basis functions 5,7,11,12,13 from fragment"

do while(.true.)
	read(*,"(a)") c200
	if (c200(1:4)=="help") then
		goto 10
	else if (c200(1:6)=="addall") then
		forall(i=1:nbasis) fragtmp(i)=i
		write(*,*) "Done!"
	else if (c200(1:1)=="q") then
		numbas=count(fragtmp/=0)
		if (isel==1.and.numbas>=1) allocate(frag1(numbas))
		if (isel==2.and.numbas>=1) allocate(frag2(numbas))
		write(*,*) "Basis function index in current fragment:"
		if (numbas==0) then
			write(*,*) "None"
		else
			do i=1,nbasis
				if (fragtmp(i)/=0) write(*,"(i5)",advance="no") fragtmp(i)
			end do
		end if
		write(*,*)
		write(*,*) "Fragment is saved"
		write(*,*)
		if (numbas>=1) then
			if (isel==1) frag1=pack(fragtmp,fragtmp/=0)
			if (isel==2) frag2=pack(fragtmp,fragtmp/=0)
		end if
		exit
	else if (c200(1:5)=="clean") then
		fragtmp=0
		write(*,*) "Done!"
	else if (c200(1:3)=="all") then
		write(*,*) "The basis functions with asterisk are those presented in current fragment"
		do i=1,nbasis
			if (any(fragtmp==i)) then
				write(*,"('* Basis:',i6,'    Shell:',i5,'    Center:',i5,'(',a2,')    Type: ',a)")&
				 i,basshell(i),bascen(i),a(bascen(i))%name,GTFtype2name(bastype(i))
			else
				write(*,"('  Basis:',i6,'    Shell:',i5,'    Center:',i5,'(',a2,')    Type: ',a)")&
				 i,basshell(i),bascen(i),a(bascen(i))%name,GTFtype2name(bastype(i))
			end if
		end do
	else if (c200(1:4)=="list") then
		write(*,*) "Basis functions in current fragment:"
		if (all(fragtmp==0)) then
			write(*,*) "None"
		else
            ntmp=0
			do i=1,nbasis
				if (fragtmp(i)/=0) then
                    write(*,"(i5)",advance="no") fragtmp(i)
                    ntmp=ntmp+1
                end if
			end do
            write(*,"(/,' Totally',i8,' basis functions')") ntmp
			write(*,*)
		end if
	else if (c200(1:3)=="inv") then
		vectmp=fragtmp
		fragtmp=0
		itmp=0
		do ibas=1,nbasis
			if (all(vectmp/=ibas)) then
				itmp=itmp+1
				fragtmp(itmp)=ibas
			end if 
		end do
		write(*,*) "Done!"
	else if (c200(1:4)=="cond") then
		write(*,"(a)") " Note: You will be prompted to input three conditions in turn, &
		the basis functions satisfying all conditions will be added to current fragment"
		write(*,*)
		write(*,*) "Condition 1: Input range of atoms, e.g. 2,5-8,12"
		write(*,*) "To select all atoms, simply input ""a"""
		read(*,"(a)") c200
		if (index(c200,'a')/=0) then
			nselatm=ncenter
			forall(i=1:nselatm) atmsellist(i)=i
		else
			call str2arr(c200,nselatm,atmsellist)
			if (any(atmsellist(1:nselatm)>ncenter)) then
				write(*,*) "ERROR: One or more atom indices exceeded valid range!"
				cycle
			end if
		end if
		write(*,*) "Condition 2: Input range of basis functions, e.g. 2,5-8,12"
		write(*,*) "To select all basis functions, simply input ""a"""
		read(*,"(a)") c200
		if (index(c200,'a')/=0) then
			nselbas=nbasis
			forall(i=1:nselbas) bassellist(i)=i
		else
			call str2arr(c200,nselbas,bassellist)
			if (any(bassellist(1:nselbas)>nbasis)) then
				write(*,*) "ERROR: One or more basis function indices exceeded valid range!"
				cycle
			end if
		end if
		write(*,*) "Condition 3: Choose basis function type"
        write(*,*) "Could be one of such as S, Y, Z, XY, YY, ZZZ, D+1, D 0 ..."
		write(*,*) "You can also choose shell type, one of S, P, D, F, G, H"
		write(*,*) """a"" means all types"
		read(*,"(a)") c200
		naddbas=0
		do ibasidx=1,nselbas !Examine all basis functions
			ibas=bassellist(ibasidx)
			iadd=0
			if ( any(fragtmp==ibas) ) cycle !Skip if already presented in current fragment
			if ( all(atmsellist(1:nselatm)/=bascen(ibas)) ) cycle !Atom index condition
			if ( index(c200,'a')==0) then !Basis function type condition
				itype=bastype(ibas)
				if ( trim(c200)=='P' .and. ( itype>=2.and.itype<=4 ) ) iadd=1
				if ( trim(c200)=='D' .and. ( (itype>=-5 .and.itype<=-1 ).or.(itype>=5 .and.itype<=10) ) ) iadd=1
				if ( trim(c200)=='F' .and. ( (itype>=-12.and.itype<=-6 ).or.(itype>=11.and.itype<=20) ) ) iadd=1
				if ( trim(c200)=='G' .and. ( (itype>=-21.and.itype<=-13).or.(itype>=21.and.itype<=35) ) ) iadd=1
				if ( trim(c200)=='H' .and. ( (itype>=-32.and.itype<=-22).or.(itype>=36.and.itype<=56) ) ) iadd=1
				if ( trim(c200)==GTFtype2name(bastype(ibas)) ) iadd=1 !Inputted is detailed type
			else
				iadd=1
			end if
			if (iadd==1) then
				do i=1,nbasis !Find space slot to save this basis function
					if (fragtmp(i)==0) then
						fragtmp(i)=ibas
						naddbas=naddbas+1
						exit
					end if
				end do
			end if
		end do
		write(*,"(' Done!',i6,' new basis functions have been added to current fragment')") naddbas
		
	else if (c200(1:2)=="a ".or.c200(1:2)=="s ".or.c200(1:2)=="b ".or.c200(1:2)=="db".or.c200(1:2)=="da") then
		call str2arr(c200(3:),nterm,termtmp)
		if (c200(1:2)=="a ") then
			if (any(termtmp(1:nterm)<=0).or.any(termtmp(1:nterm)>ncenter)) then
				write(*,*) "Atom index exceeded valid range! Ignoring..."
				cycle
			end if
			do iatm=1,nterm
				do ibas=basstart(termtmp(iatm)),basend(termtmp(iatm))
					if (all(fragtmp/=ibas)) then
						do i=1,nbasis !Find an empty slot to record this basis function
							if (fragtmp(i)==0) then
								fragtmp(i)=ibas
								exit
							end if
						end do
					end if
				end do
			end do
		else if (c200(1:2)=="s ") then
			do ibas=1,nbasis
				if (any(termtmp(1:nterm)==basshell(ibas)).and.all(fragtmp/=ibas)) then
					do j=1,nbasis !Find an empty slot to record this basis function
						if (fragtmp(j)==0) then
							fragtmp(j)=ibas
							exit
						end if
					end do
				end if
			end do
		else if (c200(1:2)=="b ") then
			do i=1,nterm
				if (all(fragtmp/=termtmp(i)).and.termtmp(i)<=nbasis.and.termtmp(i)>0) then
					do j=1,nbasis !Find empty slot to save this basis function
						if (fragtmp(j)==0) then
							fragtmp(j)=termtmp(i)
							exit
						end if
					end do
				end if
			end do
		else if (c200(1:2)=="db") then
			do i=1,nterm
				where(fragtmp==termtmp(i)) fragtmp=0
			end do
		else if (c200(1:2)=="da") then
			do iatm=1,nterm
				do ibas=basstart(termtmp(iatm)),basend(termtmp(iatm))
					where(fragtmp==ibas) fragtmp=0
				end do
			end do
		end if
		write(*,*) "Done!"
		
	else if (c200(1:2)=="l ") then
		naddbas=0
		do ibas=1,nbasis
			ido=0
			itype=bastype(ibas)
			if ( (index(c200,'s')/=0.or.index(c200,'S')/=0).and.itype==1 ) ido=1
			if ( (index(c200,'p')/=0.or.index(c200,'P')/=0).and.( itype>=2.and.itype<=4) ) ido=1
			if ( (index(c200,'d')/=0.or.index(c200,'D')/=0).and.( (itype>=-5 .and.itype<=-1 ).or.(itype>=5 .and.itype<=10) ) ) ido=1
			if ( (index(c200,'f')/=0.or.index(c200,'F')/=0).and.( (itype>=-12.and.itype<=-6 ).or.(itype>=11.and.itype<=20) ) ) ido=1
			if ( (index(c200,'g')/=0.or.index(c200,'G')/=0).and.( (itype>=-21.and.itype<=-13).or.(itype>=21.and.itype<=35) ) ) ido=1
			if ( (index(c200,'h')/=0.or.index(c200,'H')/=0).and.( (itype>=-32.and.itype<=-22).or.(itype>=36.and.itype<=56) ) ) ido=1
			if (ido==1.and.all(fragtmp/=ibas)) then
				do j=1,nbasis !Find an empty slot to record this basis function
					if (fragtmp(j)==0) then
						fragtmp(j)=ibas
						naddbas=naddbas+1
						exit
					end if
				end do
			end if
		end do
		write(*,"(' Done!',i6,' new basis functions have been added to current fragment')") naddbas
	else
		write(*,*) "Error: Unrecognized command, ignoring..."
	end if
end do
if (allocated(frag1).and.allocated(frag2)) then
	j=0
	do i=1,size(frag2)
		if (any(frag1==frag2(i))) then
			write(*,"(' Warning: basis function',i6,' in fragment 2 also present in fragment 1!')") frag2(i)
			j=1
		end if
	end do
	if (j==1) then
		write(*,*) "You must redefine fragment 1 or 2 to eliminate intersection set!"
		write(*,*)
	end if
end if
end subroutine



!!--------- fragment and inter-fragment composition analysis
!isel==1 fragment 1 & inter-fragment composition with Mulliken partition"
!isel==2 fragment 1 & inter-fragment composition with Stout & Politzer partition"
!isel==3 fragment 1 composition with Ros & Schuit (SCPA) partition"
subroutine orbfragcomp_MMPA(isel)
use defvar
use util
implicit real*8 (a-h,o-z)
integer isel
real*8,pointer :: tmpmat(:,:)
real*8 :: ovpfrg12(nmo),ovpfrg12_1(nmo)
character orbtype*2
ovpfrg12=0D0
ovpfrg12_1=0D0
if (isel==1.or.isel==2) then
	write(*,*)
	write(*,"(a)") "Note:"
	write(*,"(a)") """c^2"" means the square of coefficients of all basis functions within fragment 1"
	write(*,"(a)") """Int.cross"" means cross term within fragment 1"
	write(*,"(a,/)") """Ext.cross"" means the fragment 1 part of the total cross term between fragment 1 and all other basis functions"
	write(*,"('Orb# Type  Energy   Occ         c^2      Int.cross    Ext.cross     Total')")
else if (isel==3) then
	write(*,"('Orb# Type  Energy   Occ         Composition')")
end if

do imo=1,nmo
	if (imo<=nbasis) then
		irealmo=imo
		tmpmat=>CObasa
	else if (imo>nbasis) then
		irealmo=imo-nbasis
		tmpmat=>CObasb
	end if
	floc=0D0 !Local term in fragment 1
	crossint=0D0
	crossext=0D0 !fragment 1 part of all overlap between fragment 1 and external atoms
	if (isel==3) allsqr=sum(tmpmat(:,irealmo)**2)
	do i=1,size(frag1)
		ibas=frag1(i)
		if (isel==1.or.isel==2) floc=floc+tmpmat(ibas,irealmo)**2
		if (isel==3) floc=floc+tmpmat(ibas,irealmo)**2/allsqr
		!Calculate cross term
		if (isel==1.or.isel==2) then
			do jbas=1,nbasis
				if (jbas==ibas) cycle
				crossij=tmpmat(ibas,irealmo)*tmpmat(jbas,irealmo)*Sbas(ibas,jbas)
				if (any(frag1==jbas)) then
					crossint=crossint+crossij !Internal cross term
				else if (isel==1) then
					crossext=crossext+crossij
				else if (isel==2) then
					tmpdenom=tmpmat(ibas,irealmo)**2+tmpmat(jbas,irealmo)**2
					if (tmpdenom>1D-30) crossext=crossext+tmpmat(ibas,irealmo)**2/tmpdenom*crossij*2
				end if
				!Cross term between fragment 1 and 2. We assume there is no intersection set between frag1 and frag2
				!Note: any(frag2==jbas) is a subset of .not.any(frag1==jbas), so ovpfrg12 is part of crossext
				if (allocated(frag2).and.any(frag2==jbas)) then
					ovpfrg12(imo)=ovpfrg12(imo)+2*crossij
					if (isel==1) then
						ovpfrg12_1(imo)=ovpfrg12_1(imo)+crossij
					else if (isel==2) then
						tmpdenom=tmpmat(ibas,irealmo)**2+tmpmat(jbas,irealmo)**2
						if (tmpdenom>1D-30) ovpfrg12_1(imo)=ovpfrg12_1(imo)+tmpmat(ibas,irealmo)**2/tmpdenom*crossij*2
					end if
				end if
			end do
		end if
	end do
	if (MOtype(imo)==0) orbtype="AB"
	if (MOtype(imo)==1) orbtype="A "
	if (MOtype(imo)==2) orbtype="B "
	if (isel==1.or.isel==2) write(*,"(i5,1x,a,f9.3,f7.3,f12.3,'%',f12.3,'%',f12.3,'%',f12.3,'%')") &
	imo,orbtype,MOene(imo),MOocc(imo),floc*100,crossint*100,crossext*100,(floc+crossint+crossext)*100
	if (isel==3) write(*,"(i5,1x,a,f9.3,f7.3,f18.6,'%')") imo,orbtype,MOene(imo),MOocc(imo),floc*100
end do

if (isel/=3.and.allocated(frag2)) then !Print cross term between fragment 1 and 2
	write(*,*)
	write(*,*) "Cross term between fragment 1 and 2 and their individual parts:"
	write(*,"('Orb# Type  Energy   Occ      Frag1 part     Frag2 part       Total')")
	do imo=1,nmo
		if (MOtype(imo)==0) orbtype="AB"
		if (MOtype(imo)==1) orbtype="A "
		if (MOtype(imo)==2) orbtype="B "
		write(*,"(i5,1x,a,f9.3,f7.3,f14.4,'%',f14.4,'%',f14.4,'%')") &
		imo,orbtype,MOene(imo),MOocc(imo),ovpfrg12_1(imo)*100,(ovpfrg12(imo)-ovpfrg12_1(imo))*100,ovpfrg12(imo)*100
	end do
end if
end subroutine




!!---------- Calculate basis function, shell and atom contribution to orbitals
!imethod=1 : Mulliken partition    =2:Stout & Politzer partition   =3: Ros & Schuit (SCPA) partition
subroutine orballcomp_MMPA(imethod)
use defvar
implicit real*8 (a-h,o-z)
real*8 basloc(nbasis),bascross(nbasis),bastot(nbasis)
real*8,pointer :: tmpmat(:,:)
integer imethod
character orbtype*10
do while(.true.)
	write(*,*) "Input the orbital index to print composition, e.g. 4"
	write(*,*) "Note: Input -1 can print basic information of all orbitals, input 0 to return"
	read(*,*) ishowmo
	if (ishowmo<-1.or.ishowmo>nmo) then
		write(*,"(' Orbital index should be in the range of 1 to',i6)") nmo
		cycle
	else if (ishowmo==0) then
		exit
	else if (ishowmo==-1) then !show all orbital information
		do i=1,nmo
			if (MOtype(i)==0) orbtype="Alpha&Beta"
			if (MOtype(i)==1) orbtype="Alpha     "
			if (MOtype(i)==2) orbtype="Beta      "
			write(*,"(' Orbital:',i5,' Energy(a.u.):',f14.8,' Occ:',f14.8,' Type: ',a)") i,MOene(i),MOocc(i),orbtype
		end do
		write(*,*)
	else
		write(*,*)
		write(*,"(' Threshold of absolute value:  >',f12.6,'%')") compthres
		if (MOtype(ishowmo)==0) orbtype="Alpha&Beta"
		if (MOtype(ishowmo)==1) orbtype="Alpha     "
		if (MOtype(ishowmo)==2) orbtype="Beta      "
		if (ishowmo<=nbasis) tmpmat=>CObasa
		if (ishowmo>nbasis) tmpmat=>CObasb
		write(*,"(' Orbital:',i5,' Energy(a.u.):',f14.8,' Occ:',f14.8,' Type: ',a)") ishowmo,MOene(ishowmo),MOocc(ishowmo),orbtype
		if (imethod==1.or.imethod==2) write(*,"('  Basis Type    Atom    Shell     Local       Cross term      Total   ')")
		if (imethod==3) write(*,"('  Basis Type    Atom    Shell   Composition')")
		if (ishowmo>nbasis) ishowmo=ishowmo-nbasis !For wfntype==1.or.wfntype==4, change to #beta for CObasb
		if (imethod==3) allsqr=sum(tmpmat(:,ishowmo)**2)
		bascross=0D0
		do ibas=1,nbasis
			basloc(ibas)=tmpmat(ibas,ishowmo)**2
			if (imethod==3) then
				basloc(ibas)=basloc(ibas)/allsqr
			else if (imethod==1.or.imethod==2) then
				do jbas=1,nbasis
					if (jbas==ibas) cycle
					if (imethod==1) then
						bascross(ibas)=bascross(ibas)+tmpmat(ibas,ishowmo)*tmpmat(jbas,ishowmo)*Sbas(ibas,jbas)
! 						write(*,*) ibas,jbas,tmpmat(ibas,ishowmo)*tmpmat(jbas,ishowmo)*Sbas(ibas,jbas)
					else if (imethod==2) then
						tmp=tmpmat(ibas,ishowmo)**2+tmpmat(jbas,ishowmo)**2
						if (tmp>1D-30) bascross(ibas)=bascross(ibas)+tmpmat(ibas,ishowmo)**2/tmp*2*tmpmat(ibas,ishowmo)*tmpmat(jbas,ishowmo)*Sbas(ibas,jbas)
					end if
				end do
			end if
			bastot(ibas)=basloc(ibas)+bascross(ibas)
			if (abs(bastot(ibas))*100>compthres) then
				if (imethod==1.or.imethod==2) write(*,"(i6,3x,a,i5,a,i5,f13.5,'%',f13.5,'%',f13.5,'%')") ibas,GTFtype2name(bastype(ibas)),bascen(ibas),'('//a(bascen(ibas))%name//')',&
				basshell(ibas),basloc(ibas)*100,bascross(ibas)*100,bastot(ibas)*100
				if (imethod==3) write(*,"(i6,3x,a,i5,a,i5,f13.5,'%')") ibas,GTFtype2name(bastype(ibas)),bascen(ibas),'('//a(bascen(ibas))%name//')',&
				basshell(ibas),bastot(ibas)*100
			end if
		end do
		aboveloc=sum(basloc(:),abs(bastot)*100>compthres)*100
		abovecross=sum(bascross(:),abs(bastot)*100>compthres)*100
		if (imethod==1.or.imethod==2) write(*,"(' Sum up those listed above: ',f13.5,'%',f13.5,'%',f13.5,'%')") aboveloc,abovecross,aboveloc+abovecross
		if (imethod==1.or.imethod==2) write(*,"(' Sum up all basis functions:',f13.5,'%',f13.5,'%',f13.5,'%')") sum(basloc(:))*100,sum(bascross(:))*100,sum(bastot(:))*100
		if (imethod==3) write(*,"(' Sum up those listed above: ',f13.5,'%')") sum(bastot(:),abs(bastot)*100>compthres)*100
		if (imethod==3) write(*,"(' Sum up all basis functions:',f13.5,'%')") sum(bastot(:))*100
		write(*,*)
		write(*,"(' Composition of each shell, threshold of absolute value:  >',f12.6,'%')") compthres
		do i=1,nshell
			shellcom=0D0
			do j=1,nbasis
				if (basshell(j)==i) then
					shellcom=shellcom+bastot(j)
					iatm=bascen(j)
				end if
			end do
			if (abs(shellcom)*100>compthres) write(*,"(' Shell',i6,' Type: ',a,'    in atom',i5,'(',a,') :',f14.5,'%')") i,shtype2name(shtype(i)),iatm,a(iatm)%name,shellcom*100
		end do
		write(*,*)
		write(*,*) "Composition of each atom:"
		do i=1,ncenter
			write(*,"(' Atom',i6,'(',a,') :',f12.6,'%')") i,a(i)%name,sum(bastot(basstart(i):basend(i)))*100
		end do
		if (allocated(frag1)) then
			write(*,*)
			write(*,"(' Composition of the fragment:',f12.6,'%')") sum(bastot(frag1(:)))*100
		end if
		write(*,*)
	end if
end do
end subroutine


!!-------- A routine to calculate all atomic contributions to specific orbital, utilized by a few routines (e.g. detecting pi orbitals)
!iorb is the index of the interesting orbital, atmcomp is array with dimension of ncenter to return atomic contribution
!imethod=1: Mulliken    =2: SCPA
subroutine gen_orbatmcomp_MMPA(imethod,iorbin,atmcomp)
use defvar
implicit real*8 (a-h,o-z)
real*8 atmcomp(ncenter),bastot(nbasis)
integer iorbin,imethod
real*8,pointer :: tmpmat(:,:)
if (iorbin<=nbasis) then
	tmpmat=>CObasa
	iorb=iorbin
else if (iorbin>nbasis) then
	tmpmat=>CObasb
	iorb=iorbin-nbasis
end if

if (imethod==1) then !Mulliken
	do ibas=1,nbasis
		bascross=0D0
		do jbas=1,nbasis
			if (jbas==ibas) cycle
			bascross=bascross+tmpmat(ibas,iorb)*tmpmat(jbas,iorb)*Sbas(ibas,jbas)
		end do
		bastot(ibas)=tmpmat(ibas,iorb)**2+bascross
	end do
	do iatm=1,ncenter
		atmcomp(iatm)=sum(bastot(basstart(iatm):basend(iatm)))
	end do
else if (imethod==2) then !SCPA
	sumsqr=sum(tmpmat(:,iorb)**2)
	do iatm=1,ncenter
		atmcomp(iatm)=sum(tmpmat(basstart(iatm):basend(iatm),iorb)**2)/sumsqr
	end do
end if
end subroutine



!!!-------- Partition orbital to atomic contributions by space partition methods
!itype=1: Hirshfeld, itype=2: Becke, itype=3: Hirshfeld-I (must have "call Hirshfeld-I")
subroutine orbatmcomp_space(itype)
use defvar
use util
use function
implicit real*8(a-h,o-z)
type(content) gridorg(radpot*sphpot),gridatm(radpot*sphpot)
real*8 resultvec(ncenter)
real*8 allpotx(ncenter,radpot*sphpot),allpoty(ncenter,radpot*sphpot),allpotz(ncenter,radpot*sphpot),allpotw(ncenter,radpot*sphpot)
real*8 tmpdens(radpot*sphpot),selfdens(radpot*sphpot),promol(radpot*sphpot),orbval(nmo),orbcomp(ncenter,nmo)
integer,allocatable :: fragorbcomp(:)
character orbtype*10,c2000tmp*2000
real*8,external :: fdens_rad

if (iautointgrid==1) then
	nradpotold=radpot
	nsphpotold=sphpot
	radcutold=radcut
	radpot=30
	sphpot=302
    radcut=15
end if

!Generate allpotx/y/z/w
!allpotx(iatm,j) is x coordinate of the jth point for integrating center iatm
call gen1cintgrid(gridorg,iradcut)
do iatm=1,ncenter
	allpotx(iatm,:)=gridorg(:)%x+a(iatm)%x
	allpoty(iatm,:)=gridorg(:)%y+a(iatm)%y
	allpotz(iatm,:)=gridorg(:)%z+a(iatm)%z
end do

!allpotw combines Becke multi-center integration weight with Becke/Hirshfeld/Hirshfeld-I weight
allpotw=0D0
write(*,"(i6,' quadrature points are used for each atom to compute orbital composition')") radpot*sphpot
write(*,"(a)") " Note: You can manually define the number of radial and angular points by &
setting ""iautointgrid"" in settings.ini to 0 and setting ""radpot"" and ""sphpot"""
call walltime(iwalltime1)

if (itype==1.or.itype==3) then !Hirshfeld or Hirshfeld-I partition
	write(*,*)
	if (itype==1) then
		write(*,*) "Hirshfeld analysis requests atomic densities, please select how to obtain them"
		write(*,*) "1 Use build-in sphericalized atomic densities in free-states (recommended)"
		write(*,"(a)") " 2 Provide wavefunction file of involved elements by yourself or invoke Gaussian to automatically calculate them"
		read(*,*) ihirshmode
	end if
    
	if ((itype==1.and.ihirshmode==1).or.itype==3) then !Hirshfeld or Hirshfeld-I based on interpolation density
		if (itype==1.and.ihirshmode==1) write(*,*) "Generating Hirshfeld atomic weighting functions at all grids..."
		if (itype==3) write(*,*) "Generating Hirshfeld-I atomic weighting functions at all grids..."
		do iatm=1,ncenter
			promol=0D0
			do jatm=1,ncenter
				if (itype==1.and.ihirshmode==1) then !Hirshfeld based on interpolation of built-in atomic radius density
					!$OMP parallel do shared(tmpdens) private(ipt) num_threads(nthreads)
					do ipt=1+iradcut*sphpot,radpot*sphpot
						tmpdens(ipt)=calcatmdens(jatm,allpotx(iatm,ipt),allpoty(iatm,ipt),allpotz(iatm,ipt),0)
					end do
					!$OMP end parallel do
				else if (itype==3) then !Hirshfeld-I based on refined atomic radial density
					!$OMP parallel do shared(tmpdens) private(ipt) num_threads(nthreads)
					do ipt=1+iradcut*sphpot,radpot*sphpot
						tmpdens(ipt)=fdens_rad(jatm,allpotx(iatm,ipt),allpoty(iatm,ipt),allpotz(iatm,ipt))
					end do
					!$OMP end parallel do
				end if
				promol=promol+tmpdens
				if (jatm==iatm) selfdens=tmpdens
			end do
			do i=1+iradcut*sphpot,radpot*sphpot !Get Hirshfeld weight of present atom
				if (promol(i)/=0D0) then
					allpotw(iatm,i)=selfdens(i)/promol(i)
				else
					allpotw(iatm,i)=0D0
				end if
			end do
			allpotw(iatm,:)=allpotw(iatm,:)*gridorg(:)%value !Combine Hirshfeld weight with single-center integration weight
            
            call showprog(iatm,ncenter)
		end do
		
	else if (itype==1.and.ihirshmode==2) then !Hirshfeld based on atomic .wfn file
		call setpromol
        write(*,*) "Generating Hirshfeld atomic weighting functions at all grids..."
		do iatm=1,ncenter_org
			promol=0D0
			do jatm=1,ncenter_org
				call dealloall
				call readwfn(custommapname(jatm),1)
				!$OMP parallel do shared(tmpdens) private(ipt) num_threads(nthreads)
				do ipt=1+iradcut*sphpot,radpot*sphpot
					tmpdens(ipt)=fdens(allpotx(iatm,ipt),allpoty(iatm,ipt),allpotz(iatm,ipt))
				end do
				!$OMP end parallel do
				promol=promol+tmpdens
				if (jatm==iatm) selfdens=tmpdens
			end do
			do i=1+iradcut*sphpot,radpot*sphpot !Get Hirshfeld weight of present atom
				if (promol(i)/=0D0) then
					allpotw(iatm,i)=selfdens(i)/promol(i)
				else
					allpotw(iatm,i)=0D0
				end if
			end do
			allpotw(iatm,:)=allpotw(iatm,:)*gridorg(:)%value !Combine Hirshfeld weight with single-center integration weight
            
            call showprog(iatm,ncenter_org)
		end do
		call dealloall
		call readinfile(firstfilename,1) !Retrieve the firstly loaded file(whole molecule) in order to calculate real rho later
	end if

else if (itype==2) then !Becke partition
	write(*,*) "Generating Becke atomic weighting functions at all grids..."
	do iatm=1,ncenter !Cycle points of each atom
		gridatm%x=gridorg%x+a(iatm)%x
		gridatm%y=gridorg%y+a(iatm)%y
		gridatm%z=gridorg%z+a(iatm)%z
		call gen1cbeckewei(iatm,iradcut,gridatm,allpotw(iatm,:))
		allpotw(iatm,:)=allpotw(iatm,:)*gridorg%value !Combine Becke weight with single-center integration weight
        call showprog(iatm,ncenter)
	end do
end if

call walltime(iwalltime2)
write(*,"(' Done! Initialization step took up wall clock time',i10,'s')") iwalltime2-iwalltime1
write(*,*)

do while(.true.)
	write(*,*) "Now input the orbital index to print orbital composition, e.g. 5"
	write(*,"(a)") " You can also input:"
	if (.not.allocated(fragorbcomp)) then
	    write(*,"(a,i6)") " -9: Define fragment, current: undefined"
	else
    	write(*,"(a,i6)") " -9: Redefine fragment, current number of atoms:",size(fragorbcomp)
    end if
	write(*,"(a)") "  0: Return"
	write(*,"(a)") " -1: Print basic information of all orbitals"
	write(*,"(a)") " -2: Print atom contribution to a range of orbitals"
	write(*,"(a)") " -3: Print fragment contribution to a range of orbitals"
	write(*,"(a)") " -4: Export composition of every atom in every orbital to orbcomp.txt"
	read(*,*) ishowmo
	if (ishowmo>nmo) then
		write(*,"(' Error: Orbital index should within the range of 1 to',i6,/)") nmo
		cycle
	else if (ishowmo==0) then
        if (iautointgrid==1) then
	        radpot=nradpotold
	        sphpot=nsphpotold
            radcut=radcutold
        end if
		exit
	else if (ishowmo==-1) then !Show all orbital information
		do i=1,nmo
			if (MOtype(i)==0) orbtype="Alpha&Beta"
			if (MOtype(i)==1) orbtype="Alpha     "
			if (MOtype(i)==2) orbtype="Beta      "
			write(*,"(' Orbital:',i5,' Energy(a.u.):',f14.8,' Occ:',f14.8,' Type: ',a)") i,MOene(i),MOocc(i),orbtype
		end do
	else if (ishowmo==-2.or.ishowmo==-3) then !Print atom/fragment contribution to specific range of orbitals
		if ((.not.allocated(fragorbcomp)).and.ishowmo==-3) then
			write(*,*) "Error: You must defined the fragment first!"
			write(*,*)
			cycle
		end if
		if (ishowmo==-2) then
			write(*,*) "Input atom index, e.g. 4"
			read(*,*) iatm
		end if
		write(*,*) "Input orbital range, e.g. 20,25 means from 20 to 25"
		read(*,*) iorbbeg,iorbend
		if (iorbbeg<=0) iorbbeg=1
		if (iorbend>nmo) iorbend=nmo
		write(*,"(' Orb#    Type       Ene(a.u.)     Occ    Composition    Population')")
		pop=0D0
		do imo=iorbbeg,iorbend
			if (MOtype(imo)==0) orbtype="Alpha&Beta"
			if (MOtype(imo)==1) orbtype="Alpha     "
			if (MOtype(imo)==2) orbtype="Beta      "
			if (ishowmo==-2) then !Calculate only on atom
				tmp=0D0
				!$OMP parallel shared(tmp) private(ipot,value,tmpprivate) num_threads(nthreads)
				tmpprivate=0D0
				!$OMP do schedule(dynamic)
				do ipot=1+iradcut*sphpot,radpot*sphpot
					if (allpotw(iatm,ipot)<1D-8) cycle !May lose 0.001% accuracy
					value=fmo(allpotx(iatm,ipot),allpoty(iatm,ipot),allpotz(iatm,ipot),imo)**2
					tmpprivate=tmpprivate+value*allpotw(iatm,ipot)
				end do
				!$OMP end do
				!$OMP CRITICAL
				tmp=tmp+tmpprivate
				!$OMP end CRITICAL
				!$OMP end parallel
			else if (ishowmo==-3) then
				tmp=0D0
				do itmp=1,nfragorbcomp
					iatm=fragorbcomp(itmp)
					!$OMP parallel shared(tmp) private(ipot,value,tmpprivate) num_threads(nthreads)
					tmpprivate=0D0
					!$OMP do schedule(dynamic)
					do ipot=1+iradcut*sphpot,radpot*sphpot
						if (allpotw(iatm,ipot)<1D-8) cycle !May lose 0.001% accuracy
						value=fmo(allpotx(iatm,ipot),allpoty(iatm,ipot),allpotz(iatm,ipot),imo)**2
						tmpprivate=tmpprivate+value*allpotw(iatm,ipot)
					end do
					!$OMP end do
					!$OMP CRITICAL
					tmp=tmp+tmpprivate
					!$OMP end CRITICAL
					!$OMP end parallel
				end do
			end if
			write(*,"(i5,1x,a,f13.4,f9.3,f11.3,'%',f15.6)") imo,orbtype,MOene(imo),MOocc(imo),tmp*100,MOocc(imo)*tmp
			pop=pop+MOocc(imo)*tmp
		end do
		if (iorbend-iorbbeg>0) write(*,"(a,f12.6)") " Population of this atom in these orbitals:",pop
		
	else if (ishowmo==-4) then !Export composition of every atom in every orbital to orbcomp.txt in current folder
		write(*,*) "Calculating, please wait..."
		orbcomp=0
		!$OMP parallel do shared(orbcomp) private(iatm,ipot,orbval) num_threads(nthreads) schedule(dynamic)
		do iatm=1,ncenter
			do ipot=1+iradcut*sphpot,radpot*sphpot
				if (allpotw(iatm,ipot)<1D-9) cycle !May lose 0.001% accuracy
				call orbderv(1,1,nmo,allpotx(iatm,ipot),allpoty(iatm,ipot),allpotz(iatm,ipot),orbval)
				orbcomp(iatm,:)=orbcomp(iatm,:)+orbval(:)**2*allpotw(iatm,ipot)
			end do
		end do
		!$OMP end parallel do
		open(10,file="orbcomp.txt",status="replace")
		do imo=1,nmo
			write(10,"(' Orbital',i6)") imo
            valnorm=sum(orbcomp(:,imo))
			do iatm=1,ncenter
				write(10,"(i6,f11.3,' %')") iatm,orbcomp(iatm,imo)/valnorm*100
			end do
		end do
		close(10)
		write(*,*) "Done! orbcomp.txt has been exported to current folder"
	
	else if (ishowmo==-9) then !Define fragment
	    if (allocated(fragorbcomp)) then
			write(*,*) "Atoms in current fragment:"
			write(*,"(13i6)") fragorbcomp
			write(*,"(a)") " Input 0 to keep unchanged, or redefine fragment, e.g. 1,3-6,8,10-11 means the atoms 1,3,4,5,6,8,10,11 will constitute the fragment"
		else
			write(*,"(a)") " Input atomic indices to define fragment. e.g. 1,3-6,8,10-11 means the atoms 1,3,4,5,6,8,10,11 will constitute the fragment"
		end if
		read(*,"(a)") c2000tmp
		if (c2000tmp(1:1)=='0') then
		    continue
		else if (c2000tmp(1:1)==" ") then
		    deallocate(fragorbcomp)
		else
			if (allocated(fragorbcomp)) deallocate(fragorbcomp)
			call str2arr(c2000tmp,nfragorbcomp)
			allocate(fragorbcomp(nfragorbcomp))
			call str2arr(c2000tmp,nfragorbcomp,fragorbcomp)
		end if
	    
	else
		if (MOtype(ishowmo)==0) orbtype="Alpha&Beta"
		if (MOtype(ishowmo)==1) orbtype="Alpha     "
		if (MOtype(ishowmo)==2) orbtype="Beta      "
		write(*,"(' Orbital:',i5,' Energy(a.u.):',f14.8,' Occ:',f14.8,' Type: ',a)") ishowmo,MOene(ishowmo),MOocc(ishowmo),orbtype
		write(*,*) "Please wait..."
		accum=0D0
		do iatm=1,ncenter
			tmp=0D0
			!$OMP parallel shared(tmp) private(ipot,value,tmpprivate) num_threads(nthreads)
			tmpprivate=0
			!$OMP do schedule(dynamic)
			do ipot=1+iradcut*sphpot,radpot*sphpot
				if (allpotw(iatm,ipot)<1D-8) cycle !May lose 0.001% accuracy
				value=fmo(allpotx(iatm,ipot),allpoty(iatm,ipot),allpotz(iatm,ipot),ishowmo)**2
				tmpprivate=tmpprivate+value*allpotw(iatm,ipot)
			end do
			!$OMP end do
			!$OMP CRITICAL
            tmp=tmp+tmpprivate
			!$OMP end CRITICAL
			!$OMP end parallel
			accum=accum+tmp
			resultvec(iatm)=tmp
		end do
		write(*,"(' The sum of contributions before normalization',11x,f12.6,'%',/)") accum*100
		if (allocated(fragorbcomp)) then
		    fragresult=sum(resultvec(fragorbcomp))
		end if
		write(*,*) "Contributions after normalization:"
		do iatm=1,ncenter
			write(*,"(' Atom',i6,'(',a,') :',f11.3,'%')") iatm,a(iatm)%name,resultvec(iatm)/accum*100
		end do
		if (allocated(fragorbcomp)) write(*,"(/,' Fragment contribution:',f11.3,'%')") fragresult/accum*100
	end if
	write(*,*)
end do
end subroutine





!!!-------- Calculate atomic contributions to specific range of orbitals by Hirshfeld (built-in density) or Becke methods
!In fact this is a simplified version of orbatmcomp_space
!itype=1: Hirshfeld, itype=2: Becke
!atmcomp: The array returned, atmcomp(A,i) is contribution of atom A to orbital ibeg+i-1. The second index has length of iend-ibeg+1
!ibeg,iend: The beginning and ending index of the orbitals to be computed, ranging from 1 to nmo
!info=0: silent mode, info=1: print intermediate prompts
!igrid=0: Use lowest acceptable grid (accurate to one decimal place), =1: Use medium quality grid
subroutine gen_orbatmcomp_space(itype,atmcomp,ibeg,iend,info,igrid)
use defvar
use util
use function
implicit real*8(a-h,o-z)
real*8 atmcomp(ncenter,iend-ibeg+1)
type(content) gridorg(radpot*sphpot),gridatm(radpot*sphpot)
real*8 allpotx(ncenter,radpot*sphpot),allpoty(ncenter,radpot*sphpot),allpotz(ncenter,radpot*sphpot),allpotw(ncenter,radpot*sphpot)
real*8 tmpdens(radpot*sphpot),selfdens(radpot*sphpot),promol(radpot*sphpot),orbval(nmo)

if (iautointgrid==1) then
	nradpotold=radpot
	nsphpotold=sphpot
	radcutold=radcut
    if (igrid==0) then
	    radpot=25
	    sphpot=170
    else if (igrid==1) then
	    radpot=30
	    sphpot=302
    end if
    radcut=15
end if

if (itype==1) write(*,*) "Calculating atomic contributions to orbitals by Hirshfeld method..."
if (itype==2) write(*,*) "Calculating atomic contributions to orbitals by Becke method..."

if (info==1) then
    write(*,"(i7,' quadrature points are used for each atom')") radpot*sphpot
    write(*,"(a)") " Note: You can manually define the number of radial and angular points by &
    setting ""iautointgrid"" in settings.ini to 0 and setting ""radpot"" and ""sphpot"""
end if

call gen1cintgrid(gridorg,iradcut)
do iatm=1,ncenter
	allpotx(iatm,:)=gridorg(:)%x+a(iatm)%x
	allpoty(iatm,:)=gridorg(:)%y+a(iatm)%y
	allpotz(iatm,:)=gridorg(:)%z+a(iatm)%z
end do

!allpotw combines Becke multi-center integration weight with Becke/Hirshfeld weight
allpotw=0D0
if (itype==1) then !Hirshfeld based on built-in atomic density
	if (info==1) write(*,*) "Generating Hirshfeld atomic weighting functions at all grids..."
	do iatm=1,ncenter
		promol=0D0
		do jatm=1,ncenter
			!$OMP parallel do shared(tmpdens) private(ipt) num_threads(nthreads)
			do ipt=1+iradcut*sphpot,radpot*sphpot
				tmpdens(ipt)=calcatmdens(jatm,allpotx(iatm,ipt),allpoty(iatm,ipt),allpotz(iatm,ipt),0)
			end do
			!$OMP end parallel do
			promol=promol+tmpdens
			if (jatm==iatm) selfdens=tmpdens
		end do
		do i=1+iradcut*sphpot,radpot*sphpot !Get Hirshfeld weight of present atom
			if (promol(i)/=0D0) then
				allpotw(iatm,i)=selfdens(i)/promol(i)
			else
				allpotw(iatm,i)=0D0
			end if
		end do
		allpotw(iatm,:)=allpotw(iatm,:)*gridorg(:)%value !Combine Hirshfeld weight with single-center integration weight
	end do
else if (itype==2) then !Becke partition
	if (info==1) write(*,*) "Generating Becke weights..."
	do iatm=1,ncenter !Cycle points of each atom
		gridatm%x=gridorg%x+a(iatm)%x
		gridatm%y=gridorg%y+a(iatm)%y
		gridatm%z=gridorg%z+a(iatm)%z
		call gen1cbeckewei(iatm,iradcut,gridatm,allpotw(iatm,:))
		allpotw(iatm,:)=allpotw(iatm,:)*gridorg%value !Combine Becke weight with single-center integration weight
	end do
end if

if (info==1) write(*,*) "Calculating orbital compositions, please wait..."
atmcomp=0
do iatm=1,ncenter
    !$OMP parallel do shared(atmcomp) private(ipot,orbval) num_threads(nthreads) schedule(dynamic)
	do ipot=1+iradcut*sphpot,radpot*sphpot
		if (allpotw(iatm,ipot)<1D-9) cycle !May lose 0.001% accuracy
		call orbderv(1,ibeg,iend,allpotx(iatm,ipot),allpoty(iatm,ipot),allpotz(iatm,ipot),orbval)
		atmcomp(iatm,:)=atmcomp(iatm,:)+orbval(ibeg:iend)**2*allpotw(iatm,ipot)
	end do
    !$OMP end parallel do
    call showprog(iatm,ncenter)
end do

!Do normalization
do imo=ibeg,iend
	!write(*,"(' Orbital',i6)") imo
	!do iatm=1,ncenter
	!	write(*,"(i6,f12.6,' %')") iatm,atmcomp(iatm,imo)*100
	!end do
    !write(*,"(i6,f12.6)") imo,sum(atmcomp(:,imo))*100
    itmp=imo-ibeg+1
    atmcomp(:,itmp)=atmcomp(:,itmp)/sum(atmcomp(:,itmp))
end do

if (iautointgrid==1) then
	radpot=nradpotold
	sphpot=nsphpotold
    radcut=radcutold
end if
end subroutine





!!------------ Orbital composition analysis by NAO method
subroutine orballcomp_NAO
use defvar
use util
implicit real*8 (a-h,o-z)
integer :: ioutmode=1,ifragend=0,iorboutcomp !The orbital index selected
integer :: ispinmode=0 !0/1=close/open-shell
integer numNAO
integer,allocatable :: fragidx(:),NAOinit(:),NAOend(:),NAOcen(:),termtmp(:)
character :: c80tmp*80,c80tmp2*80,c200*200 !type2char(0:2)=(/"Cor","Val","Ryd"/)
character,allocatable :: NAOlang(:)*7,NAOcenname(:)*2,NAOtype(:)*3,centername(:)*2,NAOshtype(:)*2
real*8 :: outcrit=0.5D0
real*8,allocatable :: NAOMO(:,:)

open(10,file=filename,status="old")
nmo=0
call loclabel(10,"NBsUse=",ifound) !Gaussian may eliminate some linear dependency basis functions, so MO may be smaller than numNAO. NBsUse always equals to actual number of MO
if (ifound==1) then
	read(10,"(a)") c80tmp
	!For DKH case, G09 may output such as RelInt: Using uncontracted basis, NUniq=   180 NBsUse=   180 0.100E-13, this nbsuse is meaningless, use next nbsuse
	if (index(c80tmp,"RelInt")/=0) call loclabel(10,"NBsUse=",ifound)
	itmp=index(c80tmp,'=')
	read(c80tmp(itmp+1:),*) nmo
end if
call loclabel(10,"MOs in the NAO basis:",ifound,1)
if (ifound==0) then
	write(*,"(a)") " Error: Cannot found MOs in NAO basis in the input file, you should use ""NAOMO"" keyword in NBO module"
    write(*,*) "Press ENTER button to return"
	read(*,*)
    close(10)
	return
else !Acquire number of NAOs and centers
	call loclabel(10,"NATURAL POPULATIONS",ifound,1)
	read(10,*)
	read(10,*)
	read(10,*)
	read(10,*)
	ilastspc=0
	do while(.true.) !Find how many center and how many NAOs. We need to carefully check where is ending
		read(10,"(a)") c80tmp
		if (c80tmp==''.or.index(c80tmp,"low occupancy")/=0.or.index(c80tmp,"Population inversion found")/=0.or.index(c80tmp,"effective core potential")/=0) then
			if (ilastspc==1) then
				ncenter=iatm
				numNAO=inao
				exit
			end if
			ilastspc=1 !last line is space
		else
			read(c80tmp,*) inao,c80tmp2,iatm
			ilastspc=0
		end if
	end do
	if (nmo==0) nmo=numNAO
	write(*,"(' The number of atoms:',i10)") ncenter
	write(*,"(' The number of NAOs: ',i10)") numNAO
	write(*,"(' The number of MOs : ',i10)") nmo
	allocate(fragidx(numNAO),termtmp(numNAO))
	ifragend=0 !ifragend is acutal ending position of fragidx
	allocate(NAOinit(ncenter),NAOend(ncenter),centername(ncenter))
	allocate(NAOcen(numNAO),NAOcenname(numNAO),NAOtype(numNAO),NAOlang(numNAO),NAOMO(numNAO,nmo),NAOshtype(numNAO))
	!Get relationship between center and NAO indices, as well as center name, then store these informationto NAOcen
	!We delay to read NAO information, because in alpha and beta cases may be different
	call loclabel(10,"NATURAL POPULATIONS",ifound,1)
	read(10,*);read(10,*);read(10,*);read(10,*)
	ilastspc=1
	do while(.true.)
		read(10,"(a)") c80tmp
		if (c80tmp/='') then
			read(c80tmp,*) inao,c80tmp2,iatm
			NAOcen(inao)=iatm
			if (ilastspc==1) NAOinit(iatm)=inao
			ilastspc=0
		else
			NAOend(iatm)=inao
			centername(iatm)=c80tmp2
			if (iatm==ncenter) exit
			ilastspc=1
		end if
	end do
end if

!Determine if this file is close or open shell calculation
call loclabel(10,"*******         Alpha spin orbitals         *******",ispinmode,1)
if (ispinmode==0) write(*,*) "This is closed-shell calculation"
if (ispinmode==1) write(*,*) "This is open-shell calculation"

!Interface
NAOcompmaincyc: do while(.true.)
do while(.true.)
	write(*,*)
    write(*,*) "------- Orbital composition analysis based on natural atomic orbitals -------"
	write(*,*) "-10 Return"
	write(*,*) "-1 Define fragment (for option 1)"
	write(*,*) " 0 Show composition of an orbital"
	write(*,*) " 1 Show fragment contribution in a range of orbitals"
	if (ioutmode==0) write(*,"(a)") "  2 Select output mode (for option 0), current: Show all NAOs"
	if (ioutmode==1) write(*,"(a)") "  2 Select output mode (for option 0), current: Only show core and valence NAOs"
	if (ioutmode==2) write(*,"(a,f6.2,'%')") "  2 Select output mode (for option 0), current: Show NAOs whose contributions are >",outcrit
	if (ispinmode==1) write(*,*) " 3 Switch spin type, current: Alpha"
	if (ispinmode==2) write(*,*) " 3 Switch spin type, current: Beta"
	read(*,*) isel

	if (isel==-10) then
		close(10)
		return
	else if (isel==-1) then
20		write(*,*) "Commands and examples:"
		write(*,*) """q"" : Save setting and exit"
		write(*,*) """help"" : Print these help content again"
		write(*,*) """clean"": Clean current fragment"
		write(*,*) """list"" : List NAOs in current fragment"
		write(*,*) """all""  : Print all NAOs of current system"
		write(*,*) """addall"": Add all NAOs to fragment"
		write(*,*) """a 2,5-8,12"": Add all NAOs in atoms 2,5,6,7,8,12 to fragment"
		write(*,*) """b 2,5-8,12"": Add NAOs 2,5,6,7,8,12 to fragment"
		write(*,*) """da 5,7,11-13"": Delete all NAOs in atoms 5,7,11,12,13 from fragment"
		write(*,*) """db 5,7,11-13"": Delete NAOs 5,7,11,12,13 from fragment"
		do while(.true.)
			read(*,"(a)") c200
			if (c200(1:4)=="help") then
				goto 20
			else if (c200(1:6)=="addall") then
				forall(i=1:numNAO) fragidx(i)=i
				ifragend=numNAO
				write(*,*) "Done!"
			else if (c200(1:1)=="q") then
				write(*,*)
				write(*,*) "Fragment is saved, indices of NAOs in current fragment:"
				if (ifragend==0) then
					write(*,*) "None"
				else
					write(*,"(12i6)") fragidx(1:ifragend)
				end if
				exit
			else if (c200(1:5)=="clean") then
				fragidx=0
				ifragend=0
				write(*,*) "Done!"
			else if (c200(1:3)=="all") then
				if (ispinmode==0) then
					call loclabel(10,"NATURAL POPULATIONS",ifound,1)
				else if (ispinmode==1) then
					call loclabel(10,"NATURAL POPULATIONS",ifound,1) !Total density
					read(10,*)
					call loclabel(10,"NATURAL POPULATIONS",ifound,0) !Alpha density
				else if (ispinmode==2) then
					call loclabel(10,"NATURAL POPULATIONS",ifound,1) !Total density
					read(10,*)
					call loclabel(10,"NATURAL POPULATIONS",ifound,0) !Alpha density
					read(10,*)
					call loclabel(10,"NATURAL POPULATIONS",ifound,0) !Beta density
				end if
				read(10,*)
				read(10,*)
				read(10,*)
				read(10,*)
				write(*,*) "Note: The ones with asterisks are those presented in current fragment"
				write(*,*) "   NAO#     Atom   Label    Type      Occupancy      Energy"
				itmp=0
				do iatm=1,ncenter
					do i=NAOinit(iatm),NAOend(iatm)
						itmp=itmp+1
						read(10,"(a)") c200
						if (any(fragidx(1:ifragend)==itmp)) then
							write(*,"(' *',a)") trim(c200)
						else
							write(*,"('  ',a)") trim(c200)
						end if
					end do
					read(10,*)
				end do
			else if (c200(1:4)=="list") then
				write(*,*) "NAO indices in current fragment:"
				if (ifragend==0) then
					write(*,*) "None"
				else
					write(*,"(12i6)") fragidx(1:ifragend)
				end if
				
			else if (c200(1:2)=="a ".or.c200(1:2)=="s ".or.c200(1:2)=="b ".or.c200(1:2)=="db".or.c200(1:2)=="da") then
				call str2arr(c200(3:),nterm,termtmp)
				if (c200(1:2)=="a ".or.c200(1:2)=="da") then !Check sanity of the input
					if (any(termtmp(1:nterm)<=0).or.any(termtmp(1:nterm)>ncenter)) then
						write(*,*) "ERROR: Atom index exceeded valid range! Ignoring..."
						cycle
					end if
				else if (c200(1:2)=="b ".or.c200(1:2)=="db") then
					if (any(termtmp(1:nterm)<=0).or.any(termtmp(1:nterm)>numNAO)) then
						write(*,*) "ERROR: NAO index exceeded valid range! Ignoring..."
						cycle
					end if
				end if
				!Modify fragidx according to the selection
				if (c200(1:2)=="a ") then
					do iterm=1,nterm
						iatm=termtmp(iterm)
						do ibas=NAOinit(iatm),NAOend(iatm)
							if ( any(fragidx(1:ifragend)==ibas) ) cycle
							ifragend=ifragend+1
							fragidx(ifragend)=ibas
						end do
					end do
				else if (c200(1:2)=="b ") then
					do ibas=1,nterm
						if ( any(fragidx(1:ifragend)==termtmp(ibas)) ) cycle
						ifragend=ifragend+1
						fragidx(ifragend)=termtmp(ibas)
					end do
				else if (c200(1:2)=="db") then
					do itmp=1,nterm
						do iscan=1,ifragend
							if (fragidx(iscan)==termtmp(itmp)) then
								fragidx(iscan:ifragend-1)=fragidx(iscan+1:ifragend)
								ifragend=ifragend-1
								exit
							end if
							if (iscan==ifragend) write(*,"('Note: NAO with index of',i7,' does not present in current fragment')") termtmp(itmp)
						end do
					end do
				else if (c200(1:2)=="da") then
					do itmp=1,nterm
						iatm=termtmp(itmp)
						do iNAOincen=NAOinit(iatm),NAOend(iatm) !Cycle NAO index in itmp atom
							do iscan=1,ifragend !Scan fragidx array to find out iNAOincen
								if (fragidx(iscan)==iNAOincen) then
									fragidx(iscan:ifragend-1)=fragidx(iscan+1:ifragend)
									ifragend=ifragend-1
									exit
								end if
								if (iscan==ifragend) write(*,"('Note: NAO with index of',i7,' does not present in current fragment')") iNAOincen
							end do
						end do
					end do
				end if
				write(*,*) "Done!"
			else
				write(*,*) "Error: Cannot recognize the command you inputted"
			end if
			
		end do !End of fragment definition module
		
	else if (isel==0) then
		exit
		
	else if (isel==1) then
		if (ifragend==0) then
			write(*,*) "Error: You haven't defined fragment or the fragment is empty!"
		else
			write(*,*) "Input orbital range to be outputted  e.g. 1,10"
			write(*,"(a,i7)") " Note: Should within   1 to",nmo
			read(*,*) iorblow,iorbhigh
			if (iorbhigh<iorblow.or.iorblow<=0.or.iorbhigh>nmo) then
				write(*,*) "Error: The range you inputted is invalid!"
				cycle
			else
				exit
			end if
		end if
		
	else if (isel==2) then
		write(*,*) "0 Show all NAOs"
		write(*,*) "1 Only show core and valence NAOs"
		write(*,*) "2 Show NAOs whose contribution is larger than specified criteria"
		read(*,*) ioutmode
		if (ioutmode==2) then
			write(*,*) "Input criteria in percentage, e.g. 0.5"
			read(*,*) outcrit
		end if
		
	else if (isel==3) then
		if (ispinmode==1) then
			ispinmode=2
		else
			ispinmode=1
		end if
	end if
end do

!Start analysis!
if (isel==0.or.isel==1) then
	!Before analysis, read in NAO information according to spin mode
	if (ispinmode==0) then
		call loclabel(10,"NATURAL POPULATIONS",ifound,1)
	else if (ispinmode==1) then
		call loclabel(10,"NATURAL POPULATIONS",ifound,1) !Total density
		read(10,*)
		call loclabel(10,"NATURAL POPULATIONS",ifound,0) !Alpha density
	else if (ispinmode==2) then
		call loclabel(10,"NATURAL POPULATIONS",ifound,1) !Total density
		read(10,*)
		call loclabel(10,"NATURAL POPULATIONS",ifound,0) !Alpha density
		read(10,*)
		call loclabel(10,"NATURAL POPULATIONS",ifound,0) !Beta density
	end if
	read(10,*)
	read(10,*)
	read(10,*)
	read(10,*)
	do iatm=1,ncenter
		do inao=NAOinit(iatm),NAOend(iatm)
			read(10,"(a)") c80tmp
			if (index(c80tmp,"Cor")/=0) then
				NAOtype(inao)="Cor"
			else if (index(c80tmp,"Val")/=0) then
				NAOtype(inao)="Val"
			else if (index(c80tmp,"Ryd")/=0) then
				NAOtype(inao)="Ryd"
			end if
			read(c80tmp,*) c80tmp2,NAOcenname(inao),c80tmp2,NAOlang(inao),c80tmp2,c80tmp2
			NAOshtype(inao)=c80tmp2(1:2)
		end do
		read(10,*)
	end do
	!Read MO in NAO basis
	call loclabel(10,"MOs in the NAO basis:",ifound,0) !Don't rewind
    !Check columns should be skipped during matrix reading, then return to title line
    read(10,*);read(10,*);read(10,*)
    read(10,"(a)") c80tmp
    nskipcol=index(c80tmp,"- -")
    backspace(10);backspace(10);backspace(10);backspace(10)
    call readmatgau(10,NAOMO,0,"f8.4 ",nskipcol,8,3)
end if

if (isel==0) then
	do while(.true.)
		write(*,*) "Analyze which orbital? (Input 0 can return)"
		read(*,*) iorboutcomp
		if (iorboutcomp==0) exit
		if (iorboutcomp<0.or.iorboutcomp>nmo) then
			write(*,"(a,i7)") "Error: The orbital index should between  1 and",nmo
			cycle
		end if
		
		write(*,*)
		if (ioutmode==2) write(*,"(a,f6.2,a)") "Note: All NAOs whose contribution <=",outcrit,"% are ignored"
		if (ispinmode==1) write(*,*) "Below are composition of alpha orbitals"
		if (ispinmode==2) write(*,*) "Below are composition of beta orbitals"
		write(*,*) "   NAO#   Center   Label      Type      Composition"
		sumcomp=0D0
		do inao=1,numNAO
			tmpcomp=NAOMO(inao,iorboutcomp)**2*100D0
			if (ioutmode==1.and.NAOtype(inao)=="Ryd") cycle !skip Ryd
			if (ioutmode==2.and.tmpcomp<=outcrit) cycle
			write(*,"( i8,i5,'(',a,')',4x,a,2x,a,'(',a,')',f14.6,'%' )") inao,NAOcen(inao),NAOcenname(inao),NAOlang(inao),NAOtype(inao),NAOshtype(inao),tmpcomp
			sumcomp=sumcomp+tmpcomp
		end do
		write(*,"(' Summing up the compositions listed above:',f14.6,'%')") sumcomp
		if (ioutmode==1) write(*,"( ' Rydberg composition:',f14.6,'%')") 100D0-sumcomp
		write(*,*)
		write(*,*) "Condensed above result to atoms:"
		write(*,*) "  Center   Composition"
		do icen=1,ncenter
			sumcomp=0D0
			do inao=NAOinit(icen),NAOend(icen)
				tmpcomp=NAOMO(inao,iorboutcomp)**2*100D0
				if (ioutmode==1.and.NAOtype(inao)=="Ryd") cycle !skip Ryd
				if (ioutmode==2.and.tmpcomp<=outcrit) cycle	
				sumcomp=sumcomp+tmpcomp
			end do
			write(*,"( i6,'(',a,')',f12.6,'%' )") icen,centername(icen),sumcomp
		end do
		write(*,*)
	end do
else if (isel==1) then
	if (ispinmode==1) write(*,*) "Below are composition of alpha orbitals"
	if (ispinmode==2) write(*,*) "Below are composition of beta orbitals"
	write(*,*) "  Orb.#         Core        Valence      Rydberg       Total"
	do imo=iorblow,iorbhigh
		sumcompcor=0D0
		sumcompval=0D0
		sumcompryd=0D0
		do itmp=1,ifragend
			inao=fragidx(itmp)
			tmpcomp=NAOMO(inao,imo)**2*100D0
			if (NAOtype(inao)=="Cor") sumcompcor=sumcompcor+tmpcomp
			if (NAOtype(inao)=="Val") sumcompval=sumcompval+tmpcomp
			if (NAOtype(inao)=="Ryd") sumcompryd=sumcompryd+tmpcomp
		end do
		sumcomptot=sumcompcor+sumcompval+sumcompryd
		write(*,"(i6,5x,4(f12.6,'%'))") imo,sumcompcor,sumcompval,sumcompryd,sumcomptot
	end do
end if

end do NAOcompmaincyc

end subroutine



!!---------- Localized orbital bonding analysis (LOBA) based on SCPA partition
!Input file should contains localized MO
subroutine LOBA
use defvar
use util
implicit real*8 (a-h,o-z)
real*8 oxdstat(ncenter),atmcomp(ncenter,nmo)
integer,allocatable :: fragLOBA(:)
character c2000tmp*2000

write(*,*) "Citation: Phys. Chem. Chem. Phys., 11, 11297-11304 (2009)"
write(*,*)
call gen_orbatmcomp_space(1,atmcomp,1,nmo,0,0)

nfragLOBA=0
do while(.true.)
	write(*,*) "Input percentage threshold to determine oxidation state (50 is commonly used)"
	if (allocated(fragLOBA)) then
        write(*,"(a,i6,a)") " Input -1 can redefine the fragment, current:",nfragLOBA," atoms"
    else
        write(*,*) "Input -1 can define fragment, current: Undefined"
    end if
	write(*,*) "Input 0 can exit"
	read(*,*) thres
	if (thres==0) then
		return
	else if (thres==-1) then
		write(*,*) "Input indices of the atoms constituting the fragment"
		write(*,*) "e.g. 1,3-5,9"
		read(*,"(a)") c2000tmp
		if (allocated(fragLOBA)) deallocate(fragLOBA)
		call str2arr(c2000tmp,nfragLOBA)
		allocate(fragLOBA(nfragLOBA))
		call str2arr(c2000tmp,nfragLOBA,fragLOBA)
		write(*,*) "Done!"
		write(*,*)
		cycle
	else
	    oxdstat(:)=a(:)%charge
		thres=thres/100
		do iatm=1,ncenter
			do imo=1,nmo
				if (atmcomp(iatm,imo)>thres) oxdstat(iatm)=oxdstat(iatm)-MOocc(imo)
 				!if (MOocc(imo)>0.and.compval>thres) write(*,"(' Orbital',i4,' belongs to atom',i4)") imo,iatm
			end do
			write(*,"(' Oxidation state of atom',i4,'(',a,') :',i3)") iatm,a(iatm)%name,nint(oxdstat(iatm))
		end do
		write(*,"(' The sum of oxidation states:',i4)") sum(nint(oxdstat))
		if (allocated(fragLOBA)) then
			oxdfrag=0
			do iatmidx=1,nfragLOBA
				iatm=fragLOBA(iatmidx)
				oxdfrag=oxdfrag+a(iatm)%charge
			end do
			do imo=1,nmo
				compval=sum(atmcomp(fragLOBA(:),imo))
				if (compval>thres) oxdfrag=oxdfrag-MOocc(imo)
			end do
			write(*,"(' Oxidation state of the fragment:',i4)") nint(oxdfrag)
		end if
		write(*,*)
	end if
end do

end subroutine






!!------- Generate all atomic composition of all orbitals by SCPA method, or load them from orbcomp.txt in current folder
!The orbcomp.txt can be exported via e.g. Becke and Hirshfeld orbital composition module
!Currently not employed by any function
subroutine gen_allorbatmcomp_SCPA(atmcomp)
use defvar
implicit real*8 (a-h,o-z)
real*8 atmcomp(ncenter,nmo)
inquire(file="orbcomp.txt",exist=alive)
if (alive) then !Load atomic contribution from orbcomp.txt, which may be outputted by option -4 of Hirshfeld/Becke composition analysis
	write(*,"(a)") " orbcomp.txt was found in current folder, now load atomic contribution to all orbitals from this file..."
	open(10,file="orbcomp.txt",status="old")
	do imo=1,nmo
		read(10,*)
		do iatm=1,ncenter
			read(10,*,iostat=ierror) inouse,atmcomp(iatm,imo)
			if (ierror/=0) then
				write(*,"(a)") " Error: Problem was encountered while loading orbcomp.txt in current folder! Please carefully check this file!"
				write(*,*) "Press ENTER button to exit program"
				read(*,*)
				stop
			end if
		end do
	end do
	close(10)
	atmcomp=atmcomp/100
else
	write(*,"(a)") " orbcomp.txt was not found in current folder, therefore calculate atomic contributions to all orbitals by SCPA method..."
	do imo=1,nmo
		if (MOtype(imo)==0.or.MOtype(imo)==1) then !Closed-shell or alpha part of open-shell
			sumsqr=sum(CObasa(:,imo)**2)
			do iatm=1,ncenter
				atmcomp(iatm,imo)=sum(CObasa(basstart(iatm):basend(iatm),imo)**2)/sumsqr
			end do
		else !Beta part of open-shell
			iimo=imo-nbasis
			sumsqr=sum(CObasb(:,iimo)**2)
			do iatm=1,ncenter
				atmcomp(iatm,imo)=sum(CObasb(basstart(iatm):basend(iatm),iimo)**2)/sumsqr
			end do
		end if
	end do
end if
end subroutine



!!------- Generate all basis function composition in all orbitals by SCPA method
!Currently not employed by any function
subroutine gen_allorbbascomp_SCPA(bascomp)
use defvar
implicit real*8 (a-h,o-z)
real*8 bascomp(nbasis,nmo)
do imo=1,nmo
	if (MOtype(imo)==0.or.MOtype(imo)==1) then !Closed-shell or alpha part of open-shell
		sumsqr=sum(CObasa(:,imo)**2)
		do ibas=1,nbasis
			bascomp(ibas,imo)=CObasa(ibas,imo)**2/sumsqr
		end do
	else !Beta part of open-shell
		iimo=imo-nbasis
		sumsqr=sum(CObasb(:,iimo)**2)
		do ibas=1,nbasis
			bascomp(ibas,imo)=CObasb(ibas,iimo)**2/sumsqr
		end do
	end if
end do
end subroutine