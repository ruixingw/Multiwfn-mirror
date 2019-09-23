!------------- Modify & Check wavefunction
subroutine modwfn
use defvar
use util
implicit real*8 (a-h,o-z)
character seltmpc*10,selectyn,c1000tmp*1000,c2000tmp*2000
real*8 eigval(nbasis),eigvec(nbasis,nbasis),tmpmat(nbasis,nbasis)
integer orbarr(nmo)
integer,allocatable :: exclfragatm(:),tmparrint(:)
character*3 :: orbtype(0:2)=(/ "A+B"," A"," B" /)
character*6 :: symstr

do while(.true.)
	write(*,*) "          ============ Modify & Check wavefunction ============ "
	write(*,"(' Number of GTFs:',i6,', Orb:',i6,', Atoms:',i5,', A/B elec:',2f8.3)") nprims,nmo,ncenter,naelec,nbelec
	if (ifragcontri/=1) write(*,*) "-4 Exclude contribution of some atoms to real space functions"
	if (ifragcontri/=1) write(*,*) "-3 Only retain contribution of some atoms to real space functions"
	write(*,*) "-1 Return"
	write(*,*) "0 Save the modified wavefunction to a new .wfn file"
	if (allocated(CObasa)) then
		write(*,*) "1 List all primitive function      2 List all basis function"
	else
		write(*,*) "1 List all primitive function"
	end if
	write(*,*) "3 List all orbitals                4 Print detail information of an orbital"
	if (allocated(CObasa)) write(*,*) "5 Print coefficient matrix in basis function"
	if (allocated(CObasa)) write(*,*) "6 Print density matrix in basis function"
	if (allocated(CObasa)) write(*,*) "7 Print various kinds of integral matrix between basis functions"
	write(*,*) "11 Swap some information of two primitive functions"
	write(*,*) "21 Set center of a primitive       22 Set type of a primitive"
	write(*,*) "23 Set exponent of a primitive     24 Set coefficient of a primitive"
	if (allocated(CObasa)) then
		write(*,*) "25 Set coefficients of GTFs/basis functions that satisfied certain conditions"
	else
		write(*,*) "25 Set coefficients of GTFs that satisfied certain conditions"
	end if
	write(*,*) "26 Set occupation number of some orbitals"
	write(*,*) "27 Set type of some orbitals       28 Set energy of some orbitals"
	write(*,*) "31 Translate the system            32 Translate and duplicate the system"
	write(*,*) "33 Rotate wavefunction, namely X->Y, Y->Z, Z->X"
	if (imodwfn==0) write(*,*) "34 Set occupation number of inner orbitals to zero" !If occupation has been modified, don't do this to complicate things
	if (allocated(MOsym)) write(*,*) "35 Keep or discard orbital contributions according to irreducible rep."
	write(*,*) "36 Invert phase of some orbitals"
	read(*,*) iselect
	
	write(*,*)
	if (iselect==-1) then
		if (allocated(CObasa).and.imodwfn==1) then
			write(*,*) "Updating density matrix..."
			call genP
			write(*,*) "Density matrix has been updated"
		end if
		exit
		
	else if (iselect==-3.or.iselect==-4) then
		deallocate(fragatm) !fragatm has been defined previously by default, fragatm contains all atoms
		if (iselect==-3) then
			! "fragatm" is convertion relationship from fragment to the whole,
			! e.g. fragatm(4) is the actual atom index corresponding the 4th atom in fragment list
			write(*,"(a)") " Input atomic indices to define the fragment, e.g. 1,3-6,8,10-11 means atoms 1,3,4,5,6,8,10,11 will constitute the fragment"
			read(*,"(a)") c2000tmp
			call str2arr(c2000tmp,nfragatmnum)
			allocate(fragatm(nfragatmnum))
			call str2arr(c2000tmp,nfragatmnum,fragatm)
			call sort(fragatm,"val")
		else if (iselect==-4) then
			write(*,*) "How many atoms will be excluded?"
			write(*,*) "e.g. 1,3-6,8,10-11 means the atoms 1,3,4,5,6,8,10,11 will be excluded"
			read(*,"(a)") c2000tmp
			call str2arr(c2000tmp,nexclatm)
			nfragatmnum=ncenter-nexclatm
			allocate(fragatm(nfragatmnum),exclfragatm(nexclatm))
			call str2arr(c2000tmp,nexclatm,exclfragatm)
			j=0
			do i=1,ncenter
				if (all(exclfragatm/=i)) then
					j=j+1
					fragatm(j)=i
				end if
			end do
		end if
		j=0
		do i=1,nprims
			if (any(fragatm==b(i)%center)) then
				j=j+1      !Move function in the fragment to head of list
				CO(:,j)=CO(:,i)
				b(j)=b(i)
			end if
		end do
		ifragcontri=1 !Fragment has been defined by users
		write(*,"(' Done,',i8,' GTFs have been discarded,',i8,' GTFs reserved')") nprims-j,j
		nprims=j !Cut list at j, all functions after j seem non exist
		if (iselect==-4) deallocate(exclfragatm)

		!Modification of wavefunction has finished, now reduce size of b, CO... to current nprims and nmo to avoid potential problems
		if (allocated(b)) then !Only for input file contains wavefunctions
			call resizebynmo(nmo,nprims) !Reduce size of CO, MOene, MOocc, MOtype
			allocate(b_tmp(nprims))
			b_tmp(:)=b(1:nprims)
			deallocate(b)
			allocate(b(nprims))
			b=b_tmp
			deallocate(b_tmp)
		end if
	else if (iselect==0) then
		call outwfn("new.wfn",1,1,10)
		write(*,*) "New .wfn file has been outputted to new.wfn in current folder"
	
	else if (iselect==1) then
		do i=1,nprims
			write(*,"(i6,' Center:',i5,'(',a2,')','   Type: ',a,'   Exponent:',E16.7)") i,b(i)%center,a(b(i)%center)%name,GTFtype2name(b(i)%type),b(i)%exp
		end do
	
	else if (iselect==2) then
		do i=1,nbasis
            if (isphergau==1) then
			    write(*,"(' Basis:',i5,'   Shell:',i5,'   Center:',i5,'(',a2,')   Type:',a)")&
                i,basshell(i),bascen(i),a(bascen(i))%name,GTFtype2name(bastype(i))
            else !The primstart/end is constructed w.r.t. Cartesian basis functions
			    write(*,"(' Basis:',i5,'   Shell:',i5,'   Center:',i5,'(',a2,')   Type:',a,'  GTF:',i6,' to',i6)")&
                i,basshell(i),bascen(i),a(bascen(i))%name,GTFtype2name(bastype(i)),primstart(i),primend(i)
            end if
		end do
	
	else if (iselect==3) then
		write(*,*) "Basic information of all orbitals:"
		symstr=" "
		naorb=count(MOtype==1)
		do i=1,nmo
			if (allocated(MOsym)) symstr='('//MOsym(i)//')'
			if (wfntype==0.or.wfntype==2.or.wfntype==3) then
				write(*,"(' Orb:',i6,' Ene(au/eV):',f13.6,f13.4,' Occ:',f9.6,' Type:',a,1x,a)") &
				i,MOene(i),MOene(i)*au2eV,MOocc(i),orbtype(MOtype(i)),symstr
			else
				if (MOtype(i)==1) then
					write(*,"(i6,9x,' E(au/eV):',f12.5,f13.4,' Occ:',f9.6,' Typ:',a,1x,a)") &
					i,MOene(i),MOene(i)*au2eV,MOocc(i),orbtype(MOtype(i)),symstr
				else
					write(*,"(i6,' (',i6,')',' E(au/eV):',f12.5,f13.4,' Occ:',f9.6,' Typ:',a,1x,a)") &
					i,i-naorb,MOene(i),MOene(i)*au2eV,MOocc(i),orbtype(MOtype(i)),symstr
				end if
			end if
		end do
		if (any(MOtype==2)) write(*,"(a)") " Note: For beta orbitals, &
		the index in the parenthese shown above is the index counted from the first beta orbital"
		
	else if (iselect==4) then
		write(*,*) "Input the orbital index, e.g. 12"
		read(*,*) i
		if (i<1.or.i>nmo) then
			write(*,"(' Invalid orbital index, should within range of',i5,' and ',i5)") 1,nmo
		else
			write(*,"(' Occupation number is ',f12.7,'     Energy is',f12.6,' Hartree')") MOocc(i),MOene(i)
			if (MOtype(i)==0) write(*,*) "This is a closed-shell orbital"
			if (MOtype(i)==1) write(*,*) "This is an alpha orbital"
			if (MOtype(i)==2) write(*,*) "This is a beta orbital"
			write(*,*)
			do j=1,nprims
				write(*,"(' GTF:',i6,' Cen:',i5,'(',a2,')',' Type: ',a,' Coeff:',1PE15.8,'  Exp: ',1PE13.7)") &
				j,b(j)%center,a(b(j)%center)%name,GTFtype2name(b(j)%type),co(i,j),b(j)%exp
			end do
			write(*,"(a,/)") " Note: The ""coeff."" are expansion coefficients of orbitals with respect to GTFs, including normalization constant"
			if (allocated(b)) then
				do j=1,nbasis
					if (MOtype(i)==0.or.MOtype(i)==1) covalue=CObasa(j,i)
					if (MOtype(i)==2) covalue=CObasb(j,i-nbasis)
					write(*,"(' Basis func:',i6,'  Cen:',i5,'(',a2,')',' Shell:',i5,' Type: ',a,' Coeff:',f12.8)") &
					j,bascen(j),a(bascen(j))%name,basshell(j),GTFtype2name(bastype(j)),covalue
				end do
			end if
			write(*,"(a,/)") " Note: The ""coeff."" are expansion coefficients of orbitals with respect to basis functions, which are normalized functions"
		end if
	
	else if (iselect==5) then
		write(*,*) "0 Return"
		write(*,*) "1 Print on screen"
		write(*,*) "2 Print to Cmat.txt in current folder"
		read(*,*) iseltmp
		if (iseltmp==1.or.iseltmp==2) then
			if (iseltmp==1) ides=6
			if (iseltmp==2) then
				ides=10
				open(ides,file="Cmat.txt",status="replace")
			end if
			write(ides,*) "Note: (i,j) element means coefficient of ith basis function in jth orbital"
			if (wfntype==0.or.wfntype==2.or.wfntype==3) then
				call showmatgau(CObasa,"Coefficient matrix",0,fileid=ides)
			else if (wfntype==1.or.wfntype==4) then
				call showmatgau(CObasa,"Alpha coefficient matrix",0,fileid=ides)
				call showmatgau(CObasb,"Beta coefficient matrix",0,fileid=ides)
			end if
			if (iseltmp==2) then
				write(*,*) "Done! The matrix has been outputted to Cmat.txt in current folder"
				close(ides)
			end if
		end if
	
	else if (iselect==6) then
		write(*,*) "0 Return"
		write(*,*) "1 Print on screen"
		write(*,*) "2 Print to Pmat.txt in current folder"
		read(*,*) iseltmp
		if (iseltmp==1.or.iseltmp==2) then
			if (iseltmp==1) ides=6
			if (iseltmp==2) then
				ides=10
				open(ides,file="Pmat.txt",status="replace")
			end if
			call showmatgau(Ptot,"Total density matrix",1,fileid=ides)
			sumt=0
			do i=1,nbasis
				sumt=sumt+Ptot(i,i)
			end do
			write(ides,"(' The trace of the density matrix:',f12.6)") sumt
			write(ides,"(' The trace of the density matrix multiplied by overlap matrix:',f12.6)") sum(Ptot*Sbas)
			if (wfntype==1.or.wfntype==2.or.wfntype==4) then
				suma=0
				sumb=0
				do i=1,nbasis
					suma=suma+Palpha(i,i)
					sumb=sumb+Pbeta(i,i)
				end do
				write(ides,*)
				call showmatgau(Palpha-Pbeta,"Spin density matrix",1,fileid=ides)
				write(ides,*)
				call showmatgau(Palpha,"Alpha density matrix",1,fileid=ides)
				write(ides,*)
				call showmatgau(Pbeta,"Beta density matrix",1,fileid=ides)
				write(ides,*)
				write(ides,"(' The trace of the alpha and beta density matrix:',2f12.6)") suma,sumb
				write(ides,"(' The trace of the density matrix multiplied by overlap matrix:',/,' Alpha:',f12.6,'   Beta:',f12.6)") sum(Palpha*Sbas),sum(Pbeta*Sbas)
			end if
			if (iseltmp==2) then
				write(*,*) "Done! The matrix has been outputted to Pmat.txt in current folder"
				close(ides)
			end if
		end if
	
	!Print various kinds of integral matrix between basis functions
	else if (iselect==7) then
		write(*,*) "Print which kind of integral matrix?"
		write(*,*) "1 Overlap integral"
		write(*,*) "2 Electric dipole moment integral"
		write(*,*) "3 Magnetic dipole moment integral"
		write(*,*) "4 Velocity integral"
		write(*,*) "5 Kinetic energy integral"
		read(*,*) imattype
		write(*,*) "Select destination for output"
		write(*,*) "1 Print on screen"
		write(*,*) "2 Print to intmat.txt in current folder"
		read(*,*) iout
		if (iout==1) ides=6
		if (iout==2) then
			ides=10
			open(ides,file="intmat.txt",status="replace")
		end if
		if (imattype==1) then
			call showmatgau(Sbas,"Overlap matrix",1,fileid=ides)
			call diagsymat(Sbas,eigvec,eigval,ierror)
			write(ides,*)
			write(ides,*) "Eigenvalues:"
			write(ides,"(6f12.8)") eigval
		else if (imattype==2) then
			if (allocated(Dbas)) then
				write(ides,*)
				call showmatgau(Dbas(1,:,:),"Electric dipole moment matrix (X component)",1,fileid=ides)
				write(ides,*)
				call showmatgau(Dbas(2,:,:),"Electric dipole moment matrix (Y component)",1,fileid=ides)
				write(ides,*)
				call showmatgau(Dbas(3,:,:),"Electric dipole moment matrix (Z component)",1,fileid=ides)
			else
				write(*,"(a)") " Error: Electric dipole moment integral matrix has not been calculated, please set ""igenDbas"" &
				in settings.ini to 1, so that this matrix can be generated when loading input file"
				write(*,*) "Press Enter button to skip"
				read(*,*)
				cycle
			end if
		else if (imattype==3) then
			if (allocated(Magbas)) then
				write(ides,*)
				call showmatgau(Magbas(1,:,:),"Magnetic dipole moment matrix (X component)",0,fileid=ides)
				write(ides,*)
				call showmatgau(Magbas(2,:,:),"Magnetic dipole moment matrix (Y component)",0,fileid=ides)
				write(ides,*)
				call showmatgau(Magbas(3,:,:),"Magnetic dipole moment matrix (Z component)",0,fileid=ides)
			else
				write(*,"(a)") " Error: Magnetic dipole moment integral matrix has not been calculated, please set ""igenMagbas"" &
				in settings.ini to 1, so that this matrix can be generated when loading input file"
				write(*,*) "Press Enter button to skip"
				read(*,*)
				cycle
			end if
		else if (imattype==4.or.imattype==5) then
			if (isphergau==1) then !If you want, you can generate the matrix and perform Cartesian->spherical transformation at file loading stage for Velbas
				write(*,"(a)") " Error: Spherical-harmonic type of basis functions are found. This function only works when all basis functions are Cartesian type!"
				write(*,*) "Press Enter button to skip"
				read(*,*)
				cycle
			end if
			if (imattype==4) then
				if (.not.allocated(Velbas)) then
					write(*,*) "Calculating velocity matrix..."
					allocate(Velbas(3,nbasis,nbasis))
    				call genvelbas
				end if
				write(ides,*)
				call showmatgau(Velbas(1,:,:),"Velocity matrix (X component)",0,fileid=ides)
				write(ides,*)
				call showmatgau(Velbas(2,:,:),"Velocity matrix (Y component)",0,fileid=ides)
				write(ides,*)
				call showmatgau(Velbas(3,:,:),"Velocity matrix (Z component)",0,fileid=ides)
			else if (imattype==5) then
				if (.not.allocated(Tbas)) then
					write(*,*) "Calculating Kinetic energy matrix..."
					allocate(Tbas(nbasis,nbasis))
    				call genTbas
				end if
				write(ides,*)
				call showmatgau(Tbas(:,:),"Kinetic energy matrix",1,fileid=ides)
			end if
		end if
	
		if (iout==2) then
			write(*,*) "Done! The matrix has been outputted to intmat.txt in current folder"
			close(ides)
		end if
		
	else if (iselect==11) then
		write(*,*) "Swap information of which two GTFs? Input their indices  e.g. 18,21"
		read(*,*) i,j
		write(*,*) "Swap which information for the two GTFs?"
		write(*,*) "1 Swap all propertie"
		write(*,*) "2 Swap center"
		write(*,*) "3 Swap function type"
		write(*,*) "4 Swap exponent"
		write(*,*) "5 Swap orbital expansion coefficient"
		read(*,*) iswapcontent
		if (iswapcontent==1) call swapGTF(i,j,"all")
		if (iswapcontent==2) call swapGTF(i,j,"cen")
		if (iswapcontent==3) call swapGTF(i,j,"typ")
		if (iswapcontent==4) call swapGTF(i,j,"exp")
		if (iswapcontent==5) call swapGTF(i,j,"MO ")
		write(*,*) "Swapping finished!"
	
	else if (iselect==21) then
		write(*,*) "Input the index of primitive function"
		read(*,*) i
		write(*,*) "Input the center"
		read(*,*) j
		if (j<=ncenter.and.j>0) then
			b(i)%center=j
		else
			write(*,"('Error: The value should >0 and <=',i7)") ncenter
		end if
	
	else if (iselect==22) then
		write(*,*) "Input the index of primitive function"
		read(*,*) i
		write(*,*) "Input the type"
		write(*,*) "Valid input: S/X/Y/Z/XX/YY/ZZ/XY/XZ/YZ/XXX/YYY/ZZZ/XXY/XXZ/YYZ/XYY/XZZ/YZZ/XYZ"
		write(*,*) "ZZZZ/YZZZ/YYZZ/YYYZ/YYYY/XZZZ/XYZZ/XYYZ/XYYY/XXZZ/XXYZ/XXYY/XXXZ/XXXY/XXXX"
		write(*,"(a)") " ZZZZZ/YZZZZ/YYZZZ/YYYZZ/YYYYZ/YYYYY/XZZZZ/XYZZZ/XYYZZ/XYYYZ/XYYYY/XXZZZ/XXYZZ/XXYYZ/XXYYY/XXXZZ/XXXYZ/XXXYY/XXXXZ/XXXXY/XXXXX"
		read(*,*) seltmpc
		do j=1,size(GTFtype2name)
			if (seltmpc==GTFtype2name(j)) then
				b(i)%type=j
				exit
			end if
			if (j==20) write(*,*) "Error: Could not recognize this type"
		end do
	
	else if (iselect==23) then
		write(*,*) "Input the index of primitive function"
		read(*,*) i
		write(*,*) "Input the exponent"
		read(*,*) rexp
		b(i)%exp=rexp
	
	else if (iselect==24) then
		write(*,*) "Input the index of primitive function"
		read(*,*) iprm
		write(*,*) "Input the orbital index, e.g. 12"
		read(*,*) imonum
		if (iprm<=nprims.and.iprm>0.and.imonum<=nmo.and.imonum>0) then
			write(*,*) "Input the coefficient"
			read(*,*) rcoeff
			CO(imonum,iprm)=rcoeff
		else
			write(*,"(' Error: The index of function or orbital exceed valid range')")
		end if
	
	else if (iselect==25) then
		isetmode=1
		if (allocated(CObasa)) then
			write(*,*) "1 Set coefficients of GTFs"
			write(*,*) "2 Set coefficients of basis functions"
			read(*,*) isetmode
		end if
		if (isetmode==1) then
			write(*,*) "The following your inputs are conditions for filtering GTFs"
			write(*,*) "Rule of range input: 3,17 means from 3 to 17, 6,6 means only 6, 0,0 means all"
			write(*,*)
			write(*,*) "Input the range of index of GTFs"
			read(*,*) ind1,ind2
			write(*,*) "Input the range of atoms"
			read(*,*) iatm1,iatm2
			write(*,*) "Input the type of GTFs (one of S,X,Y,Z,XX,XY... ALL means all types)"
			write(*,*) "You can also input S,P,D,F,G,H to select GTF according to angular moment"
			read(*,*) seltmpc
			write(*,*) "Input the range of orbitals"
			read(*,*) imo1,imo2
			write(*,*) "Input the expansion coefficient you want to set, e.g. 0.5"
			read(*,*) coval
			if (ind1==0) ind1=1
			if (ind2==0) ind2=nprims
			if (iatm1==0) iatm1=1
			if (iatm2==0) iatm2=ncenter
			if (imo1==0) imo1=1
			if (imo2==0) imo2=nmo
			nsel=0
			do iGTF=ind1,ind2
				if (b(iGTF)%center>=iatm1.and.b(iGTF)%center<=iatm2) then
					if (seltmpc=="ALL".or.seltmpc=="all".or.GTFtype2name(b(iGTF)%type)==trim(seltmpc).or.type2ang(b(iGTF)%type)==trim(seltmpc)) then
						CO(imo1:imo2,iGTF)=coval
						nsel=nsel+1
					end if
				end if
			end do
			write(*,"(' Coefficient of',i8,' GTFs are set')") nsel
		else if (isetmode==2) then
			write(*,*) "The following your inputs are conditions for filtering basis functions"
			write(*,*) "Rule of range input: 3,17 means from 3 to 17, 6,6 means only 6, 0,0 means all"
			write(*,*) "You can also input S,P,D,F,G,H to select according to angular moment"
			write(*,*)
			write(*,*) "Input the range of index of basis functions"
			read(*,*) ind1,ind2
			write(*,*) "Input the range of atoms"
			read(*,*) iatm1,iatm2
			write(*,"(a)") " Input the type of basis functions (one of S,X,Y,Z,XX... ALL means all types)"
			read(*,*) seltmpc
			write(*,*) "Input the range of orbitals"
			read(*,*) imo1,imo2
			ispinsel=1
			if (wfntype==1.or.wfntype==4) then
				write(*,*) "For which type of orbitals? 0=Both 1=Alpha 2=Beta"
				read(*,*) ispinsel
			end if
			write(*,*) "Input the expansion coefficient you want to set, e.g. 0.5"
			read(*,*) coval
			if (ind1==0) ind1=1
			if (ind2==0) ind2=nbasis
			if (iatm1==0) iatm1=1
			if (iatm2==0) iatm2=ncenter
			if (imo1==0) imo1=1
			if (imo2==0) imo2=nbasis
			nsel=0
			do ibas=ind1,ind2
				if (bascen(ibas)>=iatm1.and.bascen(ibas)<=iatm2) then
					if (seltmpc=="ALL".or.seltmpc=="all".or.GTFtype2name(bastype(ibas))==trim(seltmpc).or.type2ang(bastype(ibas))==trim(seltmpc)) then
						if (ispinsel==0.or.ispinsel==1) CObasa(ibas,imo1:imo2)=coval
						if (ispinsel==0.or.ispinsel==2) CObasb(ibas,imo1:imo2)=coval
						nsel=nsel+1
					end if
				end if
			end do
			write(*,"(' Coefficient of',i8,' basis functions are set')") nsel
			imodwfn=1
		end if
		write(*,*) "Done!"

	else if (iselect==26) then
		do while(.true.)
			write(*,*) "Select the orbitals for which the occupation numbers are needed to be changed"
			write(*,*) "e.g. 2,4,13-16,20 means selecting orbitals 2,4,13,14,15,16,20"
			write(*,*) "Input 0 can select all orbitals, input q or 00 can return"
			read(*,"(a)") c1000tmp
			if (c1000tmp(1:1)=='q'.or.c1000tmp(1:2)=='00') exit
			if (c1000tmp(1:1)=='0') then
				numorbsel=nmo
				do i=1,nmo
					orbarr(i)=i
				end do
			else
				call str2arr(c1000tmp,numorbsel,orbarr)
				if ( any(orbarr(1:numorbsel)<1).or.any(orbarr(1:numorbsel)>nmo) ) then
					write(*,*) "Error: One or more orbital indices exceeded valid range!"
					cycle
				end if
			end if
            write(*,*)
			write(*,*) "Set occupation number to which value? e.g. 1.2"
			write(*,*) "Note:"
			write(*,"(a)") " You can also input for example ""+1.1"" ""-1.1"" ""*1.1"" ""/1.1"" to add, minus, multiply and divide the occupation numbers by 1.1"
			write(*,"(a)") " To recover the initial occupation numbers, input ""i"""
			write(*,"(a)") " To generate occupation state for calculating odd electron density, input ""odd"""
			read(*,"(a)") c1000tmp
			if (index(c1000tmp,"odd")/=0) then
				do iorb=1,numorbsel
					MOocc(orbarr(iorb))=min(2-MOocc(orbarr(iorb)),MOocc(orbarr(iorb)))
				end do
				write(*,*) "Done!"
			else if (index(c1000tmp,"i")/=0) then
				MOocc(orbarr(1:numorbsel))=MOocc_org(orbarr(1:numorbsel))
				write(*,*) "The occupation numbers have been recovered"
			else if (c1000tmp(1:1)=='+'.or.c1000tmp(1:1)=='-'.or.c1000tmp(1:1)=='*'.or.c1000tmp(1:1)=='/') then
				read(c1000tmp(2:),*) tmpval
				if (c1000tmp(1:1)=='+') MOocc(orbarr(1:numorbsel))=MOocc(orbarr(1:numorbsel))+tmpval
				if (c1000tmp(1:1)=='-') MOocc(orbarr(1:numorbsel))=MOocc(orbarr(1:numorbsel))-tmpval
				if (c1000tmp(1:1)=='*') MOocc(orbarr(1:numorbsel))=MOocc(orbarr(1:numorbsel))*tmpval
				if (c1000tmp(1:1)=='/') MOocc(orbarr(1:numorbsel))=MOocc(orbarr(1:numorbsel))/tmpval
				write(*,*) "Done!"
			else
				read(c1000tmp,*) tmpval
				MOocc(orbarr(1:numorbsel))=tmpval
				write(*,*) "Done!"
			end if
			call updatenelec !Update the number of electrons
			imodwfn=1
			if (any(MOocc/=int(MOocc))) then
				if (wfntype==0) then
					wfntype=3 !RHF-> Restricted post-HF wavefunction
					write(*,*) "Note: Now the wavefunction is recognized as restricted post-HF wavefunction"
				else if (wfntype==1.or.wfntype==2) then !UHF/ROHF-> Unrestricted post-HF wavefunction
					wfntype=4
					write(*,*) "Note: Now the wavefunction is recognized as unrestricted post-HF wavefunction"
				end if
			end if
			write(*,*)
		end do
	
	else if (iselect==27) then
		do while(.true.)
			write(*,*) "Set type for which range of orbitals?"
			write(*,*) "e.g. 2,4,13-16,20 means selecting orbitals 2,4,13,14,15,16,20"
			write(*,*) "Input 0 can select all orbitals, input q or 00 can return"
			read(*,"(a)") c1000tmp
			if (c1000tmp(1:1)=='q'.or.c1000tmp(1:2)=='00') exit
			if (c1000tmp(1:1)=='0') then
				numorbsel=nmo
				do i=1,nmo
					orbarr(i)=i
				end do
			else
				call str2arr(c1000tmp,numorbsel,orbarr)
				if ( any(orbarr(1:numorbsel)<1).or.any(orbarr(1:numorbsel)>nmo) ) then
					write(*,*) "Error: One or more orbital indices exceeded valid range!"
					cycle
				end if
			end if
			write(*,*) "Set to which type?  0=Alpha+Beta  1=Alpha  2=Beta"
			read(*,*) isettype
			MOtype(orbarr(1:numorbsel))=isettype
			!Recount alpha and beta electrons
			call updatenelec
			write(*,*) "Done!"
			!Update wavefunction type
			if (all(MOtype==0)) then
				if (all(MOocc==nint(MOocc))) then !All A+B orbital & integer occupation
					wfntype=0
					write(*,"(' Note: Now the wavefunction is recognized as restricted closed-shell single-determinant wavefunction')")
				else !All A+B orbital & partial occupation
					wfntype=3
					write(*,"(' Note: Now the wavefunction is recognized as closed-shell post-HF wavefunction')")
				end if
			else
				if (any(MOocc/=nint(MOocc)).and.all(MOtype/=0)) then !Either A or B, and partial occupation
					wfntype=4
					write(*,"(' Note: Now the wavefunction is recognized as open-shell post-HF wavefunction')")
				else if (all(MOocc==nint(MOocc)).and.all(MOtype/=0)) then !Integer occupation and either A or B
					wfntype=1
					write(*,"(' Note: Now the wavefunction is recognized as unrestricted single-determinant wavefunction')")
				else if (all(MOocc==nint(MOocc)).and.any(MOtype==0).and.all(MOtype/=2)) then !Integer occupation and at least one orbital is A, and B is unexisted
					wfntype=2
					write(*,"(' Note: Now the wavefunction is recognized as restricted open-shell wavefunction')")
				else
					write(*,"(' Warning: The type of present wavefunction cannot be identified! You need to reset orbital types')")
					write(*,*) "Press ENTER button to continue"
					read(*,*)
				end if
			end if
			write(*,*)
			imodwfn=1
		end do
		
	else if (iselect==28) then
		do while(.true.)
			write(*,*) "Select the orbitals for which the energy are needed to be changed"
			write(*,*) "e.g. 2,4,13-16,20 means selecting orbitals 2,4,13,14,15,16,20"
			write(*,*) "Input 0 can select all orbitals, input q or 00 can return"
			read(*,"(a)") c1000tmp
			if (c1000tmp(1:1)=='q'.or.c1000tmp(1:2)=='00') exit
			if (c1000tmp(1:1)=='0') then
				numorbsel=nmo
				do i=1,nmo
					orbarr(i)=i
				end do
			else
				call str2arr(c1000tmp,numorbsel,orbarr)
				if ( any(orbarr(1:numorbsel)<1).or.any(orbarr(1:numorbsel)>nmo) ) then
					write(*,*) "Error: One or more orbital indices exceeded valid range!"
					cycle
				end if
			end if
			write(*,*) "0 Recover their initial orbital energies"
			write(*,*) "1 Set the orbital energies to a specific value"
			write(*,*) "2 Add a value to the orbital energies"
			write(*,*) "3 Minus a value from the orbital energies"
			write(*,*) "4 Multiply the orbital energies by a value"
			write(*,*) "5 Divide the orbital energies by a value"
			read(*,*) iselop
			if (iselop==0) then
				MOene(orbarr(1:numorbsel))=MOene_org(orbarr(1:numorbsel))
			else if (iselop==1) then
				write(*,*) "Input the energy in eV, e.g. -3.6"
				read(*,*) tmpval
				MOene(orbarr(1:numorbsel))=tmpval/au2eV
			else if (iselop==2) then
				write(*,*) "Input the value in eV, e.g. 0.6"
				read(*,*) tmpval
				MOene(orbarr(1:numorbsel))=MOene(orbarr(1:numorbsel))+tmpval/au2eV
			else if (iselop==3) then
				write(*,*) "Input the value in eV, e.g. 0.6"
				read(*,*) tmpval
				MOene(orbarr(1:numorbsel))=MOene(orbarr(1:numorbsel))-tmpval/au2eV
			else if (iselop==4) then
				write(*,*) "Input the factor, e.g. 0.9"
				read(*,*) tmpval
				MOene(orbarr(1:numorbsel))=MOene(orbarr(1:numorbsel))*tmpval
			else if (iselop==5) then
				write(*,*) "Input the factor, e.g. 1.2"
				read(*,*) tmpval
				MOene(orbarr(1:numorbsel))=MOene(orbarr(1:numorbsel))/tmpval
			end if
			write(*,*) "Done!"
			write(*,*)
		end do
	
	else if (iselect==31) then
		write(*,*) "Input X,Y,Z of translation vector (e.g. 3.2,1.0,0)"
		read(*,*) pbctransx,pbctransy,pbctransz
		write(*,*) "You inputted coordinates are in which unit?  1:Bohr  2:Angstrom"
		read(*,*) iunit
		if (iunit==2) then
			pbctransx=pbctransx/b2a
			pbctransy=pbctransy/b2a
			pbctransz=pbctransz/b2a
		end if
		do i=1,ncenter
			a(i)%x=a(i)%x+pbctransx
			a(i)%y=a(i)%y+pbctransy
			a(i)%z=a(i)%z+pbctransz
		end do
		imodwfn=1
	
	else if (iselect==32) then
		write(*,*) "Input X,Y,Z of translation vector (e.g. 3.2,1.0,0)"
		read(*,*) pbctransx,pbctransy,pbctransz
		write(*,*) "You inputted coordinates are in which unit?  1:Bohr  2:Angstrom"
		read(*,*) iunit
		if (iunit==2) then
			pbctransx=pbctransx/b2a
			pbctransy=pbctransy/b2a
			pbctransz=pbctransz/b2a
		end if
		write(*,*) "Duplicate system how many times? e.g. 3"
		read(*,*) numdup
		!_tmp is for backing up current information
		allocate(a_tmp(ncenter))
		allocate(b_tmp(nprims))
		allocate(CO_tmp(nmo,nprims))
		a_tmp=a
		b_tmp=b
		CO_tmp=CO
		deallocate(a,b,CO)
		nprims_tmp=nprims
		ncenter_tmp=ncenter
		nprims=nprims*(numdup+1)
		ncenter=ncenter*(numdup+1)
		nelec=nelec*(numdup+1)
		naelec=naelec*(numdup+1)
		nbelec=nbelec*(numdup+1)
		allocate(a(ncenter))
		allocate(b(nprims))
		allocate(CO(nmo,nprims))
		do idup=0,numdup
			a(ncenter_tmp*idup+1:ncenter_tmp*(idup+1))=a_tmp(1:center_tmp)
			a(ncenter_tmp*idup+1:ncenter_tmp*(idup+1))%x=a_tmp(1:center_tmp)%x+pbctransx*idup
			a(ncenter_tmp*idup+1:ncenter_tmp*(idup+1))%y=a_tmp(1:center_tmp)%y+pbctransy*idup
			a(ncenter_tmp*idup+1:ncenter_tmp*(idup+1))%z=a_tmp(1:center_tmp)%z+pbctransz*idup
			b(nprims_tmp*idup+1:nprims_tmp*(idup+1))=b_tmp(1:nprims_tmp)
			b(nprims_tmp*idup+1:nprims_tmp*(idup+1))%center=b_tmp(1:nprims_tmp)%center+ncenter_tmp*idup
			CO(:,nprims_tmp*idup+1:nprims_tmp*(idup+1))=CO_tmp(:,1:nprims_tmp) !Notice that the orbitals do not satisify normalization condition any more, and the orbital occupation number will be artifical
		end do
		deallocate(a_tmp,b_tmp,CO_tmp)
		imodwfn=1
		call gendistmat !The number of atoms have changed, so we must update distance matrix
	
	else if (iselect==33) then
		write(*,*) "Rotate which orbital? (Input 0 to rotate all orbitals)"
		read(*,*) iorb
		if (iorb/=0) then
			call orbcoeffrotate(iorb)
		else if (iorb==0) then
			do imo=1,nmo
				call orbcoeffrotate(imo)
			end do
			write(*,*) "Also rotate atomic coordinates? (y/n)"
			read(*,*) selectyn
			if (selectyn=='y'.or.selectyn=='Y') then
				do iatm=1,ncenter
					tmpval=a(iatm)%x
					a(iatm)%x=a(iatm)%z
					a(iatm)%z=a(iatm)%y
					a(iatm)%y=tmpval
				end do
			end if
		end if
		write(*,*) "Done!"
	
	else if (iselect==34) then
		call getninnerele(ninnerele,1)
		nelec=nelec-ninnerele
		naelec=naelec-ninnerele/2
		nbelec=nbelec-ninnerele/2
		if (wfntype==1.or.wfntype==4) then !UHF and U-post-HF wfn
			MOocc(1:ninnerele/2)=0D0
			do j=1,nmo !Where the first beta orbital appear now
				if (motype(j)==2) exit
			end do
			MOocc(1:j+ninnerele/2-1)=0D0
			write(*,"(' The occupation of',i7,' lowest energy orbitals have been set to zero')") ninnerele
		else if (wfntype==0.or.wfntype==2.or.wfntype==3) then !restricted(=0) or RO(=2) or post-R(=3) wavefunction
			MOocc(1:ninnerele/2)=0D0
			write(*,"(' The occupation of',i7,' lowest energy orbitals have been set to zero')") ninnerele/2
		end if
		if (wfntype==3.or.wfntype==4) write(*,"(' Warning: Discarding inner orbitals for post-HF wavefunction will lead to unexpected result!')") 
		imodwfn=1
	
	else if (iselect==35) then
		call selMO_IRREP
	
	else if (iselect==36) then
		write(*,*) "Input index of the orbitals, e.g. 2,3,7-10"
		read(*,"(a)") c1000tmp
		call str2arr(c1000tmp,ntmp,orbarr)
		do idxtmp=1,ntmp
			idx=orbarr(idxtmp)
			CO(idx,:)=-CO(idx,:)
			if (allocated(CObasa)) then
				if (idx<=nbasis) then
					CObasa(:,idx)=-CObasa(:,idx)
				else
					CObasb(:,idx-nbasis)=-CObasb(:,idx-nbasis)
				end if
			end if
		end do
		write(*,*) "Done!"
		imodwfn=1
	end if
	write(*,*)
end do
end subroutine




!!---------------- Select MOs according to irreducible representation
subroutine selMO_IRREP
use defvar
use util
character symlab(nmo)*4,c2000tmp*2000,symstat(nmo)*9 !Allocate the array lengths as upper limit
integer tmparr(nmo),symNorb(nmo) !Allocate the array lengths as upper limit
if (wfntype/=0.and.wfntype/=1) then
	write(*,"(a)") " Error: This function only works for RHF or UHF wavefunctions (or the DFT counterparts)"
	return
end if
nsym=0
do imo=1,nmo
	if (MOocc_org(imo)==0D0) cycle
	if (all(symlab(1:nsym)/=MOsym(imo))) then
		nsym=nsym+1
		symlab(nsym)=MOsym(imo)
	end if
end do
symNorb=0
do imo=1,nmo
	if (MOocc_org(imo)==0D0) cycle
	do isym=1,nsym
		if (MOsym(imo)==symlab(isym)) symNorb(isym)=symNorb(isym)+1
	end do
end do
symstat="Normal"
MOocc=MOocc_org
if (imodwfn==1) write(*,*) "Note: Original occupation status has been recovered"
write(*,*) "Note: Only the orbitals that originally occupied are taken into account here"
do while(.true.)
	write(*,*)
	write(*,*) "Information of various irreducible representations:"
	do isym=1,nsym
		write(*,"(i5,'  Sym: ',a,'  N_orb:',i5,'    Status: ',a)") isym,symlab(isym),symNorb(isym),symstat(isym)
	end do
	write(*,*)
	write(*,*) "0 Save and return"
	write(*,*) "1 Discard specific irreducible representations"
	write(*,*) "2 Recover original status"
	write(*,*) "3 Reverse status"
	read(*,*) isel
	
	if (isel==0) then
		call updatenelec
		imodwfn=1
		write(*,*) "The current orbital occupation status has been saved"
		write(*,*) "Updating density matrix..."
		call genP
		write(*,*) "Density matrix has been updated"
		exit
	else if (isel==2) then
		MOocc=MOocc_org
		symstat="Normal"
	else if (isel==1.or.isel==3) then
		if (isel==1) then
			write(*,*) "Input the index of the irreducible representations to be discarded, e.g. 1,3-5"
			read(*,"(a)") c2000tmp
			call str2arr(c2000tmp,nsymsel,tmparr)
			do isym=1,nsymsel
				symstat(tmparr(isym))="Discarded"
			end do
		else if (isel==3) then
			do isym=1,nsym
				if (symstat(isym)=="Normal") then
					symstat(isym)="Discarded"
				else
					symstat(isym)="Normal"
				end if
			end do
		end if
		do imo=1,nmo
			if (MOocc_org(imo)==0D0) cycle
			do isym=1,nsym
				if (MOsym(imo)==symlab(isym)) then
					if (symstat(isym)=="Normal") MOocc(imo)=MOocc_org(imo)
					if (symstat(isym)=="Discarded") MOocc(imo)=0D0
					exit
				end if
			end do
		end do
	end if
	write(*,*) "Done!"
end do
end subroutine



!!---------- Update the number of electrons
subroutine updatenelec
use defvar
integer imo
nelec=0
naelec=0
nbelec=0
do imo=1,nmo
	if (MOtype(imo)==0) then
		naelec=naelec+MOocc(imo)/2D0
		nbelec=nbelec+MOocc(imo)/2D0
	else if (MOtype(imo)==1) then
		naelec=naelec+MOocc(imo)
	else if (MOtype(imo)==2) then
		nbelec=nbelec+MOocc(imo)
	end if
end do
nelec=naelec+nbelec
end subroutine
			
			

!!!-------- Check if present wavefunction is sanity, i.e. all orbital satisfies normalization condition
subroutine wfnsanity
use defvar
implicit real*8 (a-h,o-z)
real*8 GTFSmat(nprims*(nprims+1)/2)
call genGTFSmat(GTFSmat,nprims*(nprims+1)/2)
rmaxdev=0
rmaxdevint=0
do imo=1,nmo
	tmp=0
	!$OMP parallel shared(tmp) private(iGTF,jGTF,tmpprivate) num_threads(nthreads)
	tmpprivate=0
	!$OMP do schedule(dynamic)
	do iGTF=1,nprims
		do jGTF=iGTF+1,nprims
			tmpprivate=tmpprivate+2*co(imo,iGTF)*co(imo,jGTF)*GTFSmat(jGTF*(jGTF-1)/2+iGTF)
		end do
		tmpprivate=tmpprivate+co(imo,iGTF)**2*GTFSmat(iGTF*(iGTF-1)/2+iGTF)
	end do
	!$OMP END DO
	!$OMP CRITICAL
	tmp=tmp+tmpprivate
	!$OMP END CRITICAL
	!$OMP END PARALLEL
	write(*,"(' Orbital',i7,', Occ:',f8.4,'   Value:',f16.10)") imo,MOocc(imo),tmp
	tmpt=abs(tmp-1)
	if (tmpt>rmaxdev) rmaxdev=tmpt
	tmpt=abs(tmp-nint(tmp))
	if (tmpt>rmaxdevint) rmaxdevint=tmpt
end do
write(*,"(' Maximum deviation to 1:',f16.10)") rmaxdev
write(*,"(' Maximum deviation to integer:',f16.10)") rmaxdevint
write(*,*) "Press ENTER button to continue"
read(*,*)
end subroutine




!!---------- Return normalization coefficient for specific type of cartesian type GTF, see Levine 5ed p487
!The meaning of itype is defined in GTFtype2name
real*8 function normgau(itype,exp)
use defvar
use util
implicit real*8 (a-h,o-z)
real*8 exp
ix=type2ix(itype)
iy=type2iy(itype)
iz=type2iz(itype)
normgau=(2*exp/pi)**0.75D0*dsqrt( (8*exp)**(ix+iy+iz)*ft(ix)*ft(iy)*ft(iz)/(ft(2*ix)*ft(2*iy)*ft(2*iz)) )
end function


!---- Renormalizing shells (modifying contraction coefficients in basis shells) for Molden input file
!Various basis functions in a shell may have different normalization factor, however, since we are dealing with shell, 
!we can use any specific basis function to derive normalization factor and fix contraction coefficients
!Here we only consider (Lval,0,0) or (0,Lval,0) or (0,0,Lval) type, which have identical normalization factor
!Lval=0/1/2/3/4/5 corresponds to s/p/d/f/g/h, exp and con are exponents and contraction coefficients in the basis shell, respectively
subroutine renormmoldengau(nlen,Lval,exp,con)
implicit real*8 (a-h,o-z)
integer nlen,Lval
real*8 exp(nlen),con(nlen),ctmp(nlen)
pi=acos(-1D0)
call genn1n2nf(Lval,n1,n2,nf)
fc=2D0**n1/(pi**3*nf)
do i=1,nlen
	prmnormfac=sqrt(sqrt(fc*exp(i)**n2)) !Normalization factor of (Lval,0,0) type of GTF
	ctmp(i)=con(i)*prmnormfac
end do
facnorm=0D0 !Calculate <Lval,0,0|Lval,0,0> overlap integral for (Lval,0,0) type of contracted basis function
do i=1,nlen
	do j=1,i
	  expavg=(exp(i)+exp(j))/2D0
	  facadd=ctmp(i)*ctmp(j)/sqrt(fc*expavg**n2)
	  if (i/=j) facadd=facadd*2
	  facnorm=facnorm+facadd
	end do
end do
if (facnorm>1D-10) facnorm=1/sqrt(facnorm)
con=con*facnorm !Fix contraction coefficients
end subroutine
!!---- Produce normalization factor of (Lval,0,0) type of GTF with angular moment of Lval, &
!used to fix the contraction coefficient problem of the Molden input file generated by ORCA
!This routine can also be replaced by the slower routine "normgau", e.g. renormgau_ORCA(0.8D0,3) = normgau(21,0.8D0)
!cf. fnorm_lmn of m2a
real*8 function renormgau_ORCA(exp,Lval)
real*8 exp,pi
integer Lval,n1,n2,nf
pi=acos(-1D0)
call genn1n2nf(Lval,n1,n2,nf)
renormgau_ORCA=dsqrt(dsqrt(2**n1*exp**n2/(pi**3*nf))) !Get norm, the norm^4 = 2^n1 * a^n2 / (pi^3 * nf)
end function
!!--- Generate n1,n2,f for a given angular moment (up to h), the data only correspond to (Lval,0,0)=(0,Lval,0)=(0,0,Lval) case
!n1=3+4*(l+m+n)
!n2=3+2*(l+m+n)
!nf=[(2l-1)!!(2m-1)!!(2n-1)!!]^2  <--- e.g. f shell, putting XXXX, i.e. (4,0,0) into it returns 11025. Here 7!! means 7*5*3*1
subroutine genn1n2nf(Lval,n1,n2,nf)
integer Lval,n1,n2,nf
if (Lval==0) then
	n1=3
	n2=3
	nf=1
else if (Lval==1) then
	n1=7
	n2=5
	nf=1
else if (Lval==2) then
	n1=11
	n2=7
	nf=9
else if (Lval==3) then
	n1=15
	n2=9
	nf=225
else if (Lval==4) then
	n1=19
	n2=11
	nf=11025
else if (Lval==5) then
	n1=23
	n2=13
	nf=893025
end if
end subroutine



!!----- Use Lowdin orthogonalization method to transform density matrix and coefficient matrix &
!!----- to orthonormal basis, meanwhile update Sbas to identity matrix
!See Szabo p143 for details
subroutine symmortho
use defvar
use util
implicit real*8 (a-h,o-z)
real*8 Umat(nbasis,nbasis),svalvec(nbasis),Xmat(nbasis,nbasis) !workvec(3*nbasis-1)
! call DSYEV('V','U',nbasis,sbas,nbasis,svalvec,workvec,3*nbasis-1,ierror) !lapack, the resultant sbas is eigenvector matrix
call diagsymat(Sbas,Umat,svalvec,ierror)
if (ierror/=0) write(*,*) "Error: Diagonalization of overlap matrix failed!"
!Now Sbas is already diagonalized
forall (i=1:nbasis) Sbas(i,i)=dsqrt(svalvec(i)) !Use Sbas as temporary matrix here
Xmat=matmul(matmul(Umat,Sbas),transpose(Umat)) !Then Xmat is S^0.5
Ptot=matmul(matmul(Xmat,Ptot),Xmat)
if (allocated(Palpha)) then
	Palpha=matmul(matmul(Xmat,Palpha),Xmat)
	Pbeta=Ptot-Palpha
end if
Cobasa=matmul(Xmat,Cobasa)
if (allocated(Cobasb)) Cobasb=matmul(Xmat,Cobasb)
forall(i=1:nbasis) Sbas(i,i)=1D0 !Reconstruct overlap matrix in orthonormal basis function
end subroutine

!!--- Input overlap matrix and return Lowdin orthogonalization transformation matrix Xmat=S^0.5 and Xmatinv=S^-0.5
! Smatin is input overlap matrix, which will not be modified
subroutine symmorthomat(Smatin,Xmat,Xmatinv)
use defvar
use util
real*8 Umat(nbasis,nbasis),svalvec(nbasis),Smatin(nbasis,nbasis),Smat(nbasis,nbasis),Xmat(nbasis,nbasis),Xmatinv(nbasis,nbasis)
Smat=Smatin
call diagsymat(Smat,Umat,svalvec,ierror)
if (ierror/=0) write(*,*) "Error: Diagonalization of overlap matrix is fail!"
forall (i=1:nbasis) Smat(i,i)=dsqrt(svalvec(i))
Xmat=matmul(matmul(Umat,Smat),transpose(Umat))
forall (i=1:nbasis) Smat(i,i)=1D0/Smat(i,i)
Xmatinv=matmul(matmul(Umat,Smat),transpose(Umat))
end subroutine


!!!------------------------- Generate distance matrix
subroutine gendistmat
use defvar
implicit real*8 (a-h,o-z)
if (allocated(distmat)) deallocate(distmat)
allocate(distmat(ncenter,ncenter))
distmat=0.0D0
!$OMP PARALLEL DO SHARED(distmat) PRIVATE(i,j,tmp) schedule(dynamic) NUM_THREADS(nthreads)
do i=1,ncenter
	do j=i+1,ncenter
        tmp=dsqrt((a(i)%x-a(j)%x)**2+(a(i)%y-a(j)%y)**2+(a(i)%z-a(j)%z)**2)
		distmat(i,j)=tmp
        distmat(j,i)=tmp
	end do
end do
!$OMP END PARALLEL DO
end subroutine


!!!------------ Return the number of inner-core orbitals
subroutine getninnerele(ninnerele,info)
use defvar
integer ninnerele,info
ninnerele=0
do i=1,ncenter
	if (int(a(i)%charge)/=a(i)%index) then
		if (info==1) write(*,"(' Note: Atom',i5,' is not taken into account since it utilizes pseudopotential')") i
		cycle
	end if
	if (a(i)%index>2.and.a(i)%index<=10) ninnerele=ninnerele+2
	if (a(i)%index>10.and.a(i)%index<=18) ninnerele=ninnerele+10
	if (a(i)%index>18.and.a(i)%index<=36) ninnerele=ninnerele+18
	if (a(i)%index>36.and.a(i)%index<=54) ninnerele=ninnerele+36
	if (a(i)%index>54.and.a(i)%index<=86) ninnerele=ninnerele+54
	if (a(i)%index>86) ninnerele=ninnerele+86
end do
end subroutine


!!!------------------------- Swap two GTF
subroutine swapGTF(i,j,swaptype)
use defvar
integer n,i,j
character*3 swaptype
type(primtype) tempb !For exchanging basis functions' order
if (swaptype=="all") then
	tempb=b(i)
	b(i)=b(j)
	b(j)=tempb
else if (swaptype=="cen") then
	tempb%center=b(i)%center
	b(i)%center=b(j)%center
	b(j)%center=tempb%center
else if (swaptype=="typ") then
	tempb%type=b(i)%type
	b(i)%type=b(j)%type
	b(j)%type=tempb%type
else if (swaptype=="exp") then
	tempb%exp=b(i)%exp
	b(i)%exp=b(j)%exp
	b(j)%exp=tempb%exp
end if
if (swaptype=="all".or.swaptype=="MO ") then
	do n=1,nmo
		temp=co(n,i)
		co(n,i)=co(n,j)
		co(n,j)=temp
	end do
end if
end subroutine


!!!---- Rotate(exchange) GTF and basis function coefficients within all shell in different direction of specific orbital
! use this three times, namely XYZ->ZXY->YZX->XYZ, the coefficient recovered.
! In detail, for examples, for d-type will lead to such coefficient exchange: XX to YY, YY to ZZ, ZZ to XX, XY to YZ, XZ to XY, YZ to XZ
! exchange only involve the GTFs/basis func. in the same shell
subroutine orbcoeffrotate(orb) !orb=Rotate which orbital
use defvar
implicit real*8 (a-h,o-z)
integer orb
real*8 COorborg(nprims) !For backing up origin CO
real*8 CObasa_tmp(nbasis) !For backing up origin CObasa
real*8 CObasb_tmp(nbasis) !For backing up origin CObasb
COorborg(:)=CO(orb,:)
do i=1,nprims
	ixtmp=type2iz(b(i)%type)
	iytmp=type2ix(b(i)%type)
	iztmp=type2iy(b(i)%type)
	do j=1,nprims
		if (type2ix(b(j)%type)==ixtmp.and.type2iy(b(j)%type)==iytmp.and.&
		type2iz(b(j)%type)==iztmp.and.b(j)%exp==b(i)%exp.and.b(j)%center==b(i)%center) CO(orb,j)=COorborg(i)
	end do
end do
if (allocated(CObasa)) then
	CObasa_tmp=CObasa(:,orb)
	if (wfntype==1.or.wfntype==4) CObasb_tmp=CObasb(:,orb)
	do iatm=1,ncenter
		do ibas=basstart(iatm),basend(iatm)
			ityp=bastype(ibas)
			ixtmp=type2iz(ityp)
			iytmp=type2ix(ityp)
			iztmp=type2iy(ityp)
			do jbas=basstart(iatm),basend(iatm)
				jtyp=bastype(jbas)
				if (type2ix(jtyp)==ixtmp.and.type2iy(jtyp)==iytmp.and.type2iz(jtyp)==iztmp.and.&
				basshell(ibas)==basshell(jbas)) then
					CObasa(jbas,orb)=CObasa_tmp(ibas)
					if (wfntype==1.or.wfntype==4) CObasb(jbas,orb)=CObasb_tmp(ibas)
				end if
			end do
		end do
	end do
end if
end subroutine



!!!------ Define property/origin/spacing/grid number and then save to a 3D matrix, infomode=1 means silent
!! iorb is used to choose the orbital for whose wavefunction will be calculated. This can be an arbitrary value if functype/=4
subroutine savecubmat(functype,infomode,iorb)
use defvar
use util
use function
implicit real*8 (a-h,o-z)
integer :: infomode,functype,calcfunc,iorb !Calculate which orbital wavefunction for fmo routine
real*8 xarr(nx),yarr(ny),zarr(nz)
character c80tmp*80,c200tmp*200,c400tmp*400,filename_tmp*200
!---------- Special case, use cubegen to directly evaluate ESP grid data
alive=.false.
if (cubegenpath/=" ".and.ifiletype==1.and.functype==12) then
	inquire(file=cubegenpath,exist=alive)
	if (alive==.false.) then
		write(*,"(a)") " Note: Albeit current file type is fch/fchk/chk and ""cubegenpath"" parameter in settings.ini has been defined, &
		the cubegen cannot be found, therefore electrostatic potential will still be calculated using internal code of Multiwfn"
	end if
end if
if (alive.and.ifiletype==1.and.functype==12) then !Use cubegen to calculate ESP
	call walltime(iwalltime1)
	write(*,"(a)") " Since the input file type is fch/fchk/chk and ""cubegenpath"" parameter in settings.ini has been properly defined, &
	now Multiwfn directly invokes cubegen to calculate electrostatic potential"
	
	!Generate cubegen input file
	open(10,file="ESPgridtmp.cub",status="replace")
	write(10,"(' Generated by Multiwfn')")
	write(10,"(' Totally ',i12,' grid points')") nx*ny*nz
	write(10,"(i5,3f12.6)") ncenter,orgx,orgy,orgz
	write(10,"(i5,3f12.6)") nx,dx,0.0,0.0
	write(10,"(i5,3f12.6)") ny,0.0,dy,0.0
	write(10,"(i5,3f12.6)") nz,0.0,0.0,dz
	close(10)
	ncubegenthreads=1 !Parallel implementation prior to G16 is buggy, so test here
	if (index(cubegenpath,"G16")/=0.or.index(cubegenpath,"g16")/=0) ncubegenthreads=nthreads
	filename_tmp=filename
	if (index(filename,".chk")/=0) call chk2fch(filename_tmp)
	write(c400tmp,"(a,i5,a)") trim(cubegenpath),ncubegenthreads," potential="//trim(cubegendenstype)//" "//&
	""""//trim(filename_tmp)//""""//" ESPresult.cub -1 h ESPgridtmp.cub > nouseout"
	write(*,"(a)") " Running: "//trim(c400tmp)
	call system(c400tmp)
	if (index(filename,".chk")/=0) call delfile(filename_tmp)
	!Load ESP data from cubegen resulting file
	call readcube("ESPresult.cub",1,1)
	!Delete intermediate files
	if (isys==1) then
		call system("del cubegenpt.txt ESPresult.cub ESPgridtmp.cub nouseout /Q")
	else
		call system("rm cubegenpt.txt ESPresult.cub ESPgridtmp.cub nouseout -f")
	end if
	call walltime(iwalltime2)
	if (infomode==0) write(*,"(' Calculation of grid data took up wall clock time',i10,'s')") iwalltime2-iwalltime1
	return
end if

!---------------- Below are normal case, use Multiwfn internal code
iorbsel=iorb
calcfunc=functype
if (functype==12) calcfunc=8 !If calculate total ESP, first calculate nuclear ESP, and finally call espcub to evaluate electronic ESP
if (functype==100.and.iuserfunc==14) calcfunc=0 !Calculate electronic ESP, use this setting to skip grid calculation, and finally call espcub to evaluate electronic ESP
if (infomode==0.and.functype/=12) then
	if (expcutoff<0) write(*,"(' Note: All exponential functions exp(x) with x<',f8.3,' will be ignored ')") expcutoff
end if

ii=10
ifinish=0
!write and then read is used to cut the minimal noise at the end of the coordinate, otherwise the originally symmetry points may become unsymmetry
do k=1,nz
	write(c80tmp,"(D20.13)") orgz+(k-1)*dz
	read(c80tmp,*) zarr(k)
end do
do j=1,ny
	write(c80tmp,"(D20.13)") orgy+(j-1)*dy
	read(c80tmp,*) yarr(j)
end do
do i=1,nx
	write(c80tmp,"(D20.13)") orgx+(i-1)*dx
	read(c80tmp,*) xarr(i)
end do

call walltime(iwalltime1)
!$OMP PARALLEL DO SHARED(cubmat,ifinish) PRIVATE(i,j,k,tmpx,tmpy,tmpz,tmprho) schedule(dynamic) NUM_THREADS(nthreads)
do k=1,nz
	tmpz=zarr(k)
	do j=1,ny
		tmpy=yarr(j)
		do i=1,nx
			tmpx=xarr(i)
			if (calcfunc==1513) then !Only involved by funcvsfunc routine, when RDG and signlambda2rho is combined
				call signlambda2rho_RDG(tmpx,tmpy,tmpz,cubmat(i,j,k),cubmattmp(i,j,k),tmprho)
			else if (calcfunc==1614) then !Only involved by funcvsfunc routine, when RDG and signlambda2rho is combined
				call signlambda2rho_RDG_prodens(tmpx,tmpy,tmpz,cubmat(i,j,k),cubmattmp(i,j,k))
			else
				cubmat(i,j,k)=calcfuncall(calcfunc,tmpx,tmpy,tmpz)
			end if
		end do
	end do
	if (infomode==0.and.functype/=12.and.calcfunc/=0) then
        ifinish=ifinish+1
        call showprog(ifinish,nz)
	end if
end do
!$OMP END PARALLEL DO
call walltime(iwalltime2)
if (functype==12.or.(functype==100.and.iuserfunc==14)) then
	call espcub !Calculate electronic ESP and accumulate to existing cubmat
else
	if (infomode==0) write(*,"(' Calculation of grid data took up wall clock time',i10,'s')") iwalltime2-iwalltime1
end if
end subroutine





!!!------ Output molecular formula
subroutine showformula
use defvar
implicit real*8 (a-h,o-z)
character*6 tmp
write(*,"(' Formula: ')",advance="no")
do i=0,nelesupp
	n=0
	do iatm=1,ncenter
		if (a(iatm)%index==i) n=n+1
	end do
	write(tmp,"(i6)") n
	if (n/=0) write(*,"(a,a,' ')",advance="no") trim(ind2name(i)),trim(adjustl(tmp))
end do
write(*,*)
end subroutine



!!!----------- Resize number of orbitals of CO, MOene, MOtype, MOocc to "newnmo", also resize number of GTFs of CO to "newnprims"
subroutine resizebynmo(newnmo,newnprims)
use defvar
implicit real*8 (a-h,o-z)
real*8,allocatable :: CO_bk(:,:),MOene_bk(:),MOocc_bk(:)
integer,allocatable :: MOtype_bk(:)
integer newnmo,oldnmo,newnprims,oldnprims
oldnmo=size(CO,1)
oldnprims=size(CO,2)
allocate(CO_bk(oldnmo,oldnprims),MOene_bk(oldnmo),MOocc_bk(oldnmo),MOtype_bk(oldnmo))
CO_bk=CO
MOene_bk=MOene
MOocc_bk=MOocc
MOtype_bk=MOtype
deallocate(CO,MOene,MOocc,MOtype)
allocate(CO(newnmo,newnprims),MOene(newnmo),MOocc(newnmo),MOtype(newnmo))
if (newnmo>=oldnmo) then !Enlarge array size, don't forget to fill the gap afterwards
	if (newnprims>=oldnprims) CO(1:oldnmo,1:oldnprims)=CO_bk(:,:)
	if (newnprims<oldnprims) CO(1:oldnmo,:)=CO_bk(:,1:newnprims)
	MOene(1:oldnmo)=MOene_bk(:)
	MOocc(1:oldnmo)=MOocc_bk(:)
	MOtype(1:oldnmo)=MOtype_bk(:)
else if (newnmo<oldnmo) then !Reduce array size
	if (newnprims>=oldnprims) CO(:,1:oldnprims)=CO_bk(1:newnmo,:)
	if (newnprims<oldnprims) CO(:,:)=CO_bk(1:newnmo,1:newnprims)
	MOene(:)=MOene_bk(1:newnmo)
	MOocc(:)=MOocc_bk(1:newnmo)
	MOtype(:)=MOtype_bk(1:newnmo)
end if
deallocate(CO_bk,MOene_bk,MOocc_bk,MOtype_bk)
end subroutine


!!!------------ Generate gjf of atoms in molecule, and invoke Gaussian to get .wfn, then input them into custom list
subroutine setpromol
use defvar
use util
implicit real*8 (a-h,o-z)
integer :: itype=0
character*2 typename(100),nametmp
character*80 basisset,tmpdir,c80tmp
character*80 outwfnname
logical alivegauout,alivewfntmp,aliveatomwfn
if (isys==1) call cleangauscr !Clean Gaussian scratch files in current folder

!The only difference between c80tmp and tmpdir is that the latter has \ or / separator at the end
if (iwfntmptype==1) then
	if (isys==1) tmpdir="wfntmp\"
	if (isys==2) tmpdir="wfntmp/"
	c80tmp="wfntmp"
	inquire(directory="wfntmp",exist=alivewfntmp)
	if (isys==1.and.alivewfntmp==.true.) then !delete old wfntmp folder
		write(*,*) "Running: rmdir /S /Q wfntmp"
		call system("rmdir /S /Q wfntmp")
	else if (isys==2.and.alivewfntmp==.true.) then
		write(*,*) "Running: rm -rf wfntmp"
		call system("rm -rf wfntmp")
	end if
else if (iwfntmptype==2) then
	do i=1,9999 !Find a proper name of temporary folder
		write(c80tmp,"('wfntmp',i4.4)") i
		inquire(directory=c80tmp,exist=alivewfntmp)
		if (alivewfntmp==.false.) exit
	end do
	if (isys==1) write(tmpdir,"('wfntmp',i4.4,'\')") i
	if (isys==2) write(tmpdir,"('wfntmp',i4.4,'/')") i
end if
write(*,*) "Running: mkdir "//trim(c80tmp) !Build new temporary folder
call system("mkdir "//trim(c80tmp))
inquire(directory="atomwfn",exist=aliveatomwfn)
if (isys==1.and.aliveatomwfn==.true.) then
	write(*,*) "Running: copy atomwfn\*.wfn "//trim(tmpdir)
	call system("copy atomwfn\*.wfn "//trim(tmpdir))
else if (isys==2.and.aliveatomwfn==.true.) then
	write(*,*) "Running: cp atomwfn/*.wfn "//trim(tmpdir)
	call system("cp atomwfn/*.wfn "//trim(tmpdir))
end if

noatmwfn=0 !Check if the atomic wfn file have pre-stored in atomwfn folder, if not, invoke gaussian to calc it
do i=1,nfragatmnum
	if (isys==1) inquire(file="atomwfn\"//a(fragatm(i))%name//".wfn",exist=alive)
	if (isys==2) inquire(file="atomwfn/"//a(fragatm(i))%name//".wfn",exist=alive)
	if (.not.alive) then
		noatmwfn=1
		exit
	end if
end do

if (noatmwfn==0) then
	write(*,"(a)") " All atom .wfn files needed have already presented in ""atomwfn"" folder, we will not calculate them"
else if (noatmwfn==1) then !Some or all atomic wfn don't exist, calc them
	!Select calculation level
	write(*,"(a)") " Note: Some or all atom .wfn files needed are not present in ""atomwfn"" folder, they must be calculated now. See Section 3.7.3 of the manual for detail."
	write(*,"(a)") " Now please input the level for calculating atom wfn files, theoretical method is optional."
	write(*,"(a)") " For example: 6-31G* or B3LYP/def2SVP    You can also add other keywords at the same time, e.g. M062X/6-311G(2d,p) scf=xqc int=ultrafine"
	read(*,"(a)") basisset !Note: 6d 10f is not required for generating wfn files, since the work has been done in L607 internally
	!Check Gaussian path
	inquire(file=gaupath,exist=alive)
	if (.not.alive) then
		write(*,*) "Could not find Gaussian path defined in ""gaupath"" variable in settings.ini"
		if (isys==1) write(*,*) "Input the path of Gaussian executable file, e.g. ""D:\study\g16w\g16.exe"""
		if (isys==2) write(*,*) "Input the path of Gaussian executable file, e.g. ""/sob/g16/g16"""
		do while(.true.)
			read(*,"(a)") gaupath
			inquire(file=gaupath,exist=alive)
			if (alive) exit
			write(*,*) "Could not find Gaussian executable file, input again"
		end do
	end if
end if

!Generate .gjf file for all elements, regardless if their wfn file have already presented, meanwhile count the total number of elements
itype=0
do i=1,nfragatmnum
	inquire(file=trim(tmpdir)//a(fragatm(i))%name//".gjf",exist=alive)
	if (.not.alive) then
		itype=itype+1 !How many different types
		typename(itype)=a(fragatm(i))%name
				
		if (a_org(fragatm(i))%index>36) then
			inquire(file=trim(tmpdir)//a(fragatm(i))%name//".wfn",exist=alive)
			if (.not.alive) then !The wfn file of the heavy element hasn't been provided in "atomwfn" and hence cannot be found in "wfntmp" here
				write(*,"(a,a,a)") " Error: Multiwfn cannot invoke Gaussian to generate wavefunction file and sphericalize density for ",a(fragatm(i))%name,", since its &
				index is larger than 36! You should provide corresponding atom .wfn files in ""atomwfn"" folder manually"
				write(*,*) "Press ENTER button to continue"
				read(*,*)
				return
			end if
		end if
		
		open(14,file=trim(tmpdir)//a(fragatm(i))%name//".gjf",status="replace")
		!If user inputted including "/" e.g. B3LYP/6-31g*, will replace default theoretical method
		if (index(basisset,'/')==0) then
			if (a(fragatm(i))%index<=20.or.a(fragatm(i))%index>=31) then
				write(14,"(a,/)") "#T out=wfn ROHF/"//trim(basisset) !Main group elements. If not use scf=sp, in g09, RO calculations for IIIA elements are to converge
				write(14,"(a,/)") "Temporary file for promolecule, ROHF"//trim(basisset)
			else
				write(14,"(a,/)") "#T out=wfn UB3LYP/"//trim(basisset) !Transition metals
				write(14,"(a,/)") "Temporary file for promolecule, UB3LYP"//trim(basisset)
			end if
		else
			if (a(fragatm(i))%index<=20.or.a(fragatm(i))%index>=31) then
				write(14,"(a,/)") "#T out=wfn RO"//trim(basisset) !Main group elements
				write(14,"(a,/)") "Temporary file for promolecule, RO"//trim(basisset)
			else
				write(14,"(a,/)") "#T out=wfn U"//trim(basisset) !Transition metals (RO may leads to convergence problem)
				write(14,"(a,/)") "Temporary file for promolecule, U"//trim(basisset)
			end if
		end if

		!Currently support up to the fourth row
		if (a(fragatm(i))%name=="H ".or.a(fragatm(i))%name=="Li".or.a(fragatm(i))%name=="Na".or.a(fragatm(i))%name=="K") write(14,*) "0 2"
		if (a(fragatm(i))%name=="Be".or.a(fragatm(i))%name=="Mg".or.a(fragatm(i))%name=="Ca") write(14,*) "0 1"
		if (a(fragatm(i))%name=="B ".or.a(fragatm(i))%name=="Al".or.a(fragatm(i))%name=="Ga") write(14,*) "0 2"
		if (a(fragatm(i))%name=="C ".or.a(fragatm(i))%name=="Si".or.a(fragatm(i))%name=="Ge") then
			if (SpherIVgroup==0) write(14,*) "0 5"
			if (SpherIVgroup==1) write(14,*) "0 3"
		end if
		if (a(fragatm(i))%name=="N ".or.a(fragatm(i))%name=="P ".or.a(fragatm(i))%name=="As") write(14,*) "0 4"
		if (a(fragatm(i))%name=="O ".or.a(fragatm(i))%name=="S ".or.a(fragatm(i))%name=="Se") write(14,*) "0 3"
		if (a(fragatm(i))%name=="F ".or.a(fragatm(i))%name=="Cl".or.a(fragatm(i))%name=="Br") write(14,*) "0 2"
		if (a(fragatm(i))%name=="He".or.a(fragatm(i))%name=="Ne".or.a(fragatm(i))%name=="Ar".or.a(fragatm(i))%name=="Kr") write(14,*) "0 1"
		if (a(fragatm(i))%name=="Sc") write(14,*) "0 2" !3d1 4s2
		if (a(fragatm(i))%name=="Ti") write(14,*) "0 3" !3d2 4s2
		if (a(fragatm(i))%name=="V ") write(14,*) "0 4" !3d3 4s2
		if (a(fragatm(i))%name=="Cr") write(14,*) "0 7" !3d5 4s1, needn't sphericalization
		if (a(fragatm(i))%name=="Mn") write(14,*) "0 6" !3d5 4s2, needn't sphericalization
		if (a(fragatm(i))%name=="Fe") write(14,*) "0 5" !3d6 4s2
		if (a(fragatm(i))%name=="Co") write(14,*) "0 4" !3d7 4s2
		if (a(fragatm(i))%name=="Ni") write(14,*) "0 3" !3d8 4s2
		if (a(fragatm(i))%name=="Cu") write(14,*) "0 2" !3d10 4s1, needn't sphericalization
		if (a(fragatm(i))%name=="Zn") write(14,*) "0 1" !3d10 4s2, needn't sphericalization
		write(14,*) a(fragatm(i))%name,0.0,0.0,0.0
		write(14,*)
		write(14,*) trim(tmpdir)//a(fragatm(i))%name//".wfn" !The output path of wfn file
		write(14,*)
		write(14,*)
		close(14)
	end if
end do

if (noatmwfn==0) then
	if (isys==1) call system("del "//trim(tmpdir)//"*.gjf /Q") !The .gjf generated have valueless now, delete them for avoiding user's misunderstanding
	if (isys==2) call system("rm "//trim(tmpdir)//"*.gjf -f")
else if (noatmwfn==1) then !Some wfn needs to be genereated by Gaussian and sphericalized here
	do i=1,nfragatmnum
		nametmp=a_org(fragatm(i))%name
		inquire(file=trim(tmpdir)//nametmp//".wfn",exist=alive)
		if (alive) cycle !If the .wfn file had copied from atomwfn folder, needn't recalculate

		write(*,*) "Running:"
		write(*,*) trim(gaupath)//' "'//trim(tmpdir)//nametmp//'.gjf" "'//trim(tmpdir)//nametmp//'"'
		call system(trim(gaupath)//' "'//trim(tmpdir)//nametmp//'.gjf" "'//trim(tmpdir)//nametmp//'"')
		!Check if Gaussian task was successfully finished
		if (isys==1) inquire(file=trim(tmpdir)//trim(nametmp)//".out",exist=alivegauout)
		if (isys==2) inquire(file=trim(tmpdir)//trim(nametmp)//".log",exist=alivegauout)
		if (alivegauout) then
			if (isys==1) open(10,file=trim(tmpdir)//trim(nametmp)//".out",status="old")
			if (isys==2) open(10,file=trim(tmpdir)//trim(nametmp)//".log",status="old")
			call loclabel(10,"Normal termination",igaunormal)
			close(10)
			if (igaunormal==0) then
				write(*,"(a)") " Gaussian running may be failed! Please manually check Gaussian input and output files in wfntmp folder. Press ENTER button to continue"
				read(*,*)
			else if (igaunormal==1) then
				write(*,*) "Finished successfully!"
			end if
		else
			write(*,"(a)") " Gaussian running may be failed! Please manually check Gaussian input and output files in wfntmp folder"
			read(*,*)
		end if
	
		!Load and sphericalize electron density for the just generated wfn, and then save
		if (ispheratm==1.and.igaunormal==1) then
			call dealloall
			call readwfn(trim(tmpdir)//nametmp//".wfn",1)
			!Main group, restrict open-shell
			if (nametmp=="H ".or.nametmp=="Li".or.nametmp=="Na".or.nametmp=="K") MOocc(nmo)=1.0D0
			if (nametmp=="B ".or.nametmp=="Al".or.nametmp=="Ga") then
				nmo=nmo+2
				call resizebynmo(nmo,nprims) !Enlarge nmo by 2, but don't interfere nprims
				MOene(nmo-1:nmo)=MOene(nmo-2)
				MOtype(nmo-2:nmo)=1 !actually no use, because we only use atomic wfn. files to get total density
				MOocc(nmo-2:nmo)=1D0/3D0
				call orbcoeffrotate(nmo-2) !XYZ->ZXY, note: nmo-2 is original single occupied orbital
				CO(nmo-1,:)=CO(nmo-2,:)
				call orbcoeffrotate(nmo-2) !ZXY->YZX
				CO(nmo,:)=CO(nmo-2,:)
				call orbcoeffrotate(nmo-2) !YZX->XYZ, namely recovered
				!Now nmo-2,nmo-1,nmo correspond XYZ,ZXY,YZX
			end if
			if (nametmp=="C ".or.nametmp=="Si".or.nametmp=="Ge") then
				if (SpherIVgroup==0) then
					MOocc(nmo-3:nmo)=1.0D0
				else if (SpherIVgroup==1) then
					nmo=nmo+1
					call resizebynmo(nmo,nprims)
					MOene(nmo)=MOene(nmo-2) !MOene(nmo-1) is degenerate to MOene(nmo-2)
					MOtype(nmo-2:nmo)=1
					MOocc(nmo-2:nmo)=2D0/3D0
					!Rotate and copy the first occupied p orbital (nmo-5)
					call orbcoeffrotate(nmo-2) !XYZ->ZXY
					CO(nmo-1,:)=CO(nmo-2,:) !Overlap the already occupied orbital
					call orbcoeffrotate(nmo-2) !ZXY->YZX
					CO(nmo,:)=CO(nmo-2,:)
					call orbcoeffrotate(nmo-2) !YZX->XYZ, namely recovered
				end if
			end if
			if (nametmp=="N ".or.nametmp=="P ".or.nametmp=="As") MOocc(nmo-2:nmo)=1.0D0
			if (nametmp=="O ".or.nametmp=="S ".or.nametmp=="Se") MOocc(nmo-2:nmo)=4D0/3D0
			if (nametmp=="F ".or.nametmp=="Cl".or.nametmp=="Br") MOocc(nmo-2:nmo)=5D0/3D0
			!Transition metals, unrestrict open-shell, find boundary of alpha and beta first
			do ibound=2,nmo
				if (MOene(ibound)<MOene(ibound-1)) exit !from ii is beta orbitals
			end do
			!For Sc, Ti and V, rotate and duplicate d orbitals in each diection to get *near* spherical density, as for III main group
			!Note: Don't use Hartree-Fock, because correct energy sequence couldn't be reproduced, so can't be sphericalized correctly!
			if (nametmp=="Sc".or.nametmp=="Ti".or.nametmp=="V ") then !3d1 4s2, 3d2 4s2, 3d3 4s2
				if (nametmp=="Sc") then
					ibeg=1 !alpha 4s orbital, because this s orbital shows very strong unequlitity
					iend=2
				else if (nametmp=="Ti") then
					ibeg=2
					iend=3
				else if (nametmp=="V") then
					ibeg=2
					iend=4
				end if
				ienlarge=(iend-ibeg+1)*2
				call resizebynmo(nmo+ienlarge,nprims)
				ipass=0
				do iavgorb=ibeg,iend
					call orbcoeffrotate(ibound-iavgorb) !rotate this orbital
					CO(nmo+1+ipass,:)=CO(ibound-iavgorb,:) !Duplicate this orbital
					call orbcoeffrotate(ibound-iavgorb)
					CO(nmo+2+ipass,:)=CO(ibound-iavgorb,:)
					call orbcoeffrotate(ibound-iavgorb) !recover
					MOocc(ibound-iavgorb)=1D0/3D0
					MOene(nmo+1+ipass:nmo+2+ipass)=MOene(ibound-iavgorb)
					ipass=ipass+2 !next time skip nmo+1 and nmo+2
				end do
				MOocc(nmo+1:nmo+ienlarge)=1D0/3D0
				MOtype(nmo+1:nmo+ienlarge)=1
				nmo=nmo+ienlarge
			else if (nametmp=="Fe") then !3d6 4s2
				MOocc(nmo-1)=0D0 !delete the only d-beta orbital, the "nmo"th orbital is 4s-beta
				MOocc(ibound-6:ibound-2)=1.2D0 !Scatter one electron in beta-d orbital to alpha orbitals evenly. MOocc(ibound) is 4s orbital
			else if (nametmp=="Co") then !3d7 4s2
				MOocc(nmo-2:nmo-1)=0D0
				MOocc(ibound-6:ibound-2)=1.4D0
			else if (nametmp=="Ni") then !3d8 4s2
				MOocc(nmo-3:nmo-1)=0D0
				MOocc(ibound-6:ibound-2)=1.6D0
			end if
			call outwfn(trim(tmpdir)//nametmp//".wfn",0,0,10)
		end if
	end do
end if
write(*,*)

!Setup custom operation list
ncustommap=nfragatmnum
if (allocated(custommapname)) deallocate(custommapname)
if (allocated(customop)) deallocate(customop)
allocate(custommapname(ncustommap))
allocate(customop(ncustommap))

!Generate atomic wfn file from element wfn file, meanwhile take them into custom operation list
do i=1,itype !Scan each atomtype in current system
	call dealloall
	call readwfn(trim(tmpdir)//typename(i)//".wfn",1)
	do j=1,nfragatmnum
		if (a_org(fragatm(j))%name==typename(i)) then !Find atoms attributed to current element
			a(1)%x=a_org(fragatm(j))%x !Modify the atomic .wfn, then output to new .wfn
			a(1)%y=a_org(fragatm(j))%y
			a(1)%z=a_org(fragatm(j))%z
			write(outwfnname,"(a2,i4,a4)") typename(i),fragatm(j),".wfn"
			call outwfn(trim(tmpdir)//outwfnname,0,0,10)
			custommapname(j)=trim(tmpdir)//outwfnname !Sequence is identical to atom in fragment
		end if
	end do
end do

call dealloall
call readinfile(firstfilename,1)
end subroutine



!!!------------------ Generate density matrix, can be used when basis function information is available
subroutine genP
use defvar
implicit real*8 (a-h,o-z)
if (allocated(Ptot)) deallocate(Ptot)
if (allocated(Palpha)) deallocate(Palpha)
if (allocated(Pbeta)) deallocate(Pbeta)
allocate(Ptot(nbasis,nbasis))
Ptot=0
if (wfntype==1.or.wfntype==2.or.wfntype==4) then !open-shell
	allocate(Palpha(nbasis,nbasis))
	allocate(Pbeta(nbasis,nbasis))
	Palpha=0D0
	Pbeta=0D0
end if

!For SCF wavefunction, if the wavefunction has not been modified (imodwfn==0), use fast way to construct it
!However, if the wavefunction has been modified, the case may be complicated, for example, there is a hole orbital. In these cases
!We use general way (as used for post-HF) to construct density matrix

if (wfntype==0.and.imodwfn==0) then !RHF
	Ptot=2*matmul(CObasa(:,1:nint(naelec)),transpose(CObasa(:,1:nint(naelec))))
else if (wfntype==1.and.imodwfn==0) then !UHF
	Palpha=matmul(CObasa(:,1:nint(naelec)),transpose(CObasa(:,1:nint(naelec))))
	Pbeta=matmul(CObasb(:,1:nint(nbelec)),transpose(CObasb(:,1:nint(nbelec))))
	Ptot=Palpha+Pbeta
else if (wfntype==2.and.imodwfn==0) then !ROHF
	Palpha=matmul(CObasa(:,1:nint(naelec)),transpose(CObasa(:,1:nint(naelec))))
	Pbeta=matmul(CObasa(:,1:nint(nbelec)),transpose(CObasa(:,1:nint(nbelec))))
	Ptot=Palpha+Pbeta
else if (wfntype==3.or.((wfntype==0.or.wfntype==2).and.imodwfn==1)) then !Restricted post-HF
	do imo=1,nmo
		if (MOocc(imo)==0D0) cycle
		Ptot=Ptot+MOocc(imo)*matmul(CObasa(:,imo:imo),transpose(CObasa(:,imo:imo)))
	end do
else if (wfntype==4.or.(wfntype==1.and.imodwfn==1)) then !Unrestricted post-HF
	do imo=1,nbasis
		if (MOocc(imo)==0D0) cycle
		Palpha=Palpha+MOocc(imo)*matmul(CObasa(:,imo:imo),transpose(CObasa(:,imo:imo)))
	end do
	do imo=1,nbasis
		if (MOocc(imo+nbasis)==0D0) cycle
		Pbeta=Pbeta+MOocc(imo+nbasis)*matmul(CObasb(:,imo:imo),transpose(CObasb(:,imo:imo)))
	end do
	Ptot=Palpha+Pbeta
end if

end subroutine




!!!------ Show system one-electron properties based on density matrix and integral matrix between basis functions
!The results are correct only when Cartesian basis functions are used
subroutine sys1eprop
use defvar
if (.not.allocated(Sbas)) allocate(Sbas(nbasis,nbasis))
call genSbas
write(*,"(' Total number of electrons:',f16.8)") sum(Ptot*Sbas)
if (.not.allocated(Tbas)) allocate(Tbas(nbasis,nbasis))
call genTbas
write(*,"(' Kinetic energy:',f18.9,' a.u.')") sum(Ptot*Tbas)
if (.not.allocated(Dbas)) allocate(Dbas(3,nbasis,nbasis))
call genDbas
write(*,"(' Electric dipole moment in X/Y/Z:',3f13.7,' a.u.')") sum(Ptot*Dbas(1,:,:)),sum(Ptot*Dbas(2,:,:)),sum(Ptot*Dbas(3,:,:))
if (.not.allocated(Magbas)) allocate(Magbas(3,nbasis,nbasis))
call genMagbas
write(*,"(' Magnetic dipole moment in X/Y/Z:',3f13.7,' a.u.')") sum(Ptot*Magbas(1,:,:)),sum(Ptot*Magbas(2,:,:)),sum(Ptot*Magbas(3,:,:))
if (.not.allocated(Velbas)) allocate(Velbas(3,nbasis,nbasis))
call genVelbas
write(*,"(' Linear momentum in X/Y/Z:       ',3f13.7,' a.u.')") sum(Ptot*Velbas(1,:,:)),sum(Ptot*Velbas(2,:,:)),sum(Ptot*Velbas(3,:,:))
end subroutine



!!!------------- Show all properties at a point
!ifuncsel: controls the gradient and Hessian for which function
!ifileid: Output to which file destination, of course 6=screen
subroutine showptprop(inx,iny,inz,ifuncsel,ifileid)
use util
use defvar
use function
implicit real*8(a-h,o-z)
real*8 inx,iny,inz
real*8 elehess(3,3),eigvecmat(3,3),eigval(3),elegrad(3),funchess(3,3),funcgrad(3),tmparr(3,1)
integer ifuncsel,ifileid
if (allocated(b)) then !If loaded file contains wavefuntion information
	call gencalchessmat(2,1,inx,iny,inz,elerho,elegrad,elehess) !Generate electron density, gradient and hessian
	write(ifileid,"(' Density of all electrons:',E18.10)") elerho
	if (ipolarpara==0) then
		tmpval=fspindens(inx,iny,inz,'s')
		write(ifileid,"(' Density of Alpha electrons:',E18.10)") (elerho+tmpval)/2D0
		write(ifileid,"(' Density of Beta electrons:',E18.10)") (elerho-tmpval)/2D0
		write(ifileid,"(' Spin density of electrons:',E18.10)") tmpval
	else if (ipolarpara==1) then
		write(ifileid,"(' Spin polarization parameter function:',E18.10)") fspindens(inx,iny,inz,'s')
	end if
	valG=lagkin(inx,iny,inz,0)
	valGx=lagkin(inx,iny,inz,1)
	valGy=lagkin(inx,iny,inz,2)
	valGz=lagkin(inx,iny,inz,3)
	write(ifileid,"(' Lagrangian kinetic energy G(r):',E18.10)") valG
	write(ifileid,"(' G(r) in X,Y,Z:',3E18.10)") valGx,valGy,valGz
	valK=Hamkin(inx,iny,inz,0)
! 	valKx=Hamkin(inx,iny,inz,1)
! 	valKy=Hamkin(inx,iny,inz,2)
! 	valKz=Hamkin(inx,iny,inz,3)
	write(ifileid,"(' Hamiltonian kinetic energy K(r):',E18.10)") valK
! 	write(ifileid,"(' K(r) in X,Y,Z:',3E18.10)") valKx,valKy,valKz
	write(ifileid,"(' Potential energy density V(r):',E18.10)") -valK-valG !When without EDF, also equals to flapl(inx,iny,inz,'t')/4.0D0-2*valG
	write(ifileid,"(' Energy density E(r) or H(r):',E18.10)") -valK
	write(ifileid,"(' Laplacian of electron density:',E18.10)") laplfac*(elehess(1,1)+elehess(2,2)+elehess(3,3))
	write(ifileid,"(' Electron localization function (ELF):',E18.10)") ELF_LOL(inx,iny,inz,"ELF")
	write(ifileid,"(' Localized orbital locator (LOL):',E18.10)") ELF_LOL(inx,iny,inz,"LOL")
	write(ifileid,"(' Local information entropy:',E18.10)") infoentro(1,inx,iny,inz)
	write(ifileid,"(' Reduced density gradient (RDG):',E18.10)") fgrad(inx,iny,inz,'r')
	write(ifileid,"(' Reduced density gradient with promolecular approximation:',E18.10)") RDGprodens(inx,iny,inz)
	write(ifileid,"(' Sign(lambda2)*rho:',E18.10)") signlambda2rho(inx,iny,inz)
	write(ifileid,"(' Sign(lambda2)*rho with promolecular approximation:',E18.10)") signlambda2rho_prodens(inx,iny,inz)
	if (pairfunctype==1) write(ifileid,"(a,3f10.5,' :',E18.10)") " Corr. hole for alpha, ref.:",refx,refy,refz,pairfunc(refx,refy,refz,inx,iny,inz)
	if (pairfunctype==2) write(ifileid,"(a,3f10.5,' :',E18.10)") " Corr. hole for beta, ref.:",refx,refy,refz,pairfunc(refx,refy,refz,inx,iny,inz)
	if (pairfunctype==4) write(ifileid,"(a,3f10.5,' :',E18.10)") " Corr. fac. for alpha, ref.:",refx,refy,refz,pairfunc(refx,refy,refz,inx,iny,inz)
	if (pairfunctype==5) write(ifileid,"(a,3f10.5,' :',E18.10)") " Corr. fac. for beta, ref.:",refx,refy,refz,pairfunc(refx,refy,refz,inx,iny,inz)
	if (pairfunctype==7) write(ifileid,"(a,3f10.5,' :',E18.10)") " Exc.-corr. dens. for alpha, ref:",refx,refy,refz,pairfunc(refx,refy,refz,inx,iny,inz)
	if (pairfunctype==8) write(ifileid,"(a,3f10.5,' :',E18.10)") " Exc.-corr. dens. for beta, ref:",refx,refy,refz,pairfunc(refx,refy,refz,inx,iny,inz)
	write(ifileid,"(' Source function, ref.:',3f10.5,' :',E18.10)") refx,refy,refz,srcfunc(inx,iny,inz,srcfuncmode)
	if (nmo/=0) write(ifileid,"(' Wavefunction value for orbital',i10,' :',E18.10)") iorbsel,fmo(inx,iny,inz,iorbsel)
	if (iALIEdecomp==0) then
		write(ifileid,"(' Average local ionization energy (ALIE):',E18.10)") avglocion(inx,iny,inz)
	else if (iALIEdecomp==1) then
		call avglociondecomp(ifileid,inx,iny,inz)
	end if
	write(ifileid,"(' Delta_g:',E18.10)") delta_g_IGM(inx,iny,inz)
	write(ifileid,"(' User-defined real space function:',E18.10)") userfunc(inx,iny,inz)
	fesptmp=nucesp(inx,iny,inz)
	if (ifiletype==4) then
		write(ifileid,"(' ESP from atomic charges:',E18.10)") fesptmp
	else
		write(ifileid,"(' ESP from nuclear charges:',E18.10)") fesptmp
	end if
	if (ishowptESP==1) then
		fesptmpelec=eleesp(inx,iny,inz)
		write(ifileid,"(' ESP from electrons:',E18.10)") fesptmpelec
		write(ifileid,"(' Total ESP:',E18.10,' a.u. (',E14.7,' eV,',E14.7,' kcal/mol)')") &
		fesptmpelec+fesptmp,(fesptmpelec+fesptmp)*au2eV,(fesptmpelec+fesptmp)*au2kcal
		tmpval=totespskip(inx,iny,inz,iskipnuc)
		if (iskipnuc/=0) write(ifileid,"(' Total ESP without contribution from nuclear charge of &
		atom',i6,':',/,E18.10,' a.u. (',E15.7,' eV,',E15.7,' kcal/mol)')") iskipnuc,tmpval,tmpval*au2eV,tmpval*au2kcal
	end if
	write(ifileid,*)
	if (ifuncsel==1) then
		write(ifileid,*) "Note: Below information are for electron density"
		funchess=elehess
		funcgrad=elegrad
	else
		if (ifuncsel==3) write(ifileid,*) "Note: Below information are for Laplacian of electron density"
		if (ifuncsel==4) write(ifileid,*) "Note: Below information are for value of orbital wavefunction"
		if (ifuncsel==9) write(ifileid,*) "Note: Below information are for electron localization function"
		if (ifuncsel==10) write(ifileid,*) "Note: Below information are for localized orbital locator"
		if (ifuncsel==12) write(ifileid,*) "Note: Below information are for total ESP"
		if (ifuncsel==100) write(ifileid,*) "Note: Below information are for user-defined real space function"
		call gencalchessmat(2,ifuncsel,inx,iny,inz,funcvalue,funcgrad,funchess)
	end if
	write(ifileid,*)
	write(ifileid,*) "Components of gradient in x/y/z are:"
	write(ifileid,"(3E18.10)") funcgrad(1),funcgrad(2),funcgrad(3)
	write(ifileid,"(' Norm of gradient is:',E18.10)") dsqrt(sum(funcgrad**2))
	write(ifileid,*)
	write(ifileid,*) "Components of Laplacian in x/y/z are:"
	write(ifileid,"(3E18.10)") funchess(1,1),funchess(2,2),funchess(3,3)
	write(ifileid,"(' Total:',E18.10)") funchess(1,1)+funchess(2,2)+funchess(3,3)
	write(ifileid,*)
	write(ifileid,*) "Hessian matrix:"
	write(ifileid,"(3E18.10)") funchess
	call diagmat(funchess,eigvecmat,eigval,300,1D-12)
	write(ifileid,"(' Eigenvalues of Hessian:',3E18.10)") eigval(1:3)
	write(ifileid,*) "Eigenvectors(columns) of Hessian:"
	write(ifileid,"(3E18.10)") ((eigvecmat(i,j),j=1,3),i=1,3)
	write(ifileid,"(' Determinant of Hessian:',E18.10)") detmat(funchess)
	if (ifuncsel==1) then !Output ellipticity for rho
		call sort(eigval)
		eigmax=eigval(3)
		eigmed=eigval(2)
		eigmin=eigval(1)
		write(ifileid,"(a,f12.6)") " Ellipticity of electron density:",eigmin/eigmed-1
		write(ifileid,"(a,f12.6)") " eta index:",abs(eigmin)/eigmax
	end if

else !Only loaded structure, use YWT promolecule density
	if (ifiletype==4) then
		write(ifileid,"(' ESP from atomic charges:',E18.10)") nucesp(inx,iny,inz)
	else
		write(ifileid,"(' ESP from nuclear charges:',E18.10)") nucesp(inx,iny,inz)
	end if
	write(ifileid,*)
	call calchessmat_prodens(inx,iny,inz,elerho,elegrad,elehess)
	write(ifileid,"(a)") " Note: The loaded file does not contain wavefunction information, below results are evaluated from promolecule density"
	write(ifileid,"(' Density of electrons:',E18.10)") elerho
	write(ifileid,"(' Reduced density gradient:',E18.10)") RDGprodens(inx,iny,inz)
	write(ifileid,"(' Sign(lambda2)*rho:',E18.10)") signlambda2rho_prodens(inx,iny,inz)
	write(ifileid,"(' User-defined real space function:',E18.10)") userfunc(inx,iny,inz)
	write(ifileid,*)
	write(ifileid,*) "Components of gradient in x/y/z are:"
	write(ifileid,"(3E18.10)") elegrad(1),elegrad(2),elegrad(3)
	write(ifileid,"(' Norm of gradient is:',E18.10)") dsqrt(sum(elegrad**2))
	write(ifileid,*)
	write(ifileid,*) "Components of Laplacian in x/y/z are:"
	write(ifileid,"(3E18.10)") elehess(1,1),elehess(2,2),elehess(3,3)
	write(ifileid,"(' Total:',E18.10)") elehess(1,1)+elehess(2,2)+elehess(3,3)
	write(ifileid,*)
	write(ifileid,*) "Hessian matrix:"
	write(ifileid,"(3E18.10)") elehess
	call diagmat(elehess,eigvecmat,eigval,300,1D-12)
	write(ifileid,"(' Eigenvalues of Hessian:',3E18.10)") eigval(1:3)
	write(ifileid,*) "Eigenvectors(columns) of Hessian:"
	write(ifileid,"(3E18.10)") ((eigvecmat(i,j),j=1,3),i=1,3)
end if
end subroutine



!!!------- Decompose property at a point as contribution from various orbitals
subroutine decompptprop(x,y,z)
use defvar
use util
use function
implicit real*8(a-h,o-z)
character c2000tmp*2000
real*8 MOocctmp(nmo)
real*8,allocatable :: valarr(:)
integer,allocatable :: orbidx(:)
write(*,*) "Select the function to be studied"
call funclist
read(*,*) ifunc
write(*,*) "Input range of orbitals, e.g. 3,6-8,12-15"
write(*,"(a)") " Note: If press ENTER button directly, all occupied orbitals will be taken into account, &
and 10 orbitals having largest contributions will be shown"
read(*,"(a)") c2000tmp
if (c2000tmp==" ") then
	norb=count(MOocc(1:nmo)/=0D0)
	allocate(orbidx(norb),valarr(norb))
	idx=0
	do imo=1,nmo
		if (MOocc(imo)/=0) then
			idx=idx+1
			orbidx(idx)=imo
		end if
	end do
else
	call str2arr(c2000tmp,norb)
	allocate(orbidx(norb),valarr(norb))
	call str2arr(c2000tmp,norb,orbidx)
end if

totval=calcfuncall(ifunc,x,y,z)
MOocctmp=MOocc
sumval=0
do itmp=1,norb
	iorb=orbidx(itmp)
	MOocc=0
	MOocc(iorb)=MOocctmp(iorb)
	valarr(itmp)=calcfuncall(ifunc,x,y,z)
	sumval=sumval+valarr(itmp)
end do
MOocc=MOocctmp

call sortr8(valarr,"abs",orbidx)
call invarrr8(valarr,orbidx)

nout=norb
if (c2000tmp==" ".and.norb>10) nout=10
do itmp=1,nout
	iorb=orbidx(itmp)
	if (ifunc==1) then !For electron density, also print percentage contribution
		write(*,"(' Contribution from orbital',i6,' (occ=',f9.6,'):',f14.6,' a.u. (',f6.2,'% )')") iorb,MOocc(iorb),valarr(itmp),valarr(itmp)/totval*100
	else
		write(*,"(' Contribution from orbital',i6,' (occ=',f9.6,'):',1E16.8,' a.u.')") iorb,MOocc(iorb),valarr(itmp)
	end if
end do

if (ifunc==1) then
	write(*,"(' Sum of above values:',f16.8,' a.u. ( ',f6.2,'% )')") sumval,sumval/totval*100
	write(*,"(' Exact value:',f16.8,' a.u.')") totval
else
	write(*,"(' Sum of above values:',1E16.8,' a.u.')") sumval
	write(*,"(' Exact value:',1E16.8,' a.u.')") totval
end if
end subroutine




!!------------------ Set up grid
!If ienableloadextpt==1, then show the option used to load external points, =0 don't
!igridsel is returned variable, corresponding to the selected index; if igridsel==100, that means user didn't set up grid here &
!but choose to load a set of point coordinates from external plain text file
!Usual calling instance: call setgrid(1,inouse)
subroutine setgrid(ienableloadextpt,igridsel)
use defvar
use GUI
implicit real*8 (a-h,o-z)
real*8 molxlen,molylen,molzlen,tmpx,tmpy,tmpz
character*200 cubefilename,pointfilename
character c80*80
integer ienableloadextpt
logical filealive
ntotlow=125000
ntotmed=512000
ntothigh=1728000
do while(.true.)
	write(*,*) "Please select a method to set up grid"
	write(*,"(a,f10.6,a)") " -10 Set extension distance of grid range for mode 1~4, current:",aug3D," Bohr"
	write(*,*) "1 Low quality grid   , covering whole system, about 125000 points in total"
	write(*,*) "2 Medium quality grid, covering whole system, about 512000 points in total"
	write(*,*) "3 High quality grid  , covering whole system, about 1728000 points in total"
	write(*,*) "4 Input the number of points or grid spacing in X,Y,Z, covering whole system"
	write(*,*) "5 Input original point, translation vector and the number of points"
	write(*,*) "6 Input center coordinate, number of points and extension distance"
	write(*,*) "7 The same as 6, but input two atoms, the midpoint will be defined as center"
	write(*,*) "8 Use grid setting of another cube file"
	write(*,*) "10 Set box of grid data visually using a GUI window"
	if (ienableloadextpt==1) write(*,*) "100 Load a set of points from external file"
	read(*,*) igridsel
	if (igridsel/=-10) exit
	write(*,*) "Input extension distance (Bohr) e.g. 6.5"
	read(*,*) aug3D
end do

if (igridsel==100) then !Load points rather than set up grid
	write(*,*) "Input the path of the file containing points, e.g. C:\ltwd.txt"
	write(*,*) "Note: See program manual for the format of the file"
	do while(.true.)
		read(*,"(a)") pointfilename
		inquire(file=pointfilename,exist=filealive)
		if (filealive) then
			open(10,file=pointfilename,status="old")
			read(10,*) numextpt
			write(*,"(a,i10,a)") ' There are',numextpt,' points'
			if (allocated(extpt)) deallocate(extpt)
			allocate(extpt(numextpt,4))
			do itmp=1,numextpt
				read(10,*) extpt(itmp,1:3)
			end do
			close(10)
			exit
		else
			write(*,*) "Error: File cannot be found, input again"
		end if
	end do
	write(*,*) "Please wait..."
else
	molxlen=(maxval(a%x)-minval(a%x))+2*aug3D
	molylen=(maxval(a%y)-minval(a%y))+2*aug3D
	molzlen=(maxval(a%z)-minval(a%z))+2*aug3D
	if (molxlen==0.0D0.or.molylen==0.0D0.or.molzlen==0.0D0) then !Avoid catastrophe when aug3D=0 and system is plane
		write(*,"(a,/)") " WARNING: The box size in one of Caresian axis is zero, &
		the calculation cannot be proceeded. Therefore, the size of corresponding direction is automatically set to 3 Bohr"
		if (molxlen==0D0) then
			molxlen=3D0
		else if (molylen==0D0) then
			molylen=3D0
		else if (molzlen==0D0) then
			molzlen=3D0
		end if
	end if
	if (igridsel==1.or.igridsel==2.or.igridsel==3) then
		if (igridsel==1) dx=(molxlen*molylen*molzlen/dfloat(ntotlow))**(1.0D0/3.0D0)
		if (igridsel==2) dx=(molxlen*molylen*molzlen/dfloat(ntotmed))**(1.0D0/3.0D0)
		if (igridsel==3) dx=(molxlen*molylen*molzlen/dfloat(ntothigh))**(1.0D0/3.0D0)
		dy=dx
		dz=dx
		nx=nint(molxlen/dx)+1
		ny=nint(molylen/dy)+1
		nz=nint(molzlen/dz)+1
		orgx=minval(a%x)-aug3D
		orgy=minval(a%y)-aug3D
		orgz=minval(a%z)-aug3D
	else if (igridsel==4) then
		write(*,*) "Input the number of grid points in X,Y,Z direction, e.g. 139,59,80"
		write(*,"(a)") " or input the grid spacing (bohr) in X,Y,Z direction, e.g. 0.05,0.08,0.08  (if only input one value, it will be used for all directions)"
		read(*,"(a)") c80
		if (index(c80,'.')/=0) then
			if (index(c80,',')/=0) then
				read(c80,*) dx,dy,dz
			else
				read(c80,*) tmp
				dx=tmp
				dy=tmp
				dz=tmp
			end if
			nx=molxlen/dx+1
			ny=molylen/dy+1
			nz=molzlen/dz+1
		else
			read(c80,*) nx,ny,nz
			dx=molxlen/(nx-1)
			dy=molylen/(ny-1)
			dz=molzlen/(nz-1)
		end if
		orgx=minval(a%x)-aug3D
		orgy=minval(a%y)-aug3D
		orgz=minval(a%z)-aug3D
	else if (igridsel==5) then
		write(*,*) "Input X,Y,Z coordinate of original point (Bohr) e.g. 0.1,4,-1"
		read(*,*) orgx,orgy,orgz
		write(*,*) "Input X,Y,Z component of translation vector (Bohr) e.g. 0.1,0.1,0.15"
		read(*,*) dx,dy,dz
		write(*,*) "Input the number of points in X,Y,Z direction e.g. 139,59,80"
		read(*,*) nx,ny,nz
	else if (igridsel==6.or.igridsel==7) then
		if (igridsel==6) then
			write(*,*) "Input X,Y,Z coordinate of center (Angstrom)"
			read(*,*) cenx,ceny,cenz
			cenx=cenx/b2a
			ceny=ceny/b2a
			cenz=cenz/b2a
		else if (igridsel==7) then
			write(*,*) "Input index of the two atoms e.g. 2,5"
			write(*,*) "If the two indices are identical, box center will be placed at the nucleus"
			read(*,*) indatm1,indatm2
			cenx=(a(indatm1)%x+a(indatm2)%x)/2.0D0
			ceny=(a(indatm1)%y+a(indatm2)%y)/2.0D0
			cenz=(a(indatm1)%z+a(indatm2)%z)/2.0D0
		end if
		write(*,*) "Input the number of points in X,Y,Z direction e.g. 40,40,25"
		read(*,*) nx,ny,nz
		write(*,*) "Input the extended distance in X,Y,Z direction (Bohr) e.g. 4.0,4.0,6.5"
		read(*,*) aug3Dx,aug3Dy,aug3Dz
		orgx=cenx-aug3Dx
		orgy=ceny-aug3Dy
		orgz=cenz-aug3Dz
		dx=aug3Dx*2.0D0/(nx-1)
		dy=aug3Dy*2.0D0/(ny-1)
		dz=aug3Dz*2.0D0/(nz-1)
	else if (igridsel==8) then
		write(*,*) "Input path of a cube file, e.g. C:\wake_up_girls.cub"
		do while(.true.)
			read(*,"(a)") cubefilename
			inquire(file=cubefilename,exist=filealive)
			if (filealive) then
				open(10,file=cubefilename,status="old")
				read(10,*)
				read(10,*)
				read(10,*) nouse,orgx,orgy,orgz
				read(10,*) nx,gridvec1
				read(10,*) ny,gridvec2
				read(10,*) nz,gridvec3
				close(10)
                dx=gridvec1(1);dy=gridvec2(2);dz=gridvec3(3)
				exit
			else
				write(*,*) "Error: File cannot be found, input again"
			end if
		end do
	else if (igridsel==10) then
		call setboxGUI
	end if
	endx=orgx+dx*(nx-1) !In fact, when using setboxGUI, the endx/y/z have already been set
	endy=orgy+dy*(ny-1)
	endz=orgz+dz*(nz-1)
	write(*,"(' Coordinate of origin in X,Y,Z is   ',3f12.6,' Bohr')") orgx,orgy,orgz
	write(*,"(' Coordinate of end point in X,Y,Z is',3f12.6,' Bohr')") endx,endy,endz
	write(*,"(' Grid spacing in X,Y,Z is',3f12.6,' Bohr')") dx,dy,dz
	write(*,"(' The number of points in X,Y,Z is',3i5,'   Total:',i12)") nx,ny,nz,nx*ny*nz
    gridvec1=0;gridvec1(1)=dx
    gridvec2=0;gridvec2(2)=dy
    gridvec3=0;gridvec3(3)=dz
end if
end subroutine



!!---- Set up grid setting with fixed grid spacing, similar to setgridforbasin, but not so complicated, thus may be used for other subroutine
subroutine setgridfixspc
use defvar
use GUI
implicit real*8 (a-h,o-z)
real*8 :: molxlen,molylen,molzlen
real*8 :: spclowqual=0.2D0,spcmedqual=0.1D0,spchighqual=0.06D0,spclunaqual=0.04D0
character c80tmp*80,cubefilename*200
do while(.true.)
	orgx=minval(a%x)-aug3D
	orgy=minval(a%y)-aug3D
	orgz=minval(a%z)-aug3D
	endx=maxval(a%x)+aug3D
	endy=maxval(a%y)+aug3D
	endz=maxval(a%z)+aug3D
	molxlen=endx-orgx
	molylen=endy-orgy
	molzlen=endz-orgz
	ntotlow=(nint(molxlen/spclowqual)+1)*(nint(molylen/spclowqual)+1)*(nint(molzlen/spclowqual)+1)
	ntotmed=(nint(molxlen/spcmedqual)+1)*(nint(molylen/spcmedqual)+1)*(nint(molzlen/spcmedqual)+1)
	ntothigh=(nint(molxlen/spchighqual)+1)*(nint(molylen/spchighqual)+1)*(nint(molzlen/spchighqual)+1)
	ntotluna=(nint(molxlen/spclunaqual)+1)*(nint(molylen/spclunaqual)+1)*(nint(molzlen/spclunaqual)+1)
	
	write(*,*) "Please select a method for setting up grid"
	write(*,"(a,f10.5,a)") " -10 Set grid extension distance for mode 1~6, current:",aug3D," Bohr"
	write(*,"(a,f4.2,a,i14)") " 1 Low quality grid, spacing=",spclowqual," Bohr, number of grids:    ",ntotlow
	write(*,"(a,f4.2,a,i14)") " 2 Medium quality grid, spacing=",spcmedqual," Bohr, number of grids: ",ntotmed
	write(*,"(a,f4.2,a,i14)") " 3 High quality grid, spacing=",spchighqual," Bohr, number of grids:   ",ntothigh
	write(*,"(a,f4.2,a,i14)") " 4 Lunatic quality grid, spacing=",spclunaqual," Bohr, number of grids:",ntotluna
	write(*,*) "5 Only input grid spacing, automatically set other parameters"
	write(*,*) "6 Only input the number of points in X,Y,Z, automatically set other parameters"
	write(*,*) "7 Input original point, translation vector and the number of points"
	write(*,*) "8 Set center position, grid spacing and box length"
	write(*,*) "9 Use grid setting of another cube file"
	write(*,*) "10 Set box of grid data visually using a GUI window"
	read(*,*) igridsel
	if (igridsel/=-10) then
		exit
	else
		write(*,*) "Input extension distance (Bohr) e.g. 6.5"
		read(*,*) aug3D
	end if
end do

!Note: orgx,orgy,orgz,endx,endy,endz as well as molx/y/zlen for igridsel==1~6 have already been set above
if (igridsel==1.or.igridsel==2.or.igridsel==3.or.igridsel==4.or.igridsel==5) then
	if (igridsel==1) dx=spclowqual
	if (igridsel==2) dx=spcmedqual
	if (igridsel==3) dx=spchighqual
	if (igridsel==4) dx=spclunaqual
	if (igridsel==5) then
		write(*,*) "Input the grid spacing (bohr)  e.g. 0.08"
		read(*,*) dx
	end if
	dy=dx
	dz=dx
	nx=nint(molxlen/dx)+1
	ny=nint(molylen/dy)+1
	nz=nint(molzlen/dz)+1
else if (igridsel==6) then
	write(*,*) "Input the number of grid points in X,Y,Z direction   e.g. 139,59,80"
	read(*,*) nx,ny,nz
	dx=molxlen/(nx-1)
	dy=molylen/(ny-1)
	dz=molzlen/(nz-1)
else if (igridsel==7) then
	write(*,*) "Input X,Y,Z coordinate of original point (Bohr) e.g. 0.1,4,-1"
	read(*,*) orgx,orgy,orgz
	write(*,*) "Input X,Y,Z component of translation vector (Bohr) e.g. 0.1,0.1,0.15"
	read(*,*) dx,dy,dz
	write(*,*) "Input the number of points in X,Y,Z direction e.g. 139,59,80"
	read(*,*) nx,ny,nz
	endx=orgx+dx*(nx-1)
	endy=orgy+dy*(ny-1)
	endz=orgz+dz*(nz-1)
else if (igridsel==8) then
	write(*,*) "Input X,Y,Z coordinate of box center (in Angstrom)"
	write(*,*) "or input such as a8 to take the coordinate of atom 8 as box center"
	write(*,*) "or input such as a3,a7 to take the midpoint of atom 3 and atom 7 as box center"
	read(*,"(a)") c80tmp
	if (c80tmp(1:1)=='a') then
		do ich=1,len_trim(c80tmp)
			if (c80tmp(ich:ich)==',') exit
		end do
		if (ich==len_trim(c80tmp)+1) then
			read(c80tmp(2:),*) itmp
			cenx=a(itmp)%x
			ceny=a(itmp)%y
			cenz=a(itmp)%z
		else
			read(c80tmp(2:ich-1),*) itmp
			read(c80tmp(ich+2:),*) jtmp			
			cenx=(a(itmp)%x+a(jtmp)%x)/2D0
			ceny=(a(itmp)%y+a(jtmp)%y)/2D0
			cenz=(a(itmp)%z+a(jtmp)%z)/2D0
		end if
	else
		read(c80tmp,*) cenx,ceny,cenz
		cenx=cenx/b2a
		ceny=ceny/b2a
		cenz=cenz/b2a
	end if
	write(*,*) "Input the grid spacing (bohr)  e.g. 0.08"
	read(*,*) dx
	dy=dx
	dz=dx
	write(*,*) "Input the box lengths in X,Y,Z direction (Bohr) e.g. 8.0,8.0,13.5"
	read(*,*) molxlen,molylen,molzlen
	orgx=cenx-molxlen/2D0
	orgy=ceny-molylen/2D0
	orgz=cenz-molzlen/2D0
	endx=orgx+molxlen
	endy=orgy+molylen
	endz=orgz+molzlen
	nx=nint(molxlen/dx)+1
	ny=nint(molylen/dy)+1
	nz=nint(molzlen/dz)+1
else if (igridsel==9) then
	write(*,*) "Input path of a cube file, e.g. C:\opai.cub"
	do while(.true.)
		read(*,"(a)") cubefilename
		inquire(file=cubefilename,exist=alive)
		if (alive) then
			open(10,file=cubefilename,status="old")
			read(10,*)
			read(10,*)
			read(10,*) nouse,orgx,orgy,orgz
			read(10,*) nx,dx
			read(10,*) ny,rnouse,dy
			read(10,*) nz,rnouse,rnouse,dz
			close(10)
			exit
		else
			write(*,*) "Error: File cannot be found, input again"
		end if
	end do
	endx=orgx+dx*(nx-1)
	endy=orgy+dy*(ny-1)
	endz=orgz+dz*(nz-1)
end if

if (igridsel==10) call setboxGUI

write(*,"(' Coordinate of origin in X,Y,Z is   ',3f12.6)") orgx,orgy,orgz
write(*,"(' Coordinate of end point in X,Y,Z is',3f12.6)") endx,endy,endz
write(*,"(' Spacing in X,Y,Z is',3f11.6)") dx,dy,dz
write(*,"(' Number of points in X,Y,Z is',3i5,'   Total',i10)") nx,ny,nz,nx*ny*nz

gridvec1=0;gridvec1(1)=dx
gridvec2=0;gridvec2(2)=dy
gridvec3=0;gridvec3(3)=dz
end subroutine


!!!------------------------- Delete virtual orbitals higher than LUMO+10 for HF/DFT wavefunctions
subroutine delvirorb(infomode)
use defvar
implicit real*8 (a-h,o-z)
integer :: infomode,nvirsave=10 !Lowest nvirsave virtual orbitals will be reserved
if (ifiletype/=1.and.ifiletype/=9) return !Only works for fch and molden
if (idelvirorb==0) return
if (iuserfunc==24) return !linear response kernel require all orbital information
if (imodwfn==1) return !Don't make things more complicated!
!This routine doesn't work for post-HF instances
if (wfntype==0.or.wfntype==2) then !RHF, ROHF
	if (nmo<=naelec+nvirsave) return
	nmo=naelec+10 !Simply shield those virtual orbitals
else if (wfntype==1) then !Perserve up to LUMO+10 for alpha, and identical number of orbitals for beta
	if (nmo/2<=naelec+nvirsave) return !naelec is always >= nbelec
	nperserve=naelec+nvirsave
	!Cobasa and Cobasb are needn't to be modified
	co(naelec+11:naelec+nvirsave+nperserve,:)=co(nmo/2+1:nmo/2+nperserve,:)
	MOene(naelec+nvirsave+1:naelec+nvirsave+nperserve)=MOene(nmo/2+1:nmo/2+nperserve)
	MOocc(naelec+nvirsave+1:naelec+nvirsave+nperserve)=MOocc(nmo/2+1:nmo/2+nperserve)
	MOtype(naelec+nvirsave+1:naelec+nvirsave+nperserve)=MOtype(nmo/2+1:nmo/2+nperserve)
	nmo=2*nperserve
end if
imodwfn=1 !Will not call this routine again
if (infomode==1.and.(wfntype==0.or.wfntype==1.or.wfntype==2)) then
	write(*,"(a)") " Note: Virtual orbitals higher than LUMO+10 have been discarded for saving computational time"
	write(*,*)
end if
end subroutine


!!!-------- imode=1: Convert unit of grid/plane parameters from Bohr to Angstrom. =2: Convert them back
subroutine convgridlenunit(imode)
use defvar
implicit none
integer imode
real*8 scll
if (imode==1) scll=b2a
if (imode==2) scll=1/b2a
orgx=orgx*scll
orgy=orgy*scll
orgz=orgz*scll
orgx2D=orgx2D*scll
orgy2D=orgy2D*scll
orgz2D=orgz2D*scll
endx=endx*scll
endy=endy*scll
endz=endz*scll
dx=dx*scll;gridvec1=gridvec1*scll
dy=dy*scll;gridvec2=gridvec2*scll
dz=dz*scll;gridvec3=gridvec3*scll
v1x=v1x*scll
v1y=v1y*scll
v1z=v1z*scll
v2x=v2x*scll
v2y=v2y*scll
v2z=v2z*scll
a1x=a1x*scll
a1y=a1y*scll
a1z=a1z*scll
a2x=a2x*scll
a2y=a2y*scll
a2z=a2z*scll
a3x=a3x*scll
a3y=a3y*scll
a3z=a3z*scll
d1=d1*scll
d2=d2*scll
end subroutine


!!-------- Deallocate all arrays about wavefunction except for the _org ones
subroutine dealloall
use defvar
if (allocated(a)) deallocate(a)
if (allocated(b)) deallocate(b)
if (allocated(CO)) deallocate(CO)
if (allocated(MOocc)) deallocate(MOocc)
if (allocated(MOsym)) deallocate(MOsym)
if (allocated(MOene)) deallocate(MOene)
if (allocated(MOtype)) deallocate(MOtype)
if (allocated(b_EDF)) then
	deallocate(CO_EDF,b_EDF)
	nEDFprims=0
	nEDFelec=0
end if
if (allocated(connmat)) deallocate(connmat)
!Related to basis functions
if (allocated(shtype)) deallocate(shtype,shcen,shcon,primshexp,primshcoeff,&
basshell,bascen,bastype,basstart,basend,primstart,primend,primconnorm)
if (allocated(CObasa)) deallocate(CObasa)
if (allocated(CObasb)) deallocate(CObasb)
if (allocated(Ptot)) deallocate(Ptot)
if (allocated(Palpha)) deallocate(Palpha)
if (allocated(Pbeta)) deallocate(Pbeta)
if (allocated(Sbas)) deallocate(Sbas)
if (allocated(Dbas)) deallocate(Dbas)
if (allocated(DorbA)) deallocate(DorbA)
if (allocated(DorbB)) deallocate(DorbB)
end subroutine


!!-------- Deallocate all arrays about wavefunction for the _org ones
subroutine dealloall_org
use defvar
firstfilename=" "
ncenter_org=0
nmo_org=0
nprims_org=0
if (allocated(a_org)) deallocate(a_org)
if (allocated(b_org)) deallocate(b_org,CO_org,MOocc_org,MOene_org)
if (allocated(Sbas_org)) deallocate(Sbas_org)
if (allocated(CObasa_org)) deallocate(CObasa_org)
if (allocated(CObasb_org)) deallocate(CObasb_org)
if (allocated(Palpha_org)) deallocate(Palpha_org)
if (allocated(Pbeta_org)) deallocate(Pbeta_org)
end subroutine




!!------- Generate atomic/fragmental Hirshfeld weight and store it to planemat, calculate free-atom/fragmental density and store it to planemattmp
!The atoms in the fragment is inputted as "selatm" array, nselatm is the number of its elements
!if itype=1, use atomic wavefunction to calculate Hirshfeld weight, and setpromol must have been invoked; if =2, use built-in atomic density to generate it
subroutine genHirshplanewei(selatm,nselatm,itype)
use defvar
use function
implicit real*8 (a-h,o-z)
integer selatm(nselatm),nselatm,itype
if (allocated(planemat)) deallocate(planemat)
if (allocated(planemattmp)) deallocate(planemattmp)
allocate(planemat(ngridnum1,ngridnum2),planemattmp(ngridnum1,ngridnum2))
planemat=0D0
planemattmp=0D0
do iatm=1,ncenter_org !Calc free atomic density of each atom, get promolecular density and Hirshfeld weight of present atom
	iyes=0
	if (any(selatm==iatm)) iyes=1
	if (itype==1) then
		call dealloall
		call readwfn(custommapname(iatm),1)
	end if
	!$OMP PARALLEL DO private(i,j,rnowx,rnowy,rnowz,tmpval) shared(planemat) schedule(dynamic) NUM_THREADS(nthreads)
	do i=1,ngridnum1 !First calculate promolecular density and store it to planemat
		do j=1,ngridnum2
			rnowx=orgx2D+(i-1)*v1x+(j-1)*v2x
			rnowy=orgy2D+(i-1)*v1y+(j-1)*v2y
			rnowz=orgz2D+(i-1)*v1z+(j-1)*v2z
			if (itype==1) then
				tmpval=fdens(rnowx,rnowy,rnowz)
			else
				tmpval=calcatmdens(iatm,rnowx,rnowy,rnowz,0)
			end if
			planemat(i,j)=planemat(i,j)+tmpval
			if (iyes==1) planemattmp(i,j)=planemattmp(i,j)+tmpval
		end do
	end do
	!$OMP END PARALLEL DO
end do
if (itype==1) then
	call dealloall
	call readinfile(firstfilename,1) !Retrieve the first loaded file(whole molecule)
end if

do i=1,ngridnum1 !Calculate Hirshfeld weighting function
	do j=1,ngridnum2
		if (planemat(i,j)/=0D0) then
			planemat(i,j)=planemattmp(i,j)/planemat(i,j)
		else
			planemat(i,j)=0D0
		end if
	end do
end do
end subroutine

!!------- Calculate some quantities involved in Shubin's project in a plane
!itype=1: Calculate the sum of atomic relative Shannon entropy (namely total relative Shannon entropy)
!itype=2: Calculate the sum of x=[rhoA-rho0A]/rhoA
!itype=3: Calculate the difference between total relative Shannon entropy and deformation density
subroutine genentroplane(itype)
use defvar
use function
implicit real*8 (a-h,o-z)
integer itype
real*8 planeprodens(ngridnum1,ngridnum2),planedens(ngridnum1,ngridnum2)
if (allocated(planemat)) deallocate(planemat)
allocate(planemat(ngridnum1,ngridnum2))
planeprodens=0D0
planemat=0D0
!Calculate molecular density in the plane and store it to planedens
!$OMP PARALLEL DO private(i,j,rnowx,rnowy,rnowz) shared(planedens) schedule(dynamic) NUM_THREADS(nthreads)
do i=1,ngridnum1
	do j=1,ngridnum2
		rnowx=orgx2D+(i-1)*v1x+(j-1)*v2x
		rnowy=orgy2D+(i-1)*v1y+(j-1)*v2y
		rnowz=orgz2D+(i-1)*v1z+(j-1)*v2z
		planedens(i,j)=fdens(rnowx,rnowy,rnowz)
	end do
end do
!$OMP END PARALLEL DO
do jatm=1,ncenter_org !Calculate promolecular density in the plane and store it to planeprodens
	call dealloall
	call readwfn(custommapname(jatm),1)
	!$OMP PARALLEL DO private(i,j,rnowx,rnowy,rnowz) shared(planeprodens) schedule(dynamic) NUM_THREADS(nthreads)
	do i=1,ngridnum1
		do j=1,ngridnum2
			rnowx=orgx2D+(i-1)*v1x+(j-1)*v2x
			rnowy=orgy2D+(i-1)*v1y+(j-1)*v2y
			rnowz=orgz2D+(i-1)*v1z+(j-1)*v2z
			planeprodens(i,j)=planeprodens(i,j)+fdens(rnowx,rnowy,rnowz)
		end do
	end do
	!$OMP END PARALLEL DO
end do
!Calculate Hirshfeld weight, relative Shannon entropy and x=[rhoA-rho0A]/rhoA for each atom in the plane and accumulate them to planemat
do jatm=1,ncenter_org !Cycle each atom, calculate its contribution in the plane
	call dealloall
	call readwfn(custommapname(jatm),1)
	!$OMP PARALLEL DO private(i,j,rnowx,rnowy,rnowz,rho0A,rhoA,tmpval) shared(planemat) schedule(dynamic) NUM_THREADS(nthreads)
	do i=1,ngridnum1
		do j=1,ngridnum2
			rnowx=orgx2D+(i-1)*v1x+(j-1)*v2x
			rnowy=orgy2D+(i-1)*v1y+(j-1)*v2y
			rnowz=orgz2D+(i-1)*v1z+(j-1)*v2z
			rho0A=fdens(rnowx,rnowy,rnowz)
			rhoA=planedens(i,j)*rho0A/planeprodens(i,j)
			if (itype==1.or.itype==3) tmpval=rhoA*log(rhoA/rho0A) !Relative Shannon entropy
			if (itype==2) tmpval=(rhoA-rho0A)/rhoA !x=[rhoA-rho0A]/rhoA
			planemat(i,j)=planemat(i,j)+tmpval
		end do
	end do
	!$OMP END PARALLEL DO
end do
call dealloall
call readinfile(firstfilename,1) !Retrieve the first loaded file(whole molecule)
if (itype==3) planemat=planemat-(planedens-planeprodens) !Diff between total relative Shannon entropy and deformation density
end subroutine



!!----- Generate atomic Hirshfeld weight and store it to cubmat
!The atoms in the fragment is inputted as "selatm" array, nselatm is the number of its elements
!if itype=1, use atomic wavefunction to calculate Hirshfeld weight, and setpromol must have been invoked; if =2, use built-in atomic density to generate it
subroutine genHirshcubewei(selatm,nselatm,itype)
use defvar
use function
implicit real*8 (a-h,o-z)
integer selatm(nselatm),nselatm,itype
if (allocated(cubmat)) deallocate(cubmat)
if (allocated(cubmattmp)) deallocate(cubmattmp)
allocate(cubmat(nx,ny,nz),cubmattmp(nx,ny,nz))
cubmat=0D0
cubmattmp=0D0
do iatm=1,ncenter_org
	write(*,"(' Finished',i6,'  /',i6)") iatm,ncenter_org
	if (itype==1) then
		call dealloall
		call readwfn(custommapname(iatm),1)
	end if
	!$OMP PARALLEL DO SHARED(cubmat,cubmattmp,ifinish) PRIVATE(i,j,k,tmpx,tmpy,tmpz,tmpval) schedule(dynamic) NUM_THREADS(nthreads)
	do k=1,nz !First calculate promolecular density and store it to cubmat
		tmpz=orgz+(k-1)*dz
		do j=1,ny
			tmpy=orgy+(j-1)*dy
			do i=1,nx
				tmpx=orgx+(i-1)*dx
				if (itype==1) then
					tmpval=fdens(tmpx,tmpy,tmpz)
				else
					tmpval=calcatmdens(iatm,tmpx,tmpy,tmpz,0)
				end if
				cubmat(i,j,k)=cubmat(i,j,k)+tmpval
				if (any(selatm==iatm)) cubmattmp(i,j,k)=cubmattmp(i,j,k)+tmpval
			end do
		end do
	end do
	!$OMP END PARALLEL DO
end do
if (itype==1) then
	call dealloall
	call readinfile(firstfilename,1) !Retrieve the first loaded file(whole molecule)
end if

do k=1,nz !Calculate Hirshfeld weighting function
	do j=1,ny
		do i=1,nx
			if (cubmat(i,j,k)/=0D0) then
				cubmat(i,j,k)=cubmattmp(i,j,k)/cubmat(i,j,k)
			else
				cubmat(i,j,k)=0D0
			end if
		end do
	end do
end do
end subroutine



!!--- Generate single-center integration grid for Becke's integration. Not adapted according to element. Return iradcut and gridatm
subroutine gen1cintgrid(gridatm,iradcut)
use defvar
implicit real*8 (a-h,o-z)
integer iradcut
real*8 potx(sphpot),poty(sphpot),potz(sphpot),potw(sphpot)
type(content) gridatm(radpot*sphpot)
call Lebedevgen(sphpot,potx,poty,potz,potw)
iradcut=0 !Before where the radial points will be cut
parm=1D0
do i=1,radpot !Combine spherical point&weights with second kind Gauss-Chebyshev method for radial part
	radx=cos(i*pi/(radpot+1))
	radr=(1+radx)/(1-radx)*parm !Becke transform
	radw=2*pi/(radpot+1)*parm**3 *(1+radx)**2.5D0/(1-radx)**3.5D0 *4*pi
	gridatm( (i-1)*sphpot+1:i*sphpot )%x=radr*potx
	gridatm( (i-1)*sphpot+1:i*sphpot )%y=radr*poty
	gridatm( (i-1)*sphpot+1:i*sphpot )%z=radr*potz
	gridatm( (i-1)*sphpot+1:i*sphpot )%value=radw*potw
	if (radcut/=0D0.and.iradcut==0.and.radr<radcut) iradcut=i-1
! 	write(*,"(2f20.6)") radr,radw
end do
end subroutine
!!--- Generate Becke weight for a batch of points around iatm, sharpness parameter=3
!!--- Input: iatm, iradcut, gridatm   Return: beckeweigrid
subroutine gen1cbeckewei(iatm,iradcut,gridatm,beckeweigrid)
use defvar
implicit real*8 (a-h,o-z)
integer iatm,iradcut
real*8 beckeweigrid(radpot*sphpot),smat(ncenter,ncenter),Pvec(ncenter)
type(content) gridatm(radpot*sphpot)
!$OMP parallel do shared(beckeweigrid) private(i,rnowx,rnowy,rnowz,smat,ii,ri,jj,rj,rmiu,chi,uij,aij,tmps,Pvec) num_threads(nthreads) schedule(dynamic)
do i=1+iradcut*sphpot,radpot*sphpot
	smat=1D0
	rnowx=gridatm(i)%x
	rnowy=gridatm(i)%y
	rnowz=gridatm(i)%z
	do ii=1,ncenter
		ri=dsqrt( (rnowx-a(ii)%x)**2+(rnowy-a(ii)%y)**2+(rnowz-a(ii)%z)**2 )
		do jj=1,ncenter
			if (ii==jj) cycle
			rj=dsqrt( (rnowx-a(jj)%x)**2+(rnowy-a(jj)%y)**2+(rnowz-a(jj)%z)**2 )
			rmiu=(ri-rj)/distmat(ii,jj)
 			!Adjust for heteronuclear
			chi=covr_tianlu(a(ii)%index)/covr_tianlu(a(jj)%index)
			uij=(chi-1)/(chi+1)
			aij=uij/(uij**2-1)
			if (aij>0.5D0) aij=0.5D0
			if (aij<-0.5D0) aij=-0.5D0
			rmiu=rmiu+aij*(1-rmiu**2)
			tmps=rmiu
			do iter=1,3
				tmps=1.5D0*(tmps)-0.5D0*(tmps)**3
			end do
			smat(ii,jj)=0.5D0*(1-tmps)
		end do
	end do
	Pvec=1D0
	do ii=1,ncenter
		Pvec=Pvec*smat(:,ii)
	end do
	beckeweigrid(i)=Pvec(iatm)/sum(Pvec)
end do
!$OMP end parallel do
end subroutine



!!--------- A standalone routine to calculate atomic contribution to specific real space function
!iparttype=1: Becke partition
!atmcontri: Returned array containing atomic contribution
!ifunc: The function to be evaluated
subroutine atmcontrifunc(iparttype,atmcontri,ifunc)
use defvar
use function
implicit real*8 (a-h,o-z)
integer ifunc
type(content) gridatm(radpot*sphpot),gridatmorg(radpot*sphpot)
real*8 atmcontri(ncenter),beckeweigrid(radpot*sphpot)

atmcontri=0
! write(*,"(' Radial points:',i5,'    Angular points:',i5,'   Total:',i10,' per center')") radpot,sphpot,radpot*sphpot
if (iparttype==1) then !Becke partition
	call gen1cintgrid(gridatmorg,iradcut)
	!$OMP PARALLEL DO SHARED(atmcontri) PRIVATE(iatm,ipt,gridatm,beckeweigrid,funcval) schedule(dynamic) NUM_THREADS(nthreads)
	do iatm=1,ncenter
! 		write(*,"(' Processing center',i6,'(',a2,')   /',i6)") iatm,a(iatm)%name,ncenter
		gridatm%x=gridatmorg%x+a(iatm)%x !Move quadrature point to actual position in molecule
		gridatm%y=gridatmorg%y+a(iatm)%y
		gridatm%z=gridatmorg%z+a(iatm)%z
		call gen1cbeckewei(iatm,iradcut,gridatm,beckeweigrid)
		do ipt=1+iradcut*sphpot,radpot*sphpot
			funcval=calcfuncall(ifunc,gridatm(ipt)%x,gridatm(ipt)%y,gridatm(ipt)%z)
			atmcontri(iatm)=atmcontri(iatm)+funcval*gridatmorg(ipt)%value*beckeweigrid(ipt)
		end do
	end do
	!$OMP END PARALLEL DO
end if
end subroutine



!!--------- Convert 1RDM in MO basis outputted by MRCC program to natural orbitals
!In CCDENSITIES, the density matrix is represented in MO basis
!When frozen core is enabled, the indices are counted from the first correlated orbital
subroutine MRCC_gennatorb
use defvar
use util
character c200tmp*200
real*8,allocatable :: eigvecmat(:,:),eigvalarr(:),tmparr(:)
do while(.true.)
	write(*,*) "Input the path of CCDENSITIES, e.g. C:\lovelive\CCDENSITIES"
! 	c200tmp="D:\CM\my_program\Multiwfn\x\MRCCdens\HF_m3_CCSD\CCDENSITIES"
	read(*,"(a)") c200tmp
	inquire(file=c200tmp,exist=alive)
	if (alive) exit
	write(*,*) "Cannot find the file, input again"
end do
write(*,*)
write(*,*) "Input the number of frozen orbitals, e.g. 3"
write(*,*) "If no orbitals are frozen, simply input 0"
write(*,"(a)") " PS: For unrestricted reference, if you input n, the n lowest alpha and n lowest beta MOs will be regarded as frozen"
read(*,*) nfrz
write(*,*) "Please wait..."
if (wfntype==0) then !RHF reference
	open(10,file=c200tmp,status="old")
	Ptot=0
	do while(.true.)
		read(10,*,iostat=ierror) tmp,i,j,k,l
		if (ierror/=0) exit
		if (k==0.and.l==0) then !Only load 1RDM
			Ptot(i+nfrz,j+nfrz)=tmp
			Ptot(j+nfrz,i+nfrz)=tmp
		end if
	end do
	close(10)
	do ifrz=1,nfrz
		Ptot(ifrz,ifrz)=2D0
	end do
	allocate(eigvecmat(nbasis,nbasis),eigvalarr(nbasis),tmparr(nbasis))
	call diagsymat(Ptot,eigvecmat,eigvalarr,istat)
	MOocc=eigvalarr
	!Currently the occupation is from low to high, now invert the sequence
	do i=1,int(nmo/2D0)
		idx=i
		jdx=nmo+1-i
		tmp=MOocc(idx)
		MOocc(idx)=MOocc(jdx)
		MOocc(jdx)=tmp
		tmparr=eigvecmat(:,idx)
		eigvecmat(:,idx)=eigvecmat(:,jdx)
		eigvecmat(:,jdx)=tmparr
	end do
	CObasa=matmul(CObasa,eigvecmat)
	wfntype=3
	write(*,*) "Occupation number:"
	write(*,"(6f12.6)") MOocc
else if (wfntype==1) then !UHF reference
	!In CCDENSITIES, the sequence is:
	!2RDM-alpha
	!  0.00000000000000000000E+00   0   0   0   0
	!2RDM-beta
	!  0.00000000000000000000E+00   0   0   0   0
	!Unknown
	!  0.00000000000000000000E+00   0   0   0   0
	!1RDM-alpha
	!  0.00000000000000000000E+00   0   0   0   0
	!1RDM-beta
	!  0.00000000000000000000E+00   0   0   0   0
	open(10,file=c200tmp,status="old")
	Palpha=0
	Pbeta=0
	itime=0
	do while(.true.)
		read(10,*) tmp,i,j,k,l
		if (i==0.and.j==0.and.k==0.and.l==0) then
			itime=itime+1
			if (itime==5) exit
			cycle
		end if
		if (itime==3) then
			Palpha(i+nfrz,j+nfrz)=tmp
			Palpha(j+nfrz,i+nfrz)=tmp
		else if (itime==4) then
			Pbeta(i+nfrz,j+nfrz)=tmp
			Pbeta(j+nfrz,i+nfrz)=tmp
		end if
	end do
	close(10)
	do ifrz=1,nfrz
		Palpha(ifrz,ifrz)=1D0
		Pbeta(ifrz,ifrz)=1D0
	end do
	allocate(eigvecmat(nbasis,nbasis),eigvalarr(nbasis),tmparr(nbasis))
	!Alpha part
	call diagsymat(Palpha,eigvecmat,eigvalarr,istat)
	MOocc(1:nbasis)=eigvalarr
	do i=1,int(nbasis/2D0)
		idx=i
		jdx=nbasis+1-i
		tmp=MOocc(idx)
		MOocc(idx)=MOocc(jdx)
		MOocc(jdx)=tmp
		tmparr=eigvecmat(:,idx)
		eigvecmat(:,idx)=eigvecmat(:,jdx)
		eigvecmat(:,jdx)=tmparr
	end do
	CObasa=matmul(CObasa,eigvecmat)
	write(*,*) "Occupation number of Alpha part:"
	write(*,"(6f12.6)") MOocc(1:nbasis)
	!Beta part
	call diagsymat(Pbeta,eigvecmat,eigvalarr,istat)
	MOocc(nbasis+1:nmo)=eigvalarr
	do i=1,int(nbasis/2D0)
		idx=nbasis+i
		jdx=nmo+1-i
		tmp=MOocc(idx)
		MOocc(idx)=MOocc(jdx)
		MOocc(jdx)=tmp
		tmparr=eigvecmat(:,i)
		eigvecmat(:,i)=eigvecmat(:,nbasis+1-i)
		eigvecmat(:,nbasis+1-i)=tmparr
	end do
	CObasb=matmul(CObasb,eigvecmat)
	write(*,*) "Occupation number of Beta part:"
	write(*,"(6f12.6)") MOocc(nbasis+1:nmo)
	wfntype=4
end if

call genP
MOene=0
write(*,*) "Done! Basis function information now correspond to natural orbital cases"
write(*,"(a)") " Note: If next you would like to analyze real space functions, you should export .molden file, &
and then reload it, so that GTF information will also correspond to natural orbitals"
end subroutine




!!----------- Generate spherical harmonic -> Cartesian basis function conversion table for d,f,g,h.
!iprog=1: for readfch;  iprog=2: for readmolden
!The table comes from IJQC,54,83, which is used by Gaussian
!The sequence of d and f shell is also identical to .molden convention, but for g, another conversion table is used, &
!since in Multiwfn g cartesian shell starts from ZZZZ, but that of .molden starts from xxxx
subroutine gensphcartab(iprog,matd,matf,matg,math)
real*8 matd(6,5),matf(10,7),matg(15,9),math(21,11)
integer iprog
matd=0D0
matf=0D0
matg=0D0
math=0D0
! From 5D: D 0,D+1,D-1,D+2,D-2
! To 6D:  1  2  3  4  5  6
!        XX,YY,ZZ,XY,XZ,YZ
!
! D0=-0.5*XX-0.5*YY+ZZ
matd(1:3,1)=(/ -0.5D0,-0.5D0,1D0 /)
! D+1=XZ
matd(5,2)=1D0
! D-1=YZ
matd(6,3)=1D0
! D+2=SQRT(3)/2*(XX-YY)
matd(1:2,4)=(/ sqrt(3D0)/2D0,-sqrt(3D0)/2D0 /)
! D-2=XY
matd(4,5)=1D0

! From 7F: F 0,F+1,F-1,F+2,F-2,F+3,F-3
! To 10F:  1   2   3   4   5   6   7   8   9  10      
!         XXX,YYY,ZZZ,XYY,XXY,XXZ,XZZ,YZZ,YYZ,XYZ (Gaussian sequence, not identical to Multiwfn)
!
! F 0=-3/(2*¡Ì5)*(XXZ+YYZ)+ZZZ
matf(3,1)=1D0
matf(6,1)=-1.5D0/sqrt(5D0)
matf(9,1)=-1.5D0/sqrt(5D0)
! F+1=-¡Ì(3/8)*XXX-¡Ì(3/40)*XYY+¡Ì(6/5)*XZZ
matf(1,2)=-sqrt(3D0/8D0)
matf(4,2)=-sqrt(3D0/40D0)
matf(7,2)=sqrt(6D0/5D0)
! F-1=-¡Ì(3/40)*XXY-¡Ì(3/8)*YYY+¡Ì(6/5)*YZZ
matf(2,3)=-sqrt(3D0/8D0)
matf(5,3)=-sqrt(3D0/40D0)
matf(8,3)=sqrt(6D0/5D0)
! F+2=¡Ì3/2*(XXZ-YYZ)
matf(6,4)=sqrt(3D0)/2D0
matf(9,4)=-sqrt(3D0)/2D0
! F-2=XYZ
matf(10,5)=1D0
! F+3=¡Ì(5/8)*XXX-3/¡Ì8*XYY
matf(1,6)=sqrt(5D0/8D0)
matf(4,6)=-3D0/sqrt(8D0)
! F-3=3/¡Ì8*XXY-¡Ì(5/8)*YYY
matf(2,7)=-sqrt(5D0/8D0)
matf(5,7)=3D0/sqrt(8D0)

if (iprog==1) then !for .fch
	! From 9G: G 0,G+1,G-1,G+2,G-2,G+3,G-3,G+4,G-4
	! To 15G:   1    2    3    4    5    6    7    8
	!         ZZZZ,YZZZ,YYZZ,YYYZ,YYYY,XZZZ,XYZZ,XYYZ
	!           9   10   11   12   13   14   15
	!         XYYY,XXZZ,XXYZ,XXYY,XXXZ,XXXY,XXXX
	!
	!G 0=ZZZZ+3/8*(XXXX+YYYY)-3*¡Ì(3/35)*(XXZZ+YYZZ-1/4*XXYY)
	 matg(1,1)=1D0
	 matg(3,1)=-3D0*sqrt(3D0/35D0)
	 matg(5,1)=3D0/8D0
	 matg(10,1)=-3D0*sqrt(3D0/35D0)
	 matg(12,1)=3D0/4D0*sqrt(3D0/35D0)
	 matg(15,1)=3D0/8D0
	 !G+1=2*¡Ì(5/14)*XZZZ-3/2*¡Ì(5/14)*XXXZ-3/2/¡Ì14*XYYZ
	 matg(6,2)=2D0*sqrt(5D0/14D0)
	 matg(8,2)=-1.5D0/sqrt(14D0)
	 matg(13,2)=-1.5D0*sqrt(5D0/14D0)
	 !G-1=2*¡Ì(5/14)*YZZZ-3/2*¡Ì(5/14)*YYYZ-3/2/¡Ì14*XXYZ
	 matg(2,3)=2D0*sqrt(5D0/14D0)
	 matg(4,3)=-1.5D0*sqrt(5D0/14D0)
	 matg(11,3)=-1.5D0/sqrt(14D0)
	 !G+2=3*¡Ì(3/28)*(XXZZ-YYZZ)-¡Ì5/4*(XXXX-YYYY)
	 matg(3,4)=-3D0*sqrt(3D0/28D0)
	 matg(5,4)=sqrt(5D0)/4D0
	 matg(10,4)=3D0*sqrt(3D0/28D0)
	 matg(15,4)=-sqrt(5D0)/4D0
	 !G-2=3/¡Ì7*XYZZ-¡Ì(5/28)*(XXXY+XYYY)
	 matg(7,5)=3D0/sqrt(7D0)
	 matg(9,5)=-sqrt(5D0/28D0)
	 matg(14,5)=-sqrt(5D0/28D0)
	 !G+3=¡Ì(5/8)*XXXZ-3/¡Ì8*XYYZ
	 matg(8,6)=-3D0/sqrt(8D0)
	 matg(13,6)=sqrt(5D0/8D0)
	 !G-3=-¡Ì(5/8)*YYYZ+3/¡Ì8*XXYZ
	 matg(4,7)=-sqrt(5D0/8D0)
	 matg(11,7)=3D0/sqrt(8D0)
	 !G+4=¡Ì35/8*(XXXX+YYYY)-3/4*¡Ì3*XXYY
	 matg(5,8)=sqrt(35D0)/8D0
	 matg(12,8)=-3D0/4D0*sqrt(3D0)
	 matg(15,8)=sqrt(35D0)/8D0
	 !G-4=¡Ì5/2*(XXXY-XYYY)
	 matg(9,9)=-sqrt(5D0)/2D0
	 matg(14,9)=sqrt(5D0)/2D0
else if (iprog==2) then !For .molden
	! From 9G: G 0,G+1,G-1,G+2,G-2,G+3,G-3,G+4,G-4
	! To 15G:   1    2    3    4    5    6    7    8
	!         xxxx,yyyy,zzzz,xxxy,xxxz,yyyx,yyyz,zzzx
	!           9   10   11   12   13   14   15
	!         zzzy,xxyy,xxzz,yyzz,xxyz,yyxz,zzxy
	!
	!G 0=ZZZZ+3/8*(XXXX+YYYY)-3*¡Ì(3/35)*(XXZZ+YYZZ-1/4*XXYY)
	matg(3,1)=1D0
	matg(1,1)=3D0/8D0
	matg(2,1)=3D0/8D0
	matg(11,1)=-3D0*sqrt(3D0/35D0)
	matg(12,1)=-3D0*sqrt(3D0/35D0)
	matg(10,1)=3D0/4D0*sqrt(3D0/35D0)
	!G+1=2*¡Ì(5/14)*XZZZ-3/2*¡Ì(5/14)*XXXZ-3/2/¡Ì14*XYYZ
	matg(8,2)=2D0*sqrt(5D0/14D0)
	matg(5,2)=-1.5D0*sqrt(5D0/14D0)
	matg(14,2)=-1.5D0/sqrt(14D0)
	!G-1=2*¡Ì(5/14)*YZZZ-3/2*¡Ì(5/14)*YYYZ-3/2/¡Ì14*XXYZ
	matg(9,3)=2D0*sqrt(5D0/14D0)
	matg(7,3)=-1.5D0*sqrt(5D0/14D0)
	matg(13,3)=-1.5D0/sqrt(14D0)
	!G+2=3*¡Ì(3/28)*(XXZZ-YYZZ)-¡Ì5/4*(XXXX-YYYY)
	matg(11,4)=3D0*sqrt(3D0/28D0)
	matg(12,4)=-3D0*sqrt(3D0/28D0)
	matg(1,4)=-sqrt(5D0)/4D0
	matg(2,4)=sqrt(5D0)/4D0
	!G-2=3/¡Ì7*XYZZ-¡Ì(5/28)*(XXXY+XYYY)
	matg(15,5)=3D0/sqrt(7D0)
	matg(4,5)=-sqrt(5D0/28D0)
	matg(6,5)=-sqrt(5D0/28D0)
	!G+3=¡Ì(5/8)*XXXZ-3/¡Ì8*XYYZ
	matg(5,6)=sqrt(5D0/8D0)
	matg(14,6)=-3D0/sqrt(8D0)
	!G-3=-¡Ì(5/8)*YYYZ+3/¡Ì8*XXYZ
	matg(7,7)=-sqrt(5D0/8D0)
	matg(13,7)=3D0/sqrt(8D0)
	!G+4=¡Ì35/8*(XXXX+YYYY)-3/4*¡Ì3*XXYY
	matg(1,8)=sqrt(35D0)/8D0
	matg(2,8)=sqrt(35D0)/8D0
	matg(10,8)=-3D0/4D0*sqrt(3D0)
	!G-4=¡Ì5/2*(XXXY-XYYY)
	matg(4,9)=sqrt(5D0)/2D0
	matg(6,9)=-sqrt(5D0)/2D0
end if

! From 11H: H 0,H+1,H-1,H+2,H-2,H+3,H-3,H+4,H-4,H+5,H-5
! To 21H:   1     2     3     4     5     6     7     8     9    10
!         ZZZZZ YZZZZ YYZZZ YYYZZ YYYYZ YYYYY XZZZZ XYZZZ XYYZZ XYYYZ 
!          11    12    13    14    15    16    17    18    19    20    21
!         XYYYY XXZZZ XXYZZ XXYYZ XXYYY XXXZZ XXXYZ XXXYY XXXXZ XXXXY XXXXX
!
!H 0=ZZZZZ-5/¡Ì21*(XXZZZ+YYZZZ)+5/8*(XXXXZ+YYYYZ)+¡Ì(15/7)/4*XXYYZ
math(1,1)=1D0
math(12,1)=-5D0/sqrt(21D0)
math(3,1)=-5D0/sqrt(21D0)
math(19,1)=5D0/8D0
math(5,1)=5D0/8D0
math(14,1)=sqrt(15D0/7D0)/4D0
!H+1=¡Ì(5/3)*XZZZZ-3*¡Ì(5/28)*XXXZZ-3/¡Ì28*XYYZZ+¡Ì15/8*XXXXX+¡Ì(5/3)/8*XYYYY+¡Ì(5/7)/4*XXXYY
math(7,2)=sqrt(5D0/3D0)
math(16,2)=-3D0*sqrt(5D0/28D0)
math(9,2)=-3D0/sqrt(28D0)
math(21,2)=sqrt(15D0)/8D0
math(11,2)=sqrt(5D0/3D0)/8D0
math(18,2)=sqrt(5D0/7D0)/4D0
!H-1=¡Ì(5/3)*YZZZZ-3*¡Ì(5/28)*YYYZZ-3/¡Ì28*XXYZZ+¡Ì15/8*YYYYY+¡Ì(5/3)/8*XXXXY+¡Ì(5/7)/4*XXYYY
math(2,3)=sqrt(5D0/3D0)
math(4,3)=-3D0*sqrt(5D0/28D0)
math(13,3)=-3D0/sqrt(28D0)
math(6,3)=sqrt(15D0)/8D0
math(20,3)=sqrt(5D0/3D0)/8D0
math(15,3)=sqrt(5D0/7D0)/4D0
!H+2=¡Ì5/2*(XXZZZ-YYZZZ)-¡Ì(35/3)/4*(XXXXZ-YYYYZ)
math(12,4)=sqrt(5D0)/2D0
math(3,4)=-sqrt(5D0)/2D0
math(19,4)=-sqrt(35D0/3D0)/4D0
math(5,4)=sqrt(35D0/3D0)/4D0
!H-2=¡Ì(5/3)*XYZZZ-¡Ì(5/12)*(XXXYZ+XYYYZ)
math(8,5)=sqrt(5D0/3D0)
math(17,5)=-sqrt(5D0/12D0)
math(10,5)=-sqrt(5D0/12D0)
!H+3=¡Ì(5/6)*XXXZZ-¡Ì(3/2)*XYYZZ-¡Ì(35/2)/8*(XXXXX-XYYYY)+¡Ì(5/6)/4*XXXYY
math(16,6)=sqrt(5D0/6D0)
math(9,6)=-sqrt(1.5D0)
math(21,6)=-sqrt(17.5D0)/8D0
math(11,6)=sqrt(17.5D0)/8D0
math(18,6)=sqrt(5D0/6D0)/4D0
!H-3=-¡Ì(5/6)*YYYZZ+¡Ì(3/2)*XXYZZ-¡Ì(35/2)/8*(XXXXY-YYYYY)-¡Ì(5/6)/4*XXYYY
math(4,7)=-sqrt(5D0/6D0)
math(13,7)=sqrt(1.5D0)
math(20,7)=-sqrt(17.5D0)/8D0
math(6,7)=sqrt(17.5D0)/8D0
math(15,7)=-sqrt(5D0/6D0)/4D0
!H+4=¡Ì35/8*(XXXXZ+YYYYZ)-3/4*¡Ì3*XXYYZ
math(19,8)=sqrt(35D0)/8D0
math(5,8)=sqrt(35D0)/8D0
math(14,8)=-0.75D0*sqrt(3D0)
!H-4=¡Ì5/2*(XXXYZ-XYYYZ)
math(17,9)=sqrt(5D0)/2D0
math(10,9)=-sqrt(5D0)/2D0
!H+5=3/8*¡Ì(7/2)*XXXXX+5/8*¡Ì(7/2)*XYYYY-5/4*¡Ì(3/2)*XXXYY
math(21,10)=3D0/8D0*sqrt(3.5D0)
math(11,10)=5D0/8D0*sqrt(3.5D0)
math(18,10)=-1.25D0*sqrt(1.5D0)
!H-5=3/8*¡Ì(7/2)*YYYYY+5/8*¡Ì(7/2)*XXXXY-5/4*¡Ì(3/2)*XXYYY
math(6,11)=3D0/8D0*sqrt(3.5D0)
math(20,11)=5D0/8D0*sqrt(3.5D0)
math(15,11)=-1.25D0*sqrt(1.5D0)
end subroutine




!!---------- Load Fock matrix from NBO .47 file or plain text file
!istatus=0 means successfully loaded. =1 means failed
subroutine loadFock47(istatus)
use defvar
use util
character c200tmp*200
integer istatus
do while(.true.)
	write(*,"(a)") " Input the file recording Fock matrix in original basis functions in lower triangular form, e.g. C:\fock.txt"
	write(*,*) "Note: If the suffix is .47, the Fock matrix will be directly loaded from it"
	read(*,"(a)") c200tmp
	inquire(file=c200tmp,exist=alive)
	if (alive==.false.) then
		write(*,*) "Error: Unable to find this file! Input again"
		cycle
	end if
	exit
end do
open(10,file=c200tmp,status="old")
if (allocated(FmatA)) deallocate(FmatA)
allocate(FmatA(nbasis,nbasis))
if (index(c200tmp,".47")/=0) then
	write(*,*) "Trying to load Fock matrix from .47 file..."
	call loclabel(10,"$FOCK",ifound)
	if (ifound==0) then
		write(*,*) "Error: Unable to find $FOCK field in this file!"
		close(10)
		istatus=0
		return
	end if
	read(10,*)
end if
read(10,*) ((FmatA(i,j),j=1,i),i=1,nbasis) !Load total or alpha Fock matrix
do i=1,nbasis !Fill upper triangular part
	do j=i+1,nbasis
		FmatA(i,j)=FmatA(j,i)
	end do
end do
if (wfntype==1) then
	if (allocated(FmatB)) deallocate(FmatB)
	allocate(FmatB(nbasis,nbasis))
	read(10,*) ((FmatB(i,j),j=1,i),i=1,nbasis) !Load beta Fock matrix
	do i=1,nbasis
		do j=i+1,nbasis
			FmatB(i,j)=FmatB(j,i)
		end do
	end do
end if
close(10)
write(*,*) "Fock matrix loaded successfully!"
istatus=1
end subroutine



!!-------- Randomly generate name of Sobereva's lover
subroutine mylover(outname)
integer,parameter :: nlovers=51
character*80 lovername(nlovers),outname
CALL RANDOM_SEED()
CALL RANDOM_NUMBER(tmp)
lovername(1)="K-ON\Mio_Akiyama"
lovername(2)="K-ON\Azusa_Nakano"
lovername(3)="EVA\Rei_Ayanami"
lovername(4)="Ore_no_Imoto\Black_Cat"
lovername(5)="Touhou_project\Ran_Yakumo"
lovername(6)="Haiyore!Nyaruko-san\Nyaruko"
lovername(7)="Bodacious_Space_Pirates\Kurihara_Chiaki"
lovername(8)="Otoboku\Mariya_Mikado"
lovername(9)="Amagami\Miya_Tachibana"
lovername(10)="Shakugan_no_Shana\Shana"
lovername(11)="Tiger_Mask_W\Miss_X"
! lovername(11)="Yuru_Yuri\Akari_Akaza"
lovername(12)="Natsuiro_Kiseki\Yuka_Hanaki"
lovername(13)="Love_Live!\Nico_Yazawa"
lovername(14)="Love_Live!\Nozomi_Tojo"
lovername(15)="Love_Live!\Nishikino_Maki"
lovername(16)="Last_Exile\Dio_Eraclea"
lovername(17)="NHK_ni_Youkoso!\Misaki_Nakahara"
lovername(18)="Rio_Rainbow_Gate\Rio_Rollins"
lovername(19)="Blood-C\Saya_Kisaragi"
lovername(20)="Mahou_Shoujo_Madoka-Magica\Homura_Akemi"
lovername(21)="Saki\Hisa_Takei"
lovername(22)="Strawberry_Panic\Chikaru_Minamoto"
lovername(23)="Najica\Najica_Hiiragi"
lovername(24)="Blue_Drop\Hagino_Senkouji"
lovername(25)="Fate_Zero\Saber"
lovername(26)="Baka_to_Test_to_Shoukanjuu\Hideyoshi_Kinoshita"
lovername(27)="Watamote\Tomoko_Kuroki"
lovername(28)="Genshiken_Nidaime\Kenjirou_Hato"
lovername(29)="Love_is_Like_After_the_Rain\Akira_Tachibana"
lovername(30)="Kan_Colle\Shimakaze"
lovername(31)="Kan_Colle\Kongou"
lovername(32)="Gokukoku\Kazumi_Schlierenzauer"
lovername(33)="Vocaloid\Miku_Hatsune"
lovername(34)="Tokimeki_Memorial\Yuina_Himoo"
lovername(35)="MADLAX\MADLAX"
lovername(36)="Gun_Gale_Online\Kirito"
lovername(37)="Denkigai_No_Honyasan\Sennsei"
lovername(38)="Wake_Up,Girls!\Miyu_Okamoto"
lovername(39)="Plastic_Memories\Aira"
lovername(40)="Real_world\sell-moe-kun"
lovername(41)="Sakurako-san_no_Ashimoto_ni_wa_Shitai_ga_Umatteiru\Sakurako"
lovername(42)="Hibike!_Euphonium\Reina_Kousaka"
lovername(43)="Planetarian\Yumemi_Hoshino"
lovername(44)="Lovelive_sunshine!!\Yoshiko_Tsushima"
lovername(45)="Lovelive_sunshine!!\You_Watanabe"
lovername(46)="Lovelive_sunshine!!\Sakurauchi_Riko"
lovername(47)="Violet_Evergarden\Violet_Evergarden"
lovername(48)="Otobuko\Mizuho_Miyanokouji"
lovername(49)="iDOLM@STER\Makoto_Kikuchi"
lovername(50)="Fate\Rin_Tohsaka"
lovername(51)="Magical Girl Spec-Ops Asuka\Asuka Otori"
!Dear Kanan,
!
!You are the only one I deeply love forever in the real world,
!although you can't be with me, and I am even unable to know your name and touch your finger.
!I believe I will never love anyone else in the rest of my life.
!
!I love your brilliant dance, your kawaii smile, your lovely double ponytail, and especially, your extremely pure and beautiful heart.
!
!                     ----- 2015-May-19
outname=lovername(ceiling(tmp*nlovers))
end subroutine





!!----------- Convert current CObasa / CObasb to CO
!ispin=1: Only alpha, =2: Only beta, =3: Both alpha and beta
subroutine CObas2CO(ispin)
use defvar
implicit real*8 (a-h,o-z)
integer ispin
real*8 conv5d6d(6,5),conv7f10f(10,7),conv9g15g(15,9),conv11h21h(21,11)
real*8,allocatable :: CObasa_cart(:,:),CObasb_cart(:,:)

call gensphcartab(1,conv5d6d,conv7f10f,conv9g15g,conv11h21h)

nbasis_cart=sum(shtype2nbas(abs(shtype(:))))
if (ispin==1.or.ispin==3) allocate(CObasa_cart(nbasis_cart,nbasis))
if (ispin==2.or.ispin==3) allocate(CObasb_cart(nbasis_cart,nbasis))
CObasa_cart=0

!Map spherical coefficients to Cartesian coefficients
ipos5D=1
ipos6D=1
do ish=1,nshell
    ishtype=shtype(ish)
    numshbas5D=shtype2nbas(ishtype)
    numshbas6D=shtype2nbas(abs(ishtype))
    !write(*,*) ish,ishtype,numshbas5D,ipos5D,ipos6D
    if (ispin==1.or.ispin==3) then !alpha part
        if (ishtype>=0) then !S,P or Cartesian type, in this case numshbas5D=numshbas6D 
            CObasa_cart(ipos6D:ipos6D+numshbas6D-1,:)=CObasa(ipos5D:ipos5D+numshbas5D-1,:)
        else
	        if (ishtype==-2) then
		        CObasa_cart(ipos6D:ipos6D+numshbas6D-1,:)=matmul(conv5d6d,CObasa(ipos5D:ipos5D+numshbas5D-1,:))
	        else if (ishtype==-3) then
		        CObasa_cart(ipos6D:ipos6D+numshbas6D-1,:)=matmul(conv7f10f,CObasa(ipos5D:ipos5D+numshbas5D-1,:))
	        else if (ishtype==-4) then
		        CObasa_cart(ipos6D:ipos6D+numshbas6D-1,:)=matmul(conv9g15g,CObasa(ipos5D:ipos5D+numshbas5D-1,:))
	        else if (ishtype==-5) then
		        CObasa_cart(ipos6D:ipos6D+numshbas6D-1,:)=matmul(conv11h21h,CObasa(ipos5D:ipos5D+numshbas5D-1,:))
	        end if
        end if
    end if
    if (ispin==2.or.ispin==3) then !beta part
        if (ishtype>=0) then !S,P or Cartesian type, in this case numshbas5D=numshbas6D 
            CObasb_cart(ipos6D:ipos6D+numshbas6D-1,:)=CObasb(ipos5D:ipos5D+numshbas5D-1,:)
        else
	        if (ishtype==-2) then
		        CObasb_cart(ipos6D:ipos6D+numshbas6D-1,:)=matmul(conv5d6d,CObasb(ipos5D:ipos5D+numshbas5D-1,:))
	        else if (ishtype==-3) then
		        CObasb_cart(ipos6D:ipos6D+numshbas6D-1,:)=matmul(conv7f10f,CObasb(ipos5D:ipos5D+numshbas5D-1,:))
	        else if (ishtype==-4) then
		        CObasb_cart(ipos6D:ipos6D+numshbas6D-1,:)=matmul(conv9g15g,CObasb(ipos5D:ipos5D+numshbas5D-1,:))
	        else if (ishtype==-5) then
		        CObasb_cart(ipos6D:ipos6D+numshbas6D-1,:)=matmul(conv11h21h,CObasb(ipos5D:ipos5D+numshbas5D-1,:))
	        end if
        end if
    end if
	ipos5D=ipos5D+numshbas5D
	ipos6D=ipos6D+numshbas6D
end do

do imo=1,nbasis
    do ibas=1,nbasis_cart
        !if (imo==1) write(*,*) ibas,primstart(ibas),primend(ibas)
        if (ispin==1.or.ispin==3) then
            do iGTF=primstart(ibas),primend(ibas)
                CO(imo,iGTF)=CObasa_cart(ibas,imo)*primconnorm(iGTF)
            end do
        end if
        if (ispin==2.or.ispin==3) then
            do iGTF=primstart(ibas),primend(ibas)
                CO(imo+nbasis,iGTF)=CObasb_cart(ibas,imo)*primconnorm(iGTF)
            end do
        end if
    end do
end do
end subroutine




!!------- Add a Bq atom to specific position
subroutine addBq(xpos,ypos,zpos)
use defvar
real*8 xpos,ypos,zpos
allocate(a_tmp(ncenter))
a_tmp=a
ncenter=ncenter+1
deallocate(a)
allocate(a(ncenter))
a(1:ncenter-1)=a_tmp
a(ncenter)%index=0
a(ncenter)%charge=0
a(ncenter)%name="Bq"
a(ncenter)%x=xpos
a(ncenter)%y=ypos
a(ncenter)%z=zpos
deallocate(a_tmp)
end subroutine


!!------- Invoke Gaussian to run a .gjf
!If returned istate=1, means normally termination, =0 means other case or failed
subroutine runGaussian(gjfname,istate)
use defvar
use util
character(len=*) gjfname
character command*200,outname*200
outname=gjfname(:len(gjfname)-3)//"out"
command=trim(gaupath)//' "'//gjfname//'" "'//trim(outname)//'"'
write(*,*) "Running: "//trim(command)
call system(command)
open(100,file=outname,status="old")
call loclabel(100,"Normal termination",istate)
close(100)
end subroutine


!!------- Gaussian scratch file in current folder (only meaningful for Windows version)
subroutine cleangauscr
character command*200
command="del gxx.* fort.6 Gau*.inp"
write(*,*) "Running: "//trim(command)
call system(command)
end subroutine


!!------- Delete files, cannot delete folder
subroutine delfile(delname)
use defvar
character(len=*) delname
character command*200
if (isys==1) then
    command="del /Q "//trim(delname)
else if (isys==2) then
    command="rm -f "//trim(delname)
end if
write(*,*) "Deleting "//trim(delname)
call system(trim(command))
end subroutine



!!----- Generate connectivity matrix, invoked by such as subroutine outcml
subroutine genconnmat
use defvar
implicit real*8 (a-h,o-z)
if (allocated(connmat)) deallocate(connmat)
allocate(connmat(ncenter,ncenter))
write(*,*) "Generating bonding relationship..."
write(*,"(a,f6.3,a)") " Note: If distance between two atoms is smaller than sum of their &
covalent radii multiplied by ",bondcrit,", then they are regarded as bonded"
connmat=0
do iatm=1,ncenter
    do jatm=iatm+1,ncenter
        if ( distmat(iatm,jatm) < bondcrit*(covr(a(iatm)%index)+covr(a(jatm)%index)) ) connmat(iatm,jatm)=1
        connmat(jatm,iatm)=connmat(iatm,jatm)
    end do
end do
end subroutine