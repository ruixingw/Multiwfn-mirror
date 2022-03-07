!##################################################################################
!Some codes do not work normally if compiling via ifort -O2 in Linux possibly due to
!bug in compiler. These codes are collected in this file, they will be compiled via -O1 in Linux.
!In Windows, this file is also compiled with /O2 like other files.
!##################################################################################



!!----------- Calculate center, first/second moments, radius of gyration, and <r^2> of a function
!Via ifort 2018 and 2021 with -O2, result obtained by option 1 is zero or NaN
subroutine funcmoment
use defvar
use util
use function
implicit real*8 (a-h,o-z)
real*8 intval,moment1(3),moment2(3,3),moment2nuc(3,3),funcval(radpot*sphpot),beckeweigrid(radpot*sphpot),eigvecmat(3,3),eigval(3)
type(content) gridatm(radpot*sphpot),gridatmorg(radpot*sphpot)
character selectyn
ifunc=1
cenx=0
ceny=0
cenz=0
do while(.true.)
	write(*,*)
	write(*,*) "0 Return"
	write(*,*) "1 Calculate various quantities of the selected function"
	write(*,*) "2 Calculate center and integral of the selected function"
	write(*,"(a,i5)") " 3 Select the function to be studied, current:",ifunc
	write(*,"(a,3f11.5,' Ang')") " 4 Set the center for option 1, current:",cenx*b2a,ceny*b2a,cenz*b2a
	write(*,*) "5 Calculate center and integral of the absolute of the selected function"
	read(*,*) isel
	
	if (isel==0) then
		return
	else if (isel==3) then
		call selfunc_interface(1,ifunc)
		cycle
	else if (isel==4) then
		write(*,*) "Input X,Y,Z of the center in Angstrom, e.g. 2.0,0,1.5"
		read(*,*) cenx,ceny,cenz
		cenx=cenx/b2a !To Bohr
		ceny=ceny/b2a
		cenz=cenz/b2a
		cycle
	end if

	write(*,"(' Radial points:',i5,'    Angular points:',i5,'   Total:',i10,' per center')") radpot,sphpot,radpot*sphpot
	call gen1cintgrid(gridatmorg,iradcut)

	call walltime(iwalltime1)

	intval=0
	moment1=0
	moment2=0
	realcenx=0
	realceny=0
	realcenz=0
	do iatm=1,ncenter
		write(*,"(' Processing center',i6,'(',a2,')   /',i6)") iatm,a(iatm)%name,ncenter
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
			tmpval=funcval(i)*gridatmorg(i)%value*beckeweigrid(i)
			xtmp=gridatm(i)%x-cenx
			ytmp=gridatm(i)%y-ceny
			ztmp=gridatm(i)%z-cenz
            if (isel==5) tmpval=abs(tmpval)
			intval=intval+tmpval
			moment1(1)=moment1(1)+xtmp*tmpval
			moment1(2)=moment1(2)+ytmp*tmpval
			moment1(3)=moment1(3)+ztmp*tmpval
			moment2(1,1)=moment2(1,1)+xtmp*xtmp*tmpval
			moment2(2,2)=moment2(2,2)+ytmp*ytmp*tmpval
			moment2(3,3)=moment2(3,3)+ztmp*ztmp*tmpval
			moment2(1,2)=moment2(1,2)+xtmp*ytmp*tmpval
			moment2(2,3)=moment2(2,3)+ytmp*ztmp*tmpval
			moment2(1,3)=moment2(1,3)+xtmp*ztmp*tmpval
			realcenx=realcenx+gridatm(i)%x*tmpval
			realceny=realceny+gridatm(i)%y*tmpval
			realcenz=realcenz+gridatm(i)%z*tmpval
		end do
	end do
	call walltime(iwalltime2)
	write(*,"(' Calculation took up wall clock time',i10,' s',/)") iwalltime2-iwalltime1
	
	if (isel==1) then
		moment2(3,1)=moment2(1,3)
		moment2(2,1)=moment2(1,2)
		moment2(3,2)=moment2(2,3)
		write(*,*) "Note: All data shown below are in a.u."
		write(*,"(/,' Integral over whole space:',1PE16.8,/)") intval
		write(*,"(' The first moment:')")
		write(*,"(' X= ',1PE16.8,'   Y= ',1PE16.8,'   Z= ',1PE16.8)") moment1
		write(*,"(' Norm= ',1PE16.8,/)") sum(moment1**2)
		write(*,"(' The second moment:')")
		write(*,"(' XX=',1PE16.8,'   XY=',1PE16.8,'   XZ=',1PE16.8)") moment2(1,:)
		write(*,"(' YX=',1PE16.8,'   YY=',1PE16.8,'   YZ=',1PE16.8)") moment2(2,:)
		write(*,"(' ZX=',1PE16.8,'   ZY=',1PE16.8,'   ZZ=',1PE16.8)") moment2(3,:)

		call diagmat(moment2,eigvecmat,eigval,300,1D-10)
		call sort(eigval)
		write(*,"(a,3(1PE16.8))") ' Eigenvalues:',eigval
        write(*,"(a,1PE16.8)") " Sum of eigenvalues (trace of the second moment tensor):",sum(eigval)
		write(*,"(' Anisotropy:',1PE16.8,/)") eigval(3)-(eigval(1)+eigval(2))/2D0
		write(*,"(' Radius of gyration:',1PE16.8)") dsqrt((moment2(1,1)+moment2(2,2)+moment2(3,3))/intval)
        write(*,"(/,a,f16.6)") " Spatial extent of the function <r^2>:",sum(eigval)

		if (ifunc==1) then
			moment2nuc=0
			do iatm=1,ncenter
				xtmp=a(iatm)%x-cenx
				ytmp=a(iatm)%y-ceny
				ztmp=a(iatm)%z-cenz
				tmpval=a(iatm)%charge
				moment2nuc(1,1)=moment2nuc(1,1)+xtmp*xtmp*tmpval
				moment2nuc(2,2)=moment2nuc(2,2)+ytmp*ytmp*tmpval
				moment2nuc(3,3)=moment2nuc(3,3)+ztmp*ztmp*tmpval
				moment2nuc(1,2)=moment2nuc(1,2)+xtmp*ytmp*tmpval
				moment2nuc(2,3)=moment2nuc(2,3)+ytmp*ztmp*tmpval
				moment2nuc(1,3)=moment2nuc(1,3)+xtmp*ztmp*tmpval
			end do
			moment2nuc(3,1)=moment2nuc(1,3)
			moment2nuc(2,1)=moment2nuc(1,2)
			moment2nuc(3,2)=moment2nuc(2,3)
			write(*,*)
			write(*,"(' The quadrupole moment of nuclear charges:')")
			write(*,"(' XX=',f16.8,'   XY=',f16.8,'   XZ=',f16.8)") moment2nuc(1,:)
			write(*,"(' YX=',f16.8,'   YY=',f16.8,'   YZ=',f16.8)") moment2nuc(2,:)
			write(*,"(' ZX=',f16.8,'   ZY=',f16.8,'   ZZ=',f16.8)") moment2nuc(3,:)
			write(*,*)
			write(*,"(' The quadrupole moment of the system:')")
			write(*,"(' XX=',f16.8,'   XY=',f16.8,'   XZ=',f16.8)") moment2nuc(1,:)-moment2(1,:)
			write(*,"(' YX=',f16.8,'   YY=',f16.8,'   YZ=',f16.8)") moment2nuc(2,:)-moment2(2,:)
			write(*,"(' ZX=',f16.8,'   ZY=',f16.8,'   ZZ=',f16.8)") moment2nuc(3,:)-moment2(3,:)
		end if
		
	else if (isel==2.or.isel==5) then
		realcenx=realcenx/intval
		realceny=realceny/intval
		realcenz=realcenz/intval
        if (isel==2) then
			write(*,"(' Integral of the function:',1PE16.8,' a.u.')") intval
			write(*,"(/,' Center of the function:')")
        else
			write(*,"(' Integral of the absolute of the function:',1PE16.8,' a.u.')") intval
			write(*,"(/,' Center of the absolute of the function:')")
        end if
		write(*,"(' X=',f16.8,' Y=',f16.8,' Z=',f16.8,' Angstrom',/)") realcenx*b2a,realceny*b2a,realcenz*b2a
		write(*,*) "Use this center for subsequent calculations? (y/n)"
		read(*,*) selectyn
		if (selectyn=='y') then
			cenx=realcenx
			ceny=realceny
			cenz=realcenz
		end if
	end if
end do
end subroutine