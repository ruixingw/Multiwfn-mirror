!-------- Main interface of various other functions part 2
subroutine otherfunc3_main
implicit real*8 (a-h,o-z)
do while(.true.)
	write(*,*)
	write(*,*) "              ============ Other functions (Part 3) ============ "
	write(*,*) "0 Return"
	write(*,*) "1 Viewing free regions and calculating free volume in a box"
	write(*,*) "2 Fitting atomic radial density as linear combination of multiple STOs or GTFs"
    write(*,*) "3 Visualize (hyper)polarizability via unit sphere and vector representations"
    write(*,*) "4 Simulating scanning tunneling microscope (STM) image"
    write(*,*) "5 Calculate electric dipole, quadrupole and octopole moments analytically"
    write(*,*) "6 Calculate energies of the present orbitals"
    
	read(*,*) isel
	if (isel==0) then
		return
	else if (isel==1) then
        call freeregion
	else if (isel==2) then
        call fitatmdens
	else if (isel==3) then
        call vis_hypol
	else if (isel==4) then
        call STM
    else if (isel==5) then
        call calc_multipole
    else if (isel==6) then
        call calc_orb_energy
	end if
end do
end subroutine
    


!!--------- Viewing free regions and calculating free volume in a box
subroutine freeregion
use defvar
use GUI
use util
implicit real*8 (a-h,o-z)
character c80tmp*80
integer :: iPBC=1,ismooth=1,isetgrid=1,iclosebound=1
real*8 :: sclrad=1
real*8,allocatable :: cubmat_smooth(:,:,:)

do while(.true.)
    write(*,*)
    write(*,*) " -------- Viewing free regions and calculating free volume in a box --------"
    write(*,*) "0 Return"
    write(*,*) "1 Set grid and start calculation"
    if (iPBC==1) write(*,*) "2 Toggle considering periodic boundary condition, current: Yes"
    if (iPBC==0) write(*,*) "2 Toggle considering periodic boundary condition, current: No"
    if (ismooth==1) write(*,*) "3 Toggle calculating smoothed grid data of free regions, current: Yes"
    if (ismooth==0) write(*,*) "3 Toggle calculating smoothed grid data of free regions, current: No"
    if (isetgrid==1) write(*,"(' 4 Toggle the way of setting up grid, current: Manually specifying grid spacing and box lengths')")
    if (isetgrid==0) write(*,"(' 4 Toggle the way of setting up grid, current: Defining grid using normal interface')")
    if (iclosebound==0) write(*,"(' 5 Toggle making isosurface closed at boundary, current: No')")
    if (iclosebound==1) write(*,"(' 5 Toggle making isosurface closed at boundary, current: Yes')")
    write(*,"(' 6 Set scale factor of vdW radii for calculating free volume and primitive free region, current:',f6.3)") sclrad
    
    read(*,*) isel
    if (isel==0) then
        return
    else if (isel==1) then
        exit
    else if (isel==2) then
        if (iPBC==1) then
            iPBC=0
        else
            iPBC=1
        end if
    else if (isel==3) then
        if (ismooth==1) then
            ismooth=0
        else
            ismooth=1
        end if
    else if (isel==4) then
        if (isetgrid==1) then
            isetgrid=0
        else
            isetgrid=1
        end if
    else if (isel==5) then
        if (iclosebound==1) then
            iclosebound=0
        else
            iclosebound=1
        end if
    else if (isel==6) then
        write(*,*) "Input scale factor for vdW radii, e.g. 1.8"
        read(*,*) sclrad
    end if
end do

if (isetgrid==0) then
    call setgrid(1,inouse)
else
    write(*,*) "Input grid spacing in Angstrom, e.g. 0.2"
    write(*,*) "If press ENTER button directly, 0.3 Angstrom will be used"
    read(*,"(a)") c80tmp
    if (c80tmp==" ") then
        tmpval=0.3D0
    else
        read(c80tmp,*) tmpval
    end if
    itmp=len_trim(filename)
    xlen=0
    if (filename(itmp-2:itmp)=="pdb") then
        open(10,file=filename,status="old")
        call loclabel(10,"CRYST1",ifound)
        if (ifound==1) read(10,*) c80tmp,xlen,ylen,zlen
        close(10)
        write(*,*) "Box lengths are directly loaded from ""CRYST1"" field from input file"
        write(*,"(' Box length in X, Y, Z:  ',3f10.3,' Angstrom')") xlen,ylen,zlen
    end if
    if (xlen==0) then
        write(*,*) "Input X, Y, Z box lengths in Angstrom, e.g. 31.064,31.100,31.093"
        read(*,*) xlen,ylen,zlen
    end if
    xlen=xlen/b2a
    ylen=ylen/b2a
    zlen=zlen/b2a
    
    dx=tmpval/b2a
    dy=dx
    dz=dx
    nx=int(xlen/dx)+1
    ny=int(ylen/dy)+1
    nz=int(zlen/dz)+1
    !Marginally re-adjust grid spacing so that the number of grids could be integer/
    dx=xlen/(nx-1)
    dy=ylen/(ny-1)
    dz=zlen/(nz-1)
    gridvec1(:)=(/ dx,0D0,0D0 /)
    gridvec2(:)=(/ 0D0,dy,0D0 /)
    gridvec3(:)=(/ 0D0,0D0,dz /)
    
    orgx=0
    orgy=0
    orgz=0
    endx=orgx+dx*(nx-1) !In fact, when using setboxGUI, the endx/y/z have already been set
    endy=orgy+dy*(ny-1)
    endz=orgz+dz*(nz-1)
end if

write(*,"(' Origin in X,Y,Z is      ',3f10.3,' Angstrom')") orgx*b2a,orgy*b2a,orgz*b2a
write(*,"(' Grid spacing in X,Y,Z is',3f10.3,' Angstrom')") dx*b2a,dy*b2a,dz*b2a
write(*,"(' The number of points in X,Y,Z is',3i5,'   Total:',i12)") nx,ny,nz,nx*ny*nz

call walltime(iwalltime1)
allocate(cubmat(nx,ny,nz))
if (ismooth==1) allocate(cubmat_smooth(nx,ny,nz))
cubmat=1 !Free region has value of 1, occupied region has value of 0
cubmat_smooth=1
write(*,*) "Calculating, please wait..."
ifinish=0
!$OMP PARALLEL DO SHARED(cubmat,cubmat_smooth,ifinish) PRIVATE(i,j,k,tmpx,tmpy,tmpz,iatm,dist2,atmvdwr,sclvdwr,tmpval,parmc) schedule(dynamic) NUM_THREADS(nthreads)
do k=1,nz
	tmpz=orgz+(k-1)*dz
	do j=1,ny
		tmpy=orgy+(j-1)*dy
		do i=1,nx
			tmpx=orgx+(i-1)*dx
            do iatm=1,ncenter
                atmvdwr=vdwr(a(iatm)%index)
                sclvdwr=sclrad*atmvdwr
                if (iPBC==1) then
                    call pbcdist2atm(tmpx,tmpy,tmpz,xlen,ylen,zlen,iatm,dist2)
                else
                    dist2=(tmpx-a(iatm)%x)**2+(tmpy-a(iatm)%y)**2+(tmpz-a(iatm)%z)**2
                end if
                if (dist2<sclvdwr**2) then
                    cubmat(i,j,k)=0
                    if (ismooth==0) exit
                end if
                if (ismooth==1) then !Use Gaussian function to smooth, namely broadening atom as Gaussian distribution
                    if (dist2>(3*atmvdwr)**2) cycle !This was found to be very safe
                    parmc=2*atmvdwr/2.35482D0 !2*atmvdwr=FWHM, convert to parameter c, see wiki page of Gaussian function
                    tmpval=exp(-dist2/(2*parmc**2))
                    cubmat_smooth(i,j,k)=cubmat_smooth(i,j,k) - tmpval
                end if
            end do
		end do
	end do
	ifinish=ifinish+1
    call showprog(ifinish,nz)
end do
!$OMP END PARALLEL DO

where(cubmat_smooth<0) cubmat_smooth=0

!Calculate free volume. Strickly speaking, center of grid should be used to determine occupancy status, however for simplicity,
!we do below way, corresponding to use one vertex of a grid to determine occupancy
freevol=sum(cubmat(1:nx-1,1:ny-1,1:nz-1))*dx*dy*dz*b2a**3
voltot=xlen*ylen*zlen*b2a**3
write(*,"(' Volume of entire box:',f12.3,' Angstrom^3')") voltot
write(*,"(' Free volume:',f12.3,' Angstrom^3, corresponding to',f8.2,' % of whole space')") freevol,freevol/voltot*100

call walltime(iwalltime2)
write(*,"(/,' Calculation took up time',i10,' s')") iwalltime2-iwalltime1

!Set value of boundary grids to make isosurface map close
if (iclosebound==1) then
    !XY layers
    do i=1,nx
        do j=1,ny
            cubmat(i,j,1)=0
            cubmat(i,j,nz)=0
            if (ismooth==1) then
                cubmat_smooth(i,j,1)=0
                cubmat_smooth(i,j,nz)=0
            end if
        end do
    end do
    !XZ layers
    do i=1,nx
        do k=1,nz
            cubmat(i,1,k)=0
            cubmat(i,ny,k)=0
            if (ismooth==1) then
                cubmat_smooth(i,1,k)=0
                cubmat_smooth(i,ny,k)=0
            end if
        end do
    end do
    !YZ layers
    do j=1,ny
        do k=1,nz
            cubmat(1,j,k)=0
            cubmat(nx,j,k)=0
            if (ismooth==1) then
                cubmat_smooth(1,j,k)=0
                cubmat_smooth(nx,j,k)=0
            end if
        end do
    end do
end if

idrawmol=0
ishowatmlab=0
ishowaxis=0
sur_value=0.5D0
do while(.true.)
    write(*,*)
    write(*,*) "0 Return"
    write(*,*) "1 Visualize isosurface of primitive free region"
    write(*,*) "2 Export primitive grid data as free_prim.cub in current folder"
    if (ismooth==1) then
        write(*,*) "3 Visualize isosurface of smoothed free region"
        write(*,*) "4 Export smoothed grid data as free_smooth.cub in current folder"
    end if
    read(*,*) isel
    if (isel==0) then
        deallocate(cubmat)
        if (ismooth==1) deallocate(cubmat_smooth)
        return
    else if (isel==1) then
        call drawisosurgui(1)
    else if (isel==2) then
        open(10,file="free_prim.cub",status="replace")
        call outcube(cubmat,nx,ny,nz,orgx,orgy,orgz,gridvec1,gridvec2,gridvec3,10)
        close(10)
        write(*,"(' Done! Grid data has been exported to free_prim.cub in current folder')")
    else if (isel==3) then
        allocate(cubmattmp(nx,ny,nz))
        cubmattmp=cubmat
        cubmat=cubmat_smooth
        call drawisosurgui(1)
        cubmat=cubmattmp
        deallocate(cubmattmp)
    else if (isel==4) then
        open(10,file="free_smooth.cub",status="replace")
        call outcube(cubmat_smooth,nx,ny,nz,orgx,orgy,orgz,gridvec1,gridvec2,gridvec3,10)
        close(10)
        write(*,"(' Done! Grid data has been exported to free_smooth.cub in current folder')")
    end if
end do
end subroutine

!Input x,y,z coordinate, return square distance to a given atom (iatm) with consideration of PBC
subroutine pbcdist2atm(x,y,z,xlen,ylen,zlen,iatm,dist2)
use defvar
implicit real*8 (a-h,o-z)
real*8 x,y,z,xlen,ylen,zlen,dist2
integer iatm
xa=a(iatm)%x
ya=a(iatm)%y
za=a(iatm)%z
dist2=1D100
do ix=-1,1,1
    do iy=-1,1,1
        do iz=-1,1,1
            dist2tmp=(xa+ix*xlen-x)**2+(ya+iy*ylen-y)**2+(za+iz*zlen-z)**2
            if (dist2tmp<dist2) dist2=dist2tmp
        end do
    end do
end do
end subroutine




!!------- Module used by fitatmdens and related routines
module fitatmdens_mod
real*8,allocatable :: radr(:),radrho(:) !Radial distance and sphericallized density
integer :: ifittype=3 !=1: Minimize absolute error, =2: Minimize relative error, =3: Minimize RDF error
integer :: ifunctype=1 !=1: STO, =2: GTF
integer,parameter :: maxfitfunc=1000 !Maximum number of fitting functions
real*8 exp_fit(maxfitfunc) !If exponents are requested to be fixed, use this array to pass exponents to the calculation routine
integer :: ifixexp=0 !=0: Fit both coefficients and exponents, =1: Fix exponents unchanged
integer :: nfitfunc=0 !Number of STO/GTF used for fitting
end module

!!------------ Use Levenberg-Marquardt algorithm to fit sphericallized atomic radial density as multiple STOs or GTFs
subroutine fitatmdens
use defvar
use fitatmdens_mod
use function
use util
use plot
implicit real*8 (a-h,o-z)
character*3 :: funclab(2)=(/ "STO","GTF" /)
character clegend*80,c200tmp*200
real*8 parm(maxfitfunc*2) !Up to maxfitfunc fitting functions. The first half is coefficient, the latter half is exponents
real*8,allocatable :: fiterr(:),fitrho(:) !Fitting error and fitted density at each fitting point
integer :: npoint_CB=0
real*8,allocatable :: radr_CB(:),radw_CB(:),rho_CB(:) !Position, weight and sphericalized density at second kind Gauss-Chebyshev points
real*8 :: tol=1D-5 !Fitting tolerance. Should not be too small, otherwise it is too difficult to converge until reach maximum of function calls
integer :: iscale=1 !=1: Scale coefficients so that integral equals to actual number of electrons, =0: Do not scale
integer :: isort=1,idelredun=1
external :: atmdens_fiterr
integer,parameter :: nsphpt=170 !Number of points used to calculate sphericalized radial density
real*8 potx(nsphpt),poty(nsphpt),potz(nsphpt),potw(nsphpt)
integer seqidx(maxfitfunc)
integer,allocatable :: tmpidxarr(:)
real*8,allocatable :: tmpvalarr(:)

if (ncenter/=1.or.a(1)%x/=0.or.a(1)%y/=0.or.a(1)%z/=0) then
    write(*,*) "Error: In order to use this function, there must be only one atom and at (0,0,0) point!"
    write(*,*) "Press ENTER button to return"
    read(*,*)
end if

expcutoff=1 !Disable exponent truncation
radstep=0.001D0/b2a
npoint=4000
!radstep=0.002D0/b2a
!npoint=2000
call delvirorb(1)

do while(.true.)
    write(*,*)
    write(*,*) "--------------- Fitting atomic radial density as STOs or GTFs ---------------"
    write(*,*) "0 Return"
    write(*,*) "1 Start fitting"
    write(*,"(a,a)") " 2 Switch type of fitting functions, current: ",funclab(ifunctype)
    write(*,*) "3 Check or set initial guess of coefficients and exponents"
    write(*,"(a,1PE10.2)") " 4 Set fitting tolerance, current:",tol
    write(*,"(a,i5)") " 5 Set number of evenly placed fitting points, current:",npoint
    write(*,"(a,f6.3,' Angstrom')") " 6 Set spacing between fitting points, current:",radstep*b2a
    if (iscale==0) write(*,*) "7 Toggle scaling coefficients to actual number of electrons, current: No"
    if (iscale==1) write(*,*) "7 Toggle scaling coefficients to actual number of electrons, current: Yes"
    if (ifittype==1) write(*,*) "8 Select fitting type, current: Minimizing absolute error"
    if (ifittype==2) write(*,*) "8 Select fitting type, current: Minimizing relative error"
    if (ifittype==3) write(*,*) "8 Select fitting type, current: Minimizing RDF error"
    if (ifixexp==0) write(*,*) "9 Toggle fixing exponents, current: No"
    if (ifixexp==1) write(*,*) "9 Toggle fixing exponents, current: Yes"
    if (isort==0) write(*,*) "10 Toggle sorting functions according to exponents, current: No"
    if (isort==1) write(*,*) "10 Toggle sorting functions according to exponents, current: Yes"
    if (idelredun==0) write(*,*) "11 Toggle removing redundant fitting functions, current: No"
    if (idelredun==1) write(*,*) "11 Toggle removing redundant fitting functions, current: Yes"
    write(*,"(a,i4)") " 12 Set number of second kind Gauss-Chebyshev fitting points, current:",npoint_CB
    read(*,*) isel
    
    if (isel==0) then
	    call delvirorb_back(1)
        return
    else if (isel==2) then
        if (ifunctype==1) then
            ifunctype=2
        else
            ifunctype=1
        end if
    else if (isel==3) then
        do while(.true.)
            if (nfitfunc>0) then
                write(*,*)
                write(*,*) "Current initial guess:"
                write(*,*) "           Coefficient       Exponent"
                do ifunc=1,nfitfunc
                    write(*,"(1x,a,i3,':',2(1PE16.6))") funclab(ifunctype),ifunc,parm(ifunc),parm(nfitfunc+ifunc)
                end do
            else
                write(*,*) "Note: Fitting functions have not been set"
            end if
            write(*,*)
            write(*,*) "0 Return"
            write(*,*) "1 Load initial guess from text file"
            write(*,*) "2 Set initial guess as ""crude fitting by a few STOs with variable exponents"""
            write(*,*) "3 Set initial guess as ""fine fitting by 30 GTFs with fixed exponents"""
            write(*,*) "4 Set initial guess as ""fine fitting by 10 GTFs with variable exponents"""
            write(*,*) "5 Set initial guess as ""fine fitting by 15 GTFs with variable exponents"""
            !write(*,*) "6 Set initial guess as ""fine fitting by 20 GTFs with variable exponents"""
            !GTF with variable exponents higher than 15 is not a good idea, it is extremely expensive &
            !and often the lmdif1 routine return unoptimized result or convergence tolerance cannot be reached, &
            !and the result is never detectably better than 15 GTFs
            read(*,*) isel2
            
            if (isel2==0) then
                exit
            else if (isel2==1) then
                write(*,"(/,a)") " Input path of the text file containing initial guess of coefficients and exponents, e.g. C:\Popipa.txt"
                do while(.true.)
                    read(*,"(a)") c200tmp
	                inquire(file=c200tmp,exist=alive)
	                if (alive) exit
	                write(*,*) "Cannot find the file, input again!"
                end do
                open(10,file=c200tmp,status="old")
                nfitfunc=totlinenum(10,2)
                if (nfitfunc>maxfitfunc) then
                    write(*,"(' Error: Number of fitting functions should not exceed',i6)") maxfitfunc
                    write(*,*) "Press ENTER button to cancel loading"
                    read(*,*)
                else
                    do ifunc=1,nfitfunc
                        read(10,*) parm(ifunc),parm(nfitfunc+ifunc)
                    end do
                end if
                close(10)
            else if (isel2==2) then !Initial guess of STO
                ifixexp=0
                idelredun=0
                if (a(1)%index<=2) then
                    nfitfunc=1
                    parm(1)=1D0
                    parm(2)=2D0
                else if (a(1)%index<=10) then
                    nfitfunc=2
                    parm(1:nfitfunc)=(/ 100,1 /)
                    parm(nfitfunc+1:2*nfitfunc)=(/ 10,2 /)
                else if (a(1)%index<=36) then
                    !nfitfunc=3 !Qualtiy is not as good as nfitunc=4
                    !parm(1:nfitfunc)=(/ 1000,100,1 /)
                    !parm(nfitfunc+1:2*nfitfunc)=(/ 20,5,1 /)
                    nfitfunc=4 !For Mn, this setting may result in negative value at some points
                    parm(1:nfitfunc)=(/ 1000,300,20,1 /)
                    parm(nfitfunc+1:2*nfitfunc)=(/ 27,9,3,1 /)
                else
                    nfitfunc=6
                    do ifunc=nfitfunc,1,-1
                        parm(ifunc)=1*15**(ifunc-1)
                    end do
                    do ifunc=1,nfitfunc
                        parm(nfitfunc+ifunc)=0.5*3**(ifunc)
                    end do
                end if
            else if (isel2==3) then
                ifixexp=1
                ifunctype=2
                nfitfunc=30
                idelredun=1
                do ifunc=1,nfitfunc
                    !parm(nfitfunc+ifunc)=0.1D0*2D0**(ifunc-1) !Using this is safer, but tail cannot be represent as well as below setting
                    parm(nfitfunc+ifunc)=0.05D0*2D0**(ifunc-1)
                end do
                parm(1:nfitfunc)=1
            else if (isel2==4.or.isel2==5.or.isel2==6) then
                ifixexp=0
                ifunctype=2
                idelredun=1
                if (isel2==4) then
                    nfitfunc=10
                    do ifunc=1,nfitfunc
                        parm(nfitfunc+ifunc)=0.1D0*2.5D0**(ifunc-1)
                    end do
                else if (isel2==5) then
                    nfitfunc=15
                    do ifunc=1,nfitfunc
                        parm(nfitfunc+ifunc)=0.1D0*2D0**(ifunc-1)
                    end do
                else if (isel2==6) then
                    nfitfunc=20
                    do ifunc=1,nfitfunc
                        parm(nfitfunc+ifunc)=0.05D0*1.6D0**(ifunc-1)
                    end do
                end if
                parm(1:nfitfunc)=1
                tol=1E-4 !Use more loose tolerance than default make convergence easier while the quality is not detectably lowered
            end if
        end do
        
    else if (isel==4) then
        write(*,*) "Input fitting tolerance, e.g. 1E-7"
        write(*,*) "The smaller the value, the better the fitting accuracy while higher the cost"
        read(*,*) tol
    else if (isel==5) then
        write(*,*) "Input number of fitting points, e.g. 300"
        read(*,*) npoint
    else if (isel==6) then
        write(*,*) "Input spacing between fitting points (in Angstrom), e.g. 0.02"
        read(*,*) radstep
        radstep=radstep/b2a
    else if (isel==7) then
        if (iscale==1) then
            iscale=0
        else
            iscale=1
        end if
    else if (isel==8) then
        write(*,*) "1 Minimizing absolute error"
        write(*,*) "2 Minimizing relative error"
        write(*,*) "3 Minimizing radial distribution function (RDF) error"
        read(*,*) ifittype
    else if (isel==9) then
        if (ifixexp==1) then
            ifixexp=0
        else
            ifixexp=1
        end if
    else if (isel==10) then
        if (isort==1) then
            isort=0
        else
            isort=1
        end if
    else if (isel==11) then
        if (idelredun==1) then
            idelredun=0
        else
            idelredun=1
        end if
    else if (isel==12) then
        write(*,*) "Set the number of second kind Gauss-Chebyshev points used in fitting, e.g. 80"
        write(*,*) "If input 0, then this kind of points will not be included in fitting"
        read(*,*) npoint_CB
    
    else if (isel==1) then !Do fitting
    
        if (nfitfunc==0) then
            write(*,*) "Error: You should use option 3 to set initial guess of fitting functions first!"
            write(*,*) "Press ENTER button to return"
            read(*,*)
            cycle
        end if
    
        !Output initial guess
        write(*,*) "Initial guess of fitting functions:"
        write(*,*) "           Coefficient       Exponent"
        do ifunc=1,nfitfunc
            write(*,"(1x,a,i3,':',2(1PE16.6))") funclab(ifunctype),ifunc,parm(ifunc),parm(nfitfunc+ifunc)
        end do
        write(*,*)
        
        !Calculate actual sphericalized radial density at evenly distributed fitting points
        allocate(radr(npoint),radrho(npoint),fiterr(npoint),fitrho(npoint))
        write(*,*) "Calculating sphericalized radial density..."
        call Lebedevgen(nsphpt,potx,poty,potz,potw)
        !$OMP PARALLEL DO SHARED(radr,radrho) PRIVATE(ipt,tmpdens,isph,xtmp,ytmp,ztmp) schedule(dynamic) NUM_THREADS(nthreads)
        do ipt=1,npoint
            radr(ipt)=radstep*ipt
	        tmpdens=0
	        do isph=1,nsphpt
		        xtmp=potx(isph)*radr(ipt)
		        ytmp=poty(isph)*radr(ipt)
		        ztmp=potz(isph)*radr(ipt)
		        tmpdens=tmpdens+fdens(xtmp,ytmp,ztmp)*potw(isph)
	        end do
            radrho(ipt)=tmpdens
        end do
        !$OMP END PARALLEL DO
        
        !Take second kind Gauss-Chebyshev fitting points into account
        if (npoint_CB>0) then
            allocate(radr_CB(npoint_CB),radw_CB(npoint_CB),rho_CB(npoint_CB))
            !Calculate density at second kind Gauss-Chebyshev points
            parmbk=1D0
            do i=1,npoint_CB !Combine spherical point&weights with second kind Gauss-Chebyshev method for radial part
	            radx=cos(i*pi/(npoint_CB+1))
	            radr_CB(i)=(1+radx)/(1-radx)*parmbk !Becke transform
	            radw_CB(i)=2*pi/(npoint_CB+1)*parmbk**3 *(1+radx)**2.5D0/(1-radx)**3.5D0 *4*pi
            end do
            !$OMP PARALLEL DO SHARED(rho_CB) PRIVATE(ipt,tmpdens,isph,xtmp,ytmp,ztmp) schedule(dynamic) NUM_THREADS(nthreads)
            do ipt=1,npoint_CB
	            tmpdens=0
	            do isph=1,nsphpt
		            xtmp=potx(isph)*radr_CB(ipt)
		            ytmp=poty(isph)*radr_CB(ipt)
		            ztmp=potz(isph)*radr_CB(ipt)
		            tmpdens=tmpdens+fdens(xtmp,ytmp,ztmp)*potw(isph)
	            end do
                rho_CB(ipt)=tmpdens
            end do
            !$OMP END PARALLEL DO
            !Merge second kind Gauss-Chebyshev points into fitting points
            nadd=(count(radr_CB<(10/b2a))) !Ignore points farther than 10 Angstrom, their densities are negligible
            allocate(tmpvalarr(npoint))
            tmpvalarr=radr
            deallocate(radr);allocate(radr(npoint+nadd))
            radr(1:npoint)=tmpvalarr
            tmpvalarr=radrho
            deallocate(radrho);allocate(radrho(npoint+nadd))
            radrho(1:npoint)=tmpvalarr
            do ipt=1,npoint_CB !Combine spherical point&weights with second kind Gauss-Chebyshev method for radial part
                if (radr_CB(ipt)>10/b2a) cycle !Ignore > 10 Angstrom points
                npoint=npoint+1
	            radr(npoint)=radr_CB(ipt)
                radrho(npoint)=rho_CB(ipt)
            end do
            deallocate(fitrho,fiterr)
            allocate(fitrho(npoint),fiterr(npoint))
            !Sort according to r
            allocate(tmpidxarr(npoint))
            forall(i=1:npoint) tmpidxarr(i)=i
            call sortr8(radr,list=tmpidxarr)
            fitrho(:)=fitrho(tmpidxarr(:))
            radrho(:)=radrho(tmpidxarr(:))
            deallocate(tmpvalarr,tmpidxarr)
        end if
        
        !!!!!! Start fitting !!!!!!
        nparm=nfitfunc*2
        maxcall=1000*nparm
        write(*,"(/,' Maximum number of function calls:',i8)") maxcall
        write(*,"(' Convergence tolerance:',f16.8)") tol
        if (ifixexp==1) write(*,*) "Exponents are kept fixed as requested"
        if (ifittype==1) write(*,*) "Fitting type: Minimizing absolute error"
        if (ifittype==2) write(*,*) "Fitting type: Minimizing of relative error"
        if (ifittype==3) write(*,*) "Fitting type: Minimizing radial distribution function error"
        write(*,*) "Fitting via Levenberg-Marquardt algorithm..."
        write(*,*)
        if (idelredun==0) then !Simple fitting
            if (ifixexp==0) then !Fit both coefficients and exponents
                call lmdif1(atmdens_fiterr,npoint,nparm,parm(1:nparm),fiterr,tol,maxcall,info)
            else !Do not fit exponents but keep initial values
                exp_fit(1:nfitfunc)=parm(nfitfunc+1:nparm)
                call lmdif1(atmdens_fiterr,npoint,nfitfunc,parm(1:nfitfunc),fiterr,tol,maxcall,info)
            end if
        else if (idelredun==1) then !Fitting with automatical removal of redundant fitting functions
            nremove=0
            do while(.true.) !Repeat until no function is to be removed
                if (ifixexp==0) then !Fit both coefficients and exponents
                    call lmdif1(atmdens_fiterr,npoint,nparm,parm(1:nparm),fiterr,tol,maxcall,info)
                else !Do not fit exponents but keep initial values
                    exp_fit(1:nfitfunc)=parm(nfitfunc+1:nparm)
                    call lmdif1(atmdens_fiterr,npoint,nfitfunc,parm(1:nfitfunc),fiterr,tol,maxcall,info)
                end if
                iremove=0
                !Remove redundant fitting functions according to exponent and coefficient, adapted according to the ChkRed routine in denfit.f90 in molden2aim
                do ifunc=1,nfitfunc
                    coefftmp=parm(ifunc)
                    exptmp=parm(nfitfunc+ifunc)
                    if ((exptmp>1D5.and.abs(coefftmp)<5).or.(exptmp<3.and.abs(coefftmp)<1D-4)) then
                        iremove=ifunc
                        exit
                    end if    
                end do
                !Remove redundant fitting function having maximum negative contribution if this point has negative fitted density
                if (iremove==0) then
                    do ipt=1,npoint_CB+2*npoint
                        if (ipt<=npoint_CB) then !Using second kind Gauss-Chebyshev part of fitting points to check
                            rtmp=radr_CB(ipt)
                        else !Use double dense grids including r=0 to check. The evenly distributed fitting points are subset of this set
                            rtmp=radstep/2*(ipt-npoint_CB-1)
                        end if
                        contrimin=0
                        tmpval=0
                        imin=0
                        do ifunc=1,nfitfunc
                            coeff=parm(ifunc)
                            expon=parm(nfitfunc+ifunc)
                            if (ifunctype==1) then !STO
                                contrival=coeff*exp(-expon*rtmp)
                            else if (ifunctype==2) then !GTF
                                contrival=coeff*exp(-expon*rtmp**2)
                            end if
                            tmpval=tmpval+contrival
                            if (ifunc==1) then
                                imin=1
                                contrimin=contrival
                            else
                                if (contrival<contrimin) then
                                    imin=ifunc
                                    contrimin=contrival
                                end if
                            end if
                        end do
                        if (tmpval<0) then
                            iremove=imin
                            write(*,"(a,f8.4,a)") " Negative fitted density is found at r=",rtmp*b2a," Angstrom"
                            exit
                        end if
                    end do
                end if
                !Check if variation of fitted density is monotonically decrease. If not, the function with largest exponent may be harmful and should be removed
                !Use double dense grid to perform check 
                if (iremove==0) then
                    do ipt=1,npoint
                        rtmp=radstep/2*(ipt-1)
                        rhotmp=calcfitdens(rtmp,nparm,parm(1:nparm))
                        if (ipt==1) then
                            rhoold=rhotmp
                            cycle
                        else
                            if (rhotmp>rhoold) then !Function is increased with increasing r
                                write(*,"(a,f8.4,a)") " Fitted density is not monotonically decreased at r=",rtmp*b2a," Angstrom"
                                iremove=maxloc(parm(nfitfunc+1:nparm),dim=1)
                                exit
                            else
                                rhoold=rhotmp
                            end if
                        end if
                    end do
                end if
                if (iremove==0) then
                    exit
                else
                    write(*,"(' Delete redundant function (coeff=',1PE12.5,' exp=',1PE12.5,'), refitting...')") parm(iremove),parm(nfitfunc+iremove)
                    parm(nfitfunc+iremove:nfitfunc+nfitfunc-1)=parm(nfitfunc+iremove+1:nfitfunc+nfitfunc)
                    parm(iremove:nfitfunc+nfitfunc-2)=parm(iremove+1:nfitfunc+nfitfunc-1)
                    nfitfunc=nfitfunc-1
                    nparm=nfitfunc*2
                    nremove=nremove+1
                end if
            end do
            if (nremove==0) then
                write(*,*) "No redundant fitting functions were found"
            else
                write(*,"(' Totally',i3,' redundant fitting functions have been eliminated')") nremove
            end if
            write(*,*)
        end if
        
        !Output fitting status
        if (info==1.or.info==2.or.info==3) then
            write(*,*) "Fitting has successfully finished!"
        else if (info==5) then
            write(*,"(a,i7)") " Warning: Convergence tolerance has not met while the maximum number of function calls has reached",maxcall
        else if (info==6.or.info==7) then
            write(*,*) "Error: Tolerance is too small, unable to reach the tolerance!"
        end if
        
        !Sort according to exponents from small to large
        if (isort==1) then
            write(*,*) "Sorting fitting functions according to their exponents..."
            forall(i=1:maxfitfunc) seqidx(i)=i
            call sortr8(parm(nfitfunc+1:nparm),list=seqidx(1:nfitfunc))
            parm(1:nfitfunc)=parm(seqidx(1:nfitfunc))
        end if
        
        !Check integral and scale
        rhoint=fitdensint(100,nparm,parm(1:nparm))
        write(*,"(/,' Integral of fitted density calculated using 100 points:',f14.8)") rhoint
        if (iscale==1) then !Scaling fitted coefficients to actual number of electrons
            write(*,"(' Fitted coefficients are scaled by',f16.8)") nelec/rhoint
            parm(1:nfitfunc)=parm(1:nfitfunc)/rhoint*nelec
        end if
        
        !Show final fitted parameters
        write(*,*)
        if (iscale==0) write(*,*) "Fitted parameters (a.u.):"
        if (iscale==1) write(*,*) "Fitted parameters (a.u.) after scaling:"
        write(*,*) "           Coefficient       Exponent"
        do ifunc=1,nfitfunc
            write(*,"(1x,a,i3,':',2(1PE16.6))") funclab(ifunctype),ifunc,parm(ifunc),parm(nfitfunc+ifunc)
        end do
        write(*,*)
        
        !Error statistics
        call calcfitdens_arr(npoint,nparm,parm(1:nparm),fitrho) !Calculate fitted density at fitting points using final (may be scaled) parameters
        write(*,"(' RMSE of fitting error at all points:',f20.6,' a.u.^2')") dsqrt(sum((fitrho-radrho)**2)/npoint)
	    pearsoncoeff=covarray(radrho,fitrho)/stddevarray(radrho)/stddevarray(fitrho)
	    write(*,"(2(a,f12.6))") " Pearson correlation coefficient r:",pearsoncoeff,"  r^2:",pearsoncoeff**2
        if (any(fitrho<0)) write(*,"(/,a)") " Warning: Fitted density at one or more fitting points is negative!"
        
        do while(.true.)
            write(*,*)
            write(*,*) "       -------------------- Quality check & Others --------------------"
            write(*,*) "-2 Export fitted function parameters to fitparm.txt in current folder"
            write(*,*) "-1 Print fitted function parameters again"
            write(*,*) "0 Return"
            write(*,*) "1 Print actual&fitted density and error at fitting points on screen"
            write(*,*) "2 Export actual&fitted density and error at fitting points to radfit.txt"
            write(*,*) "3 Visualize actual density and fitted density curves using logarithmic scaling"
            write(*,*) "4 Visualize actual density and fitted density curves using linear scaling"
            write(*,"(a)") " 5 Export fitted density from 0 to 10 Angstrom with double dense grid to fitdens.txt in current folder"
            write(*,*) "6 Check integral of fitted density"
            write(*,*) "7 Check fitted density at a given radial distance"
            read(*,*) isel2
            if (isel2==0) then
                exit
            else if (isel2==2) then
                open(10,file="fitparm.txt",status="replace")
                do ifunc=1,nfitfunc
                    write(10,"(2(1PE16.6))") parm(ifunc),parm(nfitfunc+ifunc)
                end do
                close(10)
                write(*,*) "Done! fitparm.txt has been exported in current folder"
                write(*,*) "Column 1: Coefficients (a.u.)"
                write(*,*) "Column 2: Exponents (a.u.)"
            else if (isel2==-1) then
                write(*,*) "Note: Units are in a.u."
                write(*,*) "           Coefficient       Exponent"
                do ifunc=1,nfitfunc
                    write(*,"(1x,a,i3,':',2(1PE16.6))") funclab(ifunctype),ifunc,parm(ifunc),parm(nfitfunc+ifunc)
                end do
            else if (isel2==1) then
                write(*,"(a)") " Radial distance (Angstrom), actual density (a.u.), &
                difference between fitted and actual density (a.u.) as well as relative difference"
                do ipt=1,npoint
                    write(*,"(' #',i5,'  r:',f8.5,'  rho:',f18.8,'  Diff:',f16.8,' (',f8.2,' %)')") &
                    ipt,radr(ipt)*b2a,radrho(ipt),fitrho(ipt)-radrho(ipt),(fitrho(ipt)-radrho(ipt))/radrho(ipt)*100
                end do
            else if (isel2==2) then
                open(10,file="radfit.txt",status="replace")
                do ipt=1,npoint
                    write(10,"(f8.4,3f20.10)") radr(ipt)*b2a,radrho(ipt),fitrho(ipt),fitrho(ipt)-radrho(ipt)
                end do
                close(10)
                write(*,*)
                write(*,*) "Data has been exported to radfit.txt in current folder. Content:"
                write(*,*) "Column 1: Radial distance (Angstrom)"
                write(*,*) "Column 2: Actual density (a.u.)"
                write(*,*) "Column 3: Fitted density (a.u.)"
                write(*,*) "Column 4: Fitting error (a.u.)"
            else if (isel2==3.or.isel2==4) then
				call METAFL('xwin')
				call window(100,0,1200,720)
				call SCRMOD('REVERSE')
				CALL PAGE(3000,1800)
				call disini
				call height(40)
				CALL HNAME(45)
				call hwfont
				call AXSLEN(2450,1400)
				call WINTIT("Click right mouse button to close")
				call ERRMOD("ALL","OFF")
				call AXSPOS(380,1550)
                !Set axis style
                CALL NAMDIS(40,'Y')
				CALL NAME('Radial distance (Angstrom)','X')
				CALL NAME('Electron density (a.u.)','Y')
				CALL TICKS(1,'XY') !1 tick between two labels
                CALL LABDIG(1,"X")
                if (isel2==3) then
                    call AXSSCL('log','Y')
                    Ymin=-5 !Start from 1E-5
                    Ymax=ceiling(log10(maxval(radrho)))
                    Ystep=1
                    call labels('log','Y')
				    CALL LABDIG(-1,"Y") !Do not show digit in Y
				    CALL GRAF(0D0,4D0,0D0,0.2D0, Ymin,Ymax,Ymin,Ystep) !Plot r=0~4 Angstrom
                else if (isel2==4) then
                    call AXSSCL('lin','Y')
                    Ymin=0
                    Ymax=min(max(maxval(radrho),maxval(fitrho)),2D0)
                    Ystep=(Ymax-Ymin)/10
                    call labels('float','Y')
				    CALL LABDIG(2,"Y")
				    CALL GRAF(0D0,2D0,0D0,0.2D0, Ymin,Ymax,Ymin,Ystep) !Plot r=0~2 Angstrom
                end if
                call SETRGB(0.8D0,0.8D0,0.8D0) !Shallow gray grid
			    call dash
			    call LINWID(1)
                CALL GRID(1,1)
                call solid
                !Draw legends and curves
                call legini(clegend,2,40)
				call legtit(' ') !Do now show legend title
				call frame(0) !No box around legend
				call linwid(5) !Use thick line
                call setcolor(5) !Black to plot actual density
                CALL LEGLIN(clegend,"Actual density",1)
				CALL CURVE(radr*b2a,radrho,npoint)
                call setcolor(3) !Blue to plot fitted density
                CALL LEGLIN(clegend,"Fitted density",2)
				CALL CURVE(radr*b2a,fitrho,npoint)
                call setcolor(5) !Black legend text
                call legend(clegend,7)
				call disfin
            else if (isel2==5) then
                open(10,file="fitdens.txt",status="replace")
                rtmp=0
                do while(.true.)
                    write(10,"(f8.4,f35.12)") rtmp*b2a,calcfitdens(rtmp,nparm,parm)
                    rtmp=rtmp+radstep/2
                    if (rtmp*b2a>10) exit
                end do
                close(10)
                write(*,*) "Done! fitdens.txt has been exported in current folder"
                write(*,*) "Column 1: Radial distance (Angstrom)"
                write(*,*) "Column 2: Fitted density (a.u.)"
            else if (isel2==6) then
                do ntmp=40,300,20
                    rhoint=fitdensint(ntmp,nparm,parm(1:nparm))
                    write(*,"(' Number of integration points:',i5,'    Integral:',f16.8)") ntmp,rhoint
                end do
            else if (isel2==7) then
                write(*,*) "Input radial distance in Angstrom, e.g. 3.8"
                read(*,*) rtmp
                write(*,"(' Fitted density is',1PE16.8,' a.u.')") calcfitdens(rtmp/b2a,nparm,parm)
            end if
        end do
        
        deallocate(radr,radrho,fiterr,fitrho)
        if (npoint_CB>0) deallocate(radr_CB,radw_CB,rho_CB)
    end if
end do

end subroutine


!!---- Input coefficients and exponents, return array containing difference between actual density and fitted density at positions in radr(:) array
!radr, radrho, ifunctype and ifittype in fitatmdens_mod are involved
!If iflag=-999, the returned "diff" will correspond to fitted density
!The arguments are required by lmdif1 routine in MINPACK
subroutine atmdens_fiterr(npoint,nparm,parm,diff,iflag)
use fitatmdens_mod
implicit real*8 (a-h,o-z)
integer npoint,nparm,iflag
real*8 :: parm(nparm),diff(npoint)
do ipt=1,npoint
    tmpval=0
    do ifunc=1,nfitfunc
        coeff=parm(ifunc)
        if (ifixexp==0) then
            expon=parm(nfitfunc+ifunc)
        else
            expon=exp_fit(ifunc)
        end if
        if (ifunctype==1) then !STO
            tmpval=tmpval+coeff*exp(-expon*radr(ipt))
        else if (ifunctype==2) then !GTF
            tmpval=tmpval+coeff*exp(-expon*radr(ipt)**2)
        end if
    end do
    if (iflag==-999) then
        diff(ipt)=tmpval
    else
        if (ifittype==1) then
            diff(ipt)=tmpval-radrho(ipt)
        else if (ifittype==2) then
            diff(ipt)=abs(tmpval-radrho(ipt))/radrho(ipt)
        else if (ifittype==3) then
            diff(ipt)=(tmpval-radrho(ipt))*radr(ipt)**2
        end if
    end if
end do
end subroutine

!!---- Input coefficients and exponents, return the array containing fitted density at positions in radr(:)
subroutine calcfitdens_arr(npoint,nparm,parm,fitrho)
integer npoint,nparm
real*8 parm(nparm),fitrho(npoint)
call atmdens_fiterr(npoint,nparm,parm,fitrho,-999)
end subroutine

!!---- Input coefficients and exponents, return value of fitted density at a given radial distance
real*8 function calcfitdens(r,nparm,parm)
use fitatmdens_mod
implicit real*8 (a-h,o-z)
real*8 parm(nparm),r
calcfitdens=0
do ifunc=1,nfitfunc
    if (ifunctype==1) then !STO
        calcfitdens=calcfitdens+parm(ifunc)*exp(-parm(nfitfunc+ifunc)*r)
    else if (ifunctype==2) then !GTF
        calcfitdens=calcfitdens+parm(ifunc)*exp(-parm(nfitfunc+ifunc)*r**2)
    end if
end do
end function

!!---- Return integral of fitted density based on input number of integration points and parameters
!ifunctype in fitatmdens_mod is involved
real*8 function fitdensint(nradpt,nparm,parm)
use defvar
use fitatmdens_mod
implicit real*8 (a-h,o-z)
integer nradpt,nparm
real*8 parm(nparm),radr_int(nradpt),radw_int(nradpt)
parmbk=1D0
do i=1,nradpt !Combine spherical point&weights with second kind Gauss-Chebyshev method for radial part
	radx=cos(i*pi/(nradpt+1))
	radr_int(i)=(1+radx)/(1-radx)*parmbk
	radw_int(i)=2*pi/(nradpt+1)*parmbk**3 *(1+radx)**2.5D0/(1-radx)**3.5D0 *4*pi
end do
fitdensint=0
do i=nradpt,1,-1 !From close to far
    rhotmp=0
    do ifunc=1,nfitfunc
        if (ifunctype==1) then !STO
            rhotmp=rhotmp+parm(ifunc)*exp(-parm(nfitfunc+ifunc)*radr_int(i))
        else if (ifunctype==2) then !GTF
            rhotmp=rhotmp+parm(ifunc)*exp(-parm(nfitfunc+ifunc)*radr_int(i)**2)
        end if
    end do
    fitdensint=fitdensint+rhotmp*radw_int(i)
end do
end function




!!------------ Visualize (hyper)polarizability via unit sphere and vector representations
subroutine vis_hypol
use util
use defvar
implicit real*8 (a-h,o-z)
real*8 :: arrowscl=0.015D0,arrowrad=0.025D0,arrowradvec=0.15D0,arrowsclvec=0.05D0
real*8 alpha(3,3),beta(3,3,3),gamma(3,3,3,3),vec(3),vecrep(3),univec(3),ten2(3,3),ten3(3,3,3)
real*8,allocatable :: ptxyz(:,:),allvec(:,:)
character c200tmp*200,c80tmp*80,name*10
integer :: numpt=600,icolorarrow=1,ispecmaxlen=0

if (allocated(distmat)) then !Input file contains atom information
    sphererad=maxval(distmat)/2*b2a*1.6D0
else
    sphererad=2D0
end if

do while(.true.)
    write(*,*)
    write(*,*) "       --------------- Visualizing (hyper)polarizability ---------------"
    if (ispecmaxlen==0) write(*,*) "-8 Toggle making longest arrow on sphere has specific length, current: No"
    if (ispecmaxlen==1) write(*,*) "-8 Toggle making longest arrow on sphere has specific length, current: Yes"
    if (icolorarrow==0) write(*,*) "-7 Toggle coloring arrows on sphere, current: No"
    if (icolorarrow==1) write(*,*) "-7 Toggle coloring arrows on sphere, current: Yes"
    write(*,"(a,f9.6)") " -6 Set radius for the arrow of vector rep., current:",arrowradvec
    write(*,"(a,f9.6)") " -5 Set length scale factor for the arrow of vector rep., current:",arrowsclvec
    write(*,"(a,f9.6)") " -4 Set radius for the arrows on sphere, current:",arrowrad
    write(*,"(a,f9.6)") " -3 Set length scale factor for the arrows on sphere, current:",arrowscl
    write(*,"(a,i6)") " -2 Set number of points on the sphere, current:",numpt
    write(*,"(a,f8.3,' Angstrom')") " -1 Set sphere radius, current:",sphererad
    write(*,*) "0 Return"
    write(*,*) "1 Do analysis for polarizability (alpha)"
    write(*,*) "2 Do analysis for first-order hyperpolarizability (beta)"
    write(*,*) "3 Do analysis for second-order hyperpolarizability (gamma)"
    read(*,*) isel
    
    if (isel==-8) then
        if (ispecmaxlen==1) then
            ispecmaxlen=0
        else
            ispecmaxlen=1
        end if
    else if (isel==-7) then
        if (icolorarrow==1) then
            icolorarrow=0
        else
            icolorarrow=1
        end if
    else if (isel==-6) then
        write(*,*) "Input radius of the arrow, e.g. 0.25"
        read(*,*) arrowradvec
    else if (isel==-5) then
        write(*,*) "Input scale factor for the arrow, e.g. 0.05"
        read(*,*) arrowsclvec
    else if (isel==-4) then
        write(*,*) "Input radius of the arrows, e.g. 0.05"
        read(*,*) arrowrad
    else if (isel==-3) then
        write(*,*) "Input scale factor for the arrows, e.g. 0.15"
        read(*,*) arrowscl
    else if (isel==-2) then
        write(*,*) "Input expected number of points on the sphere, e.g. 800"
        write(*,"(a)") "  You can also input negative value to specify density of points, e.g. -8.5 means the density is 8.5 points per Angstrom^2"
        write(*,*) "Note: The actual number will be automatically maginally adjusted"
        read(*,*) numpt
        if (numpt<0) numpt=nint(4*sphererad**2*abs(numpt))
    else if (isel==-1) then
        write(*,*) "Input radius of sphere (in Angstrom), e.g. 1.5"
        read(*,*) sphererad
    else if (isel==0) then
        exit
    
    else if (isel==1.or.isel==2.or.isel==3) then
        if (isel==1) name="alpha"
        if (isel==2) name="beta"
        if (isel==3) name="gamma"
        c200tmp=trim(name)//".txt"
	    inquire(file=c200tmp,exist=alive)
        if (alive) then
            write(*,"(a)") " Since "//trim(c200tmp)//" can be found in current folder, "//trim(name)//" tensor will be directly loaded from it"
        else
            write(*,*) "Input path of the file containing full "//trim(name)//" tensor, e.g. C:\yohane.txt"
            do while(.true.)
                read(*,"(a)") c200tmp
	            inquire(file=c200tmp,exist=alive)
	            if (alive) exit
	            write(*,*) "Cannot find the file, input again!"
            end do
        end if
        write(*,*) "Loading "//trim(name)//" tensor from "//trim(c200tmp)
        open(10,file=c200tmp,status="old")
        if (isel==1) read(10,*) ((alpha(i,j),j=1,3),i=1,3)
        if (isel==2) read(10,*) (((beta(i,j,k),k=1,3),j=1,3),i=1,3)
        if (isel==3) read(10,*) ((((gamma(i,j,k,l),l=1,3),k=1,3),j=1,3),i=1,3)
        close(10)
        allocate(ptxyz(3,numpt),allvec(3,numpt))
        
        call unitspherept(ptxyz,numpt) !The inputted numpt will be automatically adjusted by this routine
        write(*,"(' Actual number of points on unit sphere:',i6)") numpt
        ptxyz=ptxyz*sphererad
        
        write(*,*) "Calculating data points on the sphere ..."
        do ipt=1,numpt
            tmpnorm=dsqrt(sum(ptxyz(:,ipt)**2))
            univec=ptxyz(:,ipt)/tmpnorm
            if (isel==1) then !alpha
                do i=1,3
                    allvec(i,ipt)=sum(alpha(i,:)*univec(:))
                end do
            else if (isel==2) then !beta
                do i=1,3
                    do j=1,3
                        ten2(i,j)=sum(beta(i,j,:)*univec(:))
                    end do
                end do
                do i=1,3
                    allvec(i,ipt)=sum(ten2(i,:)*univec(:))
                end do
            else if (isel==3) then !gamma
                do i=1,3
                    do j=1,3
                        do k=1,3
                            ten3(i,j,k)=sum(gamma(i,j,k,:)*univec(:))
                        end do
                    end do
                end do
                do i=1,3
                    do j=1,3
                        ten2(i,j)=sum(ten3(i,j,:)*univec(:))
                    end do
                end do
                do i=1,3
                    allvec(i,ipt)=sum(ten2(i,:)*univec(:))
                end do
            end if
        end do
        allvec=allvec*arrowscl
        
        valmax=0
        valmin=1E20
        do ipt=1,numpt
            tmp=dsqrt(sum(allvec(:,ipt)**2))
            if (tmp>valmax) valmax=tmp
            if (tmp<valmin) valmin=tmp
        end do
        write(*,"(' Minimal arrow length after scaling:',f12.3)") valmin
        write(*,"(' Maximal arrow length after scaling:',f12.3)") valmax
        
        if (ispecmaxlen==1) then
            write(*,*)
            write(*,*) "Input expected length of longest arrow on the sphere, e.g. 2.5"
            read(*,*) tmpmax
            sclf=tmpmax/valmax
            allvec=allvec*sclf
            valmax=valmax*sclf
            valmin=valmin*sclf
            write(*,"(' Current arrows on sphere have been further scaled by',f12.6)") sclf
        end if
        
        c80tmp=trim(name)//".tcl"
        write(*,*) "Outputting "//trim(c80tmp)//" ..."
        open(10,file=c80tmp,status="replace")
        write(10,"(a)") "color Display Background white"
        if (icolorarrow==0) then
            write(10,"(a)") "draw color white"
            do ipt=1,numpt
                call drawVMDarrow(10,ptxyz(:,ipt),allvec(:,ipt),arrowrad)
            end do
        else if (icolorarrow==1) then
            call writeVMD_BWR(10)
            do ipt=1,numpt
                tmp=dsqrt(sum(allvec(:,ipt)**2))
                !idcolor=nint(tmp/valmax*1000) !Using zero as color lower limit
                idcolor=nint((tmp-valmin)/(valmax-valmin)*1000)
                if (idcolor==0) idcolor=1
                write(10,"(a,i5)") "draw color",idcolor+50
                call drawVMDarrow(10,ptxyz(:,ipt),allvec(:,ipt),arrowrad)
            end do
        end if
        close(10)
        write(*,"(a,/)") " Done! "//trim(c80tmp)//" has been generated in current folder, it is a VMD plotting script, &
        you can run ""source "//trim(c80tmp)//""" in VMD console window to plot the map"
        deallocate(ptxyz,allvec)
        
        !Output vector representation
        vecrep=0
        if (isel==1) then
            do i=1,3
                do j=1,3
                    vecrep(i)=vecrep(i)+alpha(i,j)
                end do
            end do
            write(*,"(' Alpha_X:',1PE14.6,'   Alpha_Y:',1PE14.6,'   Alpha_Z:',1PE14.6,' a.u.')") vecrep(:)
        else if (isel==2) then
            do i=1,3
		        do j=1,3
			        vecrep(i)=vecrep(i)+beta(i,j,j)+beta(j,j,i)+beta(j,i,j)
		        end do
            end do
            vecrep=vecrep/3
            write(*,"(' Beta_X:',1PE14.6,'   Beta_Y:',1PE14.6,'   Beta_Z:',1PE14.6,' a.u.')") vecrep(:)
        else if (isel==3) then
            do i=1,3
	            do j=1,3
		            vecrep(i)=vecrep(i)+gamma(i,j,j,i)+gamma(i,j,i,j)+gamma(i,i,j,j)
	            end do
            end do
            vecrep=vecrep/15
            write(*,"(' Gamma_X:',1PE14.6,'   Gamma_Y:',1PE14.6,'   Gamma_Z:',1PE14.6,' a.u.')") vecrep(:)
        end if
        if (isel==2) then !For alpha and gamma, the vector representation is useless
            open(10,file=trim(name)//"_vec.tcl",status="replace")
            vec=0 !Origin of the arrow is (0,0,0)
            write(10,"(a)") "draw color lime"
            call drawVMDarrow(10,vec,vecrep*arrowsclvec,arrowradvec)
            close(10)
            write(*,"(1x,a)") trim(name)//"_vec.tcl has been generated in current folder, it contains VMD command to plot "//trim(name)//" &
            tensor via vector representation"
        end if
    end if
end do

end subroutine

!!--------- Write command for drawing arrow into VMD plotting script according to inputted coordinate and arrow vector
subroutine drawVMDarrow(ifileid,ptxyz,vec,arrowrad)
integer ifileid
real*8 ptxyz(3),vec(3),arrowrad
conerad=2.5D0*arrowrad !cone radius
conepos=0.65D0
write(ifileid,"( 'draw cylinder {',3f8.3,'} {',3f8.3,'} radius',f5.2,' filled yes resolution 20' )") ptxyz(:),ptxyz(:)+conepos*vec(:),arrowrad
write(ifileid,"( 'draw cone {',3f8.3,'} {',3f8.3,'} radius',f5.2,' resolution 20' )") ptxyz(:)+conepos*vec(:),ptxyz(:)+vec(:),conerad
end subroutine

!!-------- Define 1000 customized colors (index from 51 to 1050) corresponding to variation of blue-white-red
!The reason of using 50~1050: (1) 0~32 are built-in colors (2) index >=1057 is unsupported by VMD
subroutine writeVMD_BWR(ifileid)
write(ifileid,"(a)") "set j 0                                         "
write(ifileid,"(a)") "for {set i 1} {$i<=500} {incr i} {              "
write(ifileid,"(a)") "incr j                                          "
write(ifileid,"(a)") "set red [expr double($j)/500]                       "
write(ifileid,"(a)") "set green [expr double($j)/500]                     "
write(ifileid,"(a)") "set blue 1                                      "
write(ifileid,"(a)") "color change rgb [expr $i+50] $red $green $blue"
write(ifileid,"(a)") "}                                               "
write(ifileid,"(a)") "set j 0                                         "
write(ifileid,"(a)") "for {set i 501} {$i<=1000} {incr i} {            "
write(ifileid,"(a)") "incr j                                          "
write(ifileid,"(a)") "set red 1                                       "
write(ifileid,"(a)") "set green [expr double(500-$j)/500]               "
write(ifileid,"(a)") "set blue [expr double(500-$j)/500]                "
write(ifileid,"(a)") "color change rgb [expr $i+50] $red $green $blue"
write(ifileid,"(a)") "}                                               "
end subroutine







!!----------- Simulating scanning tunneling microscope (STM) image
!Main ref: https://en.wikipedia.org/wiki/Scanning_tunneling_microscope
!Partial ref: Tersoff and Hamann, Theory of the scanning tunneling microscope, PRB, 31, 805 (1985)
subroutine STM
use defvar
use GUI
use function
use util
implicit real*8 (a-h,o-z)
integer :: imode=2
real*8 :: bias=0
character c80tmp*80
real*8,external :: LDOS_STM

if (allocated(cubmat)) deallocate(cubmat)
nx=200;ny=200
!Set initial range, in Bohr
orgx=minval(a%x)-3
endx=maxval(a%x)+3
orgy=minval(a%y)-3
endy=maxval(a%y)+3
orgz=maxval(a%z)+0.7D0/b2a !Scan Z=0.7~2.5 Angstrom with respect to top atom
endz=orgz+1.8D0/b2a

if (.not.allocated(b)) then
    write(*,"(a)") " Error: In order to use this function, the input file must at least contain GTF information! See Section 2.5 of manual for detail."
    write(*,*) "Input ENTER button to return"
    read(*,*)
    return
end if
if (wfntype==3.or.wfntype==4) then
    write(*,"(a)") " Error: This function does not support multiconfiguration state wavefunction!"
    write(*,*) "Input ENTER button to return"
    read(*,*)
    return
end if

if (allocated(CObasa)) then
    call getHOMOidx
    Ef=(MOene(idxHOMO)+MOene(idxHOMO+1))/2
    bias=MOene(idxHOMO)-Ef
    
    if (wfntype==0) then
        write(*,*) "Note: The default Fermi level has been set to average of E(HOMO) and E(LUMO)"
    else
        write(*,*) "Note: The default Fermi level has been set to average of E(HOMO) and E(LUMO) of alpha spin. In this case, &
        the result will be problematic if bias voltage is set to positive value (electron flows from tip to sample)"
    end if
    write(*,"(a)") " The default bias voltage has been set to the difference between E(HOMO) and Fermi level, &
    therefore under default setting only HOMO will be imaged"
else
    Ef=maxval(MOene)
    write(*,*) "Note: The default Fermi level has been set to HOMO"
    write(*,"(a)") " Note: Since there is no unoccupied MO, the bias voltage must be set to negative value (electron flows from sample to tip)"
end if

do while(.true.)
    write(*,*)
    write(*,*) " ----------- Simulating scanning tunneling microscope (STM) image -----------"
    write(*,*) "-1 Return"
    if (imode==1) write(*,*) "0 Calculating grid data of tunneling current!"
    if (imode==2) write(*,*) "0 Calculating tunneling current on the plane!"
    if (imode==1) write(*,*) "1 Toggle mode of STM image, current: Constant current"
    if (imode==2) write(*,*) "1 Toggle mode of STM image, current: Constant distance"
    write(*,"(a,f10.3,' V')") " 2 Set bias voltage, current:",bias*au2eV
    write(*,"(a,f10.3,' eV')") " 3 Set Fermi level, current:",Ef*au2eV
    !write(*,"(a,f6.3,' eV')") " 3 Set FWHM for Gaussian broadening, current:",STM_FWHM
    if (imode==1) write(*,"(a,3i5)") " 4 Set number of grid points in X,Y,Z, current:",nx,ny,nz
    if (imode==2) write(*,"(a,3i5)") " 4 Set number of grid points in X and Y, current:",nx,ny
    write(*,"(a,f8.3,a,f8.3,a)") " 5 Set range in X direction, current: From",orgx*b2a," to",endx*b2a," Angstrom"
    write(*,"(a,f8.3,a,f8.3,a)") " 6 Set range in Y direction, current: From",orgy*b2a," to",endy*b2a," Angstrom"
    if (imode==1) write(*,"(a,f8.3,a,f8.3,a)") " 7 Set range in Z direction, current: From",orgz*b2a," to",endz*b2a," Angstrom"
    if (imode==2) write(*,"(a,f8.3,a)") " 7 Set Z coordinate of the XY plane, current:",orgz*b2a," Angstrom"
    read(*,*) isel
    
    if (isel==-1) then
        return
    else if (isel==1) then
        if (imode==1) then
            imode=2
            nx=200;ny=200
        else if (imode==2) then
            imode=1
            nx=150;ny=150;nz=80
        end if
    else if (isel==2) then
        write(*,*) "Input bias voltage in V, e.g. -3.5"
        write(*,"(a)") " Note: Negative value lets electron flow from sample to tip, thus density of occupied MOs are imaged. &
        Positive value lets electron flow from tip to sample, thus unoccupied MOs are imaged."
        read(*,*) bias
        bias=bias/au2eV
    else if (isel==3) then
        write(*,*) "Input Fermi energy in eV, e.g. -5.82"
        read(*,*) Ef
        Ef=Ef/au2eV
    else if (isel==4) then
        if (imode==1) then
            write(*,*) "1 Coarse grid (100*100*40)"
            write(*,*) "2 Medium grid (150*150*70)"
            write(*,*) "3 Fine grid (200*200*100)"
            write(*,*) "or, directly input number of grid points in X,Y,Z, e.g. 80,80,30"
            read(*,"(a)") c80tmp
            read(c80tmp,*,iostat=ierror) nx,ny,nz
            if (ierror/=0) then
                read(c80tmp,*) isel2
                if (isel2==1) then
                    nx=100;ny=100;nz=40
                else if (isel2==2) then
                    nx=150;ny=150;nz=70
                else if (isel2==3) then
                    nx=200;ny=200;nz=100
                end if
            end if
        else if (imode==2) then
            write(*,*) "Input number of grid points in X and Y, e.g. 80,80"
            read(*,*) nx,ny
        end if
    else if (isel==5) then
        write(*,*) "Input lower and upper limit of X in Angstrom, e.g. -5.8,6.4"
        write(*,"(a)") " If you only input a number, it will be employed as extension distance (in Angstrom) in X direction to properly determine the X range"
        read(*,"(a)") c80tmp
        read(c80tmp,*,iostat=ierror) orgx,endx
        if (ierror==0) then
            orgx=orgx/b2a
            endx=endx/b2a
        else
            read(c80tmp,*) ext
            orgx=minval(a%x)-ext/b2a
            endx=maxval(a%x)+ext/b2a
        end if
    else if (isel==6) then
        write(*,*) "Input lower and upper limit of Y in Angstrom, e.g. -5.8,6.4"
        write(*,"(a)") " If you only input a number, it will be employed as extension distance (in Angstrom) in Y direction to properly determine the X range"
        read(*,"(a)") c80tmp
        read(c80tmp,*,iostat=ierror) orgy,endy
        if (ierror==0) then
            orgy=orgy/b2a
            endy=endy/b2a
        else
            read(c80tmp,*) ext
            orgy=minval(a%y)-ext/b2a
            endy=maxval(a%y)+ext/b2a
        end if
    else if (isel==7) then
        if (imode==1) then
            write(*,*) "Input lower and upper limit of Z in Angstrom, e.g. 0,2.5"
            read(*,*) orgz,endz
            orgz=orgz/b2a
            endz=endz/b2a
        else if (imode==2) then
            write(*,*) "Input Z coordinate of the XY plane in Angstrom, e.g. 2.2"
            read(*,*) orgz
            orgz=orgz/b2a
        end if
    
    else if (isel==0) then !Start calculation !!!
        
        !Show which MOs will be taken into account
        if (bias<=0) then
            Elow=Ef+bias
            Ehigh=Ef
        else
            Elow=Ef
            Ehigh=Ef+bias
        end if
        write(*,"(/,' Lower limit of MO energy considered in the calculation:',f12.3,' eV')") Elow*au2eV
        write(*,"(' Upper limit of MO energy considered in the calculation:',f12.3,' eV')") Ehigh*au2eV
        nconsider=0
        write(*,*) "The MOs taken into account in the current STM simulation:"
        do imo=1,nmo
            if (bias<=0) then !Electron flows from sample to tip
                if (MOocc(imo)>0.and.MOene(imo)>=Elow.and.MOene(imo)<=Ehigh) then
                    write(*,"(' MO',i6,'   Occ=',f6.3,'   Energy=',f12.4,' eV   Type: ',a)") imo,MOocc(imo),MOene(imo)*au2eV,trim(orbtypename(MOtype(imo)))
                    nconsider=nconsider+1
                end if
            else if (bias>0) then !Electron flows from tip to sample
                if (MOocc(imo)==0.and.MOene(imo)>=Elow.and.MOene(imo)<=Ehigh) then
                    write(*,"(' MO',i6,'   Occ=',f6.3,'   Energy=',f12.4,' eV   Type: ',a)") imo,MOocc(imo),MOene(imo)*au2eV,trim(orbtypename(MOtype(imo)))
                    nconsider=nconsider+1
                end if
            end if
        end do
        if (nconsider==0) then
            write(*,*) "None. Therefore the calculation is canceled"
            cycle
        else
            write(*,"(' Totally',i4,' MOs are taken into account',/)") nconsider
        end if
        
        dx=(endx-orgx)/(nx-1)
        dy=(endy-orgy)/(ny-1)
        !Prepare settings for plane plot, they are utilized by "drawplane" routine via "planemap_interface" routine
        call gencontour(0,0D0,0D0,0) !Generate contour lines
        ngridnum1=nx
        ngridnum2=ny
        plesel=1 !XY plane
        disshowlabel=100 !Very broad threshold to make sure showing all atom labels
        ilenunit2D=2 !Use Angstrom
        planestpx=(endx-orgx)*b2a/7
        planestpy=(endy-orgy)*b2a/7
        iclrtrans=6 !Grey transition in color-filled map
        !ibond_on_plane=1 !Show bond on map
        iatom_on_plane=1 !Show atom label on map
        numdigz=4
        
        if (imode==1) then !Constant current STM
            dz=(endz-orgz)/(nz-1)
	        write(*,"(' Grid spacings in X,Y,Z are',3f12.6,' Bohr')") dx,dy,dz
            write(*,*) "Calculating, please wait..."
            allocate(cubmat(nx,ny,nz))
            call walltime(iwalltime1)
            ifinish=0
            !$OMP PARALLEL DO SHARED(cubmat,ifinish) PRIVATE(ix,xpos,iy,ypos,iz,zpos) schedule(dynamic) NUM_THREADS(nthreads)
            do ix=1,nx
                xpos=orgx+(ix-1)*dx
                do iy=1,ny
                    ypos=orgy+(iy-1)*dy
                    do iz=1,nz
                        zpos=orgz+(iz-1)*dz
                        cubmat(ix,iy,iz)=LDOS_STM(xpos,ypos,zpos,Ef,bias)
                    end do
                end do
                ifinish=ifinish+1
                call showprog(ifinish,nx)
            end do
            !$OMP END PARALLEL DO
            call walltime(iwalltime2)
            write(*,"(' Calculation took up time',i10,' s')") iwalltime2-iwalltime1
            valmax=maxval(cubmat)
            write(*,"(' Maximal value (LDOS) is',f12.6,' a.u.')") valmax
            sur_value=valmax/2 !For isosurface plot
            
            do while(.true.)
                write(*,*)
                write(*,*) "          ------------------- Post-processing menu -------------------"
                write(*,*) "0 Return"
                write(*,*) "1 Visualize isosurface of current"
                write(*,*) "2 Export grid data of current as STM.cub in current folder"
                write(*,*) "3 Calculate and visualize constant current STM image"
                read(*,*) isel2
                if (isel2==0) then
                    deallocate(cubmat)
                    exit
                else if (isel2==1) then
                    call drawisosurgui(1)
                else if (isel2==2) then
                    open(10,file="STM.cub",status="replace")
                    gridvec1=(/ dx,0D0,0D0 /)
                    gridvec2=(/ 0D0,dy,0D0 /)
                    gridvec3=(/ 0D0,0D0,dz /)
			        call outcube(cubmat,nx,ny,nz,orgx,orgy,orgz,gridvec1,gridvec2,gridvec3,10)
                    close(10)
                    write(*,*) "Exporting finished!"
                else if (isel2==3) then
                    write(*,*) "Input constant current value, e.g. 0.004"
                    write(*,"(' Note: The value should be larger than 0 and smaller than',f10.6)") valmax
                    read(*,*) constcurr
                    write(*,*) "Calculating constant current map, please wait..."
                    allocate(planemat(nx,ny))
                    ifinish=0
                    do ix=1,nx
                        xpos=orgx+(ix-1)*dx
                        do iy=1,ny
                            ypos=orgy+(iy-1)*dy
                            !Gradually decrease tip from high z to low z, until isosurface is encountered, and using linear interpolation to determine its position
                            do iz=nz,2,-1
                                zpos=orgz+(iz-1)*dz
                                zposnext=orgz+(iz-2)*dz
                                val=cubmat(ix,iy,iz)
                                valnext=cubmat(ix,iy,iz-1)
                                if (val<constcurr.and.valnext>constcurr) then
                                    zdiffmin=zposnext+(constcurr-valnext)/(val-valnext)*dz
                                    exit
                                end if
                            end do
                            if (iz==1) zdiffmin=orgz !Failed to determine isosurface position
                            planemat(ix,iy)=zdiffmin*b2a
                        end do
                    end do
                    
                    write(*,"(' Minimal Z is',f12.6,' Angstrom')") minval(planemat)
                    write(*,"(' Maximal Z is',f12.6,' Angstrom')") maxval(planemat)
                    clrlow=minval(planemat)*0.99999D0;clrhigh=maxval(planemat)*1.00001D0 !Avoid a few points marginally exceed upper limit
                    planestpz=(clrhigh-clrlow)/10
                    orgz2D=0 !In fact this is meaningless for present case, but should be initialized...
                    call gencontour(2,clrlow,clrhigh,10) !Generate contour lines evenly covering lower and upper limits
                    call planemap_interface("constant current STM","STM",orgx,endx,orgy,endy,clrlow,clrhigh)
                    deallocate(planemat)
                end if
            end do
        
        else if (imode==2) then !Constant height STM
	        write(*,"(' Grid spacings in X and Y are',2f12.6,' Bohr')") dx,dy
            write(*,*) "Calculating, please wait..."
            allocate(planemat(nx,ny))
            !$OMP PARALLEL DO SHARED(planemat) PRIVATE(ix,xpos,iy,ypos) schedule(dynamic) NUM_THREADS(nthreads)
            do ix=1,nx
                xpos=orgx+(ix-1)*dx
                do iy=1,ny
                    ypos=orgy+(iy-1)*dy
                    planemat(ix,iy)=LDOS_STM(xpos,ypos,orgz,Ef,bias)
                end do
            end do
            !$OMP END PARALLEL DO
            write(*,"(' Maximal value (LDOS) is',f12.6,' a.u.')") maxval(planemat)
            clrlow=0D0
            clrhigh=maxval(planemat)
            planestpz=(clrhigh-clrlow)/10
            orgz2D=orgz
            call planemap_interface("constant height STM","STM",orgx,endx,orgy,endy,clrlow,clrhigh)
            deallocate(planemat)
        end if
        
    end if
end do

end subroutine


!!-------- Return LDOS for MOs from Ef-bias to Ef at x,y,z (Bohr). Invoked by subroutine STM
real*8 function LDOS_STM(x,y,z,Ef,bias)
use defvar
use function
implicit real*8 (a-h,o-z)
real*8 x,y,z,Ef,bias,wfnval(nmo)

call orbderv(1,1,nmo,x,y,z,wfnval)
if (bias<=0) then
    Elow=Ef+bias
    Ehigh=Ef
else
    Elow=Ef
    Ehigh=Ef+bias
end if

LDOS_STM=0
do imo=1,nmo
    if (MOtype(imo)==0) then !Spatial orbital
        ndup=2
    else !Spin orbital
        ndup=1
    end if
    if (bias<=0) then !Electron flows from sample to tip
        if (MOocc(imo)>0.and.MOene(imo)>=Elow.and.MOene(imo)<=Ehigh) LDOS_STM=LDOS_STM+ndup*wfnval(imo)**2
    else if (bias>0) then !Electron flows from tip to sample
        if (MOocc(imo)==0.and.MOene(imo)>=Elow.and.MOene(imo)<=Ehigh) LDOS_STM=LDOS_STM+ndup*wfnval(imo)**2
    end if
end do
end function





!!--------------- Calculate electric dipole, quadrupole and octopole moments of present system based on analytic integrals
subroutine calc_multipole
use defvar
use util
implicit real*8 (a-h,o-z)

if (ispecial==1) then
    xnucdip=0
    ynucdip=0
    znucdip=0
    do iatm=1,ncenter
        xnucdip=xnucdip+a(iatm)%x*a(iatm)%charge
        ynucdip=ynucdip+a(iatm)%y*a(iatm)%charge
        znucdip=znucdip+a(iatm)%z*a(iatm)%charge
    end do
    write(*,"(/,' Dipole moment from nuclear charges (a.u.): ',3f11.6)") xnucdip,ynucdip,znucdip
    write(*,"(a)") " Because ispecial=1, now displacing nuclear coordinates to make their contributions to dipole moment vanishing"
    sumnuc=sum(a%charge)
    do iatm=1,ncenter
        a(iatm)%x=a(iatm)%x-xnucdip/sumnuc
        a(iatm)%y=a(iatm)%y-ynucdip/sumnuc
        a(iatm)%z=a(iatm)%z-znucdip/sumnuc
    end do
    write(*,*) "Done!"
end if

if (allocated(CObasa)) then
    write(*,*) "Calculating electric dipole moment integral matrix..."
    call genDbas_curr
    write(*,*) "Calculating electric quadruple and octopole moment integral matrix..."
    call genMultipolebas_curr

    xinttot=sum(Dbas(1,:,:)*Ptot(:,:))
    yinttot=sum(Dbas(2,:,:)*Ptot(:,:))
    zinttot=sum(Dbas(3,:,:)*Ptot(:,:))

    xxinttot=sum(Quadbas(1,:,:)*Ptot(:,:))
    yyinttot=sum(Quadbas(2,:,:)*Ptot(:,:))
    zzinttot=sum(Quadbas(3,:,:)*Ptot(:,:))
    xyinttot=sum(Quadbas(4,:,:)*Ptot(:,:))
    yzinttot=sum(Quadbas(5,:,:)*Ptot(:,:))
    xzinttot=sum(Quadbas(6,:,:)*Ptot(:,:))

    xxxinttot=sum(Octobas(1,:,:)*Ptot(:,:))
    yyyinttot=sum(Octobas(2,:,:)*Ptot(:,:))
    zzzinttot=sum(Octobas(3,:,:)*Ptot(:,:))
    yzzinttot=sum(Octobas(4,:,:)*Ptot(:,:))
    xzzinttot=sum(Octobas(5,:,:)*Ptot(:,:))
    xxzinttot=sum(Octobas(6,:,:)*Ptot(:,:))
    yyzinttot=sum(Octobas(7,:,:)*Ptot(:,:))
    xxyinttot=sum(Octobas(8,:,:)*Ptot(:,:))
    xyyinttot=sum(Octobas(9,:,:)*Ptot(:,:))
    xyzinttot=sum(Octobas(10,:,:)*Ptot(:,:))
    
else if (allocated(b)) then
    write(*,*) "Calculating density matrix based on GTFs..."
    call genPprim
    write(*,*) "Calculating electric dipole moment integral matrix..."
    call genDprim
    write(*,*) "Calculating electric quadruple and octopole moment integral matrix..."
    call genMultipoleprim

    xinttot=sum(Dprim(1,:,:)*Ptot_prim(:,:))
    yinttot=sum(Dprim(2,:,:)*Ptot_prim(:,:))
    zinttot=sum(Dprim(3,:,:)*Ptot_prim(:,:))

    xxinttot=sum(Quadprim(1,:,:)*Ptot_prim(:,:))
    yyinttot=sum(Quadprim(2,:,:)*Ptot_prim(:,:))
    zzinttot=sum(Quadprim(3,:,:)*Ptot_prim(:,:))
    xyinttot=sum(Quadprim(4,:,:)*Ptot_prim(:,:))
    yzinttot=sum(Quadprim(5,:,:)*Ptot_prim(:,:))
    xzinttot=sum(Quadprim(6,:,:)*Ptot_prim(:,:))

    xxxinttot=sum(Octoprim(1,:,:)*Ptot_prim(:,:))
    yyyinttot=sum(Octoprim(2,:,:)*Ptot_prim(:,:))
    zzzinttot=sum(Octoprim(3,:,:)*Ptot_prim(:,:))
    yzzinttot=sum(Octoprim(4,:,:)*Ptot_prim(:,:))
    xzzinttot=sum(Octoprim(5,:,:)*Ptot_prim(:,:))
    xxzinttot=sum(Octoprim(6,:,:)*Ptot_prim(:,:))
    yyzinttot=sum(Octoprim(7,:,:)*Ptot_prim(:,:))
    xxyinttot=sum(Octoprim(8,:,:)*Ptot_prim(:,:))
    xyyinttot=sum(Octoprim(9,:,:)*Ptot_prim(:,:))
    xyzinttot=sum(Octoprim(10,:,:)*Ptot_prim(:,:))

else
    write(*,*) "Error: The current input file does not contain wavefunction information!"
    write(*,*) "Press ENTER button to return"
    read(*,*)
    return
end if

!Combine nuclear contribution and electron contribution to obtain multiple moments
xnucdip=0
ynucdip=0
znucdip=0
do iatm=1,ncenter
    xnucdip=xnucdip+a(iatm)%x*a(iatm)%charge
    ynucdip=ynucdip+a(iatm)%y*a(iatm)%charge
    znucdip=znucdip+a(iatm)%z*a(iatm)%charge
    xxinttot=xxinttot+a(iatm)%x*a(iatm)%x*a(iatm)%charge
    yyinttot=yyinttot+a(iatm)%y*a(iatm)%y*a(iatm)%charge
    zzinttot=zzinttot+a(iatm)%z*a(iatm)%z*a(iatm)%charge
    xyinttot=xyinttot+a(iatm)%x*a(iatm)%y*a(iatm)%charge
    yzinttot=yzinttot+a(iatm)%y*a(iatm)%z*a(iatm)%charge
    xzinttot=xzinttot+a(iatm)%x*a(iatm)%z*a(iatm)%charge
	xxxinttot=xxxinttot+a(iatm)%x*a(iatm)%x*a(iatm)%x*a(iatm)%charge
	yyyinttot=yyyinttot+a(iatm)%y*a(iatm)%y*a(iatm)%y*a(iatm)%charge
	zzzinttot=zzzinttot+a(iatm)%z*a(iatm)%z*a(iatm)%z*a(iatm)%charge
	yzzinttot=yzzinttot+a(iatm)%y*a(iatm)%z*a(iatm)%z*a(iatm)%charge
	xzzinttot=xzzinttot+a(iatm)%x*a(iatm)%z*a(iatm)%z*a(iatm)%charge
	xxzinttot=xxzinttot+a(iatm)%x*a(iatm)%x*a(iatm)%z*a(iatm)%charge
	yyzinttot=yyzinttot+a(iatm)%y*a(iatm)%y*a(iatm)%z*a(iatm)%charge
	xxyinttot=xxyinttot+a(iatm)%x*a(iatm)%x*a(iatm)%y*a(iatm)%charge
	xyyinttot=xyyinttot+a(iatm)%x*a(iatm)%y*a(iatm)%y*a(iatm)%charge
	xyzinttot=xyzinttot+a(iatm)%x*a(iatm)%y*a(iatm)%z*a(iatm)%charge
end do
rrinttot=xxinttot+yyinttot+zzinttot
rrxinttot=xxxinttot+xyyinttot+xzzinttot
rryinttot=xxyinttot+yyyinttot+yzzinttot
rrzinttot=xxzinttot+yyzinttot+zzzinttot

write(*,"(/,' Dipole moment from nuclear charges (a.u.): ',3f11.6)") xnucdip,ynucdip,znucdip
write(*,"(' Dipole moment from electrons (a.u.):       ',3f11.6)") xinttot,yinttot,zinttot
xinttot=xinttot+xnucdip
yinttot=yinttot+ynucdip
zinttot=zinttot+znucdip
write(*,*)
write(*,"(' Dipole moment (a.u.): ',3f14.6)") xinttot,yinttot,zinttot
write(*,"(' Dipole moment (Debye):',3f14.6)") xinttot*au2debye,yinttot*au2debye,zinttot*au2debye
dipmag=sqrt(xinttot**2+yinttot**2+zinttot**2)
write(*,"(' Magnitude of dipole moment:',f14.6,' a.u.',f14.6,' Debye')") dipmag,dipmag*au2debye

write(*,"(/,' Quadrupole moments (Standard Cartesian form):')")
fac=1
!fac=au2debye*b2a !If using this factor, result will be identical to "Quadrupole moment (field-independent basis, Debye-Ang):" printed by Gaussian
write(*,"(' XX=',f12.6,'  XY=',f12.6,'  XZ=',f12.6)") xxinttot*fac,xyinttot*fac,xzinttot*fac
write(*,"(' YX=',f12.6,'  YY=',f12.6,'  YZ=',f12.6)") xyinttot*fac,yyinttot*fac,yzinttot*fac
write(*,"(' ZX=',f12.6,'  ZY=',f12.6,'  ZZ=',f12.6)") xzinttot*fac,yzinttot*fac,zzinttot*fac
write(*,"(' Quadrupole moments (Traceless Cartesian form):')")
!If removing the comment, the data will be identical to "Traceless Quadrupole moment (field-independent basis, Debye-Ang)" printed by Gaussian
QXX=(3*xxinttot-rrinttot)/2 !*au2debye*b2a/1.5D0
QYY=(3*yyinttot-rrinttot)/2 !*au2debye*b2a/1.5D0
QZZ=(3*zzinttot-rrinttot)/2 !*au2debye*b2a/1.5D0
QXY=3*xyinttot/2            !*au2debye*b2a/1.5D0
QXZ=3*xzinttot/2            !*au2debye*b2a/1.5D0
QYZ=3*yzinttot/2            !*au2debye*b2a/1.5D0
write(*,"(' XX=',f12.6,'  XY=',f12.6,'  XZ=',f12.6)") QXX,QXY,QXZ
write(*,"(' YX=',f12.6,'  YY=',f12.6,'  YZ=',f12.6)") QXY,QYY,QYZ
write(*,"(' ZX=',f12.6,'  ZY=',f12.6,'  ZZ=',f12.6)") QXZ,QYZ,QZZ
write(*,"(' Magnitude of the traceless quadrupole moment tensor:',f12.6)") sqrt(2D0/3D0*(QXX**2+QYY**2+QZZ**2))
R20=(3*zzinttot-rrinttot)/2D0 !Notice that the negative sign, because electrons carry negative charge
R2n1=dsqrt(3D0)*yzinttot
R2p1=dsqrt(3D0)*xzinttot
R2n2=dsqrt(3D0)*xyinttot
R2p2=dsqrt(3D0)/2D0*(xxinttot-yyinttot)
write(*,"(' Quadrupole moments (Spherical harmonic form):')")
write(*,"(' Q_2,0 =',f11.6,'   Q_2,-1=',f11.6,'   Q_2,1=',f11.6)") R20,R2n1,R2p1
write(*,"(' Q_2,-2=',f11.6,'   Q_2,2 =',f11.6)") R2n2,R2p2
write(*,"( ' Magnitude: |Q_2|=',f12.6)") dsqrt(R20**2+R2n1**2+R2p1**2+R2n2**2+R2p2**2)

R30=(5*zzzinttot-3*rrzinttot)/2D0
R3n1=dsqrt(3D0/8D0)*(5*yzzinttot-rryinttot)
R3p1=dsqrt(3D0/8D0)*(5*xzzinttot-rrxinttot)
R3n2=dsqrt(15D0)*xyzinttot
R3p2=dsqrt(15D0)*(xxzinttot-yyzinttot)/2D0
R3n3=dsqrt(5D0/8D0)*(3*xxyinttot-yyyinttot)
R3p3=dsqrt(5D0/8D0)*(xxxinttot-3*xyyinttot)
write(*,"(/,' Octopole moments (Cartesian form):')")
fac=1
!fac=au2debye*b2a*b2a !If using this factor, result will be identical to "Octapole moment (field-independent basis, Debye-Ang**2):" printed by Gaussian
write(*,"(' XXX=',f10.4,'  YYY=',f10.4,'  ZZZ=',f10.4,'  XYY=',f10.4,'  XXY=',f10.4)") &
xxxinttot*fac,yyyinttot*fac,zzzinttot*fac,xyyinttot*fac,xxyinttot*fac
write(*,"(' XXZ=',f10.4,'  XZZ=',f10.4,'  YZZ=',f10.4,'  YYZ=',f10.4,'  XYZ=',f10.4)") &
xxzinttot*fac,xzzinttot*fac,yzzinttot*fac,yyzinttot*fac,xyzinttot*fac
write(*,"(' Octopole moments (Spherical harmonic form):')")
write(*,"(' Q_3,0 =',f11.4,'  Q_3,-1=',f11.4,'  Q_3,1 =',f11.4)") R30,R3n1,R3p1
write(*,"(' Q_3,-2=',f11.4,'  Q_3,2 =',f11.4,'  Q_3,-3=',f11.4,'  Q_3,3 =',f11.4)") R3n2,R3p2,R3n3,R3p3
write(*,"( ' Magnitude: |Q_3|=',f12.4)") dsqrt(R30**2+R3n1**2+R3p1**2+R3n2**2+R3p2**2+R3n3**2+R3p3**2)

write(*,*)
write(*,*) "Note: Unless otherwise specified, all data shown above are in a.u."
end subroutine



    
!!------------ A general routine for obtaining energies of present orbitals in memory based on loaded Fock matrix    
subroutine calc_orb_energy
use defvar
implicit real*8 (a-h,o-z)
real*8 orbene(nmo),Emat(nbasis,nbasis)

if (.not.allocated(CObasa)) then
    write(*,"(a)") " Error: To use this function, the input file must contain basis function information! See Section 2.5 of Multiwfn manual for detail"
    write(*,*) "Press ENTER button to return"
    return
end if

call loadFockfile(istat)
if (istat==0) then
	write(*,*) "Unable to evaluate orbital energies!"
else
	Emat=matmul(matmul(transpose(CObasa),FmatA),CObasa)
	do iorb=1,nbasis
		orbene(iorb)=Emat(iorb,iorb)
	end do
    if (allocated(CObasb)) then
	    Emat=matmul(matmul(transpose(CObasb),FmatB),CObasb)
	    do iorb=1,nbasis
		    orbene(nbasis+iorb)=Emat(iorb,iorb)
	    end do
    end if
	write(*,*) "Orbital energies have been successfully evaluated!"
    write(*,*)
    write(*,*) "0 Do nothing"
    write(*,*) "1 Export orbital energies to orbene.txt in current folder"
    write(*,*) "2 Replace the original orbital energies in memory by the newly evaluated ones"
    write(*,*) "3 Do both 1 and 2"
    read(*,*) isel
    if (isel==1.or.isel==3) then
        open(10,file="orbene.txt")
        if (wfntype==0.or.wfntype==2.or.wfntype==3) then
            do iorb=1,nbasis
                write(10,"(i7,'  Occ=',f8.4,'  E=',f16.8,' Hartree',f12.4,' eV')") iorb,MOocc(iorb),orbene(iorb),orbene(iorb)*au2eV
            end do
        else
            write(10,*) "===== Alpha orbitals ====="
            do iorb=1,nbasis
                write(10,"(i7,'  Occ=',f8.4,'  E=',f16.8,' Hartree',f12.4,' eV')") iorb,MOocc(iorb),orbene(iorb),orbene(iorb)*au2eV
            end do
            write(10,*)
            write(10,*) "===== Beta orbitals ====="
            do iorb=1,nbasis
                write(10,"(i7,'  Occ=',f8.4,'  E=',f16.8,' Hartree',f12.4,' eV')") iorb,MOocc(nbasis+iorb),orbene(nbasis+iorb),orbene(nbasis+iorb)*au2eV
            end do
        end if
        close(10)
        write(*,*) "The new orbital energies have been exported to orbene.txt in current folder!"
    end if
    if (isel==2.or.isel==3) then
        MOene=orbene
        write(*,*) "The original orbital energies in memory have been replaced by the new ones!"
    end if
end if
end subroutine