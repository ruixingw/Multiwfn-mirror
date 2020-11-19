!-------- Main interface of visual study of weak interaction
subroutine visweak_main
use defvar
implicit real*8 (a-h,o-z)
do while(.true.)
	write(*,*)
	write(*,*) "           ============ Visual study of weak interaction ============ "
	write(*,*) "0 Return"
	write(*,*) "1 NCI analysis (also known as RDG analysis. JACS, 132, 6498)"
	write(*,*) "2 NCI analysis based on promolecular density (JACS, 132, 6498)"
	write(*,*) "3 Averaged NCI analysis (NCI analysis for multiple frames. JCTC, 9, 2226)"
	write(*,*) "4 Interaction region indicator (IRI) analysis"
	write(*,*) "5 DORI analysis (JCTC, 10, 3745)"
	write(*,*) "6 Visualization of van der Waals potential (JMM, 26, 315)"
	write(*,*) "9 Becke/Hirshfeld surface analysis (CrystEngComm, 11, 19)"
	write(*,*) "10 IGM analysis based on promolecular density (PCCP 19, 17928)"
	write(*,*) "11 IGM analysis based on Hirshfeld partition of molecular density (IGMH)"
	read(*,*) isel
	if (isel==0) then
		return
	else if (isel==1) then
		call funcvsfunc(1)
	else if (isel==2) then
		call funcvsfunc(2)
	else if (isel==3) then
		call RDG_MD
	else if (isel==4) then
		call funcvsfunc(4)
	else if (isel==5) then
		call funcvsfunc(5)
    else if (isel==6) then
        call vdWpotential
	else if (isel==9) then
		write(*,"(a)") " Note: To perform Becke or Hirshfeld surface analysis, you should use main function 12. &
		Please check Section 3.15.5 of Multiwfn manual on how to do that. Corresponding examples are given as Sections 4.12.5 and 4.12.6."
		write(*,*) "Press ENTER button to continue"
		read(*,*)
	else if (isel==10) then
		call IGM(1)
	else if (isel==11) then
		call IGM(2)
	end if
end do
end subroutine



!!-------- RDG analysis for MD, use promolecular approximation
subroutine RDG_MD
use defvar
use util
use GUI
use function
implicit real*8 (a-h,o-z)
!The first index of avggrad and the first two indices of avghess correspond to components of gradient and Hessian, respectively
real*8,allocatable :: avgdens(:,:,:),avggrad(:,:,:,:),avghess(:,:,:,:,:)
real*8 denstmp,gradtmp(3),hesstmp(3,3),eigvecmat(3,3),eigval(3)
real*8,allocatable :: avgRDG(:,:,:),thermflu(:,:,:),avgsl2r(:,:,:) !Final result
real*8,allocatable :: scatterx(:),scattery(:)
integer walltime1,walltime2
real*8 time_end,time_endtmp

write(*,*) "Input the range of the frames to be analyzed, e.g. 150,400"
write(*,*) "Note: The index of frame starts from 1"
read(*,*) ifpsstart,ifpsend

call setgrid(0,igridsel)

nfps=ifpsend-ifpsstart+1
write(*,"(' Totally',i8,' frames, from frame',i8,' to',i8,' will be processed')") nfps,ifpsstart,ifpsend

!Generate averaged density, averaged gradient and averaged Hessian
allocate(avgdens(nx,ny,nz),avggrad(3,nx,ny,nz),avghess(3,3,nx,ny,nz))
avgdens=0D0
avggrad=0D0
avghess=0D0
call walltime(walltime1)
open(10,file=filename,status="old")
write(*,*)
write(*,*) "Now calculate averaged density, density gradient and density Hessian"
do ifps=1,ifpsend
	call readxyz(filename,1,0)
	if (ifps<ifpsstart) cycle
	write(*,"(' Processing frame',i8,' ...')") ifps
	!fragatm must be defined for each frame, because each frame may have different content, and calchessmat_prodens will use it
	nfragatmnum=ncenter
	if (allocated(fragatm)) deallocate(fragatm)
	allocate(fragatm(nfragatmnum))
	do itmp=1,nfragatmnum
		fragatm(itmp)=itmp
	end do
	!$OMP PARALLEL DO SHARED(avgdens,avggrad,avghess) PRIVATE(i,j,k,tmpx,tmpy,tmpz,denstmp,gradtmp,hesstmp) schedule(dynamic) NUM_THREADS(nthreads)
	do k=1,nz
		tmpz=orgz+(k-1)*dz
		do j=1,ny
			tmpy=orgy+(j-1)*dy
			do i=1,nx
				tmpx=orgx+(i-1)*dx
				call calchessmat_prodens(tmpx,tmpy,tmpz,denstmp,gradtmp,hesstmp)
				avgdens(i,j,k)=avgdens(i,j,k)+denstmp
				avggrad(:,i,j,k)=avggrad(:,i,j,k)+gradtmp
				avghess(:,:,i,j,k)=avghess(:,:,i,j,k)+hesstmp
			end do
		end do
	end do
	!$OMP END PARALLEL DO
end do
close(10)
avgdens=avgdens/nfps
avggrad=avggrad/nfps
avghess=avghess/nfps

!Use averaged density, averaged gradient and averaged Hessian to compute averaged RDG and averaged Sign(lambda2)*rho
write(*,*)
write(*,*) "Calculating averaged RDG and averaged sign(lambda2)*rho..."
allocate(avgRDG(nx,ny,nz),avgsl2r(nx,ny,nz))
do k=1,nz
	tmpz=orgz+(k-1)*dz
	do j=1,ny
		tmpy=orgy+(j-1)*dy
		do i=1,nx
			tmpx=orgx+(i-1)*dx

			avggradnormtmp=dsqrt(sum(avggrad(:,i,j,k)**2))
			if (RDGprodens_maxrho/=0D0.and.avgdens(i,j,k)>=RDGprodens_maxrho) then
				avgRDG(i,j,k)=100D0
			else if (avggradnormtmp==0D0.or.avgdens(i,j,k)==0D0) then
				avgRDG(i,j,k)=999D0
			else
				avgRDG(i,j,k)=0.161620459673995D0*avggradnormtmp/avgdens(i,j,k)**(4D0/3D0) !0.161620459673995D0=1/(2*(3*pi**2)**(1/3))
			end if
			
			call diagmat(avghess(:,:,i,j,k),eigvecmat,eigval,100,1D-6)
			call sort(eigval)
			if (eigval(2)/=0D0) then
				avgsl2r(i,j,k)=avgdens(i,j,k)*eigval(2)/abs(eigval(2)) !At nuclei of single atom system, Hessian returned may be zero matrix
			else
				avgsl2r(i,j,k)=-avgdens(i,j,k) !Around nuclei, eigval(2)/abs(eigval(2)) always be negative
			end if
			
		end do
	end do
end do
call walltime(walltime2)
write(*,"(' Calculation totally took up wall clock time',i10,' s')") walltime2-walltime1

deallocate(avghess) !avghess will not be used further, so release its memory
allocate(scatterx(nx*ny*nz),scattery(nx*ny*nz))
ii=1
do k=1,nz
	do j=1,ny
		do i=1,nx
			scatterx(ii)=avgsl2r(i,j,k)
			scattery(ii)=avgRDG(i,j,k)
			ii=ii+1
		end do
	end do
end do
!Default axis range of scatter plot
xmin=-RDGprodens_maxrho
xmax=RDGprodens_maxrho
if (RDGprodens_maxrho==0.0D0) xmin=-2.0D0
if (RDGprodens_maxrho==0.0D0) xmax=2.0D0
ymin=0.0D0
ymax=1.0D0

write(*,*)
do while (.true.)
	write(*,*) "0 Return"
	write(*,*) "1 Draw scatter graph between averaged RDG and Sign(lambda2)*rho"
	write(*,*) "2 Save the scatter graph to file"
	write(*,*) "3 Change range of X-axis of the scatter graph"
	write(*,*) "4 Change range of Y-axis of the scatter graph"
	write(*,*) "5 Export scatter points to output.txt"
	write(*,*) "6 Export averaged RDG and sign(lambda2)*rho to cube files in current folder"
	write(*,*) "7 Compute and export thermal fluctuation index to cube file in current folder"	
	write(*,*) "8 Show isosurface of averaged RDG"
	read(*,*) isel
	
	if (isel==0) then
		exit
	else if (isel==1) then
		write(*,*) "Drawing graph, please wait..."
		call drawscatter(scatterx,scattery,nx*ny*nz,xmin,xmax,ymin,ymax,1,"Averaged $sign({\lambda}_2)\rho$ (a.u.)","Averaged reduced density gradient")
	else if (isel==2) then
		isavepic=1
		call drawscatter(scatterx,scattery,nx*ny*nz,xmin,xmax,ymin,ymax,1,"Averaged $sign({\lambda}_2)\rho$ (a.u.)","Averaged reduced density gradient")
		isavepic=0
		write(*,"(a,a,a)") " Graph have been saved to ",trim(graphformat)," file with ""DISLIN"" prefix in current directory"
	else if (isel==3) then
		write(*,*) "Input lower limit and upper limit of X axis e.g. 0,1.5"
		read(*,*) xmin,xmax
	else if (isel==4) then
		write(*,*) "Input lower limit and upper limit of Y axis e.g. 0,1.5"
		read(*,*) ymin,ymax
	else if (isel==5) then
		open(10,file="output.txt",status="replace")
		write(*,*) "Outputting output.txt in current folder..."
		write(10,"(3f11.6,2E16.8)") ((( (orgx+(i-1)*dx),(orgy+(j-1)*dy),(orgz+(k-1)*dz),avgRDG(i,j,k),avgsl2r(i,j,k),i=1,nx),j=1,ny),k=1,nz)
		close(10)
		write(*,"(a)") " Finished, column 1/2/3/4/5 = X/Y/Z/averaged RDG/averaged sign(lambda2)*rho, unit is Bohr"
	else if (isel==6) then
		write(*,*) "Outputting averaged reduced density gradient to avgRDG.cub in current folder"
		open(10,file="avgRDG.cub",status="replace")
		call outcube(avgRDG,nx,ny,nz,orgx,orgy,orgz,gridvec1,gridvec2,gridvec3,10)
		close(10)
		write(*,*) "Done!"
		write(*,*)
		write(*,*) "Outputting averaged Sign(lambda2)*rho to avgsl2r.cub in current folder"
		open(10,file="avgsl2r.cub",status="replace")
		call outcube(avgsl2r,nx,ny,nz,orgx,orgy,orgz,gridvec1,gridvec2,gridvec3,10)
		close(10)
		write(*,*) "Done!"
	else if (isel==7) then
		call walltime(walltime1)
		write(*,*) "Calculating thermal fluctuation index..."
		if (allocated(thermflu)) deallocate(thermflu)
		allocate(thermflu(nx,ny,nz))
		thermflu=0D0
		open(10,file=filename,status="old")
		!Calculate fluctuation square term of density and temporarily store it to thermflu array
		do ifps=1,ifpsend
			call readxyz(filename,1,0)
			if (ifps<ifpsstart) cycle
			write(*,"(' Processing frame',i8,' ...')") ifps
			!$OMP PARALLEL DO SHARED(thermflu) PRIVATE(i,j,k,tmpx,tmpy,tmpz,denstmp) schedule(dynamic) NUM_THREADS(nthreads)
			do k=1,nz
				tmpz=orgz+(k-1)*dz
				do j=1,ny
					tmpy=orgy+(j-1)*dy
					do i=1,nx
						tmpx=orgx+(i-1)*dx
						call calchessmat_prodens(tmpx,tmpy,tmpz,denstmp)
						thermflu(i,j,k)=thermflu(i,j,k)+(denstmp-avgdens(i,j,k))**2
					end do
				end do
			end do
			!$OMP END PARALLEL DO
		end do
		close(10)
		thermflu=dsqrt(thermflu/nfps)/avgdens
		call walltime(walltime2)
		write(*,"(' Calculation totally took up wall clock time',i10,' s',/)") walltime2-walltime1
		write(*,*) "Outputting thermal fluctuation index to thermflu.cub in current folder"
		open(10,file="thermflu.cub",status="replace")
		call outcube(thermflu,nx,ny,nz,orgx,orgy,orgz,gridvec1,gridvec2,gridvec3,10)
		close(10)
		write(*,*) "Done!"
	else if (isel==8) then
		write(*,*) "Please wait..."
	 	sur_value=0.3D0
	 	if (allocated(cubmat)) deallocate(cubmat)
	 	allocate(cubmat(nx,ny,nz))
	 	cubmat=avgRDG
		call drawisosurgui(1)
		deallocate(cubmat)
	end if
	write(*,*)
end do

end subroutine





!!------------------------------------------------------------------------------------------------
!! ----------- Independent Gradient Model (IGM) analysis based on promolecular density -----------
!!------------------------------------------------------------------------------------------------
!iIGMtype=1: Based on promolecular approximation
!iIGMtype=2: Based on Hirshfeld partition of actual density
subroutine IGM(iIGMtype)
use function
use util
use defvar
use GUI
implicit real*8 (a-h,o-z)
character c2000tmp*2000,selectyn
real*8 grad(3),IGM_grad(3),IGM_grad_inter(3),vectmp(3)
integer iIGMtype
integer,allocatable :: IGMfrag(:,:),IGMfragsize(:) !Definition of each fragment used in IGM, and the number of atoms in each fragment
real*8,allocatable :: frag_grad(:,:) !Temporarily store gradient vector of all defined fragments at a point
real*8,allocatable :: dg_intra(:,:,:) !delta_g_intra of fragments
real*8,allocatable :: dg_inter(:,:,:) !delta_g_inter between fragment 1 and 2
real*8,allocatable :: dg(:,:,:) !delta_g
real*8,allocatable :: sl2r(:,:,:) !sign(lambda2)rho
real*8,allocatable :: rhogrid(:,:,:) !real density
real*8,allocatable :: gradgrid(:,:,:,:) !real density gradient (1/2/3,i,j,k)
real*8,allocatable :: scatterx(:),scattery(:),tmparr(:),scattery2(:)
integer,allocatable :: tmpidx1(:),tmpidx2(:),allatm(:)
real*8,allocatable :: atmpairdg(:,:) !(i,j) is integral of dg between ith atom in a fragment and jth atom in another fragment
real*8,allocatable :: IBSIWmat(:,:)
write(*,*) "Citation: Phys. Chem. Chem. Phys., 19, 17928 (2017)"
write(*,*) "Note: Atomic unit is used for all outputs of this function"
write(*,*)

!----- Define fragments
write(*,*) "How many fragments will be defined? e.g. 3"
write(*,"(a)") " Note: At least one fragment should be defined. Union set of all fragment atoms will be taken into calculation"
read(*,*) nIGMfrag
if (nIGMfrag<1) then
	write(*,*) "Are you crazy?"
	return
end if
allocate(IGMfrag(nIGMfrag,ncenter),IGMfragsize(nIGMfrag),frag_grad(3,nIGMfrag))
do ifrag=1,nIGMfrag
	write(*,"(a,i3,a)") " Input atom list for fragment",ifrag,", e.g. 3,5-8,15-20"
	if (nIGMfrag==1) write(*,*) "Note: If input ""a"", the fragment will correspond to the whole system"
	read(*,"(a)") c2000tmp
	if (index(c2000tmp,'a')/=0) then
		IGMfragsize(ifrag)=ncenter
		forall (i=1:ncenter) IGMfrag(ifrag,i)=i
	else
		call str2arr(c2000tmp,IGMfragsize(ifrag),IGMfrag(ifrag,:))
	end if
end do

!Set "allatm" array, which contains index of all atoms involved in fragments (the coverage of all user-defined fragments may be not equal to the whole system)
allocate(tmpidx1(ncenter))
tmpidx1=0
do ifrag=1,nIGMfrag
	tmpidx1(IGMfrag(ifrag,1:IGMfragsize(ifrag)))=1
end do
ntmp=count(tmpidx1==1)
allocate(allatm(ntmp))
itmp=0
do iatm=1,ncenter
	if (tmpidx1(iatm)==1) then
		itmp=itmp+1
		allatm(itmp)=iatm
	end if
end do
deallocate(tmpidx1)

if (iIGMtype==1) then
    isl2r=2
    if (allocated(b)) then
	    write(*,"(a)") " Your input file contains wavefunction information, which kind of sign(lambda2)rho would you like to use?"
	    write(*,*) "1 sign(lambda2)rho based on actual electron density"
	    write(*,*) "2 sign(lambda2)rho based on promolecular density"
        write(*,*) "Note: 1 is more accurate but more expensive"
	    read(*,*) isl2r
    end if
else if (iIGMtype==2) then
    isl2r=1
end if

!----- Set grid
aug3D=2D0 !Smaller than default value
call setgrid(0,igridsel)
allocate(dg_intra(nx,ny,nz),dg_inter(nx,ny,nz),dg(nx,ny,nz),sl2r(nx,ny,nz))
if (iIGMtype==2) allocate(rhogrid(nx,ny,nz),gradgrid(3,nx,ny,nz))

!----- Calculate grid data
call delvirorb(1)
call walltime(iwalltime1)
write(*,*) "Calculating sign(lambda2)rho..."
ifinish=0
!$OMP PARALLEL DO SHARED(ifinish,sl2r,rhogrid,gradgrid) PRIVATE(i,j,k,tmpx,tmpy,tmpz) schedule(dynamic) NUM_THREADS(nthreads)
do k=1,nz
	tmpz=orgz+(k-1)*dz
	do j=1,ny
		tmpy=orgy+(j-1)*dy
		do i=1,nx
			tmpx=orgx+(i-1)*dx
			if (isl2r==1) then
                if (iIGMtype==1) then
				    sl2r(i,j,k)=signlambda2rho(tmpx,tmpy,tmpz)
                else
                    !Store density and gradient to "rhogrid" and "gradgrid", which will be passed into IGMgrad_Hirsh, thus avoiding recalculate them later
                    !This treatment is not applied to IGM, because it is quite cheap and often employed for quite large system, this strategy will double memory consuming
                    call signlambda2rho_RDG(tmpx,tmpy,tmpz,sl2r(i,j,k),RDG,rhogrid(i,j,k),gradgrid(:,i,j,k))
                end if
			else
				sl2r(i,j,k)=signlambda2rho_prodens(tmpx,tmpy,tmpz)
			end if
		end do
	end do
    ifinish=ifinish+1
    call showprog(ifinish,nz)
end do
!$OMP END PARALLEL DO

write(*,*) "Calculating delta_g, delta_g_inter and delta_g_intra..."
ifinish=0
dg_inter=0
!$OMP PARALLEL DO SHARED(ifinish,dg,dg_inter) PRIVATE(i,j,k,tmpx,tmpy,tmpz,IGM_grad,grad,frag_grad,IGM_grad_inter) &
!$OMP schedule(dynamic) NUM_THREADS(nthreads)
do k=1,nz
	tmpz=orgz+(k-1)*dz
	do j=1,ny
		tmpy=orgy+(j-1)*dy
		do i=1,nx
			tmpx=orgx+(i-1)*dx
			!Calculate gradient and IGM gradient of whole system and get dg
			if (iIGMtype==1) then
                call IGMgrad_promol(tmpx,tmpy,tmpz,allatm,grad,IGM_grad)
            else if (iIGMtype==2) then
                call IGMgrad_Hirsh(tmpx,tmpy,tmpz,allatm,grad,IGM_grad,rhogrid(i,j,k),gradgrid(:,i,j,k))
            end if
			dg(i,j,k)=dsqrt(sum(IGM_grad**2))-dsqrt(sum(grad**2))
			!Calculate IGM gradient of current fragment, and then get dg_inter by substracting actual gradient from it
			IGM_grad_inter=0
			do ifrag=1,nIGMfrag
                if (iIGMtype==1) then
				    call IGMgrad_promol(tmpx,tmpy,tmpz,IGMfrag(ifrag,1:IGMfragsize(ifrag)),frag_grad(:,ifrag),vectmp(:))
                else if (iIGMtype==2) then
				    call IGMgrad_Hirsh(tmpx,tmpy,tmpz,IGMfrag(ifrag,1:IGMfragsize(ifrag)),frag_grad(:,ifrag),vectmp(:),rhogrid(i,j,k),gradgrid(:,i,j,k))
                end if
				IGM_grad_inter=IGM_grad_inter+abs(frag_grad(:,ifrag))
			end do
			dg_inter(i,j,k)=dsqrt(sum(IGM_grad_inter**2))-dsqrt(sum(grad**2))
		end do
	end do
    ifinish=ifinish+1
    call showprog(ifinish,nz)
end do
!$OMP END PARALLEL DO

dg_intra=dg-dg_inter

call delvirorb_back(1)
call walltime(iwalltime2)
write(*,"(' Calculation took up wall clock time',i10,' s')") iwalltime2-iwalltime1
ymin=0.0D0
ymax=maxval(dg)
xmin=-0.6D0
xmax=0.2D0

do while (.true.)
	write(*,*)
	write(*,"(' -3 Change range of Y-axis of scatter graph, current:',f11.6,' to',f11.6)") ymin,ymax
	write(*,"(' -2 Change range of X-axis of scatter graph, current:',f11.6,' to',f11.6)") xmin,xmax
	write(*,*) "-1 Draw scatter graph"
	write(*,*) "0 Exit"
	write(*,*) "1 Save the scatter graph to file"
	write(*,*) "2 Output scatter points to output.txt"
	write(*,*) "3 Output cube files to current folder"
	write(*,*) "4 Show isosurface of grid data"
	write(*,*) "5 Screen delta_g_intra at high density region"
	write(*,"(a)") " 6 Evaluate contribution of atomic pairs and atoms to interfragment interaction (atom and atom pair dg indices as well as IBSIW index)"
	write(*,*) "7 Set delta_g where value of sign(lambda2)rho is out of a certain range"
	write(*,*) "8 Set delta_g_inter where value of sign(lambda2)rho is out of a certain range"
	read(*,*) isel
	if (isel==1.or.isel==-1) then
		write(*,*) "1 delta_g_inter vs. sign(lambda2)rho"
		write(*,*) "2 delta_g_intra vs. sign(lambda2)rho"
		write(*,*) "3 delta_g vs. sign(lambda2)rho"
		write(*,*) "4 delta_g_inter + delta_g_intra vs. sign(lambda2)rho"
		read(*,*) itype
		if (.not.allocated(scatterx)) allocate(scatterx(nx*ny*nz),scattery(nx*ny*nz))
		if (itype==4.and.(.not.allocated(scattery2))) allocate(scattery2(nx*ny*nz))
		write(*,*) "Drawing graph, please wait..."
		ii=1
		do k=1,nz
			do j=1,ny
				do i=1,nx
					scatterx(ii)=sl2r(i,j,k)
					if (itype==1) then
						scattery(ii)=dg_inter(i,j,k)
					else if (itype==2) then
						scattery(ii)=dg_intra(i,j,k)
					else if (itype==3) then
						scattery(ii)=dg(i,j,k)
					else
						scattery(ii)=dg_inter(i,j,k)
						scattery2(ii)=dg_intra(i,j,k)
					end if
					ii=ii+1
				end do
			end do
		end do
		if (isel==-1) then
			if (itype==1) call drawscatter(scatterx,scattery,nx*ny*nz,xmin,xmax,ymin,ymax,1,"$sign({\lambda}_2)\rho$ (a.u.)","${\delta}g^{inter}$ (a.u.)")
			if (itype==2) call drawscatter(scatterx,scattery,nx*ny*nz,xmin,xmax,ymin,ymax,1,"$sign({\lambda}_2)\rho$ (a.u.)","${\delta}g^{intra}$ (a.u.)")
			if (itype==3) call drawscatter(scatterx,scattery,nx*ny*nz,xmin,xmax,ymin,ymax,1,"$sign({\lambda}_2)\rho$ (a.u.)","${\delta}g$ (a.u.)")
			if (itype==4) call drawscatter(scatterx,scattery2,nx*ny*nz,xmin,xmax,ymin,ymax,1,"$sign({\lambda}_2)\rho$ (a.u.)","${\delta}g^{inter/intra}$ (a.u.)",scatterx,scattery,nx*ny*nz)
		else
			isavepic=1
			if (itype==1) call drawscatter(scatterx,scattery,nx*ny*nz,xmin,xmax,ymin,ymax,1,"$sign({\lambda}_2)\rho$ (a.u.)","${\delta}g^{inter}$ (a.u.)")
			if (itype==2) call drawscatter(scatterx,scattery,nx*ny*nz,xmin,xmax,ymin,ymax,1,"$sign({\lambda}_2)\rho$ (a.u.)","${\delta}g^{intra}$ (a.u.)")
			if (itype==3) call drawscatter(scatterx,scattery,nx*ny*nz,xmin,xmax,ymin,ymax,1,"$sign({\lambda}_2)\rho$ (a.u.)","${\delta}g$ (a.u.)")
			if (itype==4) call drawscatter(scatterx,scattery2,nx*ny*nz,xmin,xmax,ymin,ymax,1,"$sign({\lambda}_2)\rho$ (a.u.)","${\delta}g^{inter/intra}$ (a.u.)",scatterx,scattery,nx*ny*nz)
			isavepic=0
			write(*,"(a)") " Figure has been saved to "//trim(graphformat)//" file with ""DISLIN"" prefix in current directory"
		end if
	else if (isel==-2) then
		write(*,*) "Input lower limit and upper limit of X axis  e.g. 0,1.5"
		read(*,*) xmin,xmax
	else if (isel==-3) then
		write(*,*) "Input lower limit and upper limit of Y axis  e.g. 0,1.5"
		read(*,*) ymin,ymax
	else if (isel==0) then
		exit
	else if (isel==2) then
		open(10,file="output.txt",status="replace")
		write(*,*) "Outputting output.txt..."
		write(10,"(4E16.8)") ((( dg_inter(i,j,k),dg_intra(i,j,k),dg(i,j,k),sl2r(i,j,k),k=1,nz),j=1,ny),i=1,nx)
		close(10)
		write(*,*) "Finished, column 1/2/3/4 = delta_g_inter/delta_g_intra/delta_g/sign(lambda2)rho"
	else if (isel==3) then
		open(10,file="dg_inter.cub",status="replace")
		call outcube(dg_inter,nx,ny,nz,orgx,orgy,orgz,gridvec1,gridvec2,gridvec3,10)
		close(10)
		write(*,*) "delta_g_inter has been exported to dg_inter.cub in current folder"
		open(10,file="dg_intra.cub",status="replace")
		call outcube(dg_intra,nx,ny,nz,orgx,orgy,orgz,gridvec1,gridvec2,gridvec3,10)
		close(10)
		write(*,*) "delta_g_intra has been exported to dg_intra.cub in current folder"
		open(10,file="dg.cub",status="replace")
		call outcube(dg,nx,ny,nz,orgx,orgy,orgz,gridvec1,gridvec2,gridvec3,10)
		close(10)
		write(*,*) "delta_g has been exported to dg.cub in current folder"
		open(10,file="sl2r.cub",status="replace")
		call outcube(sl2r,nx,ny,nz,orgx,orgy,orgz,gridvec1,gridvec2,gridvec3,10)
		close(10)
		write(*,*) "sign(lambda2)rho has been exported to sl2r.cub in current folder"
	else if (isel==4) then
		write(*,*) "1 delta_g_inter"
		write(*,*) "2 delta_g_intra"
		write(*,*) "3 delta_g"
		write(*,*) "4 sign(lambda2)rho"
		read(*,*) itype
		if (allocated(cubmat)) deallocate(cubmat)
		allocate(cubmat(nx,ny,nz))
		if (itype==1) cubmat=dg_inter
		if (itype==2) cubmat=dg_intra
		if (itype==3) cubmat=dg
		if (itype==4) cubmat=sl2r
	 	if (itype==1) then
	 		write(*,*) "Input the value of isosurface, e.g. 0.02"
	 	else
	 		write(*,*) "Input the value of isosurface, e.g. 0.5"
	 	end if
		read(*,*) sur_value
		call drawisosurgui(1)
	else if (isel==5) then
		write(*,*) "Input range of sign(lambda2)rho, e.g. -0.12,0.08"
		write(*,"(a)") " delta_g_intra will be set to 0 if sign(lambda2)rho is out of the given range"
		read(*,*) denslow,denshigh
		do k=1,nz
			do j=1,ny
				do i=1,nx
					if (sl2r(i,j,k)<denslow.or.sl2r(i,j,k)>denshigh) dg_intra(i,j,k)=0
				end do
			end do
		end do
		write(*,*) "Done!"
        
	else if (isel==6) then !Calculate and print atom and atom pair dg indices as well as IBSIW
		if (nIGMfrag==2) then
			ifrag=1
			jfrag=2
		else
			write(*,*) "Input fragment index to select two fragments, e.g. 1,3"
			read(*,*) ifrag,jfrag
		end if
		ni=IGMfragsize(ifrag)
		nj=IGMfragsize(jfrag)
        
        !Calculate atom pair delta-g indices
		allocate(atmpairdg(ni,nj))
		write(*,*) "Please wait..."
        call calcatmpairdg(iIGMtype,ni,IGMfrag(ifrag,:),nj,IGMfrag(jfrag,:),atmpairdg)
        
		open(10,file="atmdg.txt",status="replace")
		!Output the first fragment
		allocate(tmparr(ni),tmpidx1(ni))
		do iatmtmp=1,ni
			tmparr(iatmtmp)=sum(atmpairdg(iatmtmp,:))
			tmpidx1(iatmtmp)=IGMfrag(ifrag,iatmtmp)
		end do
		call sort(tmparr,list=tmpidx1) !Sort from small to large
		write(10,"(' Atom dg index in fragment',i3)") ifrag
		do idx=ni,1,-1
			write(10,"(' Atom',i5,' :',f12.6)") tmpidx1(idx),tmparr(idx)
		end do
		deallocate(tmparr,tmpidx1)
		!Output the second fragment
		allocate(tmparr(nj),tmpidx1(nj))
		do jatmtmp=1,nj
			tmparr(jatmtmp)=sum(atmpairdg(:,jatmtmp))
			tmpidx1(jatmtmp)=IGMfrag(jfrag,jatmtmp)
		end do
		call sort(tmparr,list=tmpidx1) !sort from small to large
		write(10,"(/,' Atom dg index in fragment',i3)") jfrag
		do jdx=nj,1,-1
			write(10,"(' Atom',i5,' :',f12.6)") tmpidx1(jdx),tmparr(jdx)
		end do
		deallocate(tmparr,tmpidx1)
		!Output atom pair dg indices
		allocate(tmparr(ni*nj),tmpidx1(ni*nj),tmpidx2(ni*nj))
		itmp=0
		do iatmtmp=1,ni
			do jatmtmp=1,nj
				itmp=itmp+1
				tmpidx1(itmp)=IGMfrag(ifrag,iatmtmp)
				tmpidx2(itmp)=IGMfrag(jfrag,jatmtmp)
				tmparr(itmp)=atmpairdg(iatmtmp,jatmtmp)
			end do
		end do
		call sort(tmparr,list=tmpidx1,list2=tmpidx2) !Sort from small to large
		write(10,"(/,' Atom pair dg index (zero terms are not shown)')")
		do idx=ni*nj,1,-1
			if (tmparr(idx)==0) cycle
			write(10,"(2i5,' :',f12.6)") tmpidx1(idx),tmpidx2(idx),tmparr(idx)
		end do
		close(10)
        deallocate(tmparr,tmpidx1,tmpidx2)
		write(*,"(a)") " Atom and atom pair delta-g indices have been outputted to atmdg.txt in current folder"
        
        !Calculate IBSIW index
        allocate(IBSIWmat(ni,nj))
		open(10,file="IBSIW.txt",status="replace")
        !Convert atom pair dg indices to IBSIW
		do idx=1,ni
			do jdx=1,nj
                dist=distmat(IGMfrag(ifrag,idx),IGMfrag(jfrag,jdx))*b2a
                IBSIWmat(idx,jdx)=atmpairdg(idx,jdx)/dist**2*100
			end do
		end do
		!Output the first fragment
		allocate(tmparr(ni),tmpidx1(ni))
		do iatmtmp=1,ni
			tmparr(iatmtmp)=sum(IBSIWmat(iatmtmp,:))
			tmpidx1(iatmtmp)=IGMfrag(ifrag,iatmtmp)
		end do
		call sort(tmparr,list=tmpidx1) !Sort from small to large
		write(10,"(' Sum of related IBSIW for each atom in fragment',i3)") ifrag
		do idx=ni,1,-1
			write(10,"(' Atom',i5,' :',f12.6)") tmpidx1(idx),tmparr(idx)
		end do
		deallocate(tmparr,tmpidx1)
		!Output the second fragment
		allocate(tmparr(nj),tmpidx1(nj))
		do jatmtmp=1,nj
			tmparr(jatmtmp)=sum(IBSIWmat(:,jatmtmp))
			tmpidx1(jatmtmp)=IGMfrag(jfrag,jatmtmp)
		end do
		call sort(tmparr,list=tmpidx1) !sort from small to large
		write(10,"(/,' Sum of related IBSIW for each atom in fragment',i3)") jfrag
		do jdx=nj,1,-1
			write(10,"(' Atom',i5,' :',f12.6)") tmpidx1(jdx),tmparr(jdx)
		end do
		deallocate(tmparr,tmpidx1)
		!Output atom pair dg indices
		allocate(tmparr(ni*nj),tmpidx1(ni*nj),tmpidx2(ni*nj))
		itmp=0
		do iatmtmp=1,ni
			do jatmtmp=1,nj
				itmp=itmp+1
				tmpidx1(itmp)=IGMfrag(ifrag,iatmtmp)
				tmpidx2(itmp)=IGMfrag(jfrag,jatmtmp)
				tmparr(itmp)=IBSIWmat(iatmtmp,jatmtmp)
			end do
		end do
		call sort(tmparr,list=tmpidx1,list2=tmpidx2) !Sort from small to large
		write(10,"(/,' IBSIW index (zero terms are not shown)')")
		do idx=ni*nj,1,-1
			if (tmparr(idx)==0) cycle
			write(10,"(2i5,' :',f12.6)") tmpidx1(idx),tmpidx2(idx),tmparr(idx)
		end do
		close(10)
        deallocate(IBSIWmat,tmparr,tmpidx1,tmpidx2)
		write(*,*) "IBSIW have been outputted to IBSIW.txt in current folder"
        write(*,*)
        
		write(*,"(a)") " If outputting the two fragments as atmdg.pdb in current folder with atomic contribution as B-factor field? (y/n)"
		read(*,*) selectyn
		open(10,file="atmdg.pdb",status="replace")
		if (selectyn=='y') then
			write(10,"('REMARK   Generated by Multiwfn, Totally',i10,' atoms')") ni+nj
			i=1
			do iatmtmp=1,ni
				val=sum(atmpairdg(iatmtmp,:))
				iatm=IGMfrag(ifrag,iatmtmp)
				write(10,"(a6,i5,1x,a4,1x,a3, 1x,a1,i4,4x,3f8.3,2f6.2,10x,a2)") &
				"HETATM",i,' '//ind2name_up(a(iatm)%index)//' ',"MOL",'A',1,a(iatm)%x*b2a,a(iatm)%y*b2a,a(iatm)%z*b2a,1.0,val*10,adjustr(ind2name_up(a(iatm)%index))
				i=i+1
			end do
			do jatmtmp=1,nj
				val=sum(atmpairdg(:,jatmtmp))
				jatm=IGMfrag(jfrag,jatmtmp)
				write(10,"(a6,i5,1x,a4,1x,a3, 1x,a1,i4,4x,3f8.3,2f6.2,10x,a2)") &
				"HETATM",i,' '//ind2name_up(a(jatm)%index)//' ',"MOL",'A',1,a(jatm)%x*b2a,a(jatm)%y*b2a,a(jatm)%z*b2a,1.0,val*10,adjustr(ind2name_up(a(jatm)%index))
				i=i+1
			end do
			write(10,"('END')")
		end if
		close(10)
		write(*,*) "Done! The data have been outputted to atmdg.pdb in current folder"
		write(*,*) "Note that the B-factor is 10 times of atomic contribution"
		deallocate(atmpairdg)
		
	else if (isel==7.or.isel==8) then
		write(*,"(a)") " Input lower and upper limit of range of sign(lambda2)rho, e.g. -0.04,-0.025"
		read(*,*) rlower,rupper
		if (rupper<rlower) then
			write(*,*) "Error: Upper limit is smaller than lower limit!"
			write(*,*) "Press ENTER button to continue. The data will not be changed"
			read(*,*)
			cycle
		end if
		if (isel==7) write(*,*) "Input expected value of delta_g, e.g. 0"
		if (isel==8) write(*,*) "Input expected value of delta_g_inter, e.g. 0"
		read(*,*) tmpval
		if (isel==7) where (sl2r>rupper.or.sl2r<rlower) dg=tmpval
		if (isel==8) where (sl2r>rupper.or.sl2r<rlower) dg_inter=tmpval
		write(*,*) "Done!"
	end if !end of menu
end do
end subroutine




!!------ Calculate atomic pair delta-g index of atom pairs between two lists atmlist1 and atmlist2
! iIGMtype=1: IGM based on promolecular approximation; =2: IGM based on Hirshfeld partition (IGMH)
subroutine calcatmpairdg(iIGMtype,natm1,atmlist1,natm2,atmlist2,atmpairdg)
use defvar
use util
use function
implicit real*8 (a-h,o-z)
integer iIGMtype,natm1,natm2,atmlist1(natm1),atmlist2(natm2),atmpair(2)
real*8 atmpairdg(natm1,natm2),grad(3),IGM_grad(3)
type(content),allocatable :: gridatmorg(:),gridatm(:)
real*8,allocatable :: dgmat(:,:,:),beckeweigrid(:)
real*8 atmrho(ncenter),atmgrad(3,ncenter),gradtmp(3),gradIGMtmp(3)
real*8 atmprorho(ncenter),atmprograd(3,ncenter)
real*8 prorho,prograd(3),realrho,realgrad(3),hess(3,3),Hirshwei(ncenter)

imethod=1 !Using multi-center grid integration
iapprox=1 !Enable use approximation to accelerate calculation
atmpairdg=0

if (imethod==1) then !Calculate based on multi-center grid integration
    nradpot_bk=radpot
    nsphpot_bk=sphpot
    write(*,*) "Select grid for integrating the delta-g_pair functions"
    !IGMH has higher requirement on integration grid since distribution region is narrow
    if (iIGMtype==2) write(*,*) "Note: Option 1 is deprecated since numerical accuracy is too low for IGMH"
    write(*,*) "1 Medium quality (radial=30, angular=110. Cost=1.0 x)"
    write(*,*) "2 High quality (radial=40, angular=170. Cost=2.1 x)"
    write(*,*) "3 Ultrafine quality (radial=60, angular=302. Cost=5.5 x)"
    write(*,*) "4 Perfect quality (radial=75, angular=434. Cost=9.9 x)"
    write(*,*) "5 Use ""radpot"" and ""sphpot"" defined in settings.ini"
    read(*,*) isel
    if (isel==1) then !In fact this is already enough to guarantee quantitative accuracy
        radpot=30
        sphpot=110
    else if (isel==2) then
        radpot=40
        sphpot=170
    else if (isel==3) then
        radpot=60
        sphpot=302
    else if (isel==4) then
        radpot=75
        sphpot=434
    end if
    allocate(dgmat(natm1,natm2,radpot*sphpot),beckeweigrid(radpot*sphpot),gridatm(radpot*sphpot),gridatmorg(radpot*sphpot))
    write(*,"(' Radial points:',i5,'    Angular points:',i5,'   Total:',i10,' per center')") radpot,sphpot,radpot*sphpot
    call walltime(iwalltime1)
    call gen1cintgrid(gridatmorg,iradcut)
    call showprog(0,ncenter)
    
    !Becke's multi-center integration, cycling all atoms
    do icen=1,ncenter
        !If this center is far from any pair of closely contacted atoms among the two fragment, it will be skipped
        if (iapprox==1) then
            distmin=1E10
            do itmp=1,natm1
                iatm=atmlist1(itmp)
                do jtmp=1,natm2
                    jatm=atmlist2(jtmp)
                    if (distmat(iatm,jatm)>9) cycle !The two atoms are closely contacted
                    xmid=(a(iatm)%x+a(jatm)%x)/2
                    ymid=(a(iatm)%y+a(jatm)%y)/2
                    zmid=(a(iatm)%z+a(jatm)%z)/2
                    dist=dsqrt((xmid-a(icen)%x)**2+(ymid-a(icen)%y)**2+(zmid-a(icen)%z)**2)
                    if (dist<distmin) distmin=dist
                end do
            end do
            if (distmin>6) cycle !This is found to be lowest acceptable threshold. The accuracy loss in this case is fully negligible
        end if
        
        dgmat=0
		gridatm%x=gridatmorg%x+a(icen)%x !Move quadrature point to actual position in molecule
		gridatm%y=gridatmorg%y+a(icen)%y
		gridatm%z=gridatmorg%z+a(icen)%z
	    !$OMP parallel do shared(dgmat) private(ipt,rnowx,rnowy,rnowz,atmrho,atmgrad,itmp,jtmp,iatm,jatm,&
        !$OMP gradtmp,gradIGMtmp,atmprorho,atmprograd,realrho,realgrad,prorho,prograd,Hirshwei,idir,t1,t2,t3,tmp) num_threads(nthreads)
	    do ipt=1+iradcut*sphpot,radpot*sphpot
		    rnowx=gridatm(ipt)%x
            rnowy=gridatm(ipt)%y
            rnowz=gridatm(ipt)%z
            if (iIGMtype==1) then !Using promolecular approximation
                !Calculate atomic gradient in this grid if it is involved in either atmlist1 or atmlist2
                do iatm=1,ncenter
                    if (any(atmlist1==iatm).or.any(atmlist2==iatm)) then
                        call proatmgrad(iatm,rnowx,rnowy,rnowz,atmrho(iatm),atmgrad(:,iatm))
                    end if
                end do
            else if (iIGMtype==2) then !Using Hirshfeld partition
                !Calculate molecular density and its gradient. This is major overhead (>80%)
                call calchessmat_dens(1,rnowx,rnowy,rnowz,realrho,realgrad,hess)
                !Calculate atom density and gradient in free state
                do iatm=1,ncenter
                    if (iapprox==1) then !Ignore atom farther than 64 Bohr from current point, this is quite safe
                        tmp=(rnowx-a(iatm)%x)**2+(rnowy-a(iatm)%y)**2+(rnowz-a(iatm)%z)**2
                        if (tmp>64) then
                            atmprorho(iatm)=0
                            atmprograd(:,iatm)=0
                            cycle
                        end if
                    end if
                    call proatmgrad(iatm,rnowx,rnowy,rnowz,atmprorho(iatm),atmprograd(:,iatm))
                end do
                !Calculate promolecular density and gradient of molecule
                prorho=sum(atmprorho(:))
                do idir=1,3
                    prograd(idir)=sum(atmprograd(idir,:))
                end do
                !Calculate Hirshfeld weight
                do iatm=1,ncenter
                    if (prorho==0) then
                        Hirshwei(iatm)=0
                    else
                        Hirshwei(iatm)=atmprorho(iatm)/prorho
                    end if
                end do
                !Calculate gradient of atomic density partitioned by Hirshfeld
                do iatm=1,ncenter
                    if (any(atmlist1==iatm).or.any(atmlist2==iatm)) then
                        do idir=1,3
                            t1=Hirshwei(iatm)*realgrad(idir)
                            if (prorho==0) then
                                t2=0;t3=0
                            else
                                t2=realrho/prorho*atmprograd(idir,iatm)
                                t3=-realrho*atmprorho(iatm)/prorho**2 * prograd(idir)
                            end if
                            atmgrad(idir,iatm)=t1+t2+t3
                        end do
                    end if
                end do
            end if
            !Calculate atom pair delta-g matrix contributed by this point
            do itmp=1,natm1
                iatm=atmlist1(itmp)
                do jtmp=1,natm2
                    jatm=atmlist2(jtmp)
                    gradtmp(:)=atmgrad(:,iatm)+atmgrad(:,jatm)
                    gradIGMtmp(:)=abs(atmgrad(:,iatm))+abs(atmgrad(:,jatm))
                    dgmat(itmp,jtmp,ipt)=dsqrt(sum(gradIGMtmp**2))-dsqrt(sum(gradtmp**2))
                end do
            end do
	    end do
	    !$OMP end parallel do
	    call gen1cbeckewei(icen,iradcut,gridatm,beckeweigrid)
	    do ipt=1+iradcut*sphpot,radpot*sphpot
		    atmpairdg(:,:)=atmpairdg(:,:)+dgmat(:,:,ipt)*gridatmorg(ipt)%value*beckeweigrid(ipt)
	    end do
        call showprog(icen,ncenter)
    end do
    call walltime(iwalltime2)
    write(*,"(' Calculation took up wall clock time',i10,' s')") iwalltime2-iwalltime1
    radpot=nradpot_bk
    sphpot=nsphpot_bk
    
else if (imethod==2) then !Old code based on evenly distributed grid. Not only slow but also inaccurate
    !ifinish=0
    !!$OMP PARALLEL DO SHARED(ifinish,atmpairdg) PRIVATE(i,j,k,tmpx,tmpy,tmpz,itmp,iatm,jtmp,jatm,atmpair,IGM_grad,grad,tmpval) &
    !!$OMP schedule(dynamic) NUM_THREADS(nthreads)
    !do itmp=1,natm1
	   ! iatm=atmlist1(itmp)
	   ! atmpair(1)=iatm
	   ! do jtmp=1,natm2
		  !  jatm=atmlist2(jtmp)
		  !  atmpair(2)=jatm
		  !  if (distmat(iatm,jatm)> 2D0*(vdwr(a(iatm)%index)+vdwr(a(jatm)%index))) cycle
		  !  tmpval=0
		  !  do k=1,nz
			 !   tmpz=orgz+(k-1)*dz
			 !   do j=1,ny
				!    tmpy=orgy+(j-1)*dy
				!    do i=1,nx
				!	    tmpx=orgx+(i-1)*dx
    !                    if (iIGMtype==1) then
				!		    call IGMgrad_promol(tmpx,tmpy,tmpz,atmpair,grad,IGM_grad)
    !                    else if (iIGMtype==2) then
				!		    call IGMgrad_Hirsh(tmpx,tmpy,tmpz,atmpair,grad,IGM_grad)
    !                    end if
				!	    tmpval=tmpval+dsqrt(sum(IGM_grad**2))-dsqrt(sum(grad**2))
				!    end do
			 !   end do
		  !  end do
		  !  atmpairdg(itmp,jtmp)=tmpval
	   ! end do
	   ! ifinish=ifinish+1
	   ! call showprog(ifinish,natm1)
    !end do
    !!$OMP END PARALLEL DO
    !atmpairdg=atmpairdg*dx*dy*dz
end if
end subroutine





!!-------------------------------------
!!------ Calculate vdW potential ------
!!-------------------------------------
!For simplicity, only UFF is employed in current code (If using AMBER99 & GAFF, then atom types must be set first)
subroutine vdwpotential
use defvar
use GUI
implicit real*8 (a-h,o-z)
real*8 parmA(ncenter),parmB(ncenter),UFF_A(103),UFF_B(103)
real*8,allocatable :: repulgrid(:,:,:),dispgrid(:,:,:),vdwgrid(:,:,:)
character outcubfile*200,c80tmp*80

write(*,"(/,a)") " !!! If this method is employed in your work, please cite this paper along with Multiwfn original paper:"
write(*,"(a,/)") " Tian Lu, Qinxue Chen, van der Waals Potential: An Important Complement to Molecular Electrostatic &
Potential in Studying Intermolecular Interactions. J. Mol. Model., 26, 315 (2020) DOI: 10.1007/s00894-020-04577-0"

if (ivdwprobe==0) then
    write(*,*) "Input name of probe atom, e.g. Ar"
    read(*,*) c80tmp
    do iele=1,nelesupp
        if (ind2name(iele)==c80tmp(1:2)) then
            ivdwprobe=iele
            exit
        end if
    end do
end if

write(*,*) "Parameters of UFF forcefield are used in this module"
write(*,"(' Element of probe atom: ',a)") ind2name(ivdwprobe)

!call setvdWparm(1,FFtype,parmA,parmB,istatus)
call defineUFFparm(UFF_A,UFF_B)
do iatm=1,ncenter
    parmA(iatm)=UFF_A(a(iatm)%index)
    parmB(iatm)=UFF_B(a(iatm)%index)
end do
parmAj=UFF_A(ivdwprobe)
parmBj=UFF_B(ivdwprobe)
write(*,"(' UFF atomic well depth:',f10.3,' kcal/mol')") parmAj
write(*,"(' UFF atomic radius:    ',f10.3,' Angstrom')") parmBj/2
write(*,*)

aug3D=8 !Use 8 Bohr as extension distance because vdW potential largely spread
call setgrid(1,igridsel)
allocate(repulgrid(nx,ny,nz),dispgrid(nx,ny,nz),vdwgrid(nx,ny,nz))
    
write(*,*) "Calculating, please wait..."
!$OMP PARALLEL DO SHARED(repulgrid,dispgrid,vdwgrid) PRIVATE(i,j,k,tmpx,tmpy,tmpz,tmprepul,tmpdisp,iatm,dist,Dij,Xij) schedule(dynamic) NUM_THREADS(nthreads)
do k=1,nz
	tmpz=orgz+(k-1)*dz
	do j=1,ny
		tmpy=orgy+(j-1)*dy
		do i=1,nx
			tmpx=orgx+(i-1)*dx
            tmprepul=0
            tmpdisp=0
            do iatm=1,ncenter
                dist=dsqrt( (a(iatm)%x-tmpx)**2 + (a(iatm)%y-tmpy)**2 + (a(iatm)%z-tmpz)**2 )*b2a
				Dij=dsqrt(parmA(iatm)*parmAj) !Well depth
				Xij=dsqrt(parmB(iatm)*parmBj) !vdW distance
				tmprepul=tmprepul+Dij*(Xij/dist)**12 !Repulsion
				tmpdisp=tmpdisp-2*Dij*(Xij/dist)**6 !Dispersion
            end do
            repulgrid(i,j,k)=tmprepul
            dispgrid(i,j,k)=tmpdisp
            vdWgrid(i,j,k)=tmprepul+tmpdisp
		end do
	end do
end do
!$OMP END PARALLEL DO

write(*,*) "Note: The unit of the grid data is in kcal/mol"
sur_value=1D0
do while(.true.)
	write(*,*)
	write(*,*) "0 Return"
	write(*,*) "1 Show isosurface graph of repulsion potential"
	write(*,*) "2 Show isosurface graph of dispersion potential"
	write(*,*) "3 Show isosurface graph of van der Waals potential"
	write(*,*) "4 Export grid data of repulsion potential as repul.cub in current folder"
	write(*,*) "5 Export grid data of dispersion potential as disp.cub in current folder"
	write(*,*) "6 Export grid data of van der Waals potential as vdW.cub in current folder"
	read(*,*) isel
	if (isel==0) then
		exit
    else if (isel==1.or.isel==2.or.isel==3) then
        if (allocated(cubmat)) deallocate(cubmat)
        allocate(cubmat(nx,ny,nz))
        if (isel==1) cubmat=repulgrid
        if (isel==2) cubmat=dispgrid
        if (isel==3) cubmat=vdwgrid
		call drawisosurgui(1)
        deallocate(cubmat)
	else if (isel==4.or.isel==5.or.isel==6) then
        if (allocated(cubmat)) deallocate(cubmat)
        allocate(cubmat(nx,ny,nz))
        if (isel==4) then
            cubmat=repulgrid
            outcubfile="repul.cub"
        else if (isel==5) then
            cubmat=dispgrid
            outcubfile="disp.cub"
        else if (isel==6) then
            cubmat=vdwgrid
            outcubfile="vdW.cub"
        end if
		open(10,file=outcubfile,status="replace")
		call outcube(cubmat,nx,ny,nz,orgx,orgy,orgz,gridvec1,gridvec2,gridvec3,10)
		close(10)
        deallocate(cubmat)
		write(*,"(' Done! Grid data has been exported to ',a,' in current folder')") trim(outcubfile)
    end if
end do
end subroutine