!-------- Main interface of visual study of weak interaction
subroutine visweak_main
implicit real*8 (a-h,o-z)
do while(.true.)
	write(*,*)
	write(*,*) "           ============ Visual study of weak interaction ============ "
	write(*,*) "0 Return"
	write(*,*) "1 NCI analysis (Also known as RDG analysis. JACS, 132, 6498)"
	write(*,*) "2 NCI analysis based on promolecular density (JACS, 132, 6498)"
	write(*,*) "3 Averaged NCI analysis (NCI analysis for multiple frames. JCTC, 9 ,2226)"
	write(*,*) "5 DORI analysis (JCTC, 10, 3745)"
	write(*,*) "9 Becke/Hirshfeld surface analysis (CrystEngComm, 11, 19)"
	write(*,*) "10 IGM analysis based on promolecular density (PCCP 19, 17928)"
	read(*,*) isel
	if (isel==0) then
		return
	else if (isel==1) then
		call funcvsfunc(1)
	else if (isel==2) then
		call funcvsfunc(2)
	else if (isel==3) then
		call RDG_MD
	else if (isel==5) then
		call funcvsfunc(5)
	else if (isel==9) then
		write(*,"(a)") " Note: To perform Becke or Hirshfeld surface analysis, you should use main function 12. &
		Please check Section 3.15.5 of Multiwfn manual on how to do that. Corresponding examples are given as Sections 4.12.5 and 4.12.6."
		write(*,*) "Press ENTER button to continue"
		read(*,*)
	else if (isel==10) then
		call IGM
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
CALL CPU_TIME(time_begin)
open(10,file=filename,access="sequential",status="old")
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
CALL CPU_TIME(time_end)
call walltime(walltime2)
write(*,"(' Calculation totally took up CPU time',f12.2,'s, wall clock time',i10,'s')") time_end-time_begin,walltime2-walltime1

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
		call outcube(avgRDG,nx,ny,nz,orgx,orgy,orgz,dx,dy,dz,10)
		close(10)
		write(*,*) "Done!"
		write(*,*)
		write(*,*) "Outputting averaged Sign(lambda2)*rho to avgsl2r.cub in current folder"
		open(10,file="avgsl2r.cub",status="replace")
		call outcube(avgsl2r,nx,ny,nz,orgx,orgy,orgz,dx,dy,dz,10)
		close(10)
		write(*,*) "Done!"
	else if (isel==7) then
		call walltime(walltime1)
		CALL CPU_TIME(time_begin)
		write(*,*) "Calculating thermal fluctuation index..."
		if (allocated(thermflu)) deallocate(thermflu)
		allocate(thermflu(nx,ny,nz))
		thermflu=0D0
		open(10,file=filename,access="sequential",status="old")
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
		CALL CPU_TIME(time_end)
		call walltime(walltime2)
		write(*,"(' Totally took up CPU time',f12.2,'s, wall clock time',i10,'s',/)") time_end-time_begin,walltime2-walltime1
		write(*,*) "Outputting thermal fluctuation index to thermflu.cub in current folder"
		open(10,file="thermflu.cub",status="replace")
		call outcube(thermflu,nx,ny,nz,orgx,orgy,orgz,dx,dy,dz,10)
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





!!------------------------------------------------------------------------------------
!! ----------- Independent Gradient Model (IGM) analysis based on promolecular density
!!------------------------------------------------------------------------------------
subroutine IGM
use function
use util
use defvar
use GUI
implicit real*8 (a-h,o-z)
character c2000tmp*2000,selectyn
real*8 grad(3),IGM_grad(3),IGM_grad_inter(3)
integer,allocatable :: IGMfrag(:,:),IGMfragsize(:) !Definition of each fragment used in IGM, and the number of atoms in each fragment
real*8,allocatable :: frag_grad(:,:) !Temporarily store gradient vector of all defined fragments at a point
real*8,allocatable :: dg_intra(:,:,:) !delta_g_intra of fragments
real*8,allocatable :: dg_inter(:,:,:) !delta_g_inter between fragment 1 and 2
real*8,allocatable :: dg(:,:,:) !delta_g
real*8,allocatable :: sl2r(:,:,:) !sign(lambda2)rho
real*8,allocatable :: scatterx(:),scattery(:),tmparr(:),scattery2(:)
integer,allocatable :: tmpidx1(:),tmpidx2(:),allatm(:)
real*8,allocatable :: atmpairdg(:,:) !(i,j) is integral of dg between ith atom in a fragment and jth atom in another fragment 
integer atmpair(2)
write(*,*) "Citation: Phys. Chem. Chem. Phys., 19, 17928 (2017)"
write(*,*) "Atomic unit is used for all outputs of this function"
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

isl2r=2
if (allocated(b)) then
	write(*,"(a)") " Your input file contains wavefunction information, which kind of sign(lambda2)rho would you like to use?"
	write(*,*) "1 sign(lambda2)rho based on actual electron density"
	write(*,*) "2 sign(lambda2)rho based on promolecular density"
	read(*,*) isl2r
end if

!----- Set grid
aug3D=2D0 !Smaller than default value
call setgrid(0,igridsel)
allocate(dg_intra(nx,ny,nz),dg_inter(nx,ny,nz),dg(nx,ny,nz),sl2r(nx,ny,nz))

!----- Calculate grid data
write(*,*) "Calculating sign(lambda2)rho..."
ifinish=0
!$OMP PARALLEL DO SHARED(sl2r,ifinish) PRIVATE(i,j,k,tmpx,tmpy,tmpz) schedule(dynamic) NUM_THREADS(nthreads)
do k=1,nz
	tmpz=orgz+(k-1)*dz
	do j=1,ny
		tmpy=orgy+(j-1)*dy
		do i=1,nx
			tmpx=orgx+(i-1)*dx
			if (isl2r==1) then
				sl2r(i,j,k)=signlambda2rho(tmpx,tmpy,tmpz)
			else
				sl2r(i,j,k)=signlambda2rho_prodens(tmpx,tmpy,tmpz)
			end if
		end do
	end do
    ifinish=ifinish+1
    call showprog(ifinish,nz)
end do
!$OMP END PARALLEL DO

write(*,*)
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
			call IGMprodens(1,tmpx,tmpy,tmpz,IGM_grad,allatm)
			call IGMprodens(0,tmpx,tmpy,tmpz,grad,allatm)
			dg(i,j,k)=dsqrt(sum(IGM_grad**2))-dsqrt(sum(grad**2))
			!Calculate gradient vector of fragments and get dg_inter
			IGM_grad_inter=0
			do ifrag=1,nIGMfrag
				call IGMprodens(0,tmpx,tmpy,tmpz,frag_grad(:,ifrag),IGMfrag(ifrag,1:IGMfragsize(ifrag)))
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
	write(*,*) "6 Evaluate contribution of atomic pairs and atoms to interfragment interaction"
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
			write(*,"(a,a,a)") " Graph have been saved to ",trim(graphformat)," file with ""DISLIN"" prefix in current directory"
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
		call outcube(dg_inter,nx,ny,nz,orgx,orgy,orgz,dx,dy,dz,10)
		close(10)
		write(*,*) "delta_g_inter has been exported to dg_inter.cub in current folder"
		open(10,file="dg_intra.cub",status="replace")
		call outcube(dg_intra,nx,ny,nz,orgx,orgy,orgz,dx,dy,dz,10)
		close(10)
		write(*,*) "delta_g_intra has been exported to dg_intra.cub in current folder"
		open(10,file="dg.cub",status="replace")
		call outcube(dg,nx,ny,nz,orgx,orgy,orgz,dx,dy,dz,10)
		close(10)
		write(*,*) "delta_g has been exported to dg.cub in current folder"
		open(10,file="sl2r.cub",status="replace")
		call outcube(sl2r,nx,ny,nz,orgx,orgy,orgz,dx,dy,dz,10)
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
	else if (isel==6) then
		if (nIGMfrag==2) then
			ifrag=1
			jfrag=2
		else
			write(*,*) "Input fragment index to select two fragments, e.g. 1,3"
			read(*,*) ifrag,jfrag
		end if
		allocate(atmpairdg(IGMfragsize(ifrag),IGMfragsize(jfrag)))
		write(*,*) "Please wait..."
		atmpairdg=0
		ifinish=0
		!$OMP PARALLEL DO SHARED(ifinish,atmpairdg) PRIVATE(i,j,k,tmpx,tmpy,tmpz,iatmtmp,iatm,jatmtmp,jatm,atmpair,IGM_grad,grad,tmpval) &
		!$OMP schedule(dynamic) NUM_THREADS(nthreads)
		do iatmtmp=1,IGMfragsize(ifrag)
			iatm=IGMfrag(ifrag,iatmtmp)
			atmpair(1)=iatm
			do jatmtmp=1,IGMfragsize(jfrag)
				jatm=IGMfrag(jfrag,jatmtmp)
				atmpair(2)=jatm
				if (distmat(iatm,jatm)> 2D0*(vdwr(a(iatm)%index)+vdwr(a(jatm)%index))) cycle
				tmpval=0
				do k=1,nz
					tmpz=orgz+(k-1)*dz
					do j=1,ny
						tmpy=orgy+(j-1)*dy
						do i=1,nx
							tmpx=orgx+(i-1)*dx
							call IGMprodens(1,tmpx,tmpy,tmpz,IGM_grad,atmpair)
							call IGMprodens(0,tmpx,tmpy,tmpz,grad,atmpair)
							tmpval=tmpval+dsqrt(sum(IGM_grad**2))-dsqrt(sum(grad**2))
						end do
					end do
				end do
				atmpairdg(iatmtmp,jatmtmp)=tmpval
			end do
			ifinish=ifinish+1
			call showprog(ifinish,IGMfragsize(ifrag))
		end do
		!$OMP END PARALLEL DO
		atmpairdg=atmpairdg*dx*dy*dz
		open(10,file="atmdg.txt",status="replace")
		!Output the first fragment
		ni=IGMfragsize(ifrag)
		allocate(tmparr(ni),tmpidx1(ni))
		do iatmtmp=1,ni
			tmparr(iatmtmp)=sum(atmpairdg(iatmtmp,:))
			tmpidx1(iatmtmp)=IGMfrag(ifrag,iatmtmp)
		end do
		call sort(tmparr,list=tmpidx1) !Sort from small to large
		write(10,"(/,' Atom dg index in fragment',i3,' (zero terms are not shown)')") ifrag
		do idx=ni,1,-1
			write(10,"(' Atom',i5,' :',f12.6)") tmpidx1(idx),tmparr(idx)
		end do
		deallocate(tmparr,tmpidx1)
		!Output the second fragment
		nj=IGMfragsize(jfrag)
		allocate(tmparr(nj),tmpidx1(nj))
		do jatmtmp=1,nj
			tmparr(jatmtmp)=sum(atmpairdg(:,jatmtmp))
			tmpidx1(jatmtmp)=IGMfrag(jfrag,jatmtmp)
		end do
		call sort(tmparr,list=tmpidx1) !sort from small to large
		write(10,"(/,' Atom dg index in fragment',i3,' (zero terms are not shown)')") jfrag
		do jdx=nj,1,-1
			write(10,"(' Atom',i5,' :',f12.6)") tmpidx1(jdx),tmparr(jdx)
		end do
		deallocate(tmparr,tmpidx1)
		!Output the atomic pair contribution
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
		deallocate(tmparr,tmpidx1,tmpidx2)
		close(10)
		write(*,*) "Done! The data have been outputted to atmdg.txt in current folder"
		write(*,*)
		write(*,"(a)") " If output the two fragments as atmdg.pdb in current folder with atomic contribution as B-factor field? (y/n)"
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
			write(*,*) "Press ENTER to continue. The data will not be changed"
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
