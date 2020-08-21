!
! Ref: J. Zhang, J. Chem. Theory Comput. 2018, 14, 2, 572-587
!
module libreta
use ean
use util
implicit none
    type shinfo
        real*8 :: center(3)
        real*8 :: exponent
        integer :: L
        integer :: indices((MaxL+1)*(MaxL+2)/2) ! For S,P,D,F,G,H
    endtype
    integer :: neffprims,neffprimshells,neffprimshellpairs
    type(eanargs),allocatable :: ean_args(:) ! It is never deallocated!
    real*8,allocatable :: primP(:,:) ! It is never deallocated!
contains
    function eleesp2(Cx,Cy,Cz)
    use defvar
    implicit none
        real*8 :: eleesp2,Cx,Cy,Cz
        real*8 :: theta(MaxThetaSize)
        real*8 :: C(3)
        real*8,allocatable :: matV(:,:)
        integer :: i,j
        ! ================================================
        allocate(matV(neffprims, neffprims))
        ! Calculate <kai|1/|r-C||kai>.        
        C(1) = Cx; C(2) = Cy; C(3) = Cz
        !!!!!!!! parallel do schedule(dynamic) private(theta) NUM_THREADS(nthreads)
        do i = 1,neffprimshellpairs
            call calcean(ean_args(i),C,theta)
            do j = 1,ShellPairSize(ean_args(i)%idxLAB)
               matV(ean_args(i)%indices1(j),ean_args(i)%indices2(j)) = theta(TargetStart(ean_args(i)%idxLAB)+j-1)
               matV(ean_args(i)%indices2(j),ean_args(i)%indices1(j)) = theta(TargetStart(ean_args(i)%idxLAB)+j-1)
            enddo    
        enddo
        !!!!!!!! end parallel do
        ! Matrix product.
        eleesp2 = 0.
        do i = 1,neffprims
            eleesp2 = eleesp2-matV(i,i)*primP(i,i)
        enddo
        do i = 1,neffprims
            do j = 1,i-1
                eleesp2 = eleesp2-matV(j,i)*primP(j,i)*2
            enddo
        enddo
        deallocate(matV)
    end function
    ! Must be run once.
    subroutine initlibreta()
    use defvar
    implicit none
        integer :: i,j,k
        type(shinfo),allocatable :: shell(:)
        ! ================================================        
        ! Build primivitive shell informaton.
        call formshell(shell)
        
        !write (*,*) nprims, neffprims, neffprimshells
        !do i= 1,nprims
        !    write (*,'(I,I,I,E)') i,b(i)%center, b(i)%type, b(i)%exp
        !enddo
        !do i= 1,neffprimshells
        !    write (*,'(I,I,E\)') i, shell(i)%L,shell(i)%exponent
        !    do j= 1,(shell(i)%L+1)*(shell(i)%L+2)/2
        !        write (*,'(I,\)') shell(i)%indices(j)
        !    enddo
        !    write (*,*) ""
        !enddo

        !Build shell-pair information.
        if (allocated(ean_args)) deallocate(ean_args)
        allocate(ean_args(neffprimshellpairs))
        !$OMP parallel do schedule(dynamic) NUM_THREADS(nthreads)
        do i = 1,neffprimshells
            do j = 1,i
                call calceanargs(shell(i),shell(j),ean_args(i*(i-1)/2+j))
            enddo
        enddo
        !$OMP end parallel do
        deallocate(shell)
        ! Initialize EAB calculations.
        call initean()        
    end subroutine    
    ! Calculate arguments for ean calculation.
    subroutine calceanargs(shell1, shell2, args)
        type(shinfo),intent(in) :: shell1,shell2
        type(eanargs),intent(out) :: args    
        ! ================================================
        if (shell1%L >= shell2%L) then
            call calceanargs_core(shell1, shell2, args)
        else
            call calceanargs_core(shell2, shell1, args)
        endif
    end subroutine 
    ! Do not call this directly. Use caleanargs.    
    subroutine calceanargs_core(shellA, shellB, args)
        type(shinfo),intent(in) :: shellA,shellB
        type(eanargs),intent(out) :: args    
        real*8 :: LA,expA,centerA(3),LB,expB,centerB(3)
        real*8 :: mu,AB2,MAB
        integer :: iA,iB,iAB
        ! ================================================
        LA = shellA%L; expA = shellA%exponent; centerA = shellA%center
        LB = shellB%L; expB = shellB%exponent; centerB = shellB%center
        args%p = expA+expB
        mu = expA*expB/args%p
        args%rec_p2 = 0.5/args%p
        args%AB = centerA-centerB
        AB2 = dot_product(args%AB, args%AB)
        MAB = exp(-mu*AB2)
        args%MABdps = MAB/args%p
        args%centerP = (centerA*expA+centerB*expB)/args%p
        args%PA = args%centerP-centerA
        args%idxLAB = LA*(LA+1)/2+LB+1
        ! Indices.
        iAB = 1
        do iA = 1,(LA+1)*(LA+2)/2
            do iB = 1,(LB+1)*(LB+2)/2
                args%indices1(iAB) = shellA%indices(iA)
                args%indices2(iAB) = shellB%indices(iB)
                iAB = iAB+1
            enddo
        enddo
    end subroutine
    subroutine formshell(shell)
    use defvar
    implicit none
        type(shinfo),allocatable,intent(out) :: shell(:)
        integer :: i,iprim,ishell,j,k,iwalltime1,iwalltime2,naint,nbint,ibbeg,ibend
        type(primtype),allocatable :: effb(:)
        logical,allocatable :: bflag(:)
        real*8,allocatable :: effCO(:,:),primPB(:,:)
        ! ================================================
        !Generate effb.
        allocate(effb(nprims));
        allocate(bflag(nprims)); bflag = .false.     
        allocate(effCO(nmo,nprims)); effCO = 0.
        neffprims = 1
        do i = 1,nprims
            if (.not. bflag(i)) then
                effb(neffprims) = b(i)                
                do j = i,nprims
                    if(b(j)%center == b(i)%center .and. b(j)%type == b(i)%type .and. b(j)%exp == b(i)%exp) then
                        bflag(j) = .true.
                        effCO(:,neffprims) = effCO(:,neffprims)+CO(:,j)
                    endif
                enddo
                neffprims = neffprims+1
            endif
        enddo
        neffprims = neffprims-1
        deallocate(bflag)
        ! Build shell.
        neffprimshells = nprims ! An initial guess.
        allocate(shell(neffprimshells))        
        ishell = 1
        iprim = 1
        do while(iprim <= neffprims) 
            ! S 
            if (effb(iprim)%type == 1) then
                shell(ishell)%center = (/a(effb(iprim)%center)%x,a(effb(iprim)%center)%y,a(effb(iprim)%center)%z/)
                shell(ishell)%exponent = effb(iprim)%exp
                shell(ishell)%L = 0
                do i= 1,neffprims
                    if (effb(i)%center == effb(iprim)%center .and. effb(i)%exp == effb(iprim)%exp .and. &
                        effb(i)%type == 1) then
                        shell(ishell)%indices(1) = i
                    endif          
                enddo
                ishell = ishell+1
            endif
            ! P
            if (effb(iprim)%type == 2) then
                shell(ishell)%center = (/a(effb(iprim)%center)%x,a(effb(iprim)%center)%y,a(effb(iprim)%center)%z/)
                shell(ishell)%exponent = effb(iprim)%exp
                shell(ishell)%L = 1
                do i= 1,neffprims
                    if (effb(i)%center == effb(iprim)%center .and. effb(i)%exp == effb(iprim)%exp .and. &
                        effb(i)%type >= 2 .and. effb(i)%type <= 4) then
                        if (effb(i)%type == 2) shell(ishell)%indices(1) = i
                        if (effb(i)%type == 3) shell(ishell)%indices(2) = i
                        if (effb(i)%type == 4) shell(ishell)%indices(3) = i
                    endif
                enddo
                ishell = ishell+1
            endif
            ! D
            if (effb(iprim)%type == 5) then
                shell(ishell)%center = (/a(effb(iprim)%center)%x,a(effb(iprim)%center)%y,a(effb(iprim)%center)%z/)
                shell(ishell)%exponent = effb(iprim)%exp
                shell(ishell)%L = 2
                do i=1,neffprims
                    if (effb(i)%center == effb(iprim)%center .and. effb(i)%exp == effb(iprim)%exp .and. &
                        effb(i)%type >= 5 .and. effb(i)%type <= 10) then
                        if (effb(i)%type == 5) shell(ishell)%indices(1) = i
                        if (effb(i)%type == 6) shell(ishell)%indices(4) = i
                        if (effb(i)%type == 7) shell(ishell)%indices(6) = i
                        if (effb(i)%type == 8) shell(ishell)%indices(2) = i
                        if (effb(i)%type == 9) shell(ishell)%indices(3) = i
                        if (effb(i)%type == 10) shell(ishell)%indices(5) = i
                    endif
                enddo
                ishell = ishell+1
            endif
            ! F
            if (effb(iprim)%type == 11) then
                shell(ishell)%center = (/a(effb(iprim)%center)%x,a(effb(iprim)%center)%y,a(effb(iprim)%center)%z/)
                shell(ishell)%exponent = effb(iprim)%exp
                shell(ishell)%L = 3
                do i=1,neffprims
                    if (effb(i)%center == effb(iprim)%center .and. effb(i)%exp == effb(iprim)%exp .and. &
                        effb(i)%type >= 11 .and. effb(i)%type <= 20) then
                        if (effb(i)%type == 11) shell(ishell)%indices(1) = i
                        if (effb(i)%type == 12) shell(ishell)%indices(7) = i
                        if (effb(i)%type == 13) shell(ishell)%indices(10) = i
                        if (effb(i)%type == 14) shell(ishell)%indices(2) = i
                        if (effb(i)%type == 15) shell(ishell)%indices(3) = i
                        if (effb(i)%type == 16) shell(ishell)%indices(8) = i
                        if (effb(i)%type == 17) shell(ishell)%indices(4) = i
                        if (effb(i)%type == 18) shell(ishell)%indices(6) = i
                        if (effb(i)%type == 19) shell(ishell)%indices(9) = i
                        if (effb(i)%type == 20) shell(ishell)%indices(5) = i
                    endif
                enddo
                ishell = ishell+1
            endif
            ! G
            if (effb(iprim)%type == 21) then
                shell(ishell)%center = (/a(effb(iprim)%center)%x,a(effb(iprim)%center)%y,a(effb(iprim)%center)%z/)
                shell(ishell)%exponent = effb(iprim)%exp
                shell(ishell)%L = 4
                do i=1,neffprims
                    if (effb(i)%center == effb(iprim)%center .and. effb(i)%exp == effb(iprim)%exp .and. &
                        effb(i)%type >= 21 .and. effb(i)%type <= 35) then
                        if (effb(i)%type == 21) shell(ishell)%indices(15) = i
                        if (effb(i)%type == 22) shell(ishell)%indices(14) = i
                        if (effb(i)%type == 23) shell(ishell)%indices(13) = i
                        if (effb(i)%type == 24) shell(ishell)%indices(12) = i
                        if (effb(i)%type == 25) shell(ishell)%indices(11) = i
                        if (effb(i)%type == 26) shell(ishell)%indices(10) = i
                        if (effb(i)%type == 27) shell(ishell)%indices(9) = i
                        if (effb(i)%type == 28) shell(ishell)%indices(8) = i
                        if (effb(i)%type == 29) shell(ishell)%indices(7) = i
                        if (effb(i)%type == 30) shell(ishell)%indices(6) = i
                        if (effb(i)%type == 31) shell(ishell)%indices(5) = i
                        if (effb(i)%type == 32) shell(ishell)%indices(4) = i
                        if (effb(i)%type == 33) shell(ishell)%indices(3) = i
                        if (effb(i)%type == 34) shell(ishell)%indices(2) = i
                        if (effb(i)%type == 35) shell(ishell)%indices(1) = i
                    endif
                enddo
                ishell = ishell+1
            endif
            ! H
            if (effb(iprim)%type == 36) then
                shell(ishell)%center = (/a(effb(iprim)%center)%x,a(effb(iprim)%center)%y,a(effb(iprim)%center)%z/)
                shell(ishell)%exponent = effb(iprim)%exp
                shell(ishell)%L = 5
                do i=1,neffprims
                    if (effb(i)%center == effb(iprim)%center .and. effb(i)%exp == effb(iprim)%exp .and. &
                        effb(i)%type >= 36 .and. effb(i)%type <= 56) then
                        if (effb(i)%type == 36) shell(ishell)%indices(21) = i
                        if (effb(i)%type == 37) shell(ishell)%indices(20) = i
                        if (effb(i)%type == 38) shell(ishell)%indices(19) = i
                        if (effb(i)%type == 39) shell(ishell)%indices(18) = i
                        if (effb(i)%type == 40) shell(ishell)%indices(17) = i
                        if (effb(i)%type == 41) shell(ishell)%indices(16) = i
                        if (effb(i)%type == 42) shell(ishell)%indices(15) = i
                        if (effb(i)%type == 43) shell(ishell)%indices(14) = i
                        if (effb(i)%type == 44) shell(ishell)%indices(13) = i
                        if (effb(i)%type == 45) shell(ishell)%indices(12) = i
                        if (effb(i)%type == 46) shell(ishell)%indices(11) = i
                        if (effb(i)%type == 47) shell(ishell)%indices(10) = i
                        if (effb(i)%type == 48) shell(ishell)%indices(9) = i
                        if (effb(i)%type == 49) shell(ishell)%indices(8) = i
                        if (effb(i)%type == 50) shell(ishell)%indices(7) = i
                        if (effb(i)%type == 51) shell(ishell)%indices(6) = i
                        if (effb(i)%type == 52) shell(ishell)%indices(5) = i
                        if (effb(i)%type == 53) shell(ishell)%indices(4) = i
                        if (effb(i)%type == 54) shell(ishell)%indices(3) = i
                        if (effb(i)%type == 55) shell(ishell)%indices(2) = i
                        if (effb(i)%type == 56) shell(ishell)%indices(1) = i
                    endif
                enddo
                ishell = ishell+1
            endif
            iprim = iprim+1
        enddo
        deallocate(effb)   
        neffprimshells = ishell-1
        neffprimshellpairs = neffprimshells*(neffprimshells+1)/2
        ! Calculate density matrix.
        if (allocated(primP)) deallocate(primP)
        allocate(primP(neffprims, neffprims))
        !call walltime(iwalltime1)
        primP=0
        naint=nint(naelec)
        nbint=nint(nbelec)
        if (wfntype==0) then !R wavefunction
            primP=matmul_blas(transpose(effCO(1:naint,1:neffprims)),effCO(1:naint,1:neffprims),neffprims,neffprims,0,0) !Use parallelized MKL to speed up
            !!$OMP PARALLEL DO SHARED(primP) PRIVATE(i,j,k) schedule(auto) NUM_THREADS(nthreads)
            !do i = 1,neffprims
            !    do j = 1,neffprims
            !        do k = 1,naint
            !            primP(i,j) = primP(i,j)+effCO(k,i)*effCO(k,j)
            !        enddo
            !    enddo
            !enddo
            !!$OMP END PARALLEL DO
            primP=primP*2
        else if (wfntype==1) then !U wavefunction
            do ibbeg=1,nmo
	            if (MOtype(ibbeg)==2) exit
            end do
            ibend=ibbeg-1+nbint
            !Use parallelized MKL to speed up
            primP=matmul_blas(transpose(effCO(1:naint,1:neffprims)),effCO(1:naint,1:neffprims),neffprims,neffprims,0,0)
            if (nbint>0) then
                allocate(primPB(neffprims,neffprims))
                primPB=matmul_blas(transpose(effCO(ibbeg:ibend,1:neffprims)),effCO(ibbeg:ibend,1:neffprims),neffprims,neffprims,0,0)
                primP=primP+primPB
                deallocate(primPB)
            end if
            !!$OMP PARALLEL DO SHARED(primP) PRIVATE(i,j,k) schedule(auto) NUM_THREADS(nthreads)
            !do i = 1,neffprims
            !    do j = 1,neffprims
            !        do k = 1,naint
            !            primP(i,j) = primP(i,j)+effCO(k,i)*effCO(k,j)
            !        enddo
            !        if (nbint>0) then
            !            do k = ibbeg,ibend
            !                primP(i,j) = primP(i,j)+effCO(k,i)*effCO(k,j)
            !            enddo
            !        end if
            !    enddo
            !enddo
            !!$OMP END PARALLEL DO
        else !RO wavefunction, or natural orbitals (MOocc must be explicitly taken into account)
            !$OMP PARALLEL DO SHARED(primP) PRIVATE(i,j,k) schedule(auto) NUM_THREADS(nthreads)
            do i = 1,neffprims
                do j = 1,neffprims
                    do k = 1,nmo
                        primP(i,j) = primP(i,j)+MOocc(k)*effCO(k,i)*effCO(k,j)
                    enddo
                enddo
            enddo
            !$OMP END PARALLEL DO
        end if
        deallocate(effCO)
        !call walltime(iwalltime2)
        !write(*,"(' Calculation took up wall clock time',i10,' s')") iwalltime2-iwalltime1
    end subroutine    
end module
