module ean
use hrr
use eanvrr
use boysfunc
implicit none 
abstract interface
    subroutine eanvrr_x_x(args, theta)
    import
        type(eanvrrargs),intent(in) :: args
        real*8,intent(out) :: theta(:)
    end subroutine
end interface
abstract interface
    subroutine hrr_x_x(AB, vrrbuffer, D, theta)
    import
        real*8,intent(in) :: AB(3)
        real*8,intent(in) :: vrrbuffer(:,:)
        real*8,intent(in) :: D(:,:)
        real*8,intent(out) :: theta(:)
    end subroutine
end interface
include "ean_data1.h"
    type blockargs
        ! GTO pair quantities.
        real*8,allocatable :: p(:)
        real*8,allocatable :: rec_p2(:)
        real*8,allocatable :: MABdps(:)
        real*8,allocatable :: centerP(:,:)
        real*8,allocatable :: PA(:,:)
        real*8,allocatable :: D(:,:)
        real*8,allocatable :: maxD(:)
        real*8 :: AB(3)
        integer :: KAB
        integer :: idxLAB
    endtype
    type vrrfunc
        procedure(eanvrr_x_x),pointer,nopass :: f
    endtype
    type hrrfunc
        procedure(hrr_x_x),pointer,nopass :: f
    endtype
    type(vrrfunc):: vrrfuncs((MaxL+1)*(MaxL+2)/2)
    type(hrrfunc):: hrrfuncs((MaxL+1)*(MaxL+2)/2)
    real*8,parameter :: PI2 = 6.28318530717958647693
contains
    ! Must be run once.
    subroutine initean()
    implicit none
include "ean_data2.h"
    call initboys()
    end subroutine
    function eanblock(args, C, theta)
        real*8 :: eanblock
        type(blockargs),intent(in) :: args
        real*8,intent(in) :: C(3)
        real*8,intent(out) :: theta(:)        
        type(eanvrrargs) :: vrrargs
        real*8,allocatable :: vrrbuffer(:,:)
        real*8,parameter :: ZeroTol = 1.E-7
        integer :: KAB,idxLAB,i,ctrsz,vrrL0s,vrrL0e
        ! ================================================        
        KAB = args%KAB
        idxLAB = args%idxLAB
        ctrsz = CTRSize(idxLAB)
        allocate(vrrbuffer(KAB,ctrsz))
        vrrL0s = VRRL0Start(idxLAB)
        vrrL0e = vrrL0s+ctrsz-1
        ! VRR: [0,0]^{LA+LB} -> [LA+LB,0]^{0}
        do i = 1,KAB
            if (args%maxD(i) > ZeroTol) then
                vrrargs%PC = args%centerP(:,i)-C
                vrrargs%pRPC2 = args%p(i)*dot_product(vrrargs%PC, vrrargs%PC)
                vrrargs%pRPC2_2 = vrrargs%pRPC2*2
                vrrargs%rec_p2 = args%rec_p2(i)
                vrrargs%MABpi = args%MABdps(i)*PI2
                vrrargs%MexppRPC2 = vrrargs%MABpi*exp(-vrrargs%pRPC2)
                vrrargs%PA = args%PA(:,i)
                call vrrfuncs(idxLAB)%f(vrrargs, theta)
            else
                theta(vrrL0s:vrrL0e) = 0.
            endif
            ! Copy: [LA+LB,0]^{0} -> (LA+LB,0)
            vrrbuffer(i,:) = theta(vrrL0s:vrrL0e)
        enddo
        ! HRR: (LA+LB,0) -> (LA,LB)
        call hrrfuncs(idxLAB)%f(args%AB, vrrbuffer, args%D, theta)
        deallocate(vrrbuffer)
        ! Sum up.
        eanblock = sum(theta(TargetStart(idxLAB):TargetStart(idxLAB)+ShellPairSize(idxLAB)-1))
    end function
end module
