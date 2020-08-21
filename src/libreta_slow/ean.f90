module ean
use hrr
use eanvrr
use boysfunc
implicit none 
abstract interface
    subroutine eanvrr_x_x (args, theta)
    import
        type(eanvrrargs),intent(in) :: args
        real*8,intent(out) :: theta(:)
    end subroutine
end interface
abstract interface
    subroutine hrr_x_x (AB, theta)
    import
        real*8,intent(in) :: AB(3)
        real*8,intent(out) :: theta(:)
    end subroutine
end interface
include "ean_data1.h"
    type eanargs
        ! GTO pair quantities.
        real*8 :: p
        real*8 :: rec_p2
        real*8 :: MABdps
        real*8 :: centerP(3)
        real*8 :: PA(3)
        real*8 :: AB(3)
        ! Memory data.
        integer :: idxLAB
        integer :: indices1(((MaxL+1)*(MaxL+2)/2)**2)
        integer :: indices2(((MaxL+1)*(MaxL+2)/2)**2)
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
    ! Calculate
    !
    !  kai_mu, kai_nu: primitive GTO
    !  Int dr  kai_mu(r) (1/|r-C|) kai_nu(r) 
    !
    subroutine calcean(args, C, theta)
        type(eanargs),intent(in) :: args
        real*8,intent(in) :: C(3)
        real*8,intent(out) :: theta(:)        
        real*8 :: PC(3)
        real*8 :: pRPC2,pRPC2_2,MABpi,MexppRPC2
        type(eanvrrargs) :: vrrargs
        integer :: idxLAB
        ! ================================================
        idxLAB = args%idxLAB
        ! VRR: [0,0]^{LA+LB} -> [LA+LB,0]^{0}
        PC = args%centerP-C
        pRPC2 = args%p*dot_product(PC, PC)
        pRPC2_2 = pRPC2*2
        MABpi = args%MABdps*PI2
        MexppRPC2 = MABpi*exp(-pRPC2);
        vrrargs = eanvrrargs(pRPC2, pRPC2_2, args%rec_p2, MABpi, MexppRPC2, args%PA, PC)
        call vrrfuncs(idxLAB)%f(vrrargs, theta)
        ! Contraction: [LA+LB,0]^{0} -> (LA+LB,0)
        ! FIXME: no contraction is used now.
        theta(HRRStart(idxLAB):HRRStart(idxLAB)+CTRSize(idxLAB)) = &
            theta(VRRL0Start(idxLAB):VRRL0Start(idxLAB)+CTRSize(idxLAB))
        ! HRR: (LA+LB,0) -> (LA,LB)
        call hrrfuncs(idxLAB)%f(args%AB, theta)
    end subroutine
end module
