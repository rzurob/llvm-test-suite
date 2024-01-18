! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/allocEnh/construct/associate001.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/06/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the intrinsic assignment in an associate
!                               construct; the assignment involves the intrinsic
!                               type (complex(8)).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,8)
        integer, kind            :: k1
        integer, len             :: n1
        complex(k1), allocatable :: cx1(:)
    end type

    type container(k2,n2,k3)    ! (4,20,8)
        integer, kind     :: k2,k3
        integer, len      :: n2
        type(base(n2,k3)) :: data
    end type
end module

program associate001
use m
    type (container(4,:,8)), pointer :: co1

    logical(4), external :: precision_x6

    allocate (container(4,20,8) :: co1)

    co1%data%cx1 = genCmplx (start=0, lb=-1, ub=-1)

    if (.not. allocated(co1%data%cx1)) error stop 1_4

    if ((lbound(co1%data%cx1,1) /= -1) .or. &
        (ubound(co1%data%cx1,1) /= -1)) error stop 2_4

    if (.not. precision_x6(co1%data%cx1(-1), cmplx(1,kind=8))) error stop 3_4

    associate (x => co1%data)
        x%cx1 = genCmplx (0, 1000, start=10)
    end associate

    if ((lbound(co1%data%cx1,1) /= 0) .or. (ubound(co1%data%cx1,1) /= 1000)) &
        error stop 4_4

    do i = 0, 1000
        if (.not. precision_x6(co1%data%cx1(i), cmplx(i+11, kind=8))) &
            error stop 5_4
    end do

    associate (x => co1%data)
        x%cx1 = x%cx1 * cmplx(y=(/(i, i=1,size(x%cx1))/), kind = 8, x=0)
    end associate


    do i = 0, 1000
        if (.not. precision_x6(co1%data%cx1(i), &
            cmplx(i+11, kind=8)*cmplx(0,i+1,8))) error stop 5_4
    end do

    contains

    complex(8) function genCmplx(lb,ub,start)
        integer, intent(in) :: lb, ub, start

        allocatable genCmplx(:)

        allocate (genCmplx(lb:ub), source=cmplx((/(start+i, i=1,ub-lb+1)/), &
            kind = 8))
    end function
end
