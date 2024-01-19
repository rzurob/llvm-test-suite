! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn032.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/29/2005
!*
!*  DESCRIPTION                : data pointer assignment (function return is a
!                               pointer and is a type bound and apply these on a
!                               named constant)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      data

        contains

        procedure :: returnDATA
    end type

    contains

    class(base(:,4)) function returnData (b)
        pointer :: returnData
        class(base(*,4)), intent(in) :: b

        allocate (returnData, source=b)
    end function
end module

use m
    type(base(20,4)), parameter :: c1 = base(20,4)(100)

    type (base(:,4)), pointer :: c3

    nullify (c3)

    c3 => c1%returnData()

    if (.not. associated(c3)) error stop 1_4

    if (c3%data /= 100) error stop 2_4

    deallocate (c3)
end
