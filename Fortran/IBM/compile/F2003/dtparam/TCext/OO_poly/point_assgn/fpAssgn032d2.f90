! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn032d2.f
! opt variations: -qnok -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/29/2005
!*
!*  DESCRIPTION                : data pointer assignment (non-compatible types
!                               involved in the data pointer assignment)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), pointer :: data

        contains

        procedure :: returnDATA
    end type

    contains

    class(*) function returnData (b)
        pointer :: returnData
        class(base(4,*)), intent(in) :: b

        returnData => b%data
    end function
end module

program fpAssgn032d2
use m
    type(base(4,20)), parameter :: c1 = base(4,20)(null())

    type (base(4,:)), pointer :: c3

    c3 => c1%returnData()       !<-- illegal
end
