! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc023a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (deallocate of an unallocated
!                               allocatable variable causes an error condition)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type t(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type base(k2)    ! (4)
        integer, kind             :: k2
        class(t(k2)), allocatable :: data(:)
    end type
end module

program falloc023a
use m
    class (base(4)), pointer :: b1

    integer(4) :: error

    error = 10
    allocate (b1)

    deallocate (b1%data, stat = error)

    if (error /= 2) error stop 1_4
end
