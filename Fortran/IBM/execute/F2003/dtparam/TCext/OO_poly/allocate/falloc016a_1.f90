! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc016a_1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (deallocation of allocated
!                               poly-allocatable array of size-zero)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,4,20)
        integer, kind :: k2
        integer, len  :: n1
    end type
end module

program falloc016a_1
use m

    class (base(4)), allocatable :: b(:)

    allocate (child(4,4,20):: b(2:1))

    deallocate (b)

    if (allocated (b)) error stop 1_4

    allocate (b(1))
end
