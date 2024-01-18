! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 277245; problem #2:
!                               ICE for array constructor containing structure
!                               constructor, followed by the usage of the same
!                               type with initialization)
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
    type base
        integer(4) :: id = -1
    end type
end module

program fmisc010a
use m
    type (base) :: b1(10)
    type (base), allocatable :: b2(:)

    b1 = (/(base(i), i= 1, 10)/)

    allocate (b2(10))

    if (any (b1%id /= (/(j, j=1,10)/))) error stop 1_4

    if (any (b2%id /= -1)) error stop 2_4
end
