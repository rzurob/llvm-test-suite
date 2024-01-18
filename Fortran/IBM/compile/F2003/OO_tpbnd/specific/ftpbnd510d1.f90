! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (can not be used to
!*                               redefine a named constant)
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
        integer*4 :: id

        contains

        procedure :: assgnID => assgnBaseID
    end type

    contains

    subroutine assgnBaseID (b, i)
        class (base), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = i
    end subroutine
end module

program ftpbnd510d1
use m
    type(base), parameter :: b = base(10)

    call b%assgnID(100)

end
