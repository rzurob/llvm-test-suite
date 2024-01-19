! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (protected attribute
!*                               entities to be modified via type-bound)
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

        procedure :: assgn => assgnBase
    end type

    type (base), protected :: b1_pro

    contains

    subroutine assgnBase (b, i)
        class (base), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = i
    end subroutine

end module

program ftpbnd510d
use m
    call b1_pro%assgn(10) !<-- should fail

end
