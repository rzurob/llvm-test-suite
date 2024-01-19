! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (default initialization
!                               for components for structure with INTENT(OUT))
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
        integer*4 :: id = 0
        class (*), pointer :: data => null()
    end type
end module

program fArg506a2
use m
    type(base) :: b1
    complex (8), target :: c1
    type (base), target :: b2

    c1 = (1.0e0, 1.0e1)
    b1 = base (10, c1)

    call assgn (b1, 1, b2)

    if (b1%id /= 1) error stop 5_4
    if (.not. associated (b1%data, b2)) error stop 6_4

    contains

    subroutine assgn (b, i, c)
        class (base), intent(out) :: b
        integer*4, intent(in) :: i
        class (*), target, intent(in) :: c

        if (b%id /= 0) error stop 1_4
        if (associated (b%data)) error stop 2_4

        b%id = i
        b%data => c
    end subroutine
end
