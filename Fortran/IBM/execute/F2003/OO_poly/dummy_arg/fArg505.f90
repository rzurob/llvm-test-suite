! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly-pointer-dummy-arg;
!*                               a basic test)
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
        integer*4 :: id = -1
    end type

    contains

    subroutine createArray (b1, i)
        class (base), pointer, intent(out) :: b1(:)
        integer*4, intent(in) :: i

        allocate (b1(i))
    end subroutine
end module


program fArg505
use m
    class (base), pointer :: x(:) => null()


    call createArray (x, 10)

    if (.not. associated (x)) error stop 1_4

    if (size(x) /= 10) error stop 2_4

    if (any (x%id /= -1)) error stop 3_4

    deallocate (x)
end
