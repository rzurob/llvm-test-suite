! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : class keyword (intrinsic assignment, RHS can be
!*                               poly-entities; also contains pointer)
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
        class (*), pointer :: data => null()
        integer*4 :: id = 0
    end type

    type, extends(base) :: child
        character*15 :: name
    end type
end module

program flcass004a
use m
    type (base) :: b1

    class (base), pointer :: b2

    type (child), target :: c1

    real*4, target :: r1(2:10)


    c1 = child (r1(3), 10, 'c1_test')

    b2 => c1

    b1 = b2

    if (b1%id /= 10) error stop 1_4

    if (.not. associated (b1%data, r1(3))) error stop 2_4

end
