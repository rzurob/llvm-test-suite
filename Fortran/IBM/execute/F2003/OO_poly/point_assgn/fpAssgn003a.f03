! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (unlimited poly-pointer
!*                               aasigned to targets of any type)
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
    end type

    type, extends(base) :: child
        integer*4 :: id
    end type
end module

program fpAssgn003a
use m
    class (*), pointer :: x => null()
    class (*), pointer :: x1(:) => null()

    type base1
        character c
    end type

    integer*4, target :: i, j(100)

    type (base), target :: b1, b2(-1:2)
    type (base1), target :: b10, b20 (3:10)
    type (child), target :: c1, c2(100:105)
    class (base), pointer :: b1_ptr

    b1_ptr => c1
    x => b1_ptr

    if ((.not. associated (x, c1)) .or. (.not. associated (x, b1_ptr))) &
                    error stop 1_4

    x => c1%base

    x1 => c2%base           ! this is an array section

    if ((.not. associated (x)) .or. (.not. associated (x1))) &
                    error stop 2_4

    if ((associated (x, c1%base)) .or. (associated (x1, c2%base))) &
        error stop 3_4

    if ((size(x1) /= 6) .or. (lbound(x1,1) /= 1) .or. (ubound(x1,1) /= 6)) &
            error stop 4_4

    x => i
    x1 => j(::2)

    if ((.not. associated (x, i)) .or. (.not. associated (x1, j(::2)))) &
                error stop 5_4

    if ((size(x1) /= 50) .or. (lbound(x1,1) /= 1) .or. (ubound(x1,1) /= 50)) &
                error stop 6_4

    x => b1
    x1 => b2

    if ((.not. associated (x)) .or. (.not. associated (x1))) error stop 7_4

    if ((size(x1) /= 4) .or. (lbound(x1,1) /= -1) .or. (ubound(x1,1) /= 2)) &
                error stop 8_4

    x => b10
    x1 => b20

    if ((.not. associated (x, b10)) .or. (.not. associated (x1, b20))) error stop 9_4

    if ((size(x1) /= 8) .or. (lbound(x1,1) /= 3) .or. (ubound(x1,1) /= 10))&
                error stop 10_4


    x => x1 (4)

    if (.not. associated (x, b20(4))) error stop 11_4

end