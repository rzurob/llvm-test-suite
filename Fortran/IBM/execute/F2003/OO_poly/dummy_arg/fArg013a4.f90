! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (TARGET dummy-arg for
!                               assumed-shape array associated with whole array
!                               that is with TARGET attribute)
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

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    class (*), pointer :: x(:) => null()
    class (base), pointer :: b1_m (:)

    contains

    logical function associatedwithX (b)
        class (base), target, intent(inout) :: b(2:)

        associatedwithX = associated (x, b)

        if (associatedwithX) b1_m => b

        b%id = b%id + 20
    end function

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg013a3
use m
    class (base), pointer :: b1 (:)
    type (child) :: c1 (3)

    c1 = (/child (1,'c1_1'), child(2,'c1_2'), child (3,'c1_3')/)

    allocate (b1(3:5), source=c1)

    x => b1

    if (.not. associatedwithX (b1)) error stop 1_4

    if (.not. associated (x, b1_m)) error stop 2_4

    if (.not. associated (b1_m, b1)) error stop 3_4

    if ((lbound(b1_m,1) /= 2) .or. (size (b1_m) /= 3)) error stop 4_4

    if ((lbound(x,1) /= 3) .or. (size (x) /= 3)) error stop 5_4

    call b1_m(2)%print
    call b1_m(3)%print
    call b1_m(4)%print

    deallocate (b1)
end
