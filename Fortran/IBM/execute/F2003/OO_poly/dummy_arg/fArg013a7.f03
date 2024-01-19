! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (assumed-shape array with
!                               TARGET attribute associated with actual argument
!                               that having TARGET attribute and is an array
!                               section)
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

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    class (base), pointer :: b1_m(:), b2_m(:)

    contains

    subroutine test1 (b)
        class (base), target, intent(in) :: b(10:)

        if (associated (b1_m, b))    b2_m => b
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg013a7
use m
    class (child), allocatable, target :: c1 (:)

    allocate (c1 (5))

    c1%id = (/(i, i=1,5)/)
    c1%name = (/'c1_1', 'c1_2', 'c1_3', 'c1_4', 'c1_5'/)


    !! array section 1
    b1_m => c1(::2)%base

    call test1 (c1(::2)%base)

    if (.not. associated (b1_m, c1(::2)%base)) error stop 1_4

    if (.not. associated (b2_m, c1(::2)%base)) error stop 2_4

    if ((lbound (b2_m, 1) /= 10) .or. (size (b2_m) /= 3)) error stop 3_4

    call b1_m(1)%print

    call b2_m(11)%print

    call b1_m(3)%print

    !! array section 2
    b1_m => c1(2::2)

    call test1 (c1(2::2))

    if (.not. associated (b1_m, c1(2::2))) error stop 4_4

    if (.not. associated (b2_m, c1(2::2))) error stop 5_4

    if ((lbound (b2_m, 1) /= 10) .or. (size (b2_m) /= 2)) error stop 6_4

    call b1_m(1)%print

    call b2_m(11)%print
end
