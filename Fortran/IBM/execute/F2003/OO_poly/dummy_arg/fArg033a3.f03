! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (value changes by the
!                               actual-arg, not dummy-arg)
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

        contains

        procedure :: print => printBase
        procedure :: increaseID => increaseBaseID
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    class (base), pointer :: b1_m (:)

    type (child), save, target :: c1_m (10)

    contains

    elemental subroutine increaseBaseID (b, i1)
        class (base), intent(inout) :: b
        integer*4, intent(in) :: i1

        b%id = b%id + i1
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine test1 (b)
        type (base), target, intent(inout) :: b (:)

        call b1_m%increaseID (10)

        call b(1)%print     !<-- c1_m(1)
        call b(2)%print     !<-- c1_m(4)
        call b(3)%print     !<-- c1_m(7)
        call b(4)%print     !<-- c1_m(10)
    end subroutine
end module


program fArg033a3
use m

    class (base), pointer :: b1(:)

    type (child), target :: c1 (10)

    c1_m = (/(child (i,'c1_m'), i=1,10)/)

    c1 = (/(child (i, 'c1'),i=1,10)/)

    b1_m => c1_m (::3)

    call test1 (b1_m)

    b1 => c1 (::5)

    call test2 (b1)

    do i = 1, 10
        call c1_m(i)%print
        call c1(i)%print
    end do

    contains

    subroutine test2 (b)
        type (base), target, intent(out) :: b (2:)

        call b1%increaseID (100)

        call b(2)%print
        call b(3)%print
    end subroutine
end
