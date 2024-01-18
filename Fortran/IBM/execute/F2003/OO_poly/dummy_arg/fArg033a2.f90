! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (entity value change done
!                               through actual-arg, shall affect dummy-arg)
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
        procedure :: assignVal => assignBaseVal
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
        procedure :: assignVal => assignChildVal
    end type

    class (base), pointer :: b1_m (:)

    type (child), save, target :: c1_m (10)

    contains

    elemental subroutine assignBaseVal (b, id, name)
        class (base), intent(inout) :: b
        integer*4, intent(in) :: id
        character(*), intent(in) :: name

        b%id = id
    end subroutine

    elemental subroutine assignChildVal (b, id, name)
        class (child), intent(inout) :: b
        integer*4, intent(in) :: id
        character(*), intent(in) :: name

        b%id = id
        b%name = name
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
        class (base), target, intent(inout) :: b (:)

        call b1_m%assignVal (10, 'c1_m_changed')

        call b(1)%print    !<-- c1_m(1)%print
        call b(2)%print    !<-- c1_m(4)%print
        call b(3)%print    !<-- c1_m(7)%print
        call b(4)%print    !<-- c1_m(10)%print
    end subroutine
end module


program fArg033a2
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
        class (base), target, intent(out) :: b (2:)

        call b1%assignVal (100, 'c1_changed')

        call b(2)%print
        call b(3)%print
    end subroutine
end
