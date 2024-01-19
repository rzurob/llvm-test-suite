! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/07/2005
!*
!*  DESCRIPTION                : argument association (actual-arg is the array
!                               section composed of parent components)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 id

        contains

        procedure :: reset => resetBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: reset => resetChild
    end type

    contains

    subroutine resetBase (b)
        class (base), intent(out) :: b

        b%id = -1
    end subroutine

    subroutine resetChild (b)
        class (child), intent(out) :: b

        call b%base%reset

        b%name = 'no-name'
    end subroutine

    subroutine resetData (b)
        class (base), intent(out) :: b(:)

        do i = 1, size (b)
            call b(i)%reset
        end do
    end subroutine
end module

program fArg511
use m
    class (child), allocatable :: c_allo(:)

    type (child) :: c1 (2:10)

    c1 = (/(child(i, 'c1'), i=2, 10)/)

    call resetData (c1(::2)%base)

    allocate (c_allo(3))

    c_allo%name = 'c_allo'
    c_allo%id = (/1, 2, 3/)

    call resetData (c_allo%base)

    if (any (c1(::2)%id /= -1)) error stop 1_4
    if (any (c1%name /= 'c1')) error stop 2_4

    if (any (c1(3::2)%id /= (/3,5,7,9/))) error stop 3_4

    if (any (c_allo%id /= -1)) error stop 4_4

    if (any (c_allo%name /= 'c_allo')) error stop 5_4

end
