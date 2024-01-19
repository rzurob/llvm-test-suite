! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/dummy_arg/fArg511.f
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id

        contains

        procedure :: reset => resetBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: reset => resetChild
    end type

    contains

    subroutine resetBase (b)
        class (base(4)), intent(out) :: b

        b%id = -1
    end subroutine

    subroutine resetChild (b)
        class (child(4,1,*)), intent(out) :: b

        call b%base%reset

        b%name = 'no-name'
    end subroutine

    subroutine resetData (b)
        class (base(4)), intent(out) :: b(:)

        do i = 1, size (b)
            call b(i)%reset
        end do
    end subroutine
end module

program fArg511
use m
    class (child(4,1,20)), allocatable :: c_allo(:)

    type (child(4,1,20)) :: c1 (2:10)

    c1 = (/(child(4,1,20)(i, 'c1'), i=2, 10)/)

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
