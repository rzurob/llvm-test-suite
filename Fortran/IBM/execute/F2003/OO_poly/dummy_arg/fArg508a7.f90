!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg508a7.f
! %VERIFY: fArg508a7.out:fArg508a7.vf
! %STDIN:
! %STDOUT: fArg508a7.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) for
!                               explicit-shape unlimited-poly dummy array;
!                               actual-args are the array element or entire
!                               array)
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
        integer(4) :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    class (base), allocatable :: b1_m(:)

    type (child), save :: c1_m (3:5)

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine reset (b)
        class (*), intent(out) :: b(2)
    end subroutine
end module


program fArg508a7
use m
    type (base) :: b1(3)

    class (base), pointer :: b_ptr(:)

    b1%id = (/1,2,3/)

    c1_m%id = (/3,4,5/)
    c1_m%name = (/'c1_m_3', 'c1_m_4', 'c1_m_5'/)

    allocate (b_ptr(0:2), source=child (100, 'b_ptr'))

    allocate (b1_m(3), source=child(200,'b1_m'))

    call reset (b1(2))          !<-- b1(2), b1(3)
    call reset (c1_m(3))        !<-- c1_m(3), c1_m(4)
    call reset (b_ptr)          !<-- b_ptr (0, 1)
    call reset (b1_m(2:))        !<-- b1_m(2, 3)

    do i = 1, 3
        call b1(i)%print

        call c1_m(2+i)%print

        call b_ptr(i-1)%print

        call b1_m(i)%print
    end do

    deallocate (b_ptr)
end
