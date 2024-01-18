!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg500.f
! %VERIFY: fArg500.out:fArg500.vf
! %STDIN:
! %STDOUT: fArg500.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : dummy-arg (poly dummy-arg; a simple application
!                                for poly-pointer dummy-arg)
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

        procedure, pass :: print => printBase
    end type

    contains

    subroutine printBase (b)
        class (base) :: b

        print *, 'id = ', b%id
    end subroutine

    !! this can be viewed as a general utility for family base
    subroutine printBaseFamily (b)
        class (base), pointer :: b

        if (associated (b))     call b%print
    end subroutine
end module

module m1
use m
    type, extends(base) :: child
        character*20 :: name

        contains

        procedure, pass :: print => printChild
    end type

    contains

    subroutine printChild (b)
        class (child) :: b

        call printHeader

        print *, b%id, b%name
    end subroutine

    subroutine printHeader
        print *, 'Child type: '
    end subroutine
end module

module m2
use m
    type, extends(base) :: child2
        integer*2 :: flag

        contains

        procedure, pass :: print => printChild2
    end type

    contains

    subroutine printChild2 (b)
        class (child2) :: b

        print *, 'id = ', b%id, '; flag = ', b%flag
    end subroutine
end module

module m3
use m
    type container
        class(base), pointer :: data => null()
    end type
end module

program fpAssgn500
use m1
use m2
use m3

    type (container) :: co(4)

    type (base), target :: b1
    type (child), target :: c1
    type (child2), target :: c2

    class (base), pointer :: b_ptr => null()

    b1 = base (1)
    c1 = child (2, 'c1')
    c2 = child2 (3, 1)

    co = (/container(b1), container(c1), container(c2), container(b_ptr)/)

    do i = 1, 4
        call printBaseFamily (co(i)%data)
    end do

    allocate (b_ptr, source=child2 (flag=10, id = 100))

    co(4) = container(data = b_ptr)

    call printBaseFamily (co(4)%data)
end
