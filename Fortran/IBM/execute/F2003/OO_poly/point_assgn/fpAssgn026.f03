! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly-pointer as
!*                               dummy-arg)
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

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure, pass :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base) :: b

        print *, 'id = ', b%id
    end subroutine

    subroutine printChild (b)
        class (child) :: b

        call b%base%print
        print *, 'name = ', b%name
    end subroutine
end module

program fpAssgn026
use m

    class (base), pointer :: x(:)
    type (child), pointer :: c1(:)

    allocate (c1(3))

    c1 = (/(child (i, 'no-name'), i=1,3)/)

    x => c1

    call abc(x)

    contains

    subroutine abc (b)
        class (base), pointer :: b(:)

        do i = 1, size(b)
            call b(i)%print
        end do

        deallocate(b)
    end subroutine
end
