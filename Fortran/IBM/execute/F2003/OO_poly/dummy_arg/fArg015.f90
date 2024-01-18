! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (explicit-shape array;
!*                              basic test on poly dummy-arg of explicit-shape
!*                              array)
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
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine test1 (b)
        class (base), intent(in) :: b (2:6)

        do i = 2, 6
            call b(i)%print
        end do
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

program fArg015
use m
    class (base), allocatable :: b1(:)

    allocate (b1(2:6), source=child (1,'test'))

    b1%id = (/2,3,4,5,6/)

    call test1 (b1)

    deallocate (b1)

    allocate (b1(5))

    b1%id = (/1,2,3,4,5/)

    call test1 (b1)
end
