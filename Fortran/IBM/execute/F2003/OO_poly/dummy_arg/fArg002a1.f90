! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly-allocatable-array
!*                               used as the actual argument)
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
        character*20 :: name = 'no-name'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine printChild
end module

module m1
use m
    contains

    subroutine printData (d)
        class (base), intent(in) :: d(:)

        do i = 1, size (d)
            call d(i)%print
        end do
    end subroutine
end module

program fArg002a1
use m1
    type (child) :: c1 (2) = (/child(1,'test1'), child(2,'test2')/)

    class (base), allocatable :: b1(:)

    allocate (b1(2), source=c1)

    call printData (b1)
end
