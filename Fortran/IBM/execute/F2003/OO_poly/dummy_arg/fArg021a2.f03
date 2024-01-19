! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (array sector with vector
!                               subscript to be associated with dummy-arg having
!                               no INTENT attribute)
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
    end type

    type, extends (base) :: child
        character*20 :: name
    end type

    integer*4 :: i1_m (10)

    private abc
    contains

    subroutine abc (x)
        class(*), intent(inout) :: x
    end subroutine

    subroutine test2 (y)
        class (*) y(:)

        do i = 1, size (y)
            print *, i
            call abc (y(i))
        end do
    end subroutine
end module

program fArg021a2
use m
    type (child) :: c1 (10)
    integer*4 :: vec (3)

    vec = (/3,2,2/)

    call test2 ((/1,2,3/))

    call test2 (i1_m((/1,3/)))

    call test2 (c1(vec))
end
