!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/09/2005
!*
!*  DESCRIPTION                : argument association (assumed-size array as the
!                               actual arg; use the array element designator)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        integer(4) :: i
    end type

    contains

    subroutine xyz (a1)
        type (A), intent(out) :: a1(*)

        a1(1:3)%i = (/1,2,3/)
    end subroutine

    subroutine abc (a1)
        class (A), intent(inout) :: a1(*)

        call xyz(a1(2:4))
    end subroutine
end module

program fArg524
use m
    type (A) a1(10)

    a1%i = -1

    call abc (a1(2))

    if (any(a1(3:5)%i /= (/1,2,3/))) error stop 1_4
end
