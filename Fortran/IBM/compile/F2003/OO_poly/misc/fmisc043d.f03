! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/24/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 311858)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        procedure(t1), pointer, nopass :: p1 => null()
    end type

    real, target, save :: r1 = -1.0

    contains

    real function t1()
        pointer t1

        r1 = -1.0
        t1 => r1
    end function

    subroutine test1 (p1, p2)
        procedure(real), pointer, intent(inout) :: p1
        real, pointer :: p2

    end subroutine
end module

program fmisc043d
use m
    type (A) a2
    procedure(t1), pointer :: ptr

    a2%p1 => t1
    ptr => t1

    a2%p1() = 3.0   !<-- this is not allowed

    ptr() = 3.0     !<-- this is not allowed

    ptr() => r1     !<-- this is not allowed

end
