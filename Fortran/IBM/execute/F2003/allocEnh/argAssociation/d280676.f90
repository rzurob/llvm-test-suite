!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/13/2006
!*
!*  DESCRIPTION                : misceallneous (defect 280676)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id
    end type

    contains

    subroutine assgnNonPoly (a1, a2)
        type (base), pointer, intent(out) :: a1
        TYPE (base), target, intent(inout) :: a2    !<-- TYPE

        a1 => a2
    end subroutine

    subroutine assgnPoly1 (a1, a2)
        type (base), pointer, intent(out) :: a1
        CLASS (base), target, intent(inout) :: a2   !<-- CLASS

        a1 => a2
    end subroutine

    subroutine assgnPoly2 (a1, a2)
        class (base), pointer, intent(out) :: a1    !<-- class
        CLASS (base), target, intent(inout) :: a2   !<-- CLASS

        a1 => a2
    end subroutine
end module

use m
    type (base), pointer :: b1, b2
    class(base), pointer :: b3, b4

    integer stat

    allocate (b2, b4)

    call assgnNonPoly (b1, b2)

    deallocate (b1, stat=stat)

    if (stat /= 2) stop 1

    call assgnPoly1 (b1, b4)

    deallocate (b1, stat=stat)

    if (stat /= 2) stop 2

    call assgnPoly1 (b1, b2)

    deallocate (b1, stat=stat)

    if (stat /= 2) stop 3

    call assgnPoly2 (b3, b4)

    deallocate (b3, stat=stat)

    if (stat /= 2) stop 4

    call assgnPoly2 (b3, b2)

    deallocate (b3, stat=stat)

    if (stat /= 2) stop 5

    deallocate (b2, b4, stat=stat)

    if (stat /= 0) stop 6
end

