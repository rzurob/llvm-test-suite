!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2005
!*
!*  DESCRIPTION                : specific type bound (use the external procedure
!                               for the deferred binding)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module modA
    type, abstract :: base
        contains

        procedure(p), deferred :: p1
    end type

    interface               !<-- better, abstract interface
        subroutine p (b)
        import
            class(base), intent(in) :: b
        end subroutine
    end interface
end module

module modB
use modA
    type, extends(base) :: child
        real data

        contains

        procedure :: p1
    end type

    interface
        subroutine p1 (b)
        import
            class(child), intent(in) :: b
        end subroutine
    end interface
end module

program ftpbnd527
use modB
    integer, parameter :: sizeB = 5000
    class(base), allocatable :: b1(:)

    allocate (b1(sizeB), source=(/(child(i), i=1,sizeB)/))

    do i = 1, sizeB, sizeB/10
        call b1(i)%p1
    end do
end

subroutine p1 (b)
use modB, only: child
    class(child), intent(in) :: b

    write (*, '(f12.2)') b%data
end subroutine
