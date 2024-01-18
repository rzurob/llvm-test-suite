!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/15/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Try a procedure pointer together with
!                               allocatable component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k, dim)
        integer, kind :: k
        integer, len :: dim

        real(k) :: x(dim)
    end type

    type base (k)
        integer, kind :: k

        type(point(k,:)), allocatable :: data
        procedure(printBase4), pointer :: print => null()
    end type

    abstract interface
        subroutine printBase4 (b)
        import base
            class(base(4)), intent(in) :: b
        end subroutine
    end interface
end module

program dtparamConstr053
use m
    procedure(printBase4) printB

    type (base(4)), allocatable :: b1

    b1 = base(4)(point(4,10)((/(i*1.2, i=1,10)/)), printB)

    call b1%print
end


subroutine printB (b)
use m
    class(base(4)), intent(in) :: b

    if (allocated(b%data)) then
        write (*, '(6e12.4)') b%data%x
    else
        print *, 'data not allocated'
    end if
end subroutine
