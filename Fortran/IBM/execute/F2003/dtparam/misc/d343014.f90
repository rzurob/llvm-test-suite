! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/23/2007
!*
!*  DESCRIPTION                : dtparam miscellaneous (defect 343014)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type X (n)
        integer, len :: n

        sequence

        real data(n)
    end type

    interface
        subroutine sub2 (x1)
            type X (n)
                integer, len :: n

                sequence

                real data(n)
            end type

            type(x(10)) x1
        end subroutine
    end interface

    contains

    subroutine sub1 (x1)
        type(X(10)) x1

        write (*, '(10f12.3)') x1%data
    end subroutine
end module

use m
    type(X(:)), allocatable :: x1

    allocate (x(10) :: x1)

    x1%data = [(i, i=1,10)]

    call sub1(x1)    !<-- this works

    call sub2(x1)    !<-- this doesn't work
end


subroutine sub2 (x1)
    type X (n)
        integer, len :: n

        sequence

        real data(n)
    end type

    type(x(10)) x1

    write (*, '(10f15.4)') x1
end subroutine
