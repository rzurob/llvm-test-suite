!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/10/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 340698, test case 2)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type x (n)
        integer, len :: n

        real data(n)
    end type

    class(X(:)), allocatable :: y

    call t (y)

    print *, allocated(y)
    print *, y%n

    contains

    subroutine t (x1)
        class(X(:)), allocatable :: x1

        allocate(X(*) :: x1)    !<-- we already diagnose this
        allocate(X(:) :: x1)    !<-- but not this
    end subroutine
    end

