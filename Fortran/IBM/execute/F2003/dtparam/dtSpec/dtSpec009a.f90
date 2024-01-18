!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/03/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: use of derived-type-spec in the internal
!                               procedure.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtSpec009a
    type A (n)
        integer, len :: n = 100

        integer ids(n)
    end type

    class(*), allocatable :: x

    call testA (x)

    select type (x)
        type is (A(*))
        class default
            stop 10
    end select

    contains

    subroutine testA (x)
        class(*), allocatable :: x

        allocate (A(10) :: x)
    end subroutine
end
