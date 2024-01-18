!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/28/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : specific type bound (use in the specification
!                               expression)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class(*), allocatable :: data(:)

        contains

        procedure :: lbound => lowerBound
        procedure :: ubound => upperBound
    end type

    contains

    pure integer function lowerBound (b)
        class (base), intent(in) :: b

        if (allocated (b%data)) then
            lowerBound = lbound(b%data, 1)
        else
            lowerBound = 1
        end if
    end function

    pure integer function  upperBound(b)
        class (base), intent(in) :: b

        if (allocated (b%data)) then
            upperBound = ubound(b%data, 1)
        else
            upperBound = 0
        end if
    end function
end module


program ftpbnd518a1
use m
    type (base), pointer :: b1
    integer i1 (100)

    !! the following usage of structure constructor is IBM extension
    allocate (b1, source=base((/(j, j=1, size(i1))/)))


    call scalarize (b1, i1)

    if (any (i1 /= (/(j, j=1, 100)/))) error stop 2_4

    contains

    subroutine scalarize (b1, x1)
        class (base), intent(in) :: b1
        integer, intent(out) :: x1(b1%lbound():b1%ubound())

        do i = lbound(x1, 1), ubound(x1, 1)
            select type (x =>b1%data(i))
                type is (integer)
                    x1(i) = x
                class default
                    error stop 1_4
            end select
        end do
    end subroutine
end
