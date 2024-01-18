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
!*  DATE                       : 02/15/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (deallocate the allocated allocatable
!                               subobjects)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), allocatable :: data(:)

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        if (allocated (b%data)) deallocate (b%data)
    end subroutine
end module

module m1
use m
    type A
        class(base), allocatable :: b1
    end type

    type B
        class (A), allocatable :: a1(:)
    end type
end module

program ffinal533a
use m1
    type(B), pointer :: b01(:)

    allocate (b01(2))
    allocate (b01(1)%a1(1), b01(2)%a1(2))

    allocate (b01(1)%a1(1)%b1, b01(2)%a1(1)%b1, b01(2)%a1(2)%b1)
    allocate (b01(1)%a1(1)%b1%data(10), b01(2)%a1(2)%b1%data(2))

    deallocate (b01)

    print *, 'end'
end
