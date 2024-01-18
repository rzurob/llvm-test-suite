! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/func_return/ffuncRet013a1_1.f
! opt variations: -ql

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
!*  DATE                       : 05/12/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : poly-function results (recursive function
!                               return)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind     :: k1
        real(k1), pointer :: data => null()

        contains

        procedure :: print => printBase
        final :: finalizeBase
    end type

    interface assignment (=)
        module procedure assgnB1B2
    end interface

    contains

    subroutine finalizeBase (b)
        type (base(8)), intent(inout) :: b

        if (associated(b%data)) deallocate(b%data)
    end subroutine

    subroutine printBase (b)
        class (base(8)), intent(in) :: b

        if (associated (b%data)) write (*, '(e15.5)') b%data
    end subroutine

    recursive class(base(8)) function sum1 (n)
        allocatable sum1
        integer, intent(in) :: n

        real(8) :: seed = 1.0_8

        if (n <= 1) then
            seed = 1.0_8
            allocate (sum1)
            allocate (sum1%data, source=1.0_8)
        else
            associate (x => sum1 (n-1))
                seed = seed + 1.0_8

                allocate (sum1)
                allocate (sum1%data, source=seed+x%data)
            end associate
        end if
    end function

    subroutine assgnB1B2 (b1, b2)
        class (base(8)), intent(out) :: b1
        class (base(8)), intent(in) :: b2

        if (associated(b2%data)) then
            allocate (b1%data, source=b2%data)
        end if
    end subroutine
end module

program ffuncRet013a1_1
use m
    class(base(8)), allocatable :: b1

    associate (x => sum1(1000))
        call x%print
    end associate

    allocate (b1)

    b1 = sum1 (20000)

    call b1%print
end
