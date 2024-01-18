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
!*  DATE                       : 02/10/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (no finalization will occurr due to
!                               the execution termination of program by error
!                               condition)
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
        integer*4, pointer :: data(:) => null()

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase(b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        if (associated (b%data)) then
            print *, 'deallocating data'
            deallocate (b%data)
        end if
    end subroutine
end module

program ffinal506
    call test1
end

subroutine test1
use m
    type (base), allocatable, target :: b1
    class (base), allocatable :: b2
    class (base), pointer :: b3

    type (base) b4

    allocate (b1, b2)

    allocate (b1%data(20))

    b3 => b1

    print *, 'STDOUT ends here'

    deallocate (b3)     !<-- execution get terminated here

    !! neither deallocation of b1, b2 nor finalization of b4 will occurr
end subroutine
