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
!*  DATE                       : 02/08/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final subroutine (finalization of finalizable
!                               scalar components for an array)
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
    type A
        contains

        final :: finalizeA
    end type

    type B
        real(8), allocatable :: r

        contains

        final :: finalizeB
    end type

    type container
        class (b), allocatable :: b2    !<-- b2 finalized when b2 is deallocated
        type (A) a1
        type (B) b1
    end type

    contains

    subroutine finalizeA (a1)
        type (A), intent(in) :: a1

        print *, 'finalizeA'
    end subroutine

    subroutine finalizeB (b1)
        type (B), intent(in) :: b1

        print *, 'finalizeB'
    end subroutine
end module

program ffinal005a3
use m
    class (container), pointer :: co1(:)

    allocate (co1(2))
    allocate (co1(1)%b2, co1(1)%b1%r, co1(2)%b2, co1(2)%b1%r)
    allocate (co1(1)%b2%r)

    deallocate (co1)

    print *, 'end'
end
