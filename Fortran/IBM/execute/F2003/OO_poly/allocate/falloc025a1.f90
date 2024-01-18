!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc025a1.f
! %VERIFY: falloc025a1.out:falloc025a1.vf
! %STDIN:
! %STDOUT: falloc025a1.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 09/24/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (when a variable of derived type is
!                               deallocated, any allocated allocatable subobject
!                               of the variable is deallocated)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    type A
        class (base), allocatable :: data1(:)
        type (base), allocatable :: data2(:)
    end type

    contains

    subroutine finalizeBase (b)
        type(base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program falloc025a1
use m
    type(A), allocatable :: a1
    class (A), pointer :: a2(:)

    allocate (a1, a2(1:3))

    allocate (a1%data1(2), a1%data2(3))

    print *, 'deallocate a1'

    deallocate (a1)

    allocate (a2(1)%data2(100), a2(2)%data1(2), a2(2)%data2(10), a2(3)%data1(20))

    print *, 'deallocate a2'

    deallocate (a2)

    print *, 'end'
end
