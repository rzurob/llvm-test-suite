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
!*  DESCRIPTION                : final subroutine (final sub using assumed-size
!                               array as the dummy-arg)
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
        integer id

        contains

        final :: finalizeBase, finalizeBaseArray1, finalizeBaseArray2
    end type

    contains
    
    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base), intent(in) :: b(*)

        print *, 'finalizeBaseArray Rank 1'
        print *, 'first 2 elements:', b(1:2)%id
    end subroutine

    subroutine finalizeBaseArray2 (b)
        type(base), intent(in) :: b(3,*)

        print *, 'finalizeBaseArray Rank 2'
        print *, 'first 3 elements:', b(:,1)%id
    end subroutine
end module

program ffinal004a3
use m
    class (base), pointer :: b0, b1(:), b2(:,:)

    allocate (b0, b1(2), b2(2,2))

    b0%id = 1
    b1%id = (/10, 20/)
    b2%id = reshape ((/100, 200, 300, 400/), (/2,2/))

    deallocate (b0)
    deallocate (b1)
    deallocate (b2)

    print *, 'end'
end
