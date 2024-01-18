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
! %GROUP: ffinal010.f
! %VERIFY: ffinal010.out:ffinal010.vf
! %STDIN:
! %STDOUT: ffinal010.out
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
!*  DATE                       : 02/02/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : final subroutine (module pointer and
!*                               allocatable finalized; scalar and arries of 1
!*                               and 2 dimensions)
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
        integer :: x

        contains

        final :: finalizeBase
        final :: finalizeBaseArray
        final :: finalizeBaseArray2
    end type

    type (base), pointer :: b1_m, b2_m (:), b22_m(:,:)
    type (base), allocatable :: b3_m, b4_m (:), b23_m(:,:)

    contains
    subroutine finalizeBase (b1)
        type (base), intent(inout) :: b1
        print *, 'in finalizeBase'
    end subroutine

    subroutine finalizeBaseArray (b1)
        type (base), intent(in) :: b1(:)
        print *, 'in finalizeBaseArray'
    end subroutine

    subroutine finalizeBaseArray2 (b1)
        type (base), intent(in) :: b1(:,:)
        print *, 'in finalizeBaseArray2'
    end subroutine

end module

program ffinal010

use m

    allocate (b1_m, b2_m(3), b3_m, b4_m(10))
    allocate (b22_m(2,4), b23_m (1,1))

    deallocate (b1_m)
    deallocate (b2_m)
    deallocate (b3_m)
    deallocate (b4_m)

    deallocate (b22_m, b23_m)

end
