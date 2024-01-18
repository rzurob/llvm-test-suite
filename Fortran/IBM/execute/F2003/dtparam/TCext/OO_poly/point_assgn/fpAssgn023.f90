! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn023.f
! opt variations: -ql -qdefaultpv -qreuse=none

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
! %GROUP: fpAssgn023.f
! %VERIFY: fpAssgn023.out:fpAssgn023.vf
! %STDIN:
! %STDOUT: fpAssgn023.out
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
!*  DATE                       : 03/25/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : data pointer assignment (deallocate pointer
!*                               will deallocate the TARGET if allocated via
!*                               ALLOCATE; poly-pointer associated with other
!*                               targets; use final binding to verify)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type, extends(base) :: child    ! (4)
        class (base(k1)), pointer :: data => null()

        contains

        final :: finalizeChild
    end type

    class (child(4)), pointer :: c1_m
    type (child(4)), pointer :: c2_m

    contains

    subroutine finalizeChild (d)
        type (child(4)), intent(inout) :: d

        print *, 'checking data'

        if (associated (d%data)) deallocate (d%data)
    end subroutine

    subroutine allocateC2_m
        allocate (c2_m)
    end subroutine
end module

program fpAssgn023
use m
    class (base(4)), pointer :: b_ptr
    class (child(4)), pointer :: c1

    allocate (c1)

    b_ptr => c1

    deallocate (b_ptr)

    allocate (c1_m)

    b_ptr => c1_m

    deallocate (b_ptr)

    call allocateC2_m

    c1 => c2_m
    b_ptr => c1

    deallocate (b_ptr)
end
