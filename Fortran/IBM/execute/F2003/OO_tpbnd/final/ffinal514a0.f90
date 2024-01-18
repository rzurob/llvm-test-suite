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
! %GROUP: ffinal514a0.f
! %VERIFY: ffinal514a0.out:ffinal514a0.vf
! %STDIN:
! %STDOUT: ffinal514a0.out
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
!*  DATE                       : 04/12/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : final sub (temporary array objects produced by
!*                               function need to be finalized after the use in
!*                               the intrinsic assignment)
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
        integer*4 :: id

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        b%id = 0
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(inout) :: b(:)

        print *, 'finalizeBaseRank1'

        b%id = 0
    end subroutine

    function produceBaseArray (b, n)
        type (base), intent(in) :: b
        integer, intent(in) :: n
        type (base) :: produceBaseArray(n)

        produceBaseArray%id = b%id
    end function

    type (base) function produceBase (b)
        type (base), intent(in) :: b

        produceBase%id = b%id
    end function
end module

program ffinal514a0
use m
    type (base), save :: b1 (5), b2

    b2%id = 10

    b1 = produceBaseArray (b2, 5)

    b2 = produceBase (b2)

    print *, 'end'

    if (b2%id /= 10) error stop 1_4
    if (any (b1%id /= 10)) error stop 2_4
end
