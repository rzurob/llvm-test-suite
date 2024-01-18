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
! %GROUP: fArg034a.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
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
!*  DATE                       : 06/15/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (changes to dummy-arg can
!                               be seen through actual-arg for TARGET attribute)
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
        class(*), pointer :: data => null()
    end type

    complex(4), target :: c1
    integer*4, target :: i1

    class (base), pointer :: b1_m
    type (base), target, save :: b2

    contains

    subroutine test1 (b)
        class (base), intent(inout), pointer :: b

        if (associated (b1_m)) error stop 1_4

        b => b2

        if (.not. associated (b1_m, b2)) error stop 2_4
    end subroutine
end module

program fArg034a
use m

    nullify (b1_m)

    b2%data => c1

    call test1 (b1_m)

    if (.not. associated (b1_m, b2)) error stop 3_4

    if (.not. associated (b1_m%data, c1)) error stop 4_4
end
