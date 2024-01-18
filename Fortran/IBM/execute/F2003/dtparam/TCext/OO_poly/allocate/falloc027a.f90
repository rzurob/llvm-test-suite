! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc027a.f
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
! %GROUP: falloc027a.f
! %VERIFY: falloc027a.out:falloc027a.vf
! %STDIN:
! %STDOUT: falloc027a.out
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
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (deallocate a pointer whose target is
!                               not created by allocate causes an error
!                               condition)
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
        integer(k1)      id

        contains

        final :: finalizeBase
    end type

    class (base(4)), pointer :: b1_m

    contains

    subroutine finalizeBase (b)
        type(base(4)) b

        print *, 'finalizeBase'
    end subroutine

    subroutine test1 (b1, b2)
        class (base(4)), pointer :: b1
        class (base(4)), target :: b2

        integer :: err(2) = 0

        if (associated (b1)) deallocate (b1, stat=err(1))

        b1_m => b2

        deallocate (b1_m, stat=err(2))

        print *, err
    end subroutine
end module

program falloc027a
use m
    class (base(4)), pointer :: b1_ptr
    type (base(4)), target :: b1, b2

    b1_ptr => b1

    call test1 (b1_ptr, b2)
end
