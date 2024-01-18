!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/buildLib ftpbnd509a1_1.f90 libftpbnd509a1_1.a
! %COMPOPTS: -qfree=f90 -L./ -lftpbnd509a1_1
! %GROUP: ftpbnd509a1.f
! %VERIFY: ftpbnd509a1.out:ftpbnd509a1.vf
! %STDIN:
! %STDOUT: ftpbnd509a1.out
! %EXECARGS:
! %POSTCMD: rm -f libftpbnd509a1_1.a
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
!*  DATE                       : 04/02/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : specific type-bound (external procedure as type
!*                               bound; pass binding; inherited binding as
!*                               external procedure; defined in lib)
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

module m1
use m
    type, extends (base) :: child
        character*20 :: name = ''

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program ftpbnd509a1
use m1
    type (base), target :: b1
    type (child), target :: c1

    class (base), pointer :: b_ptr


    c1 = child (20, 'c1_test')
    b1 = base (10)

    b_ptr => c1

    call b_ptr%print

    b_ptr => b1

    call b_ptr%print
end
