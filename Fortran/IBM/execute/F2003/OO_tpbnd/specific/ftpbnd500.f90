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
! %GROUP: ftpbnd500.f
! %VERIFY: ftpbnd500.out:ftpbnd500.vf
! %STDIN:
! %STDOUT: ftpbnd500.out
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
!*  DATE                       : 02/18/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : specific binding (inherited binding accessible
!*                               even if the base type is not)
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
    type, private :: base
        contains

        procedure, nopass :: print => printBase
    end type

    type, extends(base) :: child
        integer*4, pointer :: id => null()
    end type

    type (base) :: b1_m
    type (child), save :: c1_m

    contains

    subroutine printBase
        print *, 'base'
    end subroutine printBase

end module

program ftpbnd500
use m, only : child, c1_m, b1_m

    type (child) :: c1
    class (child), allocatable :: c2

    call c1%print

    allocate (c2)

    call c2%print

    call c1_m%print

    call b1_m%print
end
