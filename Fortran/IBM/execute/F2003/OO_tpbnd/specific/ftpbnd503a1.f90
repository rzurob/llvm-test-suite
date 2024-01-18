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
! %GROUP: ftpbnd503a1.f
! %VERIFY: ftpbnd503a1.out:ftpbnd503a1.vf
! %STDIN:
! %STDOUT: ftpbnd503a1.out
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
!*  DATE                       : 02/05/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : type-bound specific (invoke the overriding
!*                               binding through an inherited binding; use pass
!*                               binding for both)
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

        procedure, pass :: print => printBase
        procedure, pass, private :: printHeader => printBaseHeader
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        call b%printHeader

        print *, 'id = ', b%id
    end subroutine

    subroutine printBaseHeader (b)
        class (base), intent(in) :: b
        print *, 'base type'
    end subroutine
end module

module m1
use m
    type, extends (base) :: child

        contains

        procedure, pass, private :: printHeader => printChildHeader
    end type

    contains

    subroutine printChildHeader (b)
        class (child), intent(in) :: b

        print *, 'child type'
    end subroutine
end module

program ftpbnd503a1
use m1
    class (base), pointer :: b1
    type (child), target :: c1 = child(10)

    class (child), pointer :: c_ptr

    c_ptr => c1
    b1 => c_ptr

    call c1%base%print

    call c1%print

    call b1%print

    call c_ptr%print

    call c_ptr%base%print
end
