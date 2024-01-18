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
! %GROUP: fpAssgn005.f
! %VERIFY: fpAssgn005.out:fpAssgn005.vf
! %STDIN:
! %STDOUT: fpAssgn005.out
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
!*  DESCRIPTION                : pointer assignment (polymorphic pointer
!*                               assigned to compatible target, pass binding
!*                               used for verification)
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
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure, pass :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'type base with id = ', b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, 'type Child with id = ', b%id, '; name = ', b%name
    end subroutine
end module

program fpAssgn005
use m

    type (base), target :: b1
    type (child), target :: c1

    class (base), pointer :: b_ptr
    class (child), pointer :: c_ptr

    b1 = base (1)
    c1 = child (2, 'c1')

    !! target is b1
    b_ptr => b1

    call b_ptr%print

    !! target is c1
    b_ptr => c1

    call b_ptr%print

    !! target is c1%base
    b_ptr => c1%base

    call b_ptr%print

    !! target is c1
    c_ptr => c1

    call c_ptr%print

    !! print base data
    call c_ptr%base%print
end
