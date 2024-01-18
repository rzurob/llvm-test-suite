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
! %GROUP: fconstr043.f
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
!*  DATE                       : 03/18/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (non-poly pointer
!*                               assigned to poly-pointer, parent component and
!*                               compatible component in extended type)
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
        type (base), pointer :: next => null()
    end type
end module

module m1
use m
    type, extends(base) :: child
        integer*4 :: id
    end type
end module

program fconstr043
use m1
    class (base), pointer :: b_ptr

    type (base) :: b1
    type (child), target :: c1

    c1 = child (id = 10)

    b_ptr => c1

    b1 = base (b_ptr)

    if (.not. associated (b1%next, c1%base)) error stop 1_4

    if (associated (b1%next%next)) error stop 2_4

    b1 = base (next = c1%base)

    if (associated (b1%next, b_ptr)) error stop 3_4

    !! assign to null()
    b1 = base (c1%next)

    if (associated (b1%next)) error stop 4_4
end
