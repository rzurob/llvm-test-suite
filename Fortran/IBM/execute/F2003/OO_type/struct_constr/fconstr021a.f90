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
! %GROUP: fconstr021a.f
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
!*  DATE                       : 01/19/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (keyword and default
!*                               initializations with traditional structures)
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
    type p
        type(p), pointer :: z => null()
        integer*4 :: i = 1
    end type

    type q
        type (p) :: t
    end type


    type base
        type (q) :: x
    end type

    type base1
        type (q), pointer :: x => null()
    end type
end module

program fconstr021a
use m
    type (q), target :: q1

    type (base) :: b1
    type (base1) :: bb1

    q1 = q (t = p())

    b1 = base (x = q(t = p()))

    bb1 = base1 (x = q1)

    if (associated (q1%t%z) .or. (q1%t%i /= 1)) error stop 1_4

    if (associated (b1%x%t%z) .or. (b1%x%t%i /= 1)) error stop 2_4

    if (.not. associated (bb1%x, q1)) error stop 3_4
end
