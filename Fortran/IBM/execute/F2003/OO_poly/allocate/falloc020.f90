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
! %GROUP: falloc020.f
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
!*  DATE                       : 09/15/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (allocate of associated pointers are
!                               not error)
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

program falloc020

    class (*), pointer :: x(:), x1(:)

    real(8), target :: r1(-10:1)

    r1 = (/(real(i, 8), i = 1, 12)/)

    x1 => r1

    if ((lbound(x1, 1) /= -10) .or. (ubound(x1,1) /= 1)) error stop 1_4

    allocate (x1(0:1), source=(1.0, 2.0))

    x => x1

    if (.not. associated (x, x1)) error stop 2_4


    allocate (x1(int (r1(0))), source = 10)

    if ((lbound(x, 1) /= 0) .or. (ubound(x, 1) /= 1)) error stop 3_4

    if ((lbound(x1, 1) /= 1) .or. (ubound(x1, 1) /= 11)) error stop 4_4
end
