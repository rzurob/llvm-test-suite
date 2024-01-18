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
! %GROUP: falloc005a24.f
! %VERIFY: falloc005a24.out:falloc005a24.vf
! %STDIN:
! %STDOUT: falloc005a24.out
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
!*  DATE                       : 07/27/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (tiny() and huge() in source-expr;
!                               also uses transfer to test unlimited-poly data)
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

program falloc005a24
    class (*), pointer :: x1, x2(:,:)

    real(8), pointer :: r1
    real(4), allocatable :: r2(:)
    real(4), parameter :: pi = 3.14

    real(8) :: rr1
    real(4) :: rr2(2)

    allocate (r1, source=tiny (r1))
    allocate (x1, source=tiny (1.d0))

    allocate (r2(2:3), source=-huge(pi))
    allocate (x2(2,2), source=-huge(r2))

    rr1 = transfer (x1, rr1)
    rr2 = transfer (x2((/1/), (/1,2/)), rr2, 2)

    print '(z16)', r1
    print '(2z9)', r2

    print '(z16)', rr1
    print '(2z9)', rr2
end
