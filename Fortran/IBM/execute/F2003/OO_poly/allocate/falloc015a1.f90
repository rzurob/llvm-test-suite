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
! %GROUP: falloc015a1.f
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
!*  DATE                       : 08/31/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (unallocated allocatables used as the
!                               actual arg in intrinsic epsilon())
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

program falloc015a1
    integer, parameter :: p32 = 24   !<-- p value for single precision
    integer, parameter :: p64 = 53   !<-- p value for double precision

    !! r4_ep and r8_ep are the exact representation of epsilon() for single and
    !double precisions

    real(4), parameter :: r4_ep = transfer (Z'34000000', 1.0_4)
    real(8), parameter :: r8_ep = transfer (z'3cb0000000000000', 0.0d0)

    double precision, allocatable :: d1(:,:)

    real(kind=4) r1

    allocatable r1

    if (epsilon(r1) /= r4_ep)   error stop 1_4
    if (epsilon(d1) /= r8_ep)   error stop 2_4
end
