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
! %GROUP: falloc009.f
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
!*  DATE                       : 07/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (zero-size arrays and zero-length
!                               character arrays)
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

program falloc009
    class (*), pointer :: x1(:), x2(:,:)
    class (*), allocatable :: x3(:)

    integer lb, ub, error, zero
    parameter (zero = 0)

    character(zero), allocatable :: ch(:), ch1(:)

    lb = 100
    ub = 90

    allocate (x1(0:-1), source=10_8)

    allocate (x2(3,lb:ub), source=(1.0, 0.0), stat=error)

    allocate (character(zero) :: x3(lb:ub))

    allocate (character(-10) :: ch(lb:ub))

    allocate (ch1(10), source='')

    if (size(x1) /= 0) error stop 1_4

    if ((size (x2, 1) /= 3) .or. (size(x2,2) /= 0)) error stop 2_4

    if (size (x2) /= 0) error stop 3_4

    if (size (x3) /= 0) error stop 4_4

    if (len(ch) /= 0) error stop 5_4

    if (size(ch) /= 0) error stop 6_4
end
