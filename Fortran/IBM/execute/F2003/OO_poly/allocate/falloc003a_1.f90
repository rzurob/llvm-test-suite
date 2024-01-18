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
! %GROUP: falloc003a_1.f
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
!*  DATE                       : 07/22/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (allocate rank 3 array with scalar
!                               source-expr; use character and integer types)
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

program falloc003a_1
    class (*), allocatable :: x(:,:,:)

    character(7), pointer :: c1(:,:,:)
    integer(2), allocatable :: i1(:,:,:)

    allocate (x(0:2,2,-1:1), source='xlftest')

    allocate (c1(0:2,2,-1:1), source='xlftest')

    allocate (i1(2,9:11,1000:1003), source=2_2)

    if (any (shape(x) /= (/3,2,3/))) error stop 1_4

    if (any (lbound(c1) /= (/0,1,-1/)) .or. any (ubound(c1) /= (/2,2,1/))) &
            error stop 2_4

    if (any (c1 /= 'xlftest')) error stop 3_4

    if (sum (i1) /= 48_2) error stop 4_4
end
