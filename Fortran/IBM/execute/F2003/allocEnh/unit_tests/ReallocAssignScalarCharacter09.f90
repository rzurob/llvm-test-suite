!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ReallocAssignScalarCharacter09
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : June 12, 2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with a scalar, deferred-length 
!*                               character on the left-hand side.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

call sub(12)
contains
  subroutine sub(n)
    integer n
    character(:), allocatable :: a
    character(n) b
    b = 'abcdefghijkl'
    a = b
    if (.not.allocated(a)) stop 1
    if (len(a) /= 12) stop 2
    if (a /= 'abcdefghijkl') stop 3
  end subroutine
end
