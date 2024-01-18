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
!*  TEST CASE TITLE            : ReallocAssignScalarCharacter04
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

call sub()
contains
  subroutine sub()
    character(:), allocatable :: a
    allocate(character(40) :: a)
    a = f()
    if (.not.allocated(a)) stop 1
    if (len(a) /= 5) stop 2
    if (a /= 'abcde') stop 3
  end subroutine

  function f()
    character(5) f
    f = 'abcde'
  end function
end
