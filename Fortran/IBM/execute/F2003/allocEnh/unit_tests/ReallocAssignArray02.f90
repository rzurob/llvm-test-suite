!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment
!*                               with a function call as the right-
!*                               hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

real(8), allocatable :: a(:)
allocate(a(6))
a = f()
if (any(shape(a) /= (/5/))) stop 1
if (any(a /= 1.0_8)) stop 2
if (lbound(a,1) /= 1) stop 3
if (ubound(a,1) /= 5) stop 4
contains
  function f()
    real(8), allocatable :: f(:)
    allocate(f(5))
    f = 1.0_8
  end function
end
