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
!*  TEST CASE TITLE            : MoveAllocRank1Array02
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : 04/28/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : Use of MOVE_ALLOC with allocatable
!*                               rank-1 arrays
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
  type dt
    integer i
  contains
    procedure :: foo
  end type
contains
  subroutine foo(x, y)
    class(dt) :: x
    integer :: y
    if (x%i /= y) stop 10
  end subroutine
end module

use m
type(dt), allocatable, target :: a(:)
class(dt), allocatable, target :: b(:)
class(dt), pointer :: p(:)
integer :: count
allocate(a(10))
a%i = 5
do count = lbound(a,1), ubound(a,1)
  call a(count)%foo(5)
end do
p => a
do count = lbound(p,1), ubound(p,1)
  call p(count)%foo(5)
end do
allocate(b(20))
b%i = 10
do count = lbound(b,1), ubound(b,1)
  call b(count)%foo(10)
end do
call move_alloc(a,b)
if (allocated(a)) stop 1
if (.not.allocated(b)) stop 2
if (.not.associated(p,b)) stop 3
do count = lbound(b,1), ubound(b,1)
  call b(count)%foo(5)
end do
do count = lbound(p,1), ubound(p,1)
  call p(count)%foo(5)
end do
end
