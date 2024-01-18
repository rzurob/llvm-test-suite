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
!*  TEST CASE TITLE            : MoveAllocRank2Array03
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : 04/28/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : Use of MOVE_ALLOC with allocatable
!*                               rank-2 arrays
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
  type dt
    integer i
  contains
    procedure :: foo
  end type

  type, extends(dt) :: dt2
  contains
    procedure :: foo => foo2
  end type

contains

  subroutine foo(x, y)
    class(dt) :: x
    integer :: y
    if (x%i /= y) stop 10
  end subroutine

  subroutine foo2(x, y)
    class(dt2) :: x
    integer :: y
    if (x%i /= (y/2)) stop 11
  end subroutine

end module

use m
class(dt), allocatable, target :: a(:,:)
class(dt), allocatable, target :: b(:,:)
class(dt), pointer :: p(:,:)
integer :: count, count2
allocate(a(10,10))
a%i = 5
do count = lbound(a,1), ubound(a,1)
  do count2 = lbound(a,2), ubound(a,2)
    call a(count,count2)%foo(5)
  end do
end do
p => a
do count = lbound(p,1), ubound(p,1)
  do count2 = lbound(p,2), ubound(p,2)
    call p(count,count2)%foo(5)
  end do
end do
allocate(dt2 :: b(20,20))
b%i = 10
do count = lbound(b,1), ubound(b,1)
  do count2 = lbound(b,2), ubound(b,2)
    call b(count,count2)%foo(20)
  end do
end do
call move_alloc(a,b)
if (allocated(a)) stop 1
if (.not.allocated(b)) stop 2
if (.not.associated(p,b)) stop 3
do count = lbound(b,1), ubound(b,1)
  do count2 = lbound(b,2), ubound(b,2)
    call b(count,count2)%foo(5)
  end do
end do
do count = lbound(p,1), ubound(p,1)
  do count2 = lbound(p,2), ubound(p,2)
    call p(count,count2)%foo(5)
  end do
end do
end
