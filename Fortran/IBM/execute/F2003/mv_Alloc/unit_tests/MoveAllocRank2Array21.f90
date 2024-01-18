!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2006
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
    final :: finalproc
  end type

  integer :: finalcount = 0

contains

  subroutine foo(x, y)
    class(dt) :: x
    integer :: y
    if (x%i /= y) stop 10
  end subroutine

  subroutine finalproc(a)
    type(dt) :: a(:,:)
    finalcount = finalcount + 1
  end subroutine

end module

use m
type(dt), allocatable, target :: a(:,:)
type(dt), allocatable, target :: b(:,:)
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
allocate(b(20,20))
b%i = 10
do count = lbound(b,1), ubound(b,1)
  do count2 = lbound(b,2), ubound(b,2)
    call b(count,count2)%foo(10)
  end do
end do
if (finalcount /= 0) stop 1
call move_alloc(a,b)
if (finalcount /= 1) stop 2
if (allocated(a)) stop 3
if (.not.allocated(b)) stop 4
if (.not.associated(p,b)) stop 5
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
if (finalcount /= 1) stop 6
end
