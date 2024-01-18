! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/mv_Alloc/unit_tests/MoveAllocRank1Array02.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2006
!*
!*  DESCRIPTION                : Use of MOVE_ALLOC with allocatable
!*                               rank-1 arrays
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
  type dt(k1)    ! (4)
    integer, kind :: k1
    integer(k1)      i
  contains
    procedure :: foo
  end type
contains
  subroutine foo(x, y)
    class(dt(4)) :: x
    integer :: y
    if (x%i /= y) stop 10
  end subroutine
end module

use m
type(dt(4)), allocatable, target :: a(:)
class(dt(4)), allocatable, target :: b(:)
class(dt(4)), pointer :: p(:)
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
