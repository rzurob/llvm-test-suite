! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/mv_Alloc/unit_tests/MoveAllocRank2Array21.f
! opt variations: -qnol -qnodeferredlp

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
  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)      i
  contains
    procedure :: foo
    final :: finalproc
  end type

  integer :: finalcount = 0

contains

  subroutine foo(x, y)
    class(dt(*,4)) :: x
    integer :: y
    if (x%i /= y) error stop 10
  end subroutine

  subroutine finalproc(a)
    type(dt(*,4)) :: a(:,:)
    finalcount = finalcount + 1
  end subroutine

end module

use m
type(dt(20,4)), allocatable, target :: a(:,:)
type(dt(20,4)), allocatable, target :: b(:,:)
class(dt(:,4)), pointer :: p(:,:)
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
if (finalcount /= 0) error stop 1
call move_alloc(a,b)
if (finalcount /= 1) error stop 2
if (allocated(a)) error stop 3
if (.not.allocated(b)) error stop 4
if (.not.associated(p,b)) error stop 5
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
if (finalcount /= 1) error stop 6
end