! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/mv_Alloc/unit_tests/MoveAllocScalar03.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2006
!*
!*  DESCRIPTION                : Use of MOVE_ALLOC with allocatable
!*                               scalars
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
  end type

  type, extends(dt) :: dt2    ! (20,4)
  contains
    procedure :: foo => foo2
  end type

contains

  subroutine foo(x, y)
    class(dt(*,4)) :: x
    integer :: y
    if (x%i /= y) error stop 10
  end subroutine

  subroutine foo2(x, y)
    class(dt2(*,4)) :: x
    integer :: y
    if (x%i /= (y/2)) error stop 11
  end subroutine

end module

use m
class(dt(20,4)), allocatable, target :: a
class(dt(:,4)), allocatable, target :: b
class(dt(:,4)), pointer :: p
allocate(a)
a%i = 5
call a%foo(5)
p => a
call p%foo(5)
allocate(dt2(20,4) :: b)
b%i = 10
call b%foo(20)
call move_alloc(a,b)
if (allocated(a)) error stop 1
if (.not.allocated(b)) error stop 2
if (.not.associated(p,b)) error stop 3
call b%foo(5)
call p%foo(5)
end
