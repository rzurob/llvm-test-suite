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
    if (x%i /= y) error stop 10
  end subroutine

  subroutine finalproc(a)
    type(dt) :: a(:,:)
    finalcount = finalcount + 1
  end subroutine

end module

use m
class(*), allocatable, target :: a(:,:)
class(*), allocatable, target :: b(:,:)
class(*), pointer :: p(:,:)
allocate(dt :: a(10,10))
p => a
allocate(dt :: b(20,20))
if (finalcount /= 0) error stop 1
call move_alloc(a,b)
if (finalcount /= 1) error stop 2
if (allocated(a)) error stop 3
if (.not.allocated(b)) error stop 4
if (.not.associated(p,b)) error stop 5
if (finalcount /= 1) error stop 6
end