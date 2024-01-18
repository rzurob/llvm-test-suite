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
    type(dt) :: a
    finalcount = finalcount + 1
  end subroutine

end module

use m
type(dt), allocatable, target :: a
type(dt), allocatable, target :: b
class(dt), pointer :: p
allocate(a)
a%i = 5
call a%foo(5)
p => a
call p%foo(5)
allocate(b)
b%i = 10
call b%foo(10)
if (finalcount /= 0) stop 1
call move_alloc(a,b)
if (finalcount /= 1) stop 2
if (allocated(a)) stop 3
if (.not.allocated(b)) stop 4
if (.not.associated(p,b)) stop 5
call b%foo(5)
call p%foo(5)
if (finalcount /= 1) stop 6
end
