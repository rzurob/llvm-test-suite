! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/mv_Alloc/unit_tests/MoveAllocScalar04.f
! opt variations: -ql

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
type(dt(4)), allocatable, target :: a
type(dt(4)), allocatable, target :: b
class(dt(4)), pointer :: p
allocate(a)
a%i = 5
call a%foo(5)
p => a
call p%foo(5)
allocate(b)
b%i = 10
call b%foo(10)
call move_alloc(a,b)
if (allocated(a)) stop 1
if (.not.allocated(b)) stop 2
if (.not.associated(p,b)) stop 3
call b%foo(5)
call p%foo(5)
end
