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
    final :: finalproc
  end type

  integer :: finalcount = 0

contains

  subroutine finalproc(a)
    type(dt) :: a
    finalcount = finalcount + 1
  end subroutine

end module

use m
class(*), allocatable, target :: a
class(*), allocatable, target :: b
class(*), pointer :: p
allocate(dt :: a)
p => a
allocate(dt :: b)
if (finalcount /= 0) error stop 1
call move_alloc(a,b)
if (finalcount /= 1) error stop 2
if (allocated(a)) error stop 3
if (.not.allocated(b)) error stop 4
if (.not.associated(p,b)) error stop 5
if (finalcount /= 1) error stop 6
end