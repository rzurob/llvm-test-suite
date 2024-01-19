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

type dt(x)
  integer, len :: x
  integer :: i(x)
end type
type(dt(:)), allocatable, target :: foo(:)
call sub(foo)
contains
  subroutine sub(a)
    type(dt(:)), allocatable, target :: a(:)
    type(dt(:)), allocatable, target :: b(:)
    type(dt(2)), pointer :: p(:)
    allocate(dt(2) :: a(10))
    p => a
    call move_alloc(a,b)
    if (allocated(a)) error stop 1
    if (.not.allocated(b)) error stop 2
    if (.not.associated(p,b)) error stop 3
  end subroutine
end
