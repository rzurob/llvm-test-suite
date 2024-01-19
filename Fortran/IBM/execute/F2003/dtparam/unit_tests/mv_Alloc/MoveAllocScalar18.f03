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

type dt(x)
  integer, len :: x
  integer :: i(x)
end type
type(dt(2)), allocatable, target :: foo
call sub(foo)
contains
  subroutine sub(a)
    type(dt(*)), allocatable, target :: a
    type(dt(:)), allocatable, target :: b
    type(dt(:)), pointer :: p
    allocate(dt(2) :: b)
    p => b
    call move_alloc(b,a)
    if (allocated(b)) error stop 1
    if (.not.allocated(a)) error stop 2
    if (.not.associated(p,a)) error stop 3
  end subroutine
end
