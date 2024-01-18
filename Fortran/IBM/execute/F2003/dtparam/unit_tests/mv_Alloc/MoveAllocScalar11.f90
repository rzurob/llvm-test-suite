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
type(dt(2)), allocatable, target :: foo, bar
call sub(foo, bar)
contains
  subroutine sub(a, b)
    type(dt(*)), allocatable, target :: a, b
    type(dt(:)), pointer :: p
    allocate(a)
    p => a
    call move_alloc(a,b)
    if (allocated(a)) stop 1
    if (.not.allocated(b)) stop 2
    if (.not.associated(p,b)) stop 3
  end subroutine
end
