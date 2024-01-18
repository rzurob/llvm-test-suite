!#######################################################################
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

type t
  integer, allocatable :: x
end type
type(t), target :: a, b
integer, pointer :: p
allocate(a%x)
p => a%x
call move_alloc(a%x,b%x)
if (allocated(a%x)) stop 1
if (.not.allocated(b%x)) stop 2
if (.not.associated(p,b%x)) stop 3
end
