! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/mv_Alloc/unit_tests/MoveAllocRank2Array25.f
! opt variations: -qnol

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

type t(n1,k1)    ! (20,4)
  integer, kind            :: k1
  integer, len             :: n1
  integer(k1), allocatable :: x(:)
end type
type(t(20,4)), target :: a(5), b(10)
integer, pointer :: p(:)
allocate(a(3)%x(5))
p => a(3)%x
call move_alloc(a(3)%x,b(5)%x)
if (allocated(a(3)%x)) error stop 1
if (.not.allocated(b(5)%x)) error stop 2
if (.not.associated(p,b(5)%x)) error stop 3
end
