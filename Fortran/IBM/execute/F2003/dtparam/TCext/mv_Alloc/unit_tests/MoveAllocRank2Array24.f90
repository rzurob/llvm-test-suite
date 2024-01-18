! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/mv_Alloc/unit_tests/MoveAllocRank2Array24.f
! opt variations: -qnol

! SCCS ID Information
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
type(t(20,4)), target :: a, b
integer, pointer :: p(:)
allocate(a%x(5))
p => a%x
call move_alloc(a%x,b%x)
if (allocated(a%x)) stop 1
if (.not.allocated(b%x)) stop 2
if (.not.associated(p,b%x)) stop 3
end
