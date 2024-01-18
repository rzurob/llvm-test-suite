! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/mv_Alloc/unit_tests/MoveAllocRank1Array24.f
! opt variations: -ql

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

type t(k1)    ! (4)
  integer, kind            :: k1
  integer(k1), allocatable :: x(:)
end type
type(t(4)), target :: a, b
integer, pointer :: p(:)
allocate(a%x(5))
p => a%x
call move_alloc(a%x,b%x)
if (allocated(a%x)) error stop 1
if (.not.allocated(b%x)) error stop 2
if (.not.associated(p,b%x)) error stop 3
end
