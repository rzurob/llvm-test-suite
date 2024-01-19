! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/mv_Alloc/unit_tests/MoveAllocDiag17.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2006
!*
!*  DESCRIPTION                : Diagnostic test for MOVE_ALLOC
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type t(k1)    ! (4)
  integer, kind            :: k1
  integer(k1), allocatable :: x(:)
end type
type(t(4)) :: a, b
call move_alloc(a%x(:),b%x)
end
