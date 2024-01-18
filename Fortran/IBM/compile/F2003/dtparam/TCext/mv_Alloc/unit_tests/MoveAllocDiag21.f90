! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/mv_Alloc/unit_tests/MoveAllocDiag21.f
! opt variations: -ql

! SCCS ID Information
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
type(t(4)) :: a(5), b(10)
call move_alloc(a(3)%x(:),b(2)%x)
end
