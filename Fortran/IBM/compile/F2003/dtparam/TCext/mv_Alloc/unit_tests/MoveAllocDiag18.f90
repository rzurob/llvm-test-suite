! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/mv_Alloc/unit_tests/MoveAllocDiag18.f
! opt variations: -qnol

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

type t(n1,k1)    ! (20,4)
  integer, kind            :: k1
  integer, len             :: n1
  integer(k1), allocatable :: x(:)
end type
type(t(20,4)) :: a, b
call move_alloc(a%x(3),b%x(5))
end
