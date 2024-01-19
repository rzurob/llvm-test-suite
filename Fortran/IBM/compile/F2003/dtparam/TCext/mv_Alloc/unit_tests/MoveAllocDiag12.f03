! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/mv_Alloc/unit_tests/MoveAllocDiag12.f
! opt variations: -qnol -qnodeferredlp

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
  integer, kind :: k1
  integer, len  :: n1
  integer(k1)      i
end type
class(t(:,4)), allocatable :: a
type(t(:,4)), allocatable :: b
call move_alloc(a,b)
end
