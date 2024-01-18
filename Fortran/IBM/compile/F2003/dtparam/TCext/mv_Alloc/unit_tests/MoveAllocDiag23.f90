! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/mv_Alloc/unit_tests/MoveAllocDiag23.f
! opt variations: -qnock

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2006
!*
!*  DESCRIPTION                : Diagnostic test for MOVE_ALLOC
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type t(k1,n1)    ! (1,5)
  integer, kind                          :: k1
  integer, len                           :: n1
  character(kind=k1,len=n1), allocatable :: c
end type
type(t(1,5)) :: x(10), y
call move_alloc(x(4)%c, y%c(:))
end
