! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2006
!*
!*  DESCRIPTION                : Diagnostic test for MOVE_ALLOC
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type t
  character(5), allocatable :: c
end type
type(t) :: x(10), y
call move_alloc(x(4)%c, y%c(:))
end