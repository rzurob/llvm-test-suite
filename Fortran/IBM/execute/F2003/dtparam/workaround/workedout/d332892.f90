!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2009-11-30
!*
!*  DESCRIPTION                : defect 332892
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

implicit none
integer j
type base (k, n)
  integer, kind :: k, n
end type

type, extends(base) :: child
  logical(k) :: flag(n) = (/ (.true., j=1,n) /)  ! AC is fine.  AC-IMPDO is not
end type

class(base(4, 22)), allocatable :: b1(:)  ! ICE and bad messages
!type(base(4, 22)) :: b2                  ! bad messages only
end
