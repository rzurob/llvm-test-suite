! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/allocEnh/unit_tests/ReallocAssignArray09.f
! opt variations: -qnol

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with components of derived types, and
!*                               an array expression on the right-hand
!*                               side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type dt(n1,k1)    ! (20,4)
  integer, kind            :: k1
  integer, len             :: n1
  integer(k1), allocatable :: i(:)
end type
integer :: i(2:4)
type(dt(20,4)) :: x
x%i = abs(i)
if (.not. allocated(x%i)) stop 1
if (any(shape(x%i) .ne. (/3/))) stop 2
if (lbound(x%i,1) /= 1) stop 3
if (ubound(x%i,1) /= 3) stop 4
end
