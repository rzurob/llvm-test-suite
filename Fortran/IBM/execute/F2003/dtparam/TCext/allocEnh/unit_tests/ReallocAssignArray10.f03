! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/unit_tests/ReallocAssignArray10.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with a non-1 lower bound on the
!*                               right-hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type dt(k1)    ! (4)
  integer, kind            :: k1
  integer(k1), allocatable :: i(:)
end type
integer :: i(2:4)
type(dt(4)) :: x
x%i = i
if (.not. allocated(x%i)) error stop 1
if (any(shape(x%i) .ne. (/3/))) error stop 2
if (lbound(x%i,1) /= 2) error stop 3
if (ubound(x%i,1) /= 4) error stop 4
end
