! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/allocEnh/unit_tests/ReallocAssignArray13.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with a pointer array on the right-
!*                               hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type dt(n1,k1)    ! (20,4)
  integer, kind            :: k1
  integer, len             :: n1
  integer(k1), allocatable :: i(:)
end type
integer, pointer :: p(:)
type(dt(20,4)) :: x
allocate(p(6:9))
x%i = p
if (.not. allocated(x%i)) error stop 1
if (any(shape(x%i) .ne. (/4/))) error stop 2
if (lbound(x%i,1) /= 6) error stop 3
if (ubound(x%i,1) /= 9) error stop 4
end