! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/allocEnh/unit_tests/ReallocAssignArray15.f
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
type dt2(n2,k2)    ! (20,4)
  integer, kind        :: k2
  integer, len         :: n2
  integer(k2), pointer :: p(:)
end type
type(dt2(20,4)) :: y
type(dt(20,4)) :: x
allocate(y%p(6:9))
y%p = (/1,2,3,4/)
x%i = y%p(7:9)
if (.not. allocated(x%i)) error stop 1
if (any(shape(x%i) .ne. (/3/))) error stop 2
if (lbound(x%i,1) /= 1) error stop 3
if (ubound(x%i,1) /= 3) error stop 4
if (any(x%i /= (/2,3,4/))) error stop 5
end