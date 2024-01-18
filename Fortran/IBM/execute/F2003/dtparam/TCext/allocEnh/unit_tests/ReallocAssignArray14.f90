! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/unit_tests/ReallocAssignArray14.f
! opt variations: -ql

! SCCS ID Information
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

type dt(k1)    ! (4)
  integer, kind            :: k1
  integer(k1), allocatable :: i(:)
end type
type dt2(k2)    ! (4)
  integer, kind        :: k2
  integer(k2), pointer :: p(:)
end type
type(dt2(4)) :: y
type(dt(4)) :: x
allocate(y%p(6:9))
y%p = (/4,3,2,1/)
x%i = y%p
if (.not. allocated(x%i)) stop 1
if (any(shape(x%i) .ne. (/4/))) stop 2
if (lbound(x%i,1) /= 6) stop 3
if (ubound(x%i,1) /= 9) stop 4
if (any(x%i /= (/4,3,2,1/))) stop 5
end
