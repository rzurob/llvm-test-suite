! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/unit_tests/ReallocAssignArrayDT15.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 9, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with an array of derived types with
!*                               a deferred length type parameter on
!*                               the left-hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

@process check
type t(n1,k1)    ! (20,4)
  integer, kind :: k1
  integer, len  :: n1
  integer(k1)   :: a
end type
type(t(:,4)), allocatable :: a(:)
type(t(20,4)) b(5)
b = (/t(20,4)(1),t(20,4)(2),t(20,4)(3),t(20,4)(4),t(20,4)(5)/)
allocate(t(20,4) :: a(3))
associate (x => a)
  a = b
end associate
if (.not. allocated(a)) stop 1
if (any(shape(a) /= shape(b))) stop 2
if (lbound(a,1) /= 1) stop 3
if (ubound(a,1) /= 5) stop 4
if (any(a%a /= (/1,2,3,4,5/))) stop 5
end
