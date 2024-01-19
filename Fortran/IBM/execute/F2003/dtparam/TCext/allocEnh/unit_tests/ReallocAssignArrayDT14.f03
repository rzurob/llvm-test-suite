! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/unit_tests/ReallocAssignArrayDT14.f
! opt variations: -ql

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
type t(k1)    ! (4)
  integer, kind :: k1
  integer(k1)   :: a
end type
type(t(4)), allocatable :: a(:)
type(t(4)) b(5)
b = (/t(4)(1),t(4)(2),t(4)(3),t(4)(4),t(4)(5)/)
allocate(a(3))
associate (x => a)
  x = b
end associate
end
