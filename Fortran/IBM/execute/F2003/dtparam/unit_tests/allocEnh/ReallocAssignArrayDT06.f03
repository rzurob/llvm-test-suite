! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with an array of derived types with
!*                               a deferred length type parameter on
!*                               the left-hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

@process check
@process XLF2003(NOAUTOREALLOC)

type t(x)
  integer, len :: x
  integer :: a
end type
type(t(3)), allocatable :: a(:)
type(t(3)) :: b(5)
allocate(a(4))
b = (/t(3)(1),t(3)(2),t(3)(3),t(3)(4),t(3)(5)/)
a = b(2:3)
end