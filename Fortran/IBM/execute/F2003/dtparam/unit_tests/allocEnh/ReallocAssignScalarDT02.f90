! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 12, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with a scalar, deferred-length
!*                               character on the left-hand side.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type t(k,l)
  integer, kind :: k
  integer, len :: l
  integer(k) arr(l)
end type
type(t(4,:)), allocatable :: a
type(t(4,5)) :: b
b%arr = (/1,2,3,4,5/)
allocate(t(4,6) :: a)
a = b
if (.not.allocated(a)) error stop 1
if (a%l /= 5) error stop 2
if (any(shape(a%arr) /= (/5/))) error stop 3
if (a%k /= 4) error stop 4
if (any(a%arr /= (/1,2,3,4,5/))) error stop 5
if (kind(a%arr) /= 4) error stop 6
end
