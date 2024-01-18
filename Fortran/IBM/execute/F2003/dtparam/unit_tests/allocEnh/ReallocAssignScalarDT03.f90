!#######################################################################
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
type(t(4,:)), allocatable, target :: a
type(t(4,5)) :: b
type(t(4,:)), pointer :: p
b%arr = (/1,2,3,4,5/)
allocate(t(4,5) :: a)
p => a
a = b
if (.not.allocated(a)) stop 1
if (a%l /= 5) stop 2
if (any(shape(a%arr) /= (/5/))) stop 3
if (a%k /= 4) stop 4
if (any(a%arr /= (/1,2,3,4,5/))) stop 5
if (kind(a%arr) /= 4) stop 6
if (.not.associated(p,a)) stop 7
end
