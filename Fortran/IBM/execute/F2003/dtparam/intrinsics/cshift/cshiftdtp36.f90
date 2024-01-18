!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : cshiftdtp36.f
!*
!* DATE : July 25, 2008
!*
!* DESCRIPTION:
!* CSHIFT performs a circular shift on an array expression of rank one or perform circular
!* shifts on all the complete rank one sections along a given dimension of an array expression of
!* rank two or greater. Elements shifted out at one end of a section are shifted in at the other end.
!* Different sections may be shifted by different amounts and in different directions.
!*
!* CASE:
!*  36) Applying CSHIFT on a DT component of an allocatable with deferred LEN DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
type dtp (l)
  integer, len :: l
  integer :: i
end type
end module

program a
use m

type (dtp(:)), allocatable :: dtp1(:,:)
type (dtp(:)), allocatable :: dtp2(:,:)
type (dtp(:)), allocatable :: d(:)

allocate(dtp1(3,3), SOURCE = reshape((/(dtp(9)(i), i=1,9)/), (/3, 3/)))
allocate(dtp(9)::dtp2(3,3))

allocate(dtp(9)::d(2))
d(1)%i = 11
d(2)%i = 12

dtp2 = eoshift(dtp1, (/1,-1,0/), d, 2)

print *, dtp2(1,1)%i, ' ', dtp2(1,2)%i, ' ', dtp2(1,3)%i
print *, dtp2(2,1)%i, ' ', dtp2(2,2)%i, ' ', dtp2(2,3)%i
print *, dtp2(3,1)%i, ' ', dtp2(3,2)%i, ' ', dtp2(3,3)%i
end


