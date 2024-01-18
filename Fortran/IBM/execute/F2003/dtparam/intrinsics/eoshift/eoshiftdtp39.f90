!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : eoshiftdtp39.f
!*
!* DATE : July 25, 2008
!*
!* DESCRIPTION:
!* EOSHIFT performs an end-off shift on an array expression of rank one
!* or perform end-off shifts on all the complete rank-one sections along a given
!* dimension of an array expression of rank two or greater. Elements are shifted
!* off at one end of a section and copies of a boundary value are shifted in at
!* the other end. Different sections may have different boundary values and may
!* be shifted by different amounts and in different directions.
!*
!* CASE:
!*  39) Applying EOSHIFT on a DT component of an allocatable involving offsets to components with LEN DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
type dtp (l)
  integer, len :: l
  real(4) :: r(l)
  integer ::  i=8
end type
end module

program a
use m

type (dtp(:)), allocatable :: tt(:)
integer :: tt2(3,3)
integer :: j = 0
integer :: k

ALLOCATE(dtp(9) :: tt(1))

tt(1)%r = [1,2,3,4,5,6,7,8,9]

tt2 = eoshift((reshape(tt(1)%r, (/3, 3/))), (/1,-1,0/))

k=tt(1)%i
print *, k
print *, tt(1)%i

print *, tt2(1,1), ' ', tt2(1,2), ' ', tt2(1,3)
print *, tt2(2,1), ' ', tt2(2,2), ' ', tt2(2,3)
print *, tt2(3,1), ' ', tt2(3,2), ' ', tt2(3,3)
end
