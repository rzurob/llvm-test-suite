! *********************************************************************
!* ===================================================================
!*
!* DATE : July 25, 2008
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!*
!* CASE:
!*  37)  Applying RESHAPE on a DT component of an allocatable involving offsets to components with LEN DTP
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
integer :: tt2(2,3)
integer :: j = 0
integer :: k

ALLOCATE(dtp(6) :: tt(1))

tt(1)%r = [1,2,3,4,5,6]

tt2 = reshape(tt(1)%r, (/2, 3/))

k=tt(1)%i
print *, k
print *, tt(1)%i
j = 1
do while (j .LT. 3)
  print *, tt2(j, 1), ' ', tt2(j, 2), ' ', tt2(j, 3)
  j = j+1
end do
end

