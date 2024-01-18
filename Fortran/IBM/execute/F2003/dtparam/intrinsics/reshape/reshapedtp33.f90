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
!*  33)  Applying RESHAPE on a DT component of an allocatable with run time known LEN DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
type dtp (n)
  integer, len :: n = 6
  integer ::  i(n)
end type
end module

program a
use m

type(dtp(:)), allocatable :: x1

integer :: x2(2,3)

allocate(dtp :: x1)

x1%i = [1,2,3,4,5,6]

x2 = reshape(x1%i, (/2, 3/))

j = 1
do while (j .LT. 3)
  print *, x2(j, 1), ' ', x2(j, 2), ' ', x2(j, 3)
  j = j+1
end do
end

