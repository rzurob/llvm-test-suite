!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : eoshiftdtp36.f
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
!*  36) Applying EOSHIFT on a DT component of a pointer with run time known LEN DTP with boundary argument
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
type dtp (n)
  integer, len :: n = 9
  integer ::  i(n)
end type
end module

program a
use m

type(dtp(:)), pointer :: x1,x2

integer :: res(3,3)

allocate(dtp :: x1)

x1%i = [1,2,3,4,5,6,7,8,9]

allocate(dtp :: x2)

x2%i = [11,12]

res = eoshift((reshape(x1%i, (/3, 3/))),  (/1,-1,0/), (/x2%i, x2%i/))

print *, res(1,1), ' ', res(1,2), ' ', res(1,3)
print *, res(2,1), ' ', res(2,2), ' ', res(2,3)
print *, res(3,1), ' ', res(3,2), ' ', res(3,3)
end

