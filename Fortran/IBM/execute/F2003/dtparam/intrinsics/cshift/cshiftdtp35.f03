! *********************************************************************
!* ===================================================================
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
!*  35) Applying CSHIFT on a DT component of an pointer with run time known LEN DTP
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

type(dtp(:)), pointer :: x1

integer :: res(3,3)

allocate(dtp :: x1)

x1%i = [1,2,3,4,5,6,7,8,9]

res = cshift((reshape(x1%i, (/3, 3/))),  (/1,-1,0/))

print *, res(1,1), ' ', res(1,2), ' ', res(1,3)
print *, res(2,1), ' ', res(2,2), ' ', res(2,3)
print *, res(3,1), ' ', res(3,2), ' ', res(3,3)
end

