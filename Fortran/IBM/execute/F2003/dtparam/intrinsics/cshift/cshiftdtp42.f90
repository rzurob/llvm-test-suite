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
!*  42) Allocating a pointer with RESHAPE intrinsic as the source and having an implied-do array constructor as the source of RESHAPE, then apply CSHIFT on that
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
type dtp (k)
  integer, kind :: k
  integer(k) ::  i
end type
end module

program a
use m

type (dtp(4)) :: dtp1(9)
type (dtp(4)), pointer :: dtp2(:,:)

allocate(dtp2(3,3), SOURCE = cshift((reshape((/(dtp(4)(i), i=1,9)/), (/3, 3/))), (/1,-1,0/), 2))

print *, dtp2(1,1)%i, ' ', dtp2(1,2)%i, ' ', dtp2(1,3)%i
print *, dtp2(2,1)%i, ' ', dtp2(2,2)%i, ' ', dtp2(2,3)%i
print *, dtp2(3,1)%i, ' ', dtp2(3,2)%i, ' ', dtp2(3,3)%i

end

