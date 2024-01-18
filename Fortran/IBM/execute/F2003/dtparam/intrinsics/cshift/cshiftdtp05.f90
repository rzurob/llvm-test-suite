!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : cshiftdtp05.f
!*
!* CREATED BY: Pooja Dayanand
!* MODIFIED BY: Andy Sheung
!* DATE : Jul31, 2008
!*
!* DESCRIPTION:
!* CSHIFT performs a circular shift on an array expression of rank one or perform circular
!* shifts on all the complete rank one sections along a given dimension of an array expression of
!* rank two or greater. Elements shifted out at one end of a section are shifted in at the other end.
!* Different sections may be shifted by different amounts and in different directions.
!*
!* CASE:
!* 05) Applying CSHIFT with DT with integer component with KIND and giving it a value of a KIND DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (k, n)
  integer, kind :: k, n
  integer(k) ::  i = n
end type

type (dtp(4, 3)) :: dtp1(6)
type (dtp(4, 3)) dtp2(3,3), res(3,3)

dtp2 = reshape(dtp1, (/3, 3/), (/dtp(4,3)(0)/))
res = cshift(dtp2, (/1,-1,0/), 2)

print *, res(1,1)%i, ' ', res(1,2)%i, ' ', res(1,3)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i
print *, res(3,1)%i, ' ', res(3,2)%i, ' ', res(3,3)%i
end
