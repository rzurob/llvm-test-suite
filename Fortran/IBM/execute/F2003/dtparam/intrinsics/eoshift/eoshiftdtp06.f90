!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : eoshiftdtp06.f
!*
!* CREATED BY: Pooja Dayanand
!* MODIFIED BY: Andy Sheung
!* DATE : Jul31, 2008
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
!*  06) Applying EOSHIFT with DT with integer component with KIND and giving it a value of a KIND DTP
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
res = eoshift(dtp2, (/1,-1,0/), dtp(4,3)(1),DIM=2)

print *, res(1,1)%i, ' ', res(1,2)%i, ' ', res(1,3)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i
print *, res(3,1)%i, ' ', res(3,2)%i, ' ', res(3,3)%i
end
