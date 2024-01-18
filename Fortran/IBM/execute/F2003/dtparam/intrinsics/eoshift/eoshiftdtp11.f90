!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : eoshiftdtp11.f
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
!*  11) Applying EOSHIFT to a DT's DT subtype's pointer component
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dt (k)
  integer, kind :: k
  integer(k), pointer :: i => null()
end type

type dtp (k1)
  integer, kind :: k1
  type (dt(k1)) :: d1
end type

type (dtp(4)) :: dtp1(9)
type (dtp(4)) dtp2(3,3), res(3,3), tar

do j = 1,9
  allocate(dtp1(j)%d1%i , SOURCE=j)
end do

allocate(tar%d1%i, SOURCE=10)

dtp2 = reshape(dtp1, (/3, 3/))
res = eoshift(dtp2, (/1,-1,0/), tar)

print *, res(1,1)%d1%i, ' ', res(1,2)%d1%i, ' ', res(1,3)%d1%i
print *, res(2,1)%d1%i, ' ', res(2,2)%d1%i, ' ', res(2,3)%d1%i
print *, res(3,1)%d1%i, ' ', res(3,2)%d1%i, ' ', res(3,3)%d1%i
end

