!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : cshiftdtp07.f
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
!* 07) Applying CSHIFT to a DT with a DT nested type, both with DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dt (k)
  integer, kind :: k
  integer(k) ::  i
end type

type dtp (k1)
  integer, kind :: k1
  type (dt(k1)) :: d1
end type

type (dtp(4)) :: dtp1(9) = (/dtp(4)(dt(4)(1)), dtp(4)(dt(4)(2)), &
                             dtp(4)(dt(4)(3)), dtp(4)(dt(4)(4)), &
                             dtp(4)(dt(4)(5)), dtp(4)(dt(4)(6)), &
                             dtp(4)(dt(4)(7)), dtp(4)(dt(4)(8)), &
                             dtp(4)(dt(4)(9))/)
type (dtp(4)) dtp2(3,3), res(3,3)

dtp2 = reshape(dtp1, (/3, 3/))
res = cshift(dtp2, (/1,-1,0/))

print *, res(1,1)%d1%i, ' ', res(1,2)%d1%i, ' ', res(1,3)%d1%i
print *, res(2,1)%d1%i, ' ', res(2,2)%d1%i, ' ', res(2,3)%d1%i
print *, res(3,1)%d1%i, ' ', res(3,2)%d1%i, ' ', res(3,3)%d1%i
end
