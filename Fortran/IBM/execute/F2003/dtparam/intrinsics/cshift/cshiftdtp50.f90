!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : cshiftdtp50.f
!*
!* DATE : Jul31, 2008
!*
!* DESCRIPTION:
!* CSHIFT performs a circular shift on an array expression of rank one or perform circular
!* shifts on all the complete rank one sections along a given dimension of an array expression of
!* rank two or greater. Elements shifted out at one end of a section are shifted in at the other end.
!* Different sections may be shifted by different amounts and in different directions.
!* CASE:
!*  50) Applying CSHIFT to a dummy argument of DT with pointer component
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
module m
type dtp (k)
  integer, kind :: k
  integer(k), pointer :: i => null()
end type

contains
subroutine sub1(dtp2, res)
  type (dtp(4)) dtp2(3,3), res(3,3)
  res = cshift(dtp2, (/1,-1,0/))
end subroutine
end module

program a
use m

type (dtp(4)) :: dtp1(9)
type (dtp(4)) dtp2(3,3), res(3,3)

do j = 1,9
  allocate(dtp1(j)%i , SOURCE=j)
end do

dtp2 = reshape(dtp1, (/3, 3/))

call sub1(dtp2, res)

print *, res(1,1)%i, ' ', res(1,2)%i, ' ', res(1,3)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i
print *, res(3,1)%i, ' ', res(3,2)%i, ' ', res(3,3)%i
end


