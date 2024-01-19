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
!*  45) Applying CSHIFT with dim in an initialization expression
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
type dtp (k)
  integer, kind :: k
  character(1)  :: c
  logical(2)    :: l(3,3)=.false.
end type
end module

program a
use m

type(dtp(1)), parameter :: d(3,3)=reshape((/ &
  dtp(1)("a"),dtp(1)("d"),dtp(1)("g"),     &
  dtp(1)("b"),dtp(1)("e"),dtp(1)("h"),     &
  dtp(1)("c"),dtp(1)("f"),dtp(1)("i")/),(/3,3/))

type(dtp(1)) :: res(3,3)=cshift(d, shift=-1, dim=2 )

print *, res(1,1)%c, ' ', res(1,2)%c, ' ', res(1,3)%c
print *, res(2,1)%c, ' ', res(2,2)%c, ' ', res(2,3)%c
print *, res(3,1)%c, ' ', res(3,2)%c, ' ', res(3,3)%c
end

