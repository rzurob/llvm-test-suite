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
!*  44) Applying RESHAPE with pad and order in an initialization expression
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

type(dtp(1)), parameter :: d(2,3)=reshape((/ &
  dtp(1)("a"),dtp(1)("d"),     &
  dtp(1)("b"),dtp(1)("e"),     &
  dtp(1)("c"),dtp(1)("f")/),(/2,3/))

type(dtp(1)) :: res(3,3)=reshape(d, (/3,3/), pad=(/dtp(1)("*"),dtp(1)("*"),dtp(1)("*")/), order=(/1,2/) )

print *, res(1,1)%c, ' ', res(1,2)%c, ' ', res(1,3)%c
print *, res(2,1)%c, ' ', res(2,2)%c, ' ', res(2,3)%c
print *, res(3,1)%c, ' ', res(3,2)%c, ' ', res(3,3)%c
end

