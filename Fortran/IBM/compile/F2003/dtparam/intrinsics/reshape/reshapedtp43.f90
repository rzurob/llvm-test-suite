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
!* 43)  Applying RESHAPE with pad argument with different DTP
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
type (dtp(4)) :: dtp1(6) = (/dtp(4)(1), dtp(4)(2), dtp(4)(3), dtp(4)(4), &
                             dtp(4)(5), dtp(4)(6)/)
type (dtp(4)) dtp2(2,3)
type (dtp(2)) d(3)

d(1)%i = 11
d(2)%i = 12
d(3)%i = 13
dtp2 = reshape(dtp1, (/3, 3/),d)
end

