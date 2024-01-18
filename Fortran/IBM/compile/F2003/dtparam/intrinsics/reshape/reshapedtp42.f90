!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : reshapedtp42.f
!*
!* DATE : July 25, 2008
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!*
!* CASE:
!* 42) Applying RESHAPE on an array with different DTP
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
type (dtp(4)) :: dtp1(9) = (/dtp(4)(1), dtp(4)(2), dtp(4)(3), dtp(4)(4), &
                             dtp(4)(5), dtp(4)(6), dtp(4)(7), dtp(4)(8), &
                             dtp(4)(9)/)
type (dtp(2)) dtp2(3,3)

dtp2 = reshape(dtp1, (/3, 3/))
end



