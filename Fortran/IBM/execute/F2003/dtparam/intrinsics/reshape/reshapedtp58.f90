!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : reshapedtp58.f
!*
!* DATE : Jul31, 2008
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!* CASE:
!*  58) Applying RESHAPE to a DT where pad is a dummy argument of assumed shape
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
module m
type dtp (k,l)
  integer, kind :: k
  integer, len :: l
  integer(k) ::  i
  character(l) :: ch
end type

contains
subroutine sub1(dtp2, res, pad)
  type (dtp(4,3)) dtp2(2,3), res(4,2), pad(:)
  res = reshape(dtp2, (/4,2/), pad, (/2,1/))
end subroutine
end module

program a
use m

type (dtp(4,3)) :: dtp1(6) = (/dtp(4,3)(1,'abc'), dtp(4,3)(2,'def'), dtp(4,3)(3,'ghi'), dtp(4,3)(4,'jkl'), &
                             dtp(4,3)(5,'mno'), dtp(4,3)(6,'pqr')/)

type (dtp(4,3)) dtp2(2,3), res(4,2), pad(2)

pad(1)%i= 10
pad(1)%ch = 'xxx'
pad(2)%i = 11
pad(2)%ch = 'yyy'

dtp2 = reshape(dtp1, (/2, 3/))

call sub1(dtp2, res, pad)

print *, res(1,1)%i, ' ', res(1,2)%i
print *, res(2,1)%i, ' ', res(2,2)%i
print *, res(3,1)%i, ' ', res(3,2)%i
print *, res(4,1)%i, ' ', res(4,2)%i

print *, res(1,1)%ch, ' ', res(1,2)%ch
print *, res(2,1)%ch, ' ', res(2,2)%ch
print *, res(3,1)%ch, ' ', res(3,2)%ch
print *, res(4,1)%ch, ' ', res(4,2)%ch
end

