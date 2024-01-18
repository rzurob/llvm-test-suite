!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : reshapedtp56.f
!*
!* DATE : Jul31, 2008
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!* CASE:
!*  56) Applying RESHAPE to a DT with more pad elements than needed
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
end module

program a
use m

type (dtp(4,3)) :: dtp1(6) = (/dtp(4,3)(1,'abc'), dtp(4,3)(2,'def'), dtp(4,3)(3,'ghi'), dtp(4,3)(4,'jkl'), &
                             dtp(4,3)(5,'mno'), dtp(4,3)(6,'pqr')/)
type (dtp(4,3)) res(2,4)

res = reshape(dtp1, (/2, 4/), (/dtp(4,3)(10, 'xxx'),dtp(4,3)(11, 'yyy'),dtp(4,3)(12, 'zzz')/), (/1,2/))

print *, res(1,1)%i, ' ', res(1,2)%i, ' ', res(1,3)%i, ' ', res(1,4)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i, ' ', res(2,4)%i

print *, res(1,1)%ch, ' ', res(1,2)%ch, ' ', res(1,3)%ch, ' ', res(1,4)%ch
print *, res(2,1)%ch, ' ', res(2,2)%ch, ' ', res(2,3)%ch, ' ', res(2,4)%ch

end

