!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : reshapedtp55.f
!*
!* DATE : Jul31, 2008
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!* CASE:
!*  55) Applying RESHAPE to a dummy argument of DT of assumed shape where the result's dummy argument is also of assumed shape
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
subroutine sub1(dtp2, res)
  type (dtp(4,3)) dtp2(:,:), res(:,:)
  res = reshape(dtp2, (/2,4/), (/dtp(4,3)(10, 'xxx'), dtp(4,3)(11, 'yyy')/), (/2,1/))
end subroutine
end module

program a
use m
type (dtp(4,3)) :: dtp1(6) = (/dtp(4,3)(1,'abc'), dtp(4,3)(2,'def'), dtp(4,3)(3,'ghi'), dtp(4,3)(4,'jkl'), &
                             dtp(4,3)(5,'mno'), dtp(4,3)(6,'pqr')/)

type (dtp(4,3)) :: dtp2(2,3), res(2,4)

dtp2 = reshape(dtp1, (/2,3/))

call sub1(dtp2, res)

print *, res(1,1)%i, ' ', res(1,2)%i, ' ', res(1,3)%i, ' ', res(1,4)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i, ' ', res(2,4)%i

print *, res(1,1)%ch, ' ', res(1,2)%ch, ' ', res(1,3)%ch, ' ', res(1,4)%ch
print *, res(2,1)%ch, ' ', res(2,2)%ch, ' ', res(2,3)%ch, ' ', res(2,4)%ch

end


