!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : cshiftdtp48.f
!*
!* DATE : Jul31, 2008
!*
!* DESCRIPTION:
!* CSHIFT performs a circular shift on an array expression of rank one or perform circular
!* shifts on all the complete rank one sections along a given dimension of an array expression of
!* rank two or greater. Elements shifted out at one end of a section are shifted in at the other end.
!* Different sections may be shifted by different amounts and in different directions.
!* CASE:
!*  48) Applying CSHIFT to a dummy argument of DT pointer
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
  type (dtp(4,3)), pointer :: dtp2(:,:), res(:,:)
  allocate(res(3,3), SOURCE = cshift(dtp2, (/1,-1,0/),2))
end subroutine
end module

program a
use m
type (dtp(4,3)), pointer :: dtp1(:)
type (dtp(4,3)), pointer :: dtp2(:,:)
type (dtp(4,3)), pointer :: res(:,:)

allocate(dtp1(9), SOURCE = (/dtp(4,3)(1,'abc'), dtp(4,3)(2,'def'), dtp(4,3)(3,'ghi'), dtp(4,3)(4,'jkl'), &
                             dtp(4,3)(5,'mno'), dtp(4,3)(6,'pqr'), dtp(4,3)(7,'stu'), dtp(4,3)(8,'vwx'), &
                             dtp(4,3)(9,'yz1')/))
allocate(dtp2(3,3), SOURCE = reshape(dtp1, (/3, 3/)))

call sub1(dtp2, res)

print *, res(1,1)%i, ' ', res(1,2)%i, ' ', res(1,3)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i
print *, res(3,1)%i, ' ', res(3,2)%i, ' ', res(3,3)%i

print *, res(1,1)%ch, ' ', res(1,2)%ch, ' ', res(1,3)%ch
print *, res(2,1)%ch, ' ', res(2,2)%ch, ' ', res(2,3)%ch
print *, res(3,1)%ch, ' ', res(3,2)%ch, ' ', res(3,3)%ch
end

