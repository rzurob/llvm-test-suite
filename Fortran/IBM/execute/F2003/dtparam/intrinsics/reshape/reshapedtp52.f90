!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case IBM INTERNAL USE ONLY
!* ===================================================================
!* ===================================================================
!*
!* TEST CASE TITLE : Test for DTP with RESHAPE
!*
!* TEST CASE NAME : reshapedtp52.f
!*
!* PROGRAMMER : Andy Sheung
!* DATE : Jul31, 2008
!* ORIGIN : AIX Compiler Development, Toronto Lab
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!* CASE:
!*  52) Applying EOSHIFT to a dummy argument of DT allocatable with assumed LEN DTP
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
  type (dtp(4,*)), allocatable :: dtp2(:,:), res(:,:)
  allocate(res(2,4), SOURCE = reshape(dtp2, (/2,4/), (/dtp(4,3)(10, 'xxx'), dtp(4,3)(11, 'yyy')/), (/2,1/)))
end subroutine
end module

program a
use m

type (dtp(4,3)), allocatable :: dtp1(:)
type (dtp(4,3)), allocatable :: dtp2(:,:)
type (dtp(4,3)), allocatable :: res(:,:)

allocate(dtp1(6), SOURCE = (/dtp(4,3)(1,'abc'), dtp(4,3)(2,'def'), dtp(4,3)(3,'ghi'), dtp(4,3)(4,'jkl'), &
                             dtp(4,3)(5,'mno'), dtp(4,3)(6,'pqr')/))

allocate(dtp2(2,3), SOURCE = reshape(dtp1, (/2, 3/)))

call sub1(dtp2, res)

print *, res(1,1)%i, ' ', res(1,2)%i, ' ', res(1,3)%i, ' ', res(1,4)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i, ' ', res(2,4)%i

print *, res(1,1)%ch, ' ', res(1,2)%ch, ' ', res(1,3)%ch, ' ', res(1,4)%ch
print *, res(2,1)%ch, ' ', res(2,2)%ch, ' ', res(2,3)%ch, ' ', res(2,4)%ch

end


