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
!* TEST CASE NAME : reshapedtp49.f
!*
!* PROGRAMMER : Andy Sheung
!* DATE : Jul31, 2008
!* ORIGIN : AIX Compiler Development, Toronto Lab
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!* CASE:
!*  49) Applying EOSHIFT to a dummy argument of DT with allocatable component
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
module m
type dtp (k)
  integer, kind :: k
  integer(k), allocatable :: i 
end type

contains
subroutine sub1(dtp2, res, tar1, tar2)
  type (dtp(4)) dtp2(2,3), res(2,4), tar1, tar2
  res = reshape(dtp2, (/2,4/), (/tar1,tar2/))
end subroutine
end module

program a
use m

type (dtp(4)) :: dtp1(9)
type (dtp(4)) dtp2(2,3), res(2,4), tar1, tar2

do j = 1,9
  allocate(dtp1(j)%i , SOURCE=j)
end do

allocate(tar1%i , SOURCE=10)
allocate(tar2%i , SOURCE=11)

dtp2 = reshape(dtp1, (/2, 3/))

call sub1(dtp2, res, tar1, tar2)

print *, res(1,1)%i, ' ', res(1,2)%i, ' ', res(1,3)%i, ' ',res(1,4)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i, ' ',res(2,4)%i
end

