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
!* TEST CASE NAME : reshapedtp35.f
!*
!* PROGRAMMER : Andy Sheung
!* DATE : July 25, 2008
!* ORIGIN : AIX Compiler Development, Toronto Lab
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!*
!* CASE:
!*  35)  Applying RESHAPE on a DT component of an allocatable with deferred LEN DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
type dtp (l)
  integer, len :: l
  integer :: i
end type
end module

program a
use m

type (dtp(:)), allocatable :: dtp1(:)
type (dtp(:)), allocatable :: dtp2(:,:)

allocate(dtp1(6), SOURCE = (/(dtp(6)(i), i=1,6)/))
allocate(dtp(6)::dtp2(2,3))

dtp2 = reshape(dtp1, (/2, 3/))

j = 1
do while (j .LT. 3)
  print *, dtp2(j, 1), ' ', dtp2(j, 2), ' ', dtp2(j, 3)
  j = j+1
end do

end

