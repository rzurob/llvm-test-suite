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
!* TEST CASE NAME : reshapedtp11.f
!*
!* CREATED BY : Pooja Dayanand
!* MODIFIED BY : Andy Sheung
!* DATE : Jul31, 2008
!* ORIGIN : AIX Compiler Development, Toronto Lab
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!* CASE:
!*  11) Applying reshape to a DT with a DT nested type, both with DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dt (k)
  integer, kind :: k
  integer(k) :: i
end type 

type dtp (k1)
  integer, kind :: k1
  type(dt(k1)) ::  d1
end type

type (dtp(4)) :: dtp1(6) = (/dtp(4)(dt(4)(1)),dtp(4)(dt(4)(2)),  & 
                             dtp(4)(dt(4)(3)), dtp(4)(dt(4)(4)), & 
                             dtp(4)(dt(4)(5)), dtp(4)(dt(4)(6))/)
type (dtp(4)) dtp2(2,3)

dtp2 = reshape(dtp1, (/2, 3/))
j = 1
do while (j .LT. 3)
  print *, dtp2(j, 1)%d1%i, ' ', dtp2(j, 2)%d1%i, ' ', dtp2(j, 3)%d1%i
  j = j+1
end do
end
