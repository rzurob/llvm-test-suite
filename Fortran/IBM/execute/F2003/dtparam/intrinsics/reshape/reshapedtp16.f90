!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : reshapedtp16.f
!*
!* CREATED BY : Pooja Dayanand
!* MODIFIED BY : Andy Sheung
!* DATE : Jul31, 2008
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!* CASE:
!*  16) Applying reshape to a DT's DT subtype's allocatable component
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dt (k)
  integer, kind :: k
  integer(k), allocatable :: i
end type

type dtp (k1)
  integer, kind :: k1
  type(dt(k1)) ::  d1
end type

type (dtp(4)) :: dtp1(6)
type (dtp(4)) dtp2(2,3)

do j = 1,6
  allocate (dtp1(j)%d1%i, SOURCE = j)
end do

dtp2 = reshape(dtp1, (/2, 3/))
j = 1
do while (j .LT. 3)
  print *, dtp2(j, 1)%d1%i, ' ', dtp2(j, 2)%d1%i, ' ', dtp2(j, 3)%d1%i
  j = j+1
end do
end
