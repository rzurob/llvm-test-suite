!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : reshapedtp26.f
!*
!* CREATED BY : Pooja Dayanand
!* MODIFIED BY : Andy Sheung
!* DATE : Jul31, 2008
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!* CASE:
!*  26) Applying reshape to DT with integer component with KIND DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (k)
  integer, kind :: k
  integer(k) ::  i
end type

type (dtp(4)) dtp1(2,3)

dtp1 = reshape((/dtp(4)(1),dtp(4)(2), dtp(4)(3), dtp(4)(4), dtp(4)(5), &
                 dtp(4)(6)/), (/2, 3/))
j = 1
do while (j .LT. 3)
  print *, dtp1(j, 1)%i, ' ', dtp1(j, 2)%i, ' ', dtp1(j, 3)%i
  j = j+1
end do
end
