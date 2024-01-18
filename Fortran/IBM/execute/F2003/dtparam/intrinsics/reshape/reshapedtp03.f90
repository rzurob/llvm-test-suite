!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : reshapedtp03.f
!*
!* CREATED BY : Pooja Dayanand
!* MODIFIED BY : Andy Sheung
!* DATE : Jul31, 2008
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!* CASE:
!*  03) Applying reshape with DT with integer component with KIND and giving it a value of a KIND DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (k, n)
  integer, kind :: k, n
  integer(k) ::  i = n
end type

type (dtp(4, 2)) :: dtp1(6)
type (dtp(4, 2)) dtp2(2,3)

dtp2 = reshape(dtp1, (/2, 3/))
j = 1
do while (j .LT. 3)
  print *, dtp2(j, 1)%i, ' ', dtp2(j, 2)%i, ' ', dtp2(j, 3)%i
  j = j+1
end do
end
