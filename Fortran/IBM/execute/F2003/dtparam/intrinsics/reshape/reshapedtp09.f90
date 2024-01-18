! *********************************************************************
!* ===================================================================
!*
!* CREATED BY : Pooja Dayanand
!* MODIFIED BY : Andy Sheung
!* DATE : Jul31, 2008
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!* CASE:
!*  09) Applying reshape with PAD to DT with integer array component of size LEN DTP with KIND DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (k1, n)
  integer, kind :: k1
  integer, len :: n
  integer(k1) ::  i(n)
end type

type dt
  integer :: j (2,4)
end type

type (dtp(4,6)) :: dtp1 = dtp(4,6)((/1, 2, 3, 4, 5, 6/))
type (dt) dt1

dt1%j = reshape(dtp1%i, (/2, 4/), (/8/))

k = 1
do while (k .LT. 3)
  print *, dt1%j(k, 1), ' ', dt1%j(k, 2), ' ', dt1%j(k, 3), ' ', dt1%j(k, 4)
  k = k+1
end do
end
