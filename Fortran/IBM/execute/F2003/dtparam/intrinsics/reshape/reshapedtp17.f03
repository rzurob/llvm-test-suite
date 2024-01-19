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
!*  17) Applying reshape to a DT's DT subtype's integer array component of size LEN DTP with KIND DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dt (k, l)
  integer, kind :: k
  integer, len :: l
  integer(k) :: i(l)
end type

type dtp (k1, l1)
  integer, kind :: k1
  integer, len :: l1
  type(dt(k1, l1)) ::  d1
end type

type (dtp(4, 6)) :: dtp1 = dtp(4, 6)(dt(4,6)((/1, 2, 3, 4, 5, 6/)))
integer(4) :: intarr(2,4)

intarr = reshape(dtp1%d1%i, (/2, 4/), (/9/), (/2, 1/))
j = 1
do while (j .LT. 3)
  print *, intarr(j, 1), ' ', intarr(j, 2), ' ', intarr(j, 3), ' ', intarr(j, 4)
  j = j+1
end do
end
