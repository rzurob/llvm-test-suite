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
!*  25) Same as 23 but with deferred LEN DTP for the source of reshape
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (l)
  integer, len :: l
  integer ::  i
end type

type (dtp(4)), pointer :: dtp1(:)
type (dtp(:)), pointer :: dtp2(:,:)

allocate(dtp1(6), SOURCE = (/dtp(4)(1),dtp(4)(2), dtp(4)(3), dtp(4)(4), &
                             dtp(4)(5), dtp(4)(6)/))
allocate(dtp2(2,3), SOURCE = reshape(dtp1, (/2, 3/)))
j = 1
do while (j .LT. 3)
  print *, dtp2(j, 1)%i, ' ', dtp2(j, 2)%i, ' ', dtp2(j, 3)%i
  j = j+1
end do
end
