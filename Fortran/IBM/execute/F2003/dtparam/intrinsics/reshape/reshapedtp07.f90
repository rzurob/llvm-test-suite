!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : reshapedtp07.f
!*
!* CREATED BY : Pooja Dayanand
!* MODIFIED BY : Andy Sheung
!* DATE : Jul31, 2008
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!* CASE:
!*  07) Applying reshape to DT with integer component with KIND DTP where the KIDN DTP is specified like 'k=4'
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (k,l)
  integer, kind :: k
  integer, len :: l
  integer(k) ::  i
  character(l) :: ch
end type

type (dtp(k=4, l=3)) :: dtp1(6) = (/dtp(k=4, l=3)(1,'abc'), dtp(k=4, l=3)(2,'def'), dtp(4,3)(3,'ghi'), dtp(k=4, l=3)(4,'jkl'), &
                             dtp(k=4, l=3)(5,'mno'), dtp(k=4, l=3)(6,'pqr')/)
type (dtp(k=4, l=3)) dtp2(2,4)

dtp2 = reshape(dtp1, (/2, 4/), (/dtp(k=4, l=3)(10, 'xxx')/), (/2, 1/))

j = 1
do while (j .LT. 3)
  print *, dtp2(j, 1)%i, ' ', dtp2(j, 2)%i, ' ', dtp2(j, 3)%i, ' ', dtp2(j, 4)%i
  j = j+1
end do

j=1
do while (j .LT. 3)
  print *, dtp2(j, 1)%ch, ' ', dtp2(j, 2)%ch, ' ', dtp2(j, 3)%ch, ' ', dtp2(j, 4)%ch
  j = j+1
end do

end

