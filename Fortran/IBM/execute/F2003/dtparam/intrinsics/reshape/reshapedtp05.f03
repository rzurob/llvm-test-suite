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
!*  05) Applying reshape with PAD and ORDER to DT with integer component with KIND DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (k,l)
  integer, kind :: k
  integer, len :: l
  integer(k) ::  i
  character(l) :: ch
end type

type (dtp(4,3)) :: dtp1(6) = (/dtp(4,3)(1,'abc'), dtp(4,3)(2,'def'), dtp(4,3)(3,'ghi'), dtp(4,3)(4,'jkl'), &
                             dtp(4,3)(5,'mno'), dtp(4,3)(6,'pqr')/)
type (dtp(4,3)) dtp2(2,4)

dtp2 = reshape(dtp1, (/2, 4/), (/dtp(4,3)(10, 'xxx')/), (/2, 1/))
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
