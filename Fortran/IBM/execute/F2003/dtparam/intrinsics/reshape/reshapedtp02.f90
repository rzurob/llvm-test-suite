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
!*  02) Applying reshape to DT with character component with KIND DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (k)
  integer, kind :: k
  character(k) ::  c
end type

type (dtp(1)) :: dtp1(6) = (/dtp(1)('A'),dtp(1)('B'), dtp(1)('C'), &
                             dtp(1)('D'), dtp(1)('E'), dtp(1)('F')/)
type (dtp(1)) dtp2(2,3)

dtp2 = reshape(dtp1, (/2, 3/))
j = 1
do while (j .LT. 3)
  print *, dtp2(j, 1)%c, ' ', dtp2(j, 2)%c, ' ', dtp2(j, 3)%c
  j = j+1
end do
end
