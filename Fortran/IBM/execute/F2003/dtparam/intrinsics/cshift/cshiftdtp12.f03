! *********************************************************************
!* ===================================================================
!*
!* CREATED BY: Pooja Dayanand
!* MODIFIED BY: Andy Sheung
!* DATE : Jul31, 2008
!*
!* DESCRIPTION:
!* CSHIFT performs a circular shift on an array expression of rank one or perform circular
!* shifts on all the complete rank one sections along a given dimension of an array expression of
!* rank two or greater. Elements shifted out at one end of a section are shifted in at the other end.
!* Different sections may be shifted by different amounts and in different directions.
!*
!* CASE:
!* 12) Applying CSHIFT to a DT's DT subtype's integer array component of size LEN DTP with KIND DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dt (l1, l2)
  integer, len :: l1, l2
  integer :: i(l1, l2)
end type

type dtp (l3, l4)
  integer, len :: l3, l4
  type (dt(l3, l4)) :: d1
end type

type (dtp(3,3)) dtp1, res

dtp1%d1%i = reshape((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
res%d1%i = cshift(dtp1%d1%i, (/1, -1, 0/))

print *, res%d1%i(1,1), ' ', res%d1%i(1,2), ' ', res%d1%i(1,3)
print *, res%d1%i(2,1), ' ', res%d1%i(2,2), ' ', res%d1%i(2,3)
print *, res%d1%i(3,1), ' ', res%d1%i(3,2), ' ', res%d1%i(3,3)
end
