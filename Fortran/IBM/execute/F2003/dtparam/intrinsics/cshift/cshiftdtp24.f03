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
!* 24) Applying CSHIFT to the source argument in an allocate statement with LEN DTP in allocatable DT
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (l)
  integer, len :: l
  integer i
end type

type (dtp(:)), allocatable :: dtp1(:)
type (dtp(:)), allocatable :: dtp2(:,:)
type (dtp(:)), allocatable :: res(:,:)

allocate(dtp1(9), SOURCE = (/dtp(4)(1), dtp(4)(2), dtp(4)(3), dtp(4)(4), &
                             dtp(4)(5), dtp(4)(6), dtp(4)(7), dtp(4)(8), &
                             dtp(4)(9)/))
allocate(dtp2(3,3), SOURCE = reshape(dtp1, (/3, 3/)))
allocate(res(3,3), SOURCE = cshift(dtp2, (/1,-1,0/)))

print *, res(1,1)%i, ' ', res(1,2)%i, ' ', res(1,3)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i
print *, res(3,1)%i, ' ', res(3,2)%i, ' ', res(3,3)%i
end