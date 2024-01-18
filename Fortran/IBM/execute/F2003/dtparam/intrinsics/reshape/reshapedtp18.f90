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
!*  18) Applying reshape to DT sequence
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (k)
  integer, kind :: k
  sequence
  integer(k) ::  i
  real(k) :: r
end type

type (dtp(4)) :: dtp1(6) = (/dtp(4)(1, 1.0),dtp(4)(2, 2.0), dtp(4)(3, 3.0), &
                             dtp(4)(4, 4.0), dtp(4)(5, 5.0), dtp(4)(6, 6.0)/)
type (dtp(4)) dtp2(2,3)

dtp2 = reshape(dtp1, (/2, 3/))
j = 1
do while (j .LT. 3)
  print *, dtp2(j, 1)%i, ' ', dtp2(j, 1)%r, '   ', dtp2(j, 2)%i, ' ', &
           dtp2(j, 2)%r, '   ', dtp2(j, 3)%i, ' ', dtp2(j, 3)%r
  j = j+1
end do
end
