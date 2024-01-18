! *********************************************************************
!* ===================================================================
!*
!* CREATED BY: Pooja Dayanand
!* MODIFIED BY: Andy Sheung
!* DATE : Jul31, 2008
!*
!* DESCRIPTION:
!* EOSHIFT performs an end-off shift on an array expression of rank one
!* or perform end-off shifts on all the complete rank-one sections along a given
!* dimension of an array expression of rank two or greater. Elements are shifted
!* off at one end of a section and copies of a boundary value are shifted in at
!* the other end. Different sections may have different boundary values and may
!* be shifted by different amounts and in different directions.
!*
!* CASE:
!*  27) Having a RESHAPE intrinsic as the source argument for EOSHIFT, where the source for RESHAPE is an array
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (k)
  integer, kind :: k
  integer(k) ::  i
end type

type (dtp(4)) :: dtp1(9) = (/dtp(4)(1), dtp(4)(2), dtp(4)(3), dtp(4)(4), &
                             dtp(4)(5), dtp(4)(6), dtp(4)(7), dtp(4)(8), &
                             dtp(4)(9)/)
type (dtp(4)) res(3,3)

res = eoshift(reshape(dtp1, (/3, 3/)), (/1,-1,0/), dtp(4)(10))

print *, res(1,1)%i, ' ', res(1,2)%i, ' ', res(1,3)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i
print *, res(3,1)%i, ' ', res(3,2)%i, ' ', res(3,3)%i
end

