!#######################################################################
! *********************************************************************
!* ===================================================================
!*
!* TEST CASE NAME : eoshiftdtp26.f
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
!*  26) Applying EOSHIFT to a DT with character component with KIND DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (k)
  integer, kind :: k
  character(k) :: c
end type

type (dtp(4)) :: dtp1(9) = (/dtp(4)('A'), dtp(4)('B'), dtp(4)('C'), &
                             dtp(4)('D'), dtp(4)('E'), dtp(4)('F'), &
                             dtp(4)('G'), dtp(4)('H'), dtp(4)('I')/)
type (dtp(4)) dtp2(3,3), res(3,3)

dtp2 = reshape(dtp1, (/3, 3/))
res = eoshift(dtp2, (/1,-1,0/), dtp(4)('X'))

print *, res(1,1)%c, ' ', res(1,2)%c, '  ', res(1,3)%c
print *, res(2,1)%c, ' ', res(2,2)%c, ' ', res(2,3)%c
print *, res(3,1)%c, '  ', res(3,2)%c, ' ', res(3,3)%c
end

