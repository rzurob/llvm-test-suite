!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case IBM INTERNAL USE ONLY
!* ===================================================================
!* ===================================================================
!*
!* TEST CASE TITLE : Test for DTP with CSHIFT
!*
!* TEST CASE NAME : cshiftdtp10.f
!*
!* CREATED BY: Pooja Dayanand
!* MODIFIED BY: Andy Sheung
!* DATE : Jul31, 2008
!* ORIGIN : AIX Compiler Development, Toronto Lab
!*
!* DESCRIPTION:
!* CSHIFT performs a circular shift on an array expression of rank one or perform circular
!* shifts on all the complete rank one sections along a given dimension of an array expression of
!* rank two or greater. Elements shifted out at one end of a section are shifted in at the other end.
!* Different sections may be shifted by different amounts and in different directions.
!*
!* CASE:
!* 10) Applying CSHIFT to a DT's DT subtype's pointer component
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dt (k)
  integer, kind :: k
  integer(k), pointer :: i => null()
end type

type dtp (k1)
  integer, kind :: k1
  type (dt(k1)) :: d1
end type

type (dtp(4)) :: dtp1(9) 
type (dtp(4)) dtp2(3,3), res(3,3)

do j = 1,9
  allocate(dtp1(j)%d1%i , SOURCE=j)
end do

dtp2 = reshape(dtp1, (/3, 3/))
res = cshift(dtp2, (/1,-1,0/))

print *, res(1,1)%d1%i, ' ', res(1,2)%d1%i, ' ', res(1,3)%d1%i
print *, res(2,1)%d1%i, ' ', res(2,2)%d1%i, ' ', res(2,3)%d1%i
print *, res(3,1)%d1%i, ' ', res(3,2)%d1%i, ' ', res(3,3)%d1%i
end
