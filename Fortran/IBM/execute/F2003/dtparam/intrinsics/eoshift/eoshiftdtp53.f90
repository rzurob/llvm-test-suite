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
!* TEST CASE TITLE : Test for DTP with EOSHIFT
!*
!* TEST CASE NAME : eoshiftdtp53.f
!*
!* PROGRAMMER : Andy Sheung
!* DATE : Jul31, 2008
!* ORIGIN : AIX Compiler Development, Toronto Lab
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
!*  53) Applying EOSHIFT to a dummy argument of DT with pointer component
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
module m
type dtp (k)
  integer, kind :: k
  integer(k), pointer :: i => null()
end type

contains
subroutine sub1(dtp2, res, tar)
  type (dtp(4)) dtp2(3,3), res(3,3), tar
  res = eoshift(dtp2, (/1,-1,0/),tar)
end subroutine
end module

program a
use m

type (dtp(4)) :: dtp1(9)
type (dtp(4)) dtp2(3,3), res(3,3), tar

do j = 1,9
  allocate(dtp1(j)%i , SOURCE=j)
end do

allocate(tar%i , SOURCE=10)

dtp2 = reshape(dtp1, (/3, 3/))

call sub1(dtp2, res, tar)

print *, res(1,1)%i, ' ', res(1,2)%i, ' ', res(1,3)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i
print *, res(3,1)%i, ' ', res(3,2)%i, ' ', res(3,3)%i
end


