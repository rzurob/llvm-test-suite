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
!* TEST CASE NAME : eoshiftdtp42.f
!*
!* PROGRAMMER : Andy Sheung
!* DATE : July 25, 2008
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
!*  42) Allocating an allocatable with EOSHIFT intrinsic as the source and having an implied-do array constructor as the source of RESHAPE, then apply EOSHIFT on that
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
type dtp (k)
  integer, kind :: k
  integer(k) ::  i
end type
end module

program a
use m

type (dtp(4)) :: dtp1(9)
type (dtp(4)), allocatable :: dtp2(:,:)

allocate(dtp2(3,3), SOURCE = eoshift((reshape((/(dtp(4)(i), i=1,9)/), (/3, 3/))), (/1,-1,0/),dtp(4)(10), 2))

print *, dtp2(1,1)%i, ' ', dtp2(1,2)%i, ' ', dtp2(1,3)%i
print *, dtp2(2,1)%i, ' ', dtp2(2,2)%i, ' ', dtp2(2,3)%i
print *, dtp2(3,1)%i, ' ', dtp2(3,2)%i, ' ', dtp2(3,3)%i

end

