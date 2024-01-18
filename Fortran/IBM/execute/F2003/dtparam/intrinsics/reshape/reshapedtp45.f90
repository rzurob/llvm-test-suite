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
!* TEST CASE TITLE : Test for DTP with RESHAPE
!*
!* TEST CASE NAME : reshapedtp45.f
!*
!* PROGRAMMER : Andy Sheung
!* DATE : July 25, 2008
!* ORIGIN : AIX Compiler Development, Toronto Lab
!*
!* DESCRIPTION:
!* RESHAPE constructs an array of a specified shape from the elements
!* of a given array.
!*
!* CASE:
!*  45) Applying RESHAPE in an initialization expression
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
type dtp (k)
  integer, kind :: k
  character(1)  :: c
end type
end module

program a
use m

type(dtp(1)), parameter :: d(2,3)=reshape((/ &
  dtp(1)("a"),dtp(1)("d"),     &
  dtp(1)("b"),dtp(1)("e"),     &
  dtp(1)("c"),dtp(1)("f")/),(/2,3/))

type(dtp(1)) :: res(3,2)=reshape(d, (/3,2/))

print *, res(1,1)%c, ' ', res(1,2)%c, ' '
print *, res(2,1)%c, ' ', res(2,2)%c, ' '
print *, res(3,1)%c, ' ', res(3,2)%c, ' '
end

