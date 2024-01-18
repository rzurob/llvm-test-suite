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
!* TEST CASE NAME : eoshiftdtp07.f
!*
!* CREATED BY: Pooja Dayanand
!* MODIFIED BY: Andy Sheung
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
!*  07) Applying EOSHIFT to DT with integer array component of size LEN DTP with KIND DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (l1, l2)
  integer, len :: l1, l2
  integer :: i(l1, l2)
end type

type (dtp(3,3)) dtp1, res

dtp1%i = reshape((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
res%i = eoshift(dtp1%i, (/1, -1, 0/), 10)

print *, res%i(1,1), ' ', res%i(1,2), ' ', res%i(1,3)
print *, res%i(2,1), ' ', res%i(2,2), ' ', res%i(2,3)
print *, res%i(3,1), ' ', res%i(3,2), ' ', res%i(3,3)
end

