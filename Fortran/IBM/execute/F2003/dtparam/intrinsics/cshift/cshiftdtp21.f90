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
!* TEST CASE NAME : cshiftdtp21.f
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
!* 21) Applying CSHIFT to the source argument in an allocate statement for a pointer where the source in the CSHIFT intrinsic is a pointer which is allocated with a pointer
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (l)
  integer, len :: l
  integer i
end type

type (dtp(4)) :: dtp1(9) = (/dtp(4)(1), dtp(4)(2), dtp(4)(3), dtp(4)(4), &
                             dtp(4)(5), dtp(4)(6), dtp(4)(7), dtp(4)(8), &
                             dtp(4)(9)/)
type (dtp(4)) dtp2(3,3)
type (dtp(:)), pointer :: res(:,:)

dtp2 = reshape(dtp1, (/3, 3/))
allocate(res(3,3), SOURCE = cshift(dtp2, (/1,-1,0/)))

print *, res(1,1)%i, ' ', res(1,2)%i, ' ', res(1,3)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i
print *, res(3,1)%i, ' ', res(3,2)%i, ' ', res(3,3)%i
end
