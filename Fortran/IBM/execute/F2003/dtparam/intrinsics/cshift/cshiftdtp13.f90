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
!* TEST CASE NAME : cshiftdtp13.f
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
!* 13) Applying CSHIFT to DT with real component with KIND DTP
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (k)
  integer, kind :: k
  integer(k) ::  i
  real(k) :: r
end type

type (dtp(4)) :: dtp1(9) = (/dtp(4)(1, 1.0), dtp(4)(2, 2.0), dtp(4)(3, 3.0), & 
                             dtp(4)(4, 4.0), dtp(4)(5, 5.0), dtp(4)(6, 6.0), &
                             dtp(4)(7, 7.0), dtp(4)(8, 8.0), dtp(4)(9, 9.0)/)
type (dtp(4)) dtp2(3,3), res(3,3)

dtp2 = reshape(dtp1, (/3, 3/))
res = cshift(dtp2, (/1,-1,0/))

print *, res(1,1)%i, ' ', res(1,1)%r, '  ', res(1,2)%i, ' ', res(1,2)%r, '  ', &
         res(1,3)%i, ' ', res(1,3)%r
print *, res(2,1)%i, ' ', res(2,1)%r, '  ', res(2,2)%i, ' ', res(2,2)%r, '  ', &
         res(2,3)%i, ' ', res(2,3)%r
print *, res(3,1)%i, ' ', res(3,1)%r, '  ', res(3,2)%i, ' ', res(3,2)%r, '  ', &
         res(3,3)%i, ' ', res(3,3)%r
end
