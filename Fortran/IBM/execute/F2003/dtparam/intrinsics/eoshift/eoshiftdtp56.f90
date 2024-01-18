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
!* TEST CASE NAME : eoshiftdtp56.f
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
!*  56) Applying EOSHIFT to a dummy argument of DT of an external procedure
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
module m
type dtp (k,l)
  integer, kind :: k
  integer, len :: l
  integer(k) ::  i
  character(l) :: ch
end type
end module

program a
use m

interface
subroutine sub1(dtp2, res)
  import :: dtp
  type (dtp(4,3)) dtp2(3,3), res(3,3)
end subroutine
end interface

type (dtp(4,3)) :: dtp1(9) = (/dtp(4,3)(1,'abc'), dtp(4,3)(2,'def'), dtp(4,3)(3,'ghi'), dtp(4,3)(4,'jkl'), &
                             dtp(4,3)(5,'mno'), dtp(4,3)(6,'pqr'), dtp(4,3)(7,'stu'), dtp(4,3)(8,'vwx'), &
                             dtp(4,3)(9,'yz1')/)
type (dtp(4,3)) dtp2(3,3), res(3,3)

dtp2 = reshape(dtp1, (/3, 3/))
call sub1(dtp2, res)

print *, res(1,1)%i, ' ', res(1,2)%i, ' ', res(1,3)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i
print *, res(3,1)%i, ' ', res(3,2)%i, ' ', res(3,3)%i

print *, res(1,1)%ch, ' ', res(1,2)%ch, ' ', res(1,3)%ch
print *, res(2,1)%ch, ' ', res(2,2)%ch, ' ', res(2,3)%ch
print *, res(3,1)%ch, ' ', res(3,2)%ch, ' ', res(3,3)%ch

end

subroutine sub1(dtp2, res)
  use m
  type (dtp(4,3)) dtp2(3,3), res(3,3)
  res = eoshift(dtp2, (/1,-1,0/), dtp(4,3)(0,'xxx'))
end subroutine

