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
!* TEST CASE NAME : eoshiftdtp02.f
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
!*  02) Applying EOSHIFT to a DT with DTP specified in type definition
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
type dtp (k,l)
  integer, kind :: k = 4
  integer, len :: l = 3
  integer(k) ::  i
  character(l) :: ch
end type

type (dtp) :: dtp1(9) = (/dtp(4,3)(1,'abc'), dtp(4,3)(2,'def'), dtp(4,3)(3,'ghi'), dtp(4,3)(4,'jkl'), &
                             dtp(4,3)(5,'mno'), dtp(4,3)(6,'pqr'), dtp(4,3)(7,'stu'), dtp(4,3)(8,'vwx'), &
                             dtp(4,3)(9,'yz1')/)
type (dtp) dtp2(3,3), res(3,3)

dtp2 = reshape(dtp1, (/3, 3/))
res = eoshift(dtp2, (/1,-1,0/), dtp(11, 'xxx'))

print *, res(1,1)%i, '  ', res(1,2)%i, ' ', res(1,3)%i
print *, res(2,1)%i, '  ', res(2,2)%i, '  ', res(2,3)%i
print *, res(3,1)%i, ' ', res(3,2)%i, '  ', res(3,3)%i

print *, res(1,1)%ch, ' ', res(1,2)%ch, ' ', res(1,3)%ch
print *, res(2,1)%ch, ' ', res(2,2)%ch, ' ', res(2,3)%ch
print *, res(3,1)%ch, ' ', res(3,2)%ch, ' ', res(3,3)%ch

end

