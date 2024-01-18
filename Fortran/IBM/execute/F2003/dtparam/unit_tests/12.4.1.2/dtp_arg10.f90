!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  FUNCTIONAL TESTED          : Pass object with deferred DTP to assumed
!*                               shape array. The array element is derived
!*                               type object which has deferred DTP.
!*                               - check the component bounds
!*                               - check the component values
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

module amod
type base(k, d)
  integer, kind :: k
  integer, len  :: d
  integer :: element(d)
  integer :: avar
end type

type mytype
  type(base(4, :)), pointer :: pa
end type
end module

use amod
type(mytype) :: tvar(2)
type(base(4, 4)), target  :: tgt

tgt%avar = 20
tgt%element = tgt%avar * tgt%avar

! - set the value for tvar(1)
tvar(1)%pa => tgt

call sub2(tvar)

! - verify the results of tvar
call sub1(tvar(1)%pa, tvar(2)%pa)

contains
subroutine sub1(dummy1, dummy2)
type(base(4, :)), pointer :: dummy1
type(base(4, :)), pointer :: dummy2
if (dummy1%d /= 4) stop 1
if (dummy2%d /= 2) stop 2
if (ubound(dummy1%element, 1) /= 4) stop 4
if (ubound(dummy2%element, 1) /= 2) stop 5
if (any(dummy1%element .ne. 400)) stop 6
if (any(dummy2%element .ne. (/1, 2/))) stop 7
if (dummy1%avar .ne. dummy2%avar) stop 8
end subroutine

subroutine sub2(dd)
type(mytype) :: dd(:)

! - verify values in tvar(1)
if (dd(1)%pa%d /= 4) stop 9	! - ICE defect 321684
if (ubound(dd(1)%pa%element, 1) /= 4) stop 10
if (any(dd(1)%pa%element .ne. 400)) stop 11
if (dd(1)%pa%avar /= 20) stop 12

! - set the value for tvar(2) using structure constructor
allocate(base(4, 2)::dd(2)%pa)	! - Error defect 321653
dd(2)%pa = base(4, 2)((/1, 2/), dd(1)%pa%avar)
end subroutine

end
