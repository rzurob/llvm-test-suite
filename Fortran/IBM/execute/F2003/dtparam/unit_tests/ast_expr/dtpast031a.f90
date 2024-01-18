!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 2nd, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :testing expressions with derived types
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
type A
  integer, allocatable :: i(:)
end type

type C(kc,lc)
  integer, kind :: kc
  integer, len  :: lc

  real(kc+2+4), pointer :: r(:)

end type
end module

module n
use m
type B(k,l)
  integer, kind :: k
  integer, len  :: l

  integer(k+1), allocatable :: i(:)
  complex(k*2+2), allocatable :: c(:,:)
  real(k+1), pointer :: r(:)
  character(len=l+k*l), pointer :: char(:)
  type(A), allocatable  :: dt(:)
  type(C(k-1,l/2)), pointer :: dtc(:)


end type
end module

use n
type(B(3,10)) :: B1

if(kind(B1%i).ne.4) error stop 1
if(kind(B1%c).ne.8) error stop 2
if(kind(B1%r).ne.4) error stop 3
if(len(B1%char).ne.40) error stop 4
if(B1%dtc%kc.ne.2) error stop 5
if(B1%dtc%lc.ne.5) error stop 6
if(allocated(B1%i).or.allocated(B1%c).or.&
   & allocated(B1%dt)) error stop 7
if(associated(B1%r).or.associated(B1%char).or.&
  & associated(B1%dtc)) error stop 8

end