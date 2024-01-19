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
type A(k1,k2)
  integer, kind :: k1
  integer, kind  :: k2=k1+k1

  integer((k1+k1)*(k1-1)) :: i=int(k2,(k2)*(k1-1))
  integer(k2) :: j(k1+k2:k1*k2)=(/k1*k2,k2+k1,k2-k1/)

end type
end module

use m
type(A(2)) :: A1

if(kind(A1%i).ne.4)  error stop 1
if(A1%i.ne.4_8) error stop 2
if(any(A1%j.ne.(/8,6,2/))) error stop 3
if(kind(A1%j).ne.4) error stop 4

end
