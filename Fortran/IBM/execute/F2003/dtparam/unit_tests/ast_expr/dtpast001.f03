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

type A(k,l)
  integer, kind :: k
  integer, len  :: l

  integer(K+1) :: b(l+1)
end type

type(A(3,9)) :: A1

if(A1%k.ne.3) error stop 1
if(A1%l.ne.9) error stop 2
if(ubound(A1%b,1).ne.10) error stop 3
if(kind(A1%b).ne.4) error stop 4

end