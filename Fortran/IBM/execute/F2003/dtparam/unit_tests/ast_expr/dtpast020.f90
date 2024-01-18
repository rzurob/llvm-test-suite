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

  logical :: b(k-k:l+l,k/l:max(k,l))

end type

type(A(5,5)) :: A1

if(A1%k.ne.5) error stop 1
if(A1%l.ne.5) error stop 2
if(lbound(A1%b,1).ne.0) error stop 3
if(ubound(A1%b,1).ne.10) error stop 4
if(lbound(A1%b,2).ne.1) error stop 5
if(ubound(A1%b,2).ne.5) error stop 6


end
