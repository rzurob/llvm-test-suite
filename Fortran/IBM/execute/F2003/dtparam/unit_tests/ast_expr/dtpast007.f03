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
  integer, kind :: k=4
  integer, len  :: l=8

  integer(k+1-1) :: b(k+k+k+l+l)
end type

type(A) :: A1


if(A1%k.ne.4) error stop 1
if(A1%l.ne.8) error stop 2
if(ubound(A1%b,1).ne.28) error stop 3
if(kind(A1%b).ne.4) error stop 4

end