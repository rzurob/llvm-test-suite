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
type A(ka,la)
  integer, kind :: ka
  integer, len  :: la

  integer(ka*2) :: j(la+la)

end type
end module

module n
use m
type B(kb,lb)
  integer, kind :: kb
  integer, len  :: lb

  type(A(kb+kb,(lb*lb)/(kb+kb))) :: A1(kb:lb)
  integer(kb*4)  :: i
end type
end module

use n

type(B(1,10)) :: B1

if(B1%kb.ne.1) error stop 1
if(B1%lb.ne.10) error stop 2
if(B1%A1%ka.ne.2) error stop 3
if(B1%A1%la.ne.50) error stop 4
if(lbound(B1%A1,1).ne.1) error stop 5
if(ubound(B1%A1,1).ne.10) error stop 6
if(kind(B1%A1(1)%j).ne.4) error stop 7
if(ubound(B1%A1(1)%j,1).ne.100) error stop 8
if(kind(B1%i).ne.4) error stop 9

end
