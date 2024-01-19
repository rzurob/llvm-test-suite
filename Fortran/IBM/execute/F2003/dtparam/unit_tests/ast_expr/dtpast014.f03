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
!234567890143456789014345678901434567890143456789014345678901434567890

type A(ka,la)
  integer, kind :: ka=1
  integer, len  :: la=2
end type

type , extends(A) :: B(kb,lb)
  integer, kind :: kb
  integer, len  :: lb

  real(ka+kb+ka+kb) :: c((lb-ka)*(lb/la):(ka+kb+la+lb)*(ka+kb+la+lb))
end type

type(B(1,2,3,4)) :: B1

if(B1%ka.ne.1) error stop 1
if(B1%la.ne.2) error stop 2
if(B1%kb.ne.3) error stop 3
if(B1%lb.ne.4) error stop 4

if(lbound(B1%c,1).ne.6) error stop 5
if(ubound(B1%c,1).ne.100) error stop 6
if(kind(B1%c).ne.8) error stop 7

end
