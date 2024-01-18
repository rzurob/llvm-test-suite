!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: dtpast012.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 2nd, 2006
!*  ORIGIN                     : AIX Compiler Development,
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

  integer((1+1+1+1)*k) :: b((k+1)*l:(l*2-k)*(l*2-k),l/k:(l-1)*l,k-3:(k+l)*l)
end type

type(A(1,5)) :: A1

if(A1%k.ne.1) error stop 1
if(A1%l.ne.5) error stop 2
if(lbound(A1%b,1).ne.10) error stop 3
if(ubound(A1%b,1).ne.81) error stop 4
if(lbound(A1%b,2).ne.5) error stop 5
if(ubound(A1%b,2).ne.20) error stop 6
if(lbound(A1%b,3).ne.-2) error stop 7
if(ubound(A1%b,3).ne.30) error stop 8
if(kind(A1%b).ne.4) error stop 9

end
