!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: dtpast018.f
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
  integer, kind :: k=4
  integer, len  :: l=3

  real(2*k-k) :: b(l:2*l,l+k)
end type

type(A(8,5)) :: A1

if(A1%k.ne.8) error stop 1
if(A1%l.ne.5) error stop 2
if(lbound(A1%b,1).ne.5) error stop 3
if(ubound(A1%b,1).ne.10) error stop 4
if(lbound(A1%b,2).ne.1) error stop 5
if(ubound(A1%b,2).ne.13) error stop 6
if(kind(A1%b).ne.8) error stop 7

end
