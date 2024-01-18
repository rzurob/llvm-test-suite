!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: dtpast005.f
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

  integer(k*1) :: b(l**2)
end type

type(A(4,9)) :: A1


if(A1%k.ne.4) error stop 1
if(A1%l.ne.9) error stop 2
if(ubound(A1%b,1).ne.81) error stop 3
if(kind(A1%b).ne.4) error stop 4

end
