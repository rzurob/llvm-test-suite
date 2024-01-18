!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: dtpast006.f
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

  integer(1+2+3+4+5-5-4-3-2-1+k*1-k/1+k) :: b(l+1-5*1+5-1/1+0)
  integer(k+k+k+k+k-4*k) :: c
  integer(k+k+k+k+k-4*k) :: d

end type

type(A(4,9)) :: A1

if(A1%k.ne.4) error stop 1
if(A1%l.ne.9) error stop 2
if(ubound(A1%b,1).ne.9) error stop 3
if(kind(A1%b).ne.4) error stop 4
if(kind(A1%c).ne.4) error stop 5

end
