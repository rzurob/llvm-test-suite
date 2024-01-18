!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: dtpast009.f
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

module m
type A(k,l)
  integer, kind :: k
  integer, len  :: l

  integer(k+k-4) :: b(k+k+k+l+l)
end type

end module

module m2
use m
type(A(4,10)) :: A1
end module m2

use m2


if(A1%k.ne.4) error stop 1
if(A1%l.ne.10) error stop 2
if(ubound(A1%b,1).ne.32) error stop 3
if(kind(A1%b).ne.4) error stop 4

end