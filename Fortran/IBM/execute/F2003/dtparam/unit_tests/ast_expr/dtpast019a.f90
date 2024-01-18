!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: dtpast019a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : dtpexpression019a
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : June 2nd, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :testing expressions with derived types
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
type A(k,l)
  integer, kind :: k
  integer, len  :: l
  
  complex(2*k-k) :: b(l-9:(2*l)/k,k+k:l**2)  
end type
end module

use m
type(A(4,10)) :: A1

if(A1%k.ne.4) error stop 1
if(A1%l.ne.10) error stop 2
if(lbound(A1%b,1).ne.1) error stop 3
if(ubound(A1%b,1).ne.5) error stop 4
if(lbound(A1%b,2).ne.8) error stop 5
if(ubound(A1%b,2).ne.100) error stop 6
if(kind(A1%b).ne.4) error stop 7

end
