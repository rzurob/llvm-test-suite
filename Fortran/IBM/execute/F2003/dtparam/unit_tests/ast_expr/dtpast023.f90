!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: dtpast023.f
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
!*  TEST CASE TITLE            : dtpexpression023
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

type A(ka,la)
  integer, kind :: ka
  integer, len  :: la
  
  real(ka+ka), allocatable :: r(:)
  integer :: i(la+la)
     
end type

type B(kb,lb)
  integer, kind :: kb
  integer, len  :: lb
  
  integer(kb+kb) :: j
  type(A(kb+kb,(lb*lb)/(kb+kb))) :: A1(kb:lb)
  character(len=lb+lb-kb) :: char(kb*kb:lb*lb)
  real(kb+kb+kb+kb) :: k 
end type

type(B(2,10)) :: B1

if(B1%kb.ne.2) error stop 1
if(B1%lb.ne.10) error stop 2
if(B1%A1%ka.ne.4) error stop 3
if(B1%A1%la.ne.25) error stop 4
if(lbound(B1%A1,1).ne.2) error stop 5
if(ubound(B1%A1,1).ne.10) error stop 6
if(kind(B1%A1(2)%r).ne.8) error stop 7
if(ubound(B1%A1(2)%i,1).ne.50) error stop 8
if(kind(B1%j).ne.4) error stop 9
if(kind(B1%k).ne.8) error stop 10
if(lbound(B1%char,1).ne.4) error stop 11
if(ubound(B1%char,1).ne.100) error stop 12

end
  