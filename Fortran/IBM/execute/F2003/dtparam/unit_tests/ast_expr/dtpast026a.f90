!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: dtpast026a.f
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
type A(k1,k2)
  integer, kind :: k1
  integer, kind  :: k2=k1+k1

  integer((k1+k1)*(k1-1)) :: i=int(k2,(k2)*(k1-1))
  integer(k2) :: j(k1+k2:k1*k2)=(/k1*k2,k2+k1,k2-k1/)
  integer(k2+k1+k1) :: k(k1+k2+(k1*k2)) = (/(k2*k1+l,l=1,k1+k2+(k1*k2))/)

end type
end module

use m

type(A(2)) :: A1

if(kind(A1%i).ne.4)  error stop 1
if(A1%i.ne.4_8) error stop 2
if(any(A1%j.ne.(/8,6,2/))) error stop 3
if(kind(A1%j).ne.4) error stop 4
if(lbound(A1%j,1).ne.6) error stop 5
if(ubound(A1%j,1).ne.8) error stop 6
if(kind(A1%k).ne.8) error stop 7
if(ubound(A1%k,1).ne.14) error stop 8
if(any(A1%k.ne.(/(8+i,i=1,14)/))) error stop 9


end
