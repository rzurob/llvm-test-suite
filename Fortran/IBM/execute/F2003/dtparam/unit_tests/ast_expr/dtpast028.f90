!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: dtpast028.f
! %VERifY:
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

type A(k1,k2,k3)
  integer, kind :: k1
  integer, kind :: k2
  integer, kind :: k3

  complex(k3-1) ::  c1=(k3*k3,k1-k2)
  complex(k2*k2) :: c2=cmplx(k1+k2+k3,-k1-k2-k3)
  complex(k1+k2+2) :: c3(k1-1:k3**2) =(/(cmplx(k1,k2),i=k1-1,k3**2)/)
  complex(k2) :: c4(k1:k3)=(/(k1,k2),(k2,k3),(-k3,-k1),(k1+k3+k2,k1*k2*k3)/)
  complex(k2*2) :: c5(k3:k3+k3)=(/((k2/k1,k3-k2),i=k3,10)/)

	end type

type(A(2,4,5)) :: A1

if(kind(A1%c1).ne.4)  error stop 1
if(kind(A1%c2).ne.16)  error stop 2
if(kind(A1%c3).ne.8)  error stop 3
if(kind(A1%c4).ne.4)  error stop 4
if(kind(A1%c5).ne.8)  error stop 5
if(A1%c1.ne.(25,-2)) error stop 6
if(A1%c2.ne.(11,-11)) error stop 7
if(lbound(A1%c3,1).ne.1) error stop 8
if(ubound(A1%c3,1).ne.25) error stop 9
if(lbound(A1%c4,1).ne.2) error stop 10
if(ubound(A1%c4,1).ne.5) error stop 11
if(lbound(A1%c5,1).ne.5) error stop 12
if(ubound(A1%c5,1).ne.10) error stop 13
if(any(A1%c3.ne.(/(cmplx(2,4),i=1,25)/))) error stop 14
if(any(A1%c4.ne.(/(2,4),(4,5),(-5,-2),(11,40)/))) error stop 15
if(any(A1%c5.ne.(/((2,1),i=5,10)/))) error stop 16


end

