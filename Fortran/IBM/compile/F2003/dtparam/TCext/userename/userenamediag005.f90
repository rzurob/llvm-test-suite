! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/userename/userenamediag005.f
! opt variations: -qnol

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamediag005.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 30, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : ensure op can't be redefined to intrinsic
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod

  type modreal(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    real(k1)      :: x
  end type




  interface operator(.add.)
    module procedure plus
  end interface

  contains
    function plus(a,b)
      type(modreal(20,4)) :: plus
      class(modreal(*,4)), intent(in) :: a,b
      plus%x = a%x+b%x
    end function plus


end module


program main
use opmod , operator(+) => operator(.add.)
  type(modreal(20,4)) :: a,b,c
  c=a+b
end program
