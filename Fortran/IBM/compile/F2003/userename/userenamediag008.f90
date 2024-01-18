!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamediag008.f
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
!*  DESCRIPTION                : ensure switchign target and destination yields error
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod

  type modreal
    real :: x
  end type




  interface operator(.add.)
    module procedure plus
  end interface

  contains
    function plus(a,b)
      type(modreal) :: plus
      class(modreal), intent(in) :: a,b
      plus%x = a%x+b%x
    end function plus


end module


program main
use opmod ,  operator(.add.) => operator(.plus.)
  type(modreal) :: a,b,c
  c=a.plus.b
end program