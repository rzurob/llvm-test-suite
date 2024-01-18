!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamediag013.f
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
!*  DESCRIPTION                : ensure private generic can't be renamed
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod

  type modreal
    real :: x

    contains
      procedure :: plus

      generic, private :: operator(.add.) => plus

  end type



  contains
    function plus(a,b)
      type(modreal) :: plus
      class(modreal), intent(in) :: a,b
      plus%x = a%x+b%x
    end function plus


end module


program main
use opmod , operator(.plus.) => operator(.add.)
  type(modreal) :: a,b,c
  c=a.plus.b
end program