!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamediag007.f
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
!*  DESCRIPTION                : ensure renaming to an ambiguous type bound yields error
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod

  type modreal
    real :: x

    contains
      procedure :: plus
      procedure :: plus2
      generic :: operator(.add.) => plus

  end type

  interface operator(.adda.)
    module procedure plus2
  end interface

  contains
    function plus(a,b)
      type(modreal) :: plus
      class(modreal), intent(in) :: a,b
      plus%x = a%x+b%x
    end function plus

    function plus2(a,b)
      type(modreal) :: plus2
      class(modreal), intent(in) :: a,b
      plus2%x = a%x+b%x
    end function plus2


end module


program main
use opmod , operator(.add.) => operator(.adda.)
  type(modreal) :: a,b,c
  c=a.add.b
end program