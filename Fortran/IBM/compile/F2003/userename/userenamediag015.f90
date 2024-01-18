!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamesame005.f
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
!*  DESCRIPTION                : ensure rename to a local that points to same function
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod

  interface operator(.add.)
    function plus2(a,b)
    real :: plus2
    real, intent(in) :: a,b
    end function
  end interface

  contains
    function plus(a,b)
      real :: plus
      real, intent(in) :: a,b
      plus = a+b
    end function plus

end module


program main
  use opmod ,  operator(.plus.) => operator(.add.)
  interface operator(.plus.)
    function plus2(a,b)
    real :: plus2
    real, intent(in) :: a,b
    end function
  end interface

  real :: a,b,c
  c=a.plus.b
  b=a.Plus.c
  a=b.pLuS.c
  print *,c
  print *,b
  print *,a

end program

function plus2(a,b)
      real :: plus2
      real, intent(in) :: a,b
      plus2 = a+b
end function plus2