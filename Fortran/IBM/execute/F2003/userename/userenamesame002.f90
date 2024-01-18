!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamesame002.f
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
!*  DESCRIPTION                : rename a public operator binary same name
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod


  interface operator(.power.)
    module procedure pwrreal
  end interface

  contains
    function pwrreal(a,b)
      real :: pwrreal
      real, intent(in) :: a,b
      pwrreal=a**b
    end function pwrreal

end module


program main

 use opmod , operator(.power.) => operator(.power.)
  real :: a=3.0,b=2.0,c

  c=b.power.a
  print *,c

end program