!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamediag011.f
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
!*  DESCRIPTION                : ensure renaming an operator to one within a different modules to same name gives error
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod



  interface operator(.addreal.)
    module procedure plusreal
  end interface

  contains
    function plusreal(a,b)
      real :: plusreal
      real, intent(in) :: a,b
      plusreal=a+b
    end function plusreal

end module

module opmod2


  interface operator(.add.)
    module procedure plus
  end interface

  contains
    function plus(a,b)
      real :: plus
      real, intent(in) :: a,b
      plus = a+b
    end function plus

end module


program main



 use opmod
 use opmod2 ,  operator(.thisisprobablywaytoomanyletterstouseasanameforanoperatorundertherulesgoverningfortranstandard.) => operator(.add.)




  real :: a=1.0,b=2.0,c
  real :: d=1.0,e=2.0,f
  c=a.thisisprobablywaytoomanyletterstouseasanameforanoperatorundertherulesgoverningfortranstandard.b



end program