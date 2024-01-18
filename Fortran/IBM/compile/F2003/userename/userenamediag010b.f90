!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamediag010.f
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
!*  TEST CASE TITLE            : userenamediag010.f
!*
!*  PROGRAMMER                 : Rob Wheeler
!*  DATE                       : Mar. 30, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : ensure renaming ambiguous operators from different modules to same name gives error
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

   

 use opmod ,  operator(.plus.) => operator(.addreal.)
 use opmod2 ,  operator(.PlUs.) => operator(.add.)

 
 

  real :: a=1.0,b=2.0,c
  real :: d=1.0,e=2.0,f
  c=a.plus.b
  f=d.plus.e
  
  
end program