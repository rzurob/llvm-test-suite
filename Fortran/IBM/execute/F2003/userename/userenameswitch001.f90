!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenameswitch001.f
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
!*  TEST CASE TITLE            : userenameswitch001.f
!*
!*  PROGRAMMER                 : Rob Wheeler
!*  DATE                       : Mar. 30, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2009b
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : ensure that ops from different modules are parsed correctly

!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod
  type modreal2
    real :: x=8.0

  end type
  type modreal
    real :: x=4.0

    contains
      procedure :: plus

      generic :: operator(.add.) => plus

  end type
  
  contains
    function plus(a,b)
      type(modreal) :: plus
      class(modreal), intent(in) :: a
      type(modreal2), intent(in) ::b
      plus%x = b%x+a%x
      print *, "modereal2 class second"
    end function plus

end module

module opmod2
 use opmod
 
  interface operator(.plus.)
    module procedure plus2
  end interface
  contains
   function plus2(a,b)
      type(modreal2) :: plus2
      class(modreal2), intent(in) :: a
      class(modreal), intent(in) :: b
      plus2%x = a%x+b%x
      print *, "modreal2 first"
    end function plus2
end module

program main
use opmod 
use opmod2, operator(.add.) => operator(.plus.)
  type(modreal) :: a,z
  type(modreal2) :: b,y
  
  z=a.add.b
  print *, z%x
  y=b.add.a
  print *, y%x
  z=a.add.b.add.y
  print *, z%x
  
  
  
  
  
end program