!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamecase002.f
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
!*  TEST CASE TITLE            : userenamecase002.f
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
!*  DESCRIPTION                : rename a public operator binary and mixed case usage
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

 use opmod ,  operator(.exponent.) => operator(.power.)
  real :: a=3.0,b=2.0,c
  
  c=b.eXpoNent.a
  print *,c
  
end program