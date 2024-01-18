!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenameonlys006.f
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
!*  TEST CASE TITLE            : userenameonlys006.f
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
!*  DESCRIPTION                : rename a public operator binary with 2 use and 2 names
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod

   
  interface operator(.power.)
    module procedure pwrreal
  end interface
  interface operator(.div.)
    module procedure realdiv
  end interface
  
  contains
    function pwrreal(a,b)
      real :: pwrreal
      real, intent(in) :: a,b
      pwrreal=a**b
    end function pwrreal
  
    function realdiv(a,b)
      real :: realdiv
      real, intent(in) :: a,b
      realdiv=a/b
    end function realdiv

end module


program main
 use opmod , only: operator(.divide.) => operator(.div.)
 use opmod , only: operator(.exponent.) => operator(.power.)
 use opmod , only: operator(.divit.) => operator(.div.)
 use opmod , only: operator(.expit.) => operator(.power.)
  real :: a=3.0,b=2.0,c
  
  c=b.divide.a
  print *,c
  
  c=b.exponent.a
  print *,c
  
  c=b.divit.a
  print *,c
  
  c=b.expit.a
  print *,c
  
end program