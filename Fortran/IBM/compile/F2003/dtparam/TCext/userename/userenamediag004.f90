! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/userename/userenamediag004.f
! opt variations: -ql

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: userenamediag001.f
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
!*  TEST CASE TITLE            : userenamediag001.f
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
!*  DESCRIPTION                : ensure langlvl message is issued
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module opmod

  type modreal(k1)    ! (4)
    integer, kind :: k1
    real(k1)      :: x
  end type
  
  
  private :: operator(+)
  
  interface operator(+)
    module procedure plus
  end interface

  contains
    function plus(a,b)
      type(modreal(4)) :: plus
      class(modreal(4)), intent(in) :: a,b
      plus%x = a%x+b%x
    end function plus


end module


program main
use opmod , operator(.plus.) => operator(+)
  type(modreal(4)) :: a,b,c
  c=a+b
end program
