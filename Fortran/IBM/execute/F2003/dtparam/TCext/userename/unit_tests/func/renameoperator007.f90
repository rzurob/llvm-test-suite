! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/userename/unit_tests/func/renameoperator007.f
! opt variations: -qnol

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: renameoperator007.f
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
!*  TEST CASE TITLE            : renameoperator007
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : Oct. 28, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : functional testing of renaming the operator in 
!*                               a USE statement
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mod

  type real_num(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    real(k1)      :: x
  end type

  interface operator (.add.)
    procedure addition
  end interface
  
  interface operator (.plus.)
    procedure plus
  end interface

  contains
    function addition(a,b)
      type(real_num(20,4)) :: addition
      type(real_num(*,4)), intent(in) :: a,b
      addition%x = a%x+b%x
    end function addition
    
    function plus(a,b)
      integer :: plus
      integer, intent(in) :: a,b
      plus=a+b+5
    end function plus

end module


program main
use mod  , operator(.plus.) => operator(.add.), operator(.add.) => operator(.plus.)

  type(real_num(20,4)) :: a,b,c
  integer :: d,e,f
  a%x=1.0
  b%x=2.0
  c=a.plus.b
  d=1
  e=2
  f=d.add.e
  
end program
