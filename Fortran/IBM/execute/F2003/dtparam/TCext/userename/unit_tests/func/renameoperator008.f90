! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/userename/unit_tests/func/renameoperator008.f
! opt variations: -ql

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: renameoperator008.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 28, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of renaming the operator in
!*                               a USE statement
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mod

  type real_num(k1)    ! (4)
    integer, kind :: k1
    real(k1)      :: x
  end type

  interface operator (.add.)
    module procedure addition
    module procedure plus
  end interface



  contains
    function addition(a,b)
      real :: addition
      real, intent(in) :: a,b
      addition = a+b
    end function addition

    function plus(a,b)
      integer :: plus
      integer, intent(in) :: a,b
      plus=a+b+5
    end function plus

end module


program main
use mod  , only: operator(.plus.) => operator(.add.),  proc1 => addition, proc2 => plus

  real :: a,b,c
  integer :: d,e,f
  a=1.0
  b=2.0
  c=a.plus.b
  d=1
  e=2
  f=a.plus.b

end program
