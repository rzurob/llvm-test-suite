! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/userename/unit_tests/diag/renameoperator006.f
! opt variations: -ql

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: renameoperator006.f
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
!*  DESCRIPTION                : diagnostic testing of renaming the operator in
!*                               a USE statement. If the operator has the private
!*                               attribute it cannot be renamed.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module mod

  type real_num(k1)    ! (4)
    integer, kind :: k1
    real(k1)      :: x
  end type

  interface operator (.add.)
    procedure plus
  end interface

  private :: operator(.add.)

  contains
    function plus(a,b)
      type(real_num(4)) :: plus
      type(real_num(4)), intent(in) :: a,b
      plus%x = a%x+b%x
    end function plus
end module



program main
use mod  , operator(.plus.) => operator(.addition.) !error, operator cannot have private attribute

  type(real_num(4)) :: a,b,c
  a%x=1.0
  b%x=2.0
  c=a.plus.b

end program
