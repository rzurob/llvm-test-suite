! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/userename/unit_tests/diag/renameoperator004.f
! opt variations: -ql

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 28, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : diagnostic testing of renaming the operator in
!*                               a USE statement. When an operator has an interface
!*                               and a generic binding. Its generic binding cannot be
!*                               used after its been renamed
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module temp

  type real_num(k1)    ! (4)
    integer, kind :: k1
    real(k1)      :: x

    contains
      procedure :: real_add

      generic :: operator(.add.) => real_add

  end type

  interface operator (.add.)
    procedure int_add
  end interface

  contains
    function real_add(a,b)
      type(real_num(4)) :: real_add
      class(real_num(4)), intent(in) :: a,b
      real_add%x = a%x+b%x
    end function real_add

    function int_add(c,d)
      integer :: int_add
      integer, intent(in) ::c,d
      int_add=c+d
   end function int_add

end module


program main
use temp , operator(.plus.) => operator(.add.)
  type(real_num(4)) :: a,b,c
  c=a.plus.b
end program
