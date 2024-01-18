! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/userename/unit_tests/diag/renameoperator003.f
! opt variations: -qnol

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
!*                               a USE statement. Generic binding is not allowed
!*                               for the operator being renamed
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module temp

  type real_num(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    real(k1)      :: x

    contains
      procedure :: real_add

      generic :: operator(.add.) => real_add

  end type


  contains
    function real_add(a,b)
      type(real_num(20,4)) :: real_add
      class(real_num(*,4)), intent(in) :: a,b
      real_add%x = a%x+b%x
    end function real_add


end module


program main
use temp , operator(.plus.) => operator(.add.)
  type(real_num(20,4)) :: a,b,c
  c=a.plus.b
end program
