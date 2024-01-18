!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: renameoperator003.f
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
!*                               a USE statement. Generic binding is not allowed
!*                               for the operator being renamed
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module temp

  type real_num
    real :: x

    contains
      procedure :: real_add

      generic :: operator(.add.) => real_add

  end type


  contains
    function real_add(a,b)
      type(real_num) :: real_add
      class(real_num), intent(in) :: a,b
      real_add%x = a%x+b%x
    end function real_add


end module


program main
use temp , operator(.plus.) => operator(.add.)
  type(real_num) :: a,b,c
  c=a.plus.b
end program