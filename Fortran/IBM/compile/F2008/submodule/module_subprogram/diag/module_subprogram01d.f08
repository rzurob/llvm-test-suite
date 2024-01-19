!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 6 December, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : submodule
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*    Setup subroutine naming conflict between the host module and a USE
!*  associated module, expecting a severe message at compile.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE m
  INTERFACE
    module subroutine sub(x, y)
      integer, intent(in) :: x
      integer, intent(out) :: y
    end subroutine sub
  END INTERFACE
END MODULE m

MODULE m2
  INTERFACE
    module subroutine sub(x, y)
      integer, intent(in) :: x
      integer, intent(out) :: y
    end subroutine sub
  END INTERFACE
END MODULE m2

SUBMODULE (m) b
USE m2
CONTAINS
  module procedure sub
    y = x *2
  end
END SUBMODULE b

PROGRAM module_subprogram01d
USE m
integer :: a = 2, aa
call sub(a, aa)
print *, "a =", a, ", 2a=", aa
END PROGRAM module_subprogram01d
