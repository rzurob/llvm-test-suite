!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : module_subprogram02d
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
!*    Set up conflicting function declaration between the host module
!*  and a USE associated module, expecting a severe message at compile
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890223456789022345678902234567890223456789022345678902234567890

MODULE m
  INTERFACE
    module integer function func(x)
      integer, intent(in) :: x
    end function func
  END INTERFACE
END MODULE m

MODULE m2
  INTERFACE
    module integer function func(x)
      integer, intent(in) :: x
    end function func
  END INTERFACE
END MODULE m2

SUBMODULE (m) b
USE m2
CONTAINS
  module procedure func
    func = x * 2
  end
END SUBMODULE b

PROGRAM module_subprogram02d
USE m
integer :: a = 2
print *, "a =", a, ", 2a=", func(a)
END PROGRAM module_subprogram02d
