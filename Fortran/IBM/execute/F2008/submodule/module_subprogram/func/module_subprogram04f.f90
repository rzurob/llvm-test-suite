!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : module_subprogram04f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 6 December, 2012
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : submodule
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : F2008 submodule
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   Ensure variable name reused in local scope of submodule does not
!*   overwrite the variable in host scope
!*   
!*   The variable is reused in a local scope by: 
!*   - redeclaration in a submodule
!*   - redeclaration in a subroutine
!*   - use association, with an alias in a submodule
!*   - use association, with an alias in a subroutine
!*
!*  Secondary tests:
!*  - chain of submodules (5 levels)
!*  - compile succeeds if an interface declares a subroutine, which is 
!*    never defined in the host nor the submodules
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE m
IMPLICIT NONE
integer, public :: modInt = 100

  INTERFACE 
    module subroutine sub1()
    end subroutine sub1

    module subroutine sub2()
    end subroutine sub2

    module subroutine sub3()
    end subroutine sub3
   
    module subroutine sub4()
    end subroutine sub4

    module subroutine sub5()
    end subroutine sub5

    module subroutine sub6()
    end subroutine sub6

    module subroutine sub7() ! declared, but not defined, should compile
    end subroutine sub7
  END INTERFACE
END MODULE m

MODULE m2
IMPLICIT NONE
integer :: a2 = 102
integer :: b2 = 103
END MODULE

SUBMODULE (m) subm
CONTAINS
    module procedure sub1
      print *, "in sub1: modInt = ", modInt
    end 
END SUBMODULE subm

SUBMODULE (m:subm) subm2
integer :: modInt = 101
CONTAINS
  module procedure sub2
      print *, "in sub2: modInt = ", modInt
  end procedure sub2
END SUBMODULE subm2

SUBMODULE (m:subm2) subm3
USE m2, modInt => a2
CONTAINS
  module procedure sub3
      print *, "in sub3: modInt = ", modInt
  end procedure
  
  module procedure sub4
      USE m2, modInt => b2
      print *, "in sub4: modInt = ", modInt
  end procedure sub4
END SUBMODULE

SUBMODULE (m:subm3) subm4
CONTAINS
  module procedure sub5
      integer :: modInt = 104
      print *, "in sub5: modInt = ", modInt
  end 
END SUBMODULE subm4

SUBMODULE (m:subm4) subm5
CONTAINS
  module procedure sub6
      call sub1                              ! expect modInt value from host scope
      call sub2                              ! expect modInt value from subm2 scope
      call sub3                              ! expect modInt value from subm3 scope
      call sub4                              ! expect modInt value from sub4 scope
      call sub5                              ! expect modInt value from sub5 scope
      print *, "in sub6: modInt = ", modInt  ! expect modInt value from subm3 scope
  end 
END SUBMODULE subm5

PROGRAM module_subprogram04f
USE m
IMPLICIT NONE
call sub6
END PROGRAM module_subprogram04f
