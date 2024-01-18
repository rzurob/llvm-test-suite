!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : submodule05f
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
!*   - redeclaration in a function
!*   - use association, with an alias in a submodule
!*   - use association, with an alias in a function
!*
!*   Secondary tests:
!*   - chain of submodules (5 levels)
!*   - compile succeeds if an interface declares a subroutine, which 
!*     is never defined in the host nor the submodules
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE m
IMPLICIT NONE
real, public :: modInt = 100.0

  INTERFACE 
    module function func1()
      real :: func1
    end function func1

    module function func2()
      real :: func2
    end function func2

    module function func3()
      real :: func3
    end function func3
   
    module function func4()
      real :: func4
    end function func4

    module function func5()
      real :: func5
    end function func5

    module function func6()
      real :: func6
    end function func6

    module function func7() ! declared, but not defined, should compile
      real :: func7
    end function func7
  END INTERFACE
END MODULE m

MODULE m2
IMPLICIT NONE
real :: a2 = 102.0
real :: b2 = 103.0
END MODULE

SUBMODULE (m) subm
CONTAINS
    module function func1()
      real :: func1
      func1 = modInt
    end function func1
END SUBMODULE subm

SUBMODULE (m:subm) subm2
real :: modInt = 101.0
CONTAINS
  module function func2()
      real :: func2
      func2 = modInt
  end function func2
END SUBMODULE subm2

SUBMODULE (m:subm2) subm3
USE m2, modInt => a2
CONTAINS
  module function func3() 
      real :: func3
      func3 = modInt
  end function func3
  
  module function func4()
      USE m2, modInt => b2
      real :: func4
      func4 = modInt
  end function func4
END SUBMODULE

SUBMODULE (m:subm3) subm4
CONTAINS
  module function func5() 
      real :: func5
      real :: modInt = 104.0
      func5 = modInt
  end function func5
END SUBMODULE subm4

SUBMODULE (m:subm4) subm5
CONTAINS
  module function func6() 
      real :: func6
      logical precision_r4
      if(.not.precision_r4(func1(),100.0)) error stop 66   ! expect modInt value from host scope
      if(.not.precision_r4(func2(),101.0)) error stop 67   ! expect modInt value from subm2 scope
      if(.not.precision_r4(func3(),102.0)) error stop 68   ! expect modInt value from subm3 scope
      if(.not.precision_r4(func4(),103.0)) error stop 69   ! expect modInt value from func4 scope
      if(.not.precision_r4(func5(),104.0)) error stop 70   ! expect modInt value from func5 scope
      func6 = modInt
  end function func6
END SUBMODULE subm5

PROGRAM submodule05f
USE m
IMPLICIT NONE
logical precision_r4
  if(.not.precision_r4(func6(),102.0)) error stop 77   ! expect modInt value from subm3 scope
END PROGRAM submodule05f
