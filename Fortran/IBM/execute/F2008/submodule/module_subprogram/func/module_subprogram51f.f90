!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : module_subprogram51f
!*
!*  DATE                       : 6 December, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : submodule
!*  SECONDARY FUNCTIONS TESTED :
!*
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

module t
  type base
    integer :: i = 4
  end type
end module

module t2
  type base2
    real :: i = 5.0
  end type
end module

MODULE m
USE t
IMPLICIT NONE
integer, public :: modInt = 100

  INTERFACE
    module function func1(arg1, arg2)
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2(arg1:arg1+modInt)
    end function func1

    module function func2(arg1, arg2, arg3, arg4)
      type(base) :: func2
      integer, intent(in) :: arg1, arg3
      integer, intent(in) :: arg2(arg1:arg1+modInt)
      character(arg3), allocatable :: arg4(:)
    end function func2

    module function func3(arg1, arg2, arg3)
      use t2
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2(arg1:arg1+modInt)
      type(base) :: arg3
      type(base2) :: func3
    end function func3

    module function func4(arg1, arg2, arg3, arg4)
      use t2
      integer, intent(in) :: arg1, arg3
      integer, intent(in) :: arg2(arg1:arg1+modInt)
      character(arg3), allocatable :: arg4(:)
      type(base2) :: func4
    end function func4

    module function func5(arg1, arg2, arg3)
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2(arg1-1:arg1+modInt-1)
      type(base) :: arg3
      type(base) func5
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
integer :: a2 = 102
integer :: b2 = 103
END MODULE

SUBMODULE (m) subm
CONTAINS
    ! using arg1=7, arg2 = allocated array of 101 elements
    module procedure func1
      print *, "func1 lbound = ", lbound(arg2)
      print *, "func1 ubound = ", ubound(arg2)
      func1 = modInt * 1.0
    end procedure func1
END SUBMODULE subm

SUBMODULE (m:subm) subm2
! even though modInt is redefined here locally, the modInt from the host should still be used in the index
integer :: modInt = 101
CONTAINS
  module procedure func2
      arg4 = (/ 'hello', 'world' /)
      func2 = base()
      func2%i = modInt

      print *, "func2 lbound = ", lbound(arg2)
      print *, "func2 ubound = ", ubound(arg2)
      print *, "arg4: ", arg4(1:2)

  end
END SUBMODULE subm2

SUBMODULE (m:subm2) subm3
USE m2, modInt => a2
CONTAINS
  module procedure func3
      use t2
      print *, "func3 lbound = ", lbound(arg2)
      print *, "func3 ubound = ", ubound(arg2)
      func3 = base2()
      func3%i = modInt + arg3%i * 1.0
  end procedure func3

  module procedure func4
      use t2
      USE m2, modInt => b2
      print *, "func4 lbound = ", lbound(arg2)
      print *, "func4 ubound = ", ubound(arg2)
      func4 = base2()
      func4%i = modInt
  end
END SUBMODULE

SUBMODULE (m:subm3) subm4
CONTAINS
  module procedure func5
      real :: modInt = 104.0
      print *, "func5 lbound = ", lbound(arg2)
      print *, "func5 ubound = ", ubound(arg2)
      func5 = base()
      func5%i = modInt + arg3%i
  end
END SUBMODULE subm4

SUBMODULE (m:subm4) subm5
USE t
USE t2
CONTAINS
  module procedure func6
      logical precision_r4
      integer :: a(100)
      character(5), allocatable :: b(:)
      type(base) :: res_func2, res_func5, c
      type(base2) :: res_func3, res_func4

      allocate(b(2))
      c = base()
      c%i = 1

      res_func2 = func2(7, a, 5, b)
      res_func3 = func3(7, a, c)
      res_func4 = func4(7, a, 5, b)
      res_func5 = func5(7, a, c)
      if(.not.precision_r4(func1(7,a),100.0)) error stop 66   ! expect modInt value from host scope
      if(res_func2%i.ne.101) error stop 67   ! expect modInt value from subm2 scope
      if(.not.precision_r4(res_func3%i,103.0)) error stop 68   ! expect modInt value from subm3 scope
      if(.not.precision_r4(res_func4%i,103.0)) error stop 69   ! expect modInt value from func4 scope
      if(res_func5%i.ne.105) error stop 70   ! expect modInt value from func5 scope
      func6 = modInt
  end
END SUBMODULE subm5

PROGRAM module_subprogram51f
USE m
IMPLICIT NONE
logical precision_r4
  if(.not.precision_r4(func6(),102.0)) error stop 77   ! expect modInt value from subm3 scope
END PROGRAM module_subprogram51f
