!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : module_subprogram52f
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

module t
  type base(k, l)
    integer, kind :: k
    integer, len :: l
    integer :: i = 4
  end type
end module

module t2
  type base
    real :: i = 5.0
  end type
end module

MODULE m
use t
IMPLICIT NONE
integer, public :: modInt = 100

  INTERFACE
    !In module procedure interface body, the type parameter, bounds of one argument
    !  may depend on another dummy argument. That dependency should be available in
    !  the corresponding separate module subprogram even through several descendants
    module function func1(arg1, arg2)
      integer :: yyy = 5
      type(base(4, :)) :: func1
      allocatable :: func1
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2(arg1-4:arg1+2+kind(arg1))
    end function func1

    module subroutine sub1(arg1, arg2)
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2(arg1:arg1+modInt)
    end subroutine sub1

    module subroutine sub2(arg1, arg2, arg3, arg4)
      integer, intent(in) :: arg1, arg3
      integer, intent(in) :: arg2(arg1:arg1+modInt)
      character(arg3), allocatable :: arg4(:)
    end subroutine sub2

    module subroutine sub3(arg1, arg2, arg3)
      use t2
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2(arg1:arg1+modInt)
      type(base) :: arg3
    end subroutine sub3

    module subroutine sub4()
    end subroutine sub4

    module subroutine sub5()
    end subroutine sub5

    module subroutine sub6()
      use t2
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
      print *, "in sub1: lbound = ", lbound(arg2)
      print *, "in sub1: ubound = ", ubound(arg2)
      print *, "in sub1: modInt = ", modInt
    end
END SUBMODULE subm

SUBMODULE (m:subm) subm2
integer :: modInt = 101
CONTAINS
  module procedure sub2
      arg4 = (/ 'hello', 'world' /)

      print *, "in sub2: lbound = ", lbound(arg2)
      print *, "in sub2: ubound = ", ubound(arg2)
      print *, "in sub2: arg4 = ", arg4(1:2)
      print *, "in sub2: modInt = ", modInt
  end procedure sub2
END SUBMODULE subm2

SUBMODULE (m:subm) subm3
USE m2, modInt => a2
CONTAINS
  module procedure sub3
      use t2 !-- required according to Daniel, since it wont be propogated by the use in interface, which means base would be the base in t
      print *, "in sub3: lbound = ", lbound(arg2)
      print *, "in sub3: ubound = ", ubound(arg2)
      print *, "in sub3: arg3%i = ", arg3%i
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

SUBMODULE (m:subm3) subm5
CONTAINS
  module procedure sub6
      use t2
      character(5), allocatable :: b(:)
      integer :: a(100)
      type(base) :: c
      allocate(b(2))
      c = base()
      c%i = 1

      call sub1(17,a)                        ! expect modInt value from host scope
      call sub2(7, a, 5, b)                  ! expect modInt value from subm2 scope
      call sub3(7, a, c)                     ! expect modInt value from subm3 scope
      call sub4                              ! expect modInt value from sub4 scope
      call sub5                              ! expect modInt value from sub5 scope
      print *, "in sub6: modInt = ", modInt  ! expect modInt value from subm3 scope
      call  helper1()
  end

  subroutine helper1()
      use t !-- required according to Daniel, since it wont be propogated by the use in interface
      integer, allocatable :: i(:)
      type(base(4, :)), allocatable :: res(:)
      integer  j
      j = 10
      allocate(i(j))
     ! res = func1(j, i)
  end

END SUBMODULE subm5

SUBMODULE (m:subm5) subm6
CONTAINS
  module procedure func1
    integer :: yyy = 6
    print*, arg1
    print*, lbound(arg2)
    print*, ubound(arg2)
    func1 = base(4, arg1)()
    print*, func1%i
    print*, func1%l
    func1%i = 5
    print*, func1%i
    print*, func1%l
    print*, yyy
  end
END SUBMODULE subm6

PROGRAM module_subprogram52f
USE m
IMPLICIT NONE
call sub6
END PROGRAM module_subprogram52f
