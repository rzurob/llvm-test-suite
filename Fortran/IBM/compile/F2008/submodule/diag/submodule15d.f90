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
!*    Use association of a module declared in a host module interface
!*  but not the descendant submodule procedure definition, or vice
!*  versa.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567891123456789112345678911234567891123456789112345678911234567890
MODULE m1
  type newType
    logical :: tmp
  end type
END MODULE m1

MODULE m
implicit none
INTERFACE
  module subroutine sub(x, y)
    integer :: x
    real :: y
  end subroutine

  module subroutine sub2(x, y)
    use m1
    integer :: x
    real :: y
  end subroutine

  module function func(x, y)
    integer :: x
    real :: y
    real :: func
  end function

  module function func2(x, y)
    use m1
    integer :: x
    real :: y
    real :: func2
  end function
END INTERFACE
END MODULE m

SUBMODULE (m) n
CONTAINS
  module subroutine sub2(x, y)
    integer :: x
    real :: y
    print *, x, y
  end subroutine

  module function func2(x, y)
    integer :: x
    real :: y
    real :: func2
    func2 = x * y
  end function
END SUBMODULE n

SUBMODULE (m:n) o
CONTAINS
  module subroutine sub(x, y)
    use m1
    integer :: x
    real :: y
    print *, "oops!"
  end subroutine

  module function func(x, y)
    use m1
    integer :: x
    real :: y
    real :: func
    func = x * y
  end function
END SUBMODULE o

PROGRAM submodule15d
USE m
implicit none
logical :: precision_r4
call sub(1,1.0)
call sub2(1,1.0)
if (.not.precision_r4(func(1,1.0),1.0)) error stop 66
if (.not.precision_r4(func2(1,1.0),1.0)) error stop 77

END PROGRAM submodule15d
