!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : AC as function return value (derived type)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : function, return
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Test use of AC as function return value (RHS of assignment to function name).
!*  Include recursive functions.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt56mod

  implicit none
  type derived
     integer :: ival
   contains
     procedure :: derivedEquals
     generic :: operator(.eq.) => derivedEquals
  end type derived

contains

  elemental logical function derivedEquals(this, that)
    class (derived), intent(in) :: this, that
    derivedEquals = this % ival == that % ival
  end function derivedEquals

  function func(arg)
    integer(4) :: arg
    type (derived) :: func(arg)
    integer(4) :: i
    func = [derived:: (derived(i), i=arg,1,-1)]
  end function func

  recursive function rfunc(arg) result(retval)
    integer(4) :: arg
    type (derived) :: retval(arg)
    if (arg <= 0) then
       retval = [derived::]
    else
       retval = [derived:: derived(arg), rfunc(arg-1)]
    end if
  end function rfunc

  subroutine test(arr)
    type (derived) :: arr(:)
    print *, "test:", size(arr), "[", arr, "]"
  end subroutine test

end module acetdt56mod


program acetdt56

  use acetdt56mod
  implicit none

  type (derived) :: arr(4)
  integer :: i

  print *, "func(0):", func(0)
  print *, "rfunc(0):", rfunc(0)

  print *, "func(1):", func(1)
  print *, "rfunc(1):", rfunc(1)

  print *, "func(3):", func(3)
  print *, "rfunc(3):", rfunc(3)

  ! Try it in an expression:
  print *, "compare:", rfunc(3) == func(3)

  ! and in an assignment:
  arr = [derived:: (derived(i), i=4,1,-1)]
  print *, arr

  arr(1:1) = func(1)
  arr(2:4) = func(3)
  print *, arr

  arr(1:1) = rfunc(1)
  arr(2:4) = rfunc(3)
  print *, arr

  ! and in a subroutine call:
  call test (func(0))
  call test (rfunc(0))

  call test (func(1))
  call test (rfunc(1))

  call test (func(3))
  call test (rfunc(3))

end program acetdt56
