!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : AC as function return value
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

program acetint56

  implicit none
  integer :: arr(4), i

  print *, "ifunc(0):", ifunc(0)
  print *, "irfunc(0):", irfunc(0)

  print *, "ifunc(1):", ifunc(1)
  print *, "irfunc(1):", irfunc(1)

  print *, "ifunc(3):", ifunc(3)
  print *, "irfunc(3):", irfunc(3)

  ! Try it in an expression:
  print *, "compare:", irfunc(3) == ifunc(3)

  ! and in an assignment:
  arr = [integer:: (i, i=4,1,-1)]
  print *, arr

  arr(1:1) = ifunc(1)
  arr(2:4) = ifunc(3)
  print *, arr

  arr(1:1) = irfunc(1)
  arr(2:4) = irfunc(3)
  print *, arr

  ! and in a subroutine call:
  call test (ifunc(0))
  call test (irfunc(0))

  call test (ifunc(1))
  call test (irfunc(1))

  call test (ifunc(3))
  call test (irfunc(3))

contains

  function ifunc(arg)
    integer(4) :: arg
    integer(4) :: ifunc(arg)
    integer(4) :: i
    ifunc = [integer:: (i, i=arg,1,-1)]
  end function ifunc

  recursive function irfunc(arg) result(iretval)
    integer(4) :: arg
    integer(4) :: iretval(arg)
    if (arg <= 0) then
       iretval = [integer::]
    else
       iretval = [integer:: arg, irfunc(arg-1)]
    end if
  end function irfunc

  subroutine test(arr)
    integer(4) :: arr(:)
    print *, "test:", size(arr), "[", arr, "]"
  end subroutine test

end program acetint56
