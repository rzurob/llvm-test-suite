!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint56d
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-11-23
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : AC as function return value
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : function, return
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Test misuse of recursive function definition - this should use RESULT.
!*  This is a verbatim copy of acetint56c, changing only this text and the program name.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint56d

  implicit none
  character(3) :: arr3(4)
  integer :: i

  print *, "func(1,0):", func(1,0)
  print *, "rfunc(1,0):", rfunc(1,0)

  print *, "func(2,1):", func(2,1)
  print *, "rfunc(2,1):", rfunc(2,1)

  print *, "func(3,5):", func(3,5)
  print *, "rfunc(3,5):", rfunc(3,5)

  ! Try it in an expression:
  print *, "compare:", rfunc(4,3) == func(4,3)

  ! and in an assignment:
  arr3 = [character(3):: 'abc', 'def', 'ghi', 'jkl']
  print *, arr3

  arr3(1:1) = func(3,1)
  arr3(2:4) = func(3,3)
  print *, arr3

  arr3(1:1) = rfunc(3,1)
  arr3(2:4) = rfunc(3,3)
  print *, arr3

  ! and in a subroutine call:
  call test (func(99,0))
  call test (rfunc(99,0))

  call test (func(5,6))
  call test (rfunc(5,6))

  call test (func(2,12))
  call test (rfunc(2,12))

contains

  function func(l, s)
    integer(4) :: l, s
    character(l) :: func(s)
    integer(4) :: i
    func = [character(l):: (repeat(char(64+i),l), i=s,1,-1)]
  end function func

  recursive function rfunc(l, s)
    integer(4) :: l, s
    character(l) :: rfunc(s)
    if (s <= 0) then
       rfunc = [character(l)::]
    else
       rfunc = [character(l):: repeat(char(64+s),l), rfunc(l, s-1)]
    end if
  end function rfunc

  subroutine test(arr)
    character(*) :: arr(:)
    integer :: i
    print *, "test:", size(arr), "x", len(arr), "(/", (arr(i),"/", i=1,size(arr)), ")"
  end subroutine test

end program acetint56d
