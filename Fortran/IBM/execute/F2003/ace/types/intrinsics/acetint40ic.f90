!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint40ic
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-10-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : call subroutine with integer A.C. (value test)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : intrinsic type, assignment, array constructor
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Verify that the values in an integer array constructor are correctly treated
!*  in a subroutine call.  All the values within the constructor should be
!*  converted to match the type specifier before the constructor is used to
!*  initialise the variable, whether widening or narrowing, so:
!*    call sub((/ integer*2:: <i*1value>, <i*2value>, <i*4value> /))
!*  (e.g., call sub((/ integer*2:: 31, 31000, 3100000/)))
!*  should pass in only integer*2 values
!*  (i.e., (/31, 31000, 19808/) (19808=3100000&z'FFFF')).
!*
!*  Overall, this test case is like acetint04, but calling a subroutine instead of assigning.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint40ic

  implicit none
  integer    :: iarr (4), i, errorCount
  integer(1) :: iarr1(4)
  integer(2) :: iarr2(4)
  integer(4) :: iarr4(4)
  integer(8) :: iarr8(4)

  integer(1), parameter :: MASK1 = int(z'7F',1)
  integer(2), parameter :: MASK2 = int(z'7F7F',2)
  integer(4), parameter :: MASK4 = int(z'7F7F7F7F',4)
  integer(8), parameter :: MASK8 = int(z'7F7F7F7F7F7F7F7F',8)

  ! Make sure the values in each array are correct
  !   - assigning via an array constructor makes for a circular test.
  iarr1(1) = MASK1; iarr1(2) = MASK2; iarr1(3) = MASK4; iarr1(4) = MASK8
  iarr2(1) = MASK1; iarr2(2) = MASK2; iarr2(3) = MASK4; iarr2(4) = MASK8
  iarr4(1) = MASK1; iarr4(2) = MASK2; iarr4(3) = MASK4; iarr4(4) = MASK8
  iarr8(1) = MASK1; iarr8(2) = MASK2; iarr8(3) = MASK4; iarr8(4) = MASK8
  iarr(1)  = MASK1; iarr(2)  = MASK2; iarr(3)  = MASK4; iarr(4)  = MASK8

  errorCount = 0

  call check1(iarr1, (/integer(1):: MASK1, MASK2, MASK4, MASK8/), 1)
  call check1(iarr1, (/integer(1):: (MASK1, MASK2, MASK4, MASK8, i=1,1)/), 2)
  call check1(iarr1, (/integer(1):: (MASK1,i=1,1), (MASK2,i=1,1), (MASK4,i=1,1), (MASK8, i=1,1)/), 3)

  call check2(iarr2, (/integer(2):: MASK1, MASK2, MASK4, MASK8/), 4)
  call check2(iarr2, (/integer(2):: (MASK1, MASK2, MASK4, MASK8, i=1,1)/), 5)
  call check2(iarr2, (/integer(2):: (MASK1,i=1,1), (MASK2,i=1,1), (MASK4,i=1,1), (MASK8, i=1,1)/), 6)

  call check4(iarr4, (/integer(4):: MASK1, MASK2, MASK4, MASK8/), 7)
  call check4(iarr4, (/integer(4):: (MASK1, MASK2, MASK4, MASK8, i=1,1)/), 8)
  call check4(iarr4, (/integer(4):: (MASK1,i=1,1), (MASK2,i=1,1), (MASK4,i=1,1), (MASK8, i=1,1)/), 9)

  call check8(iarr8, (/integer(8):: MASK1, MASK2, MASK4, MASK8/), 10)
  call check8(iarr8, (/integer(8):: (MASK1, MASK2, MASK4, MASK8, i=1,1)/), 11)
  call check8(iarr8, (/integer(8):: (MASK1,i=1,1), (MASK2,i=1,1), (MASK4,i=1,1), (MASK8, i=1,1)/), 12)

  call check(iarr, (/integer:: MASK1, MASK2, MASK4, MASK8/), 13)
  call check(iarr, (/integer:: (MASK1, MASK2, MASK4, MASK8, i=1,1)/), 14)
  call check(iarr, (/integer:: (MASK1,i=1,1), (MASK2,i=1,1), (MASK4,i=1,1), (MASK8, i=1,1)/), 15)

  if (errorCount > 0) then
     print *, "Error count:", errorCount
     stop 2
  end if

contains

  subroutine check1(base, arr, test)
    integer (1) :: base(:), arr(:)
    integer :: test
    if (any(base /= arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check1

  subroutine check2(base, arr, test)
    integer (2) :: base(:), arr(:)
    integer :: test
    if (any(base /= arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check2

  subroutine check4(base, arr, test)
    integer (4) :: base(:), arr(:)
    integer :: test
    if (any(base /= arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check4

  subroutine check8(base, arr, test)
    integer (8) :: base(:), arr(:)
    integer :: test
    if (any(base /= arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check8

  subroutine check(base, arr, test)
    integer :: base(:), arr(:)
    integer :: test
    if (any(base /= arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check

end program acetint40ic
