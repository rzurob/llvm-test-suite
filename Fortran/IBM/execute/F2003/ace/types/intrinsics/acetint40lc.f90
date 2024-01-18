!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint40lc
!*
!*  DATE                       : 2006-10-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : call subroutine with logical A.C. (value test)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type, assignment, array constructor
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that the values in a logical array constructor are correctly treated
!*  in a subroutine call.  All the values within the constructor should be
!*  converted to match the type specifier before the constructor is used to
!*  initialise the variable, whether widening or narrowing, so:
!*    call sub((/ logical*2:: <L*1value>, <L*2value>, <L*4value> /))
!*  (e.g., call sub((/ logical*2:: .true._1, .false._1, .true._2, .false._2, .true._4, .false._4 /)))
!*  should pass in only logical*2 values
!*  (i.e., (/ T F T F T F /))
!*
!*  Overall, this test case is like acetint04, but calling a subroutine instead of assigning.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint40lc

  implicit none
  integer    :: i, errorCount
  logical    :: larr (8)
  logical(1) :: larr1(8)
  logical(2) :: larr2(8)
  logical(4) :: larr4(8)
  logical(8) :: larr8(8)

  logical(1), parameter :: T1 = .true._1, F1 = .false._1
  logical(2), parameter :: T2 = .true._2, F2 = .false._2
  logical(4), parameter :: T4 = .true._4, F4 = .false._4
  logical(8), parameter :: T8 = .true._8, F8 = .false._8

  ! Make sure the values in each array are correct
  !   - assigning via an array constructor makes for a circular test.
  larr1(1) = T1; larr1(2) = T2; larr1(3) = T4; larr1(4) = T8; larr1(5) = F1; larr1(6) = F2; larr1(7) = F4; larr1(8) = F8
  larr2(1) = T1; larr2(2) = T2; larr2(3) = T4; larr2(4) = T8; larr2(5) = F1; larr2(6) = F2; larr2(7) = F4; larr2(8) = F8
  larr4(1) = T1; larr4(2) = T2; larr4(3) = T4; larr4(4) = T8; larr4(5) = F1; larr4(6) = F2; larr4(7) = F4; larr4(8) = F8
  larr8(1) = T1; larr8(2) = T2; larr8(3) = T4; larr8(4) = T8; larr8(5) = F1; larr8(6) = F2; larr8(7) = F4; larr8(8) = F8
  larr(1)  = T1; larr(2)  = T2; larr(3)  = T4; larr(4)  = T8; larr(5)  = F1; larr(6)  = F2; larr(7)  = F4; larr(8)  = F8

  errorCount = 0


  call check1(larr1, (/logical(1):: T1, T2, T4, T8, F1, F2, F4, F8/), 1)
  call check1(larr1, (/logical(1):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/), 2)
  call check1(larr1, (/logical(1):: (T1,i=1,1), (T2,i=1,1), (T4,i=1,1), (T8,i=1,1), (F1,i=1,1), (F2,i=1,1), (F4,i=1,1), (F8,i=1,1)/), 3)

  call check2(larr2, (/logical(2):: T1, T2, T4, T8, F1, F2, F4, F8/), 4)
  call check2(larr2, (/logical(2):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/), 5)
  call check2(larr2, (/logical(2):: (T1,i=1,1), (T2,i=1,1), (T4,i=1,1), (T8,i=1,1), (F1,i=1,1), (F2,i=1,1), (F4,i=1,1), (F8,i=1,1)/), 6)

  call check4(larr4, (/logical(4):: T1, T2, T4, T8, F1, F2, F4, F8/), 7)
  call check4(larr4, (/logical(4):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/), 8)
  call check4(larr4, (/logical(4):: (T1,i=1,1), (T2,i=1,1), (T4,i=1,1), (T8,i=1,1), (F1,i=1,1), (F2,i=1,1), (F4,i=1,1), (F8,i=1,1)/), 9)

  call check8(larr8, (/logical(8):: T1, T2, T4, T8, F1, F2, F4, F8/), 10)
  call check8(larr8, (/logical(8):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/), 11)
  call check8(larr8, (/logical(8):: (T1,i=1,1), (T2,i=1,1), (T4,i=1,1), (T8,i=1,1), (F1,i=1,1), (F2,i=1,1), (F4,i=1,1), (F8,i=1,1)/), 12)

  call check(larr, (/logical:: T1, T2, T4, T8, F1, F2, F4, F8/), 13)
  call check(larr, (/logical:: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/), 14)
  call check(larr, (/logical:: (T1,i=1,1), (T2,i=1,1), (T4,i=1,1), (T8,i=1,1), (F1,i=1,1), (F2,i=1,1), (F4,i=1,1), (F8,i=1,1)/), 15)

  if (errorCount > 0) then
     print *, "Error count:", errorCount
     stop 2
  end if

contains

  subroutine check1(base, arr, test)
    logical (1) :: base(:), arr(:)
    integer :: test
    if (any(base .neqv. arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check1

  subroutine check2(base, arr, test)
    logical (2) :: base(:), arr(:)
    integer :: test
    if (any(base .neqv. arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check2

  subroutine check4(base, arr, test)
    logical (4) :: base(:), arr(:)
    integer :: test
    if (any(base .neqv. arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check4

  subroutine check8(base, arr, test)
    logical (8) :: base(:), arr(:)
    integer :: test
    if (any(base .neqv. arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check8

  subroutine check(base, arr, test)
    logical :: base(:), arr(:)
    integer :: test
    if (any(base .neqv. arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check

end program acetint40lc
