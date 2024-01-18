!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint40lp
!*
!*  DATE                       : 2006-10-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : print logical A.C. (value test)
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
!*  in a print statement.  All the values within the constructor should be
!*  converted to match the type specifier before the constructor is used to
!*  initialise the variable, whether widening or narrowing, so:
!*    print *, (/ logical*2:: <L*1value>, <L*2value>, <L*4value> /)
!*  (e.g., print *, (/ logical*2:: .true._1, .false._1, .true._2, .false._2, .true._4, .false._4 /))
!*  should print only logical*2 values
!*  (so the output should be " T F T F T F")
!*
!*  Overall, this test case is like acetint04, but printing instead of assigning.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint40lp

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

  character (50) :: base, out1, out2, out3

  ! Make sure the values in each array are correct
  !   - assigning via an array constructor makes for a circular test.
  larr1(1) = T1; larr1(2) = T2; larr1(3) = T4; larr1(4) = T8; larr1(5) = F1; larr1(6) = F2; larr1(7) = F4; larr1(8) = F8
  larr2(1) = T1; larr2(2) = T2; larr2(3) = T4; larr2(4) = T8; larr2(5) = F1; larr2(6) = F2; larr2(7) = F4; larr2(8) = F8
  larr4(1) = T1; larr4(2) = T2; larr4(3) = T4; larr4(4) = T8; larr4(5) = F1; larr4(6) = F2; larr4(7) = F4; larr4(8) = F8
  larr8(1) = T1; larr8(2) = T2; larr8(3) = T4; larr8(4) = T8; larr8(5) = F1; larr8(6) = F2; larr8(7) = F4; larr8(8) = F8
  larr(1)  = T1; larr(2)  = T2; larr(3)  = T4; larr(4)  = T8; larr(5)  = F1; larr(6)  = F2; larr(7)  = F4; larr(8)  = F8

  errorCount = 0

  write(base,'(8(L1," "))') larr1
  write(out1,'(8(L1," "))') (/logical(1):: T1, T2, T4, T8, F1, F2, F4, F8/)
  write(out2,'(8(L1," "))') (/logical(1):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1)/)
  write(out3,'(8(L1," "))') (/logical(1):: (T1,i=1,1), (T2,i=1,1), (T4,i=1,1), (T8,i=1,1), (F1,i=1,1), (F2,i=1,1), (F4,i=1,1), (F8,i=1,1)/)
  call check(1)

  write(base,'(8(L1," "))') larr2
  write(out1,'(8(L1," "))') (/logical(2):: T1, T2, T4, T8, F1, F2, F4, F8/)
  write(out2,'(8(L1," "))') (/logical(2):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1) /)
  write(out3,'(8(L1," "))') (/logical(2):: (T1,i=1,1), (T2,i=1,1), (T4,i=1,1), (T8,i=1,1), (F1,i=1,1), (F2,i=1,1), (F4,i=1,1), (F8,i=1,1) /)
  call check(2)

  write(base,'(8(L1," "))') larr4
  write(out1,'(8(L1," "))') (/logical(4):: T1, T2, T4, T8, F1, F2, F4, F8/)
  write(out2,'(8(L1," "))') (/logical(4):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1) /)
  write(out3,'(8(L1," "))') (/logical(4):: (T1,i=1,1), (T2,i=1,1), (T4,i=1,1), (T8,i=1,1), (F1,i=1,1), (F2,i=1,1), (F4,i=1,1), (F8,i=1,1) /)
  call check(4)

  write(base,'(8(L1," "))') larr8
  write(out1,'(8(L1," "))') (/logical(8):: T1, T2, T4, T8, F1, F2, F4, F8/)
  write(out2,'(8(L1," "))') (/logical(8):: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1) /)
  write(out3,'(8(L1," "))') (/logical(8):: (T1,i=1,1), (T2,i=1,1), (T4,i=1,1), (T8,i=1,1), (F1,i=1,1), (F2,i=1,1), (F4,i=1,1), (F8,i=1,1) /)
  call check(8)

  write(base,'(8(L1," "))') larr
  write(out1,'(8(L1," "))') (/logical   :: T1, T2, T4, T8, F1, F2, F4, F8/)
  write(out2,'(8(L1," "))') (/logical   :: (T1, T2, T4, T8, F1, F2, F4, F8, i=1,1) /)
  write(out3,'(8(L1," "))') (/logical   :: (T1,i=1,1), (T2,i=1,1), (T4,i=1,1), (T8,i=1,1), (F1,i=1,1), (F2,i=1,1), (F4,i=1,1), (F8,i=1,1) /)
  call check(0) ! i.e., the default

  if (errorCount > 0) then
     print *, "Error count:", errorCount
     stop 2
  end if

contains

  subroutine check(k)
    integer :: k
    if (base /= out1 .or. base /= out2 .or. base /= out3) then
       print *, "Problem with output for kind =", k, "; base /= out[123]:"
       print *, "base: [", trim(base), "]"
       print *, "out1: [", trim(out1), "]"
       print *, "out2: [", trim(out2), "]"
       print *, "out3: [", trim(out3), "]"
       errorCount = errorCount + 1
    end if
  end subroutine check

end program acetint40lp
