!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint40ip
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-10-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : print integer A.C. (value test)
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
!*  in a print statement.  All the values within the constructor should be
!*  converted to match the type specifier before the constructor is used to
!*  initialise the variable, whether widening or narrowing, so:
!*    print *, (/ integer*2:: <i*1value>, <i*2value>, <i*4value> /)
!*  (e.g., print *, (/ integer*2:: 31, 31000, 3100000/))
!*  should print only integer*2 values
!*  (so the output should be " 31 31000 19808" (19808=3100000&z'FFFF')).
!*
!*  Overall, this test case is like acetint04, but printing instead of assigning.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint40ip

  implicit none
  integer    :: i, errorCount
  integer    :: iarr (4)
  integer(1) :: iarr1(4)
  integer(2) :: iarr2(4)
  integer(4) :: iarr4(4)
  integer(8) :: iarr8(4)

  integer(1), parameter :: MASK1 = int(z'7F',1)
  integer(2), parameter :: MASK2 = int(z'7F7F',2)
  integer(4), parameter :: MASK4 = int(z'7F7F7F7F',4)
  integer(8), parameter :: MASK8 = int(z'7F7F7F7F7F7F7F7F',8)

  character (50) :: base, out1, out2, out3

  ! Make sure the values in each array are correct
  !   - assigning via an array constructor makes for a circular test.
  iarr1(1) = MASK1; iarr1(2) = MASK2; iarr1(3) = MASK4; iarr1(4) = MASK8
  iarr2(1) = MASK1; iarr2(2) = MASK2; iarr2(3) = MASK4; iarr2(4) = MASK8
  iarr4(1) = MASK1; iarr4(2) = MASK2; iarr4(3) = MASK4; iarr4(4) = MASK8
  iarr8(1) = MASK1; iarr8(2) = MASK2; iarr8(3) = MASK4; iarr8(4) = MASK8
  iarr(1)  = MASK1; iarr(2)  = MASK2; iarr(3)  = MASK4; iarr(4)  = MASK8

  errorCount = 0

  write(base,'(4(i0," "))') iarr1
  write(out1,'(4(i0," "))') (/integer(1):: MASK1, MASK2, MASK4, MASK8/)
  write(out2,'(4(i0," "))') (/integer(1):: (MASK1, MASK2, MASK4, MASK8, i=1,1)/)
  write(out3,'(4(i0," "))') (/integer(1):: (MASK1,i=1,1), (MASK2,i=1,1), (MASK4,i=1,1), (MASK8, i=1,1)/)
  call check(1)

  write(base,'(4(i0," "))') iarr2
  write(out1,'(4(i0," "))') (/integer(2):: MASK1, MASK2, MASK4, MASK8/)
  write(out2,'(4(i0," "))') (/integer(2):: (MASK1, MASK2, MASK4, MASK8, i=1,1) /)
  write(out3,'(4(i0," "))') (/integer(2):: (MASK1,i=1,1), (MASK2,i=1,1), (MASK4,i=1,1), (MASK8, i=1,1) /)
  call check(2)

  write(base,'(4(i0," "))') iarr4
  write(out1,'(4(i0," "))') (/integer(4):: MASK1, MASK2, MASK4, MASK8/)
  write(out2,'(4(i0," "))') (/integer(4):: (MASK1, MASK2, MASK4, MASK8, i=1,1) /)
  write(out3,'(4(i0," "))') (/integer(4):: (MASK1,i=1,1), (MASK2,i=1,1), (MASK4,i=1,1), (MASK8, i=1,1) /)
  call check(4)

  write(base,'(4(i0," "))') iarr8
  write(out1,'(4(i0," "))') (/integer(8):: MASK1, MASK2, MASK4, MASK8/)
  write(out2,'(4(i0," "))') (/integer(8):: (MASK1, MASK2, MASK4, MASK8, i=1,1) /)
  write(out3,'(4(i0," "))') (/integer(8):: (MASK1,i=1,1), (MASK2,i=1,1), (MASK4,i=1,1), (MASK8, i=1,1) /)
  call check(8)

  write(base,'(4(i0," "))') iarr
  write(out1,'(4(i0," "))') (/integer   :: MASK1, MASK2, MASK4, MASK8/)
  write(out2,'(4(i0," "))') (/integer   :: (MASK1, MASK2, MASK4, MASK8, i=1,1) /)
  write(out3,'(4(i0," "))') (/integer   :: (MASK1,i=1,1), (MASK2,i=1,1), (MASK4,i=1,1), (MASK8, i=1,1) /)
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

end program acetint40ip
