!******************************************************************************
!*  ===========================================================================
!*
!*  DATE                       : 2006-10-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : call subroutine with real A.C. (value test)
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
!*  Verify that the values in a real array constructor are correctly treated
!*  in a subroutine call.  All the values within the constructor should be
!*  converted to match the type specifier before the constructor is used to
!*  initialise the variable, whether widening or narrowing, so:
!*    call sub((/ real*8:: <r*4value>, <r*8value>, <r*16value> /))
!*  (e.g., call sub((/ real*8:: 1.1_4, 3.3_8, 7.7_16/))
!*  should pass in only real*8 values (and r*4 values cast as r*8 values)
!*  (i.e., (/1.1_4, 3.3_8, 7.7_8/) --- the first is actually 1.1_4
!*  stretched to 8 bytes, but without the precision given by _8).
!*
!*  Overall, this test case is like acetint04, but calling a subroutine instead
!*  of assigning and reals instead of integers.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint40rc

  implicit none
  integer :: i, errorCount
  real(4) :: rarr4(12)
  real(8) :: rarr8(12)
  real(16):: rarr16(12)
  real    :: rarr (12)
  double precision :: dparr(12)

  real(4),  parameter :: VAL4  = 1.1_4, SMALL4 = tiny(rarr4), HUGE4 = huge(rarr4)
  real(8),  parameter :: VAL8  = 1.1_8, SMALL8 = tiny(rarr8), HUGE8 = huge(rarr8)
  real(16), parameter :: VAL16  = 1.1_16, SMALL16 = tiny(rarr16), HUGE16 = huge(rarr16)
  integer, parameter  :: DP = kind(dparr)
  double precision, parameter :: VALDP  = 1.1_DP, SMALLDP = tiny(dparr), HUGEDP = huge(dparr)

  ! Make sure the values in each array are correct
  !   - assigning via an array constructor makes for a circular test.
  rarr4(1) = VAL4; rarr4(2) = SMALL4; rarr4(3) = HUGE4; rarr4(4) = VAL8; rarr4(5) = SMALL8; rarr4(6) = HUGE8;
  rarr4(7) = VAL16; rarr4(8) = SMALL16; rarr4(9) = HUGE16; rarr4(10) = VALDP; rarr4(11) = SMALLDP; rarr4(12) = HUGEDP

  rarr8(1) = VAL4; rarr8(2) = SMALL4; rarr8(3) = HUGE4; rarr8(4) = VAL8; rarr8(5) = SMALL8; rarr8(6) = HUGE8;
  rarr8(7) = VAL16; rarr8(8) = SMALL16; rarr8(9) = HUGE16; rarr8(10) = VALDP; rarr8(11) = SMALLDP; rarr8(12) = HUGEDP

  rarr16(1) = VAL4; rarr16(2) = SMALL4; rarr16(3) = HUGE4; rarr16(4) = VAL8; rarr16(5) = SMALL8; rarr16(6) = HUGE8;
  rarr16(7) = VAL16; rarr16(8) = SMALL16; rarr16(9) = HUGE16; rarr16(10) = VALDP; rarr16(11) = SMALLDP; rarr16(12) = HUGEDP

  rarr(1) = VAL4; rarr(2) = SMALL4; rarr(3) = HUGE4; rarr(4) = VAL8; rarr(5) = SMALL8; rarr(6) = HUGE8;
  rarr(7) = VAL16; rarr(8) = SMALL16; rarr(9) = HUGE16; rarr(10) = VALDP; rarr(11) = SMALLDP; rarr(12) = HUGEDP

  dparr(1) = VAL4; dparr(2) = SMALL4; dparr(3) = HUGE4; dparr(4) = VAL8; dparr(5) = SMALL8; dparr(6) = HUGE8;
  dparr(7) = VAL16; dparr(8) = SMALL16; dparr(9) = HUGE16; dparr(10) = VALDP; dparr(11) = SMALLDP; dparr(12) = HUGEDP

  errorCount = 0

  call check4(rarr4, (/real(4):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/), 1)
  call check4(rarr4, (/real(4):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/), 2)
  call check4(rarr4, (/real(4):: (VAL4,i=1,1), (SMALL4,i=1,1), (HUGE4,i=1,1), (VAL8,i=1,1), (SMALL8,i=1,1), (HUGE8,i=1,1), (VAL16,i=1,1), &
                       (SMALL16,i=1,1), (HUGE16,i=1,1), (VALDP,i=1,1), (SMALLDP,i=1,1), (HUGEDP,i=1,1)/), 3)

  call check8(rarr8, (/real(8):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/), 4)
  call check8(rarr8, (/real(8):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/), 5)
  call check8(rarr8, (/real(8):: (VAL4,i=1,1), (SMALL4,i=1,1), (HUGE4,i=1,1), (VAL8,i=1,1), (SMALL8,i=1,1), (HUGE8,i=1,1), (VAL16,i=1,1), &
                       (SMALL16,i=1,1), (HUGE16,i=1,1), (VALDP,i=1,1), (SMALLDP,i=1,1), (HUGEDP,i=1,1)/), 6)

  call check16(rarr16, (/real(16):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/), 7)
  call check16(rarr16, (/real(16):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/), 8)
  call check16(rarr16, (/real(16):: (VAL4,i=1,1), (SMALL4,i=1,1), (HUGE4,i=1,1), (VAL8,i=1,1), (SMALL8,i=1,1), (HUGE8,i=1,1), (VAL16,i=1,1), &
                       (SMALL16,i=1,1), (HUGE16,i=1,1), (VALDP,i=1,1), (SMALLDP,i=1,1), (HUGEDP,i=1,1)/), 9)

  call check(rarr, (/real:: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/), 10)
  call check(rarr, (/real:: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/), 11)
  call check(rarr, (/real:: (VAL4,i=1,1), (SMALL4,i=1,1), (HUGE4,i=1,1), (VAL8,i=1,1), (SMALL8,i=1,1), (HUGE8,i=1,1), (VAL16,i=1,1), &
                       (SMALL16,i=1,1), (HUGE16,i=1,1), (VALDP,i=1,1), (SMALLDP,i=1,1), (HUGEDP,i=1,1)/), 12)

  call checkdp(dparr, (/double precision:: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/), 13)
  call checkdp(dparr, (/double precision:: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/), 14)
  call checkdp(dparr, (/double precision:: (VAL4,i=1,1), (SMALL4,i=1,1), (HUGE4,i=1,1), (VAL8,i=1,1), (SMALL8,i=1,1), (HUGE8,i=1,1), (VAL16,i=1,1), &
                       (SMALL16,i=1,1), (HUGE16,i=1,1), (VALDP,i=1,1), (SMALLDP,i=1,1), (HUGEDP,i=1,1)/), 15)

  if (errorCount > 0) then
     print *, "Error count:", errorCount
     stop 2
  end if

contains

  subroutine check4(base, arr, test)
    real (4) :: base(:), arr(:)
    integer :: test
    if (any(base /= arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check4

  subroutine check8(base, arr, test)
    real (8) :: base(:), arr(:)
    integer :: test
    if (any(base /= arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check8

  subroutine check16(base, arr, test)
    real (16) :: base(:), arr(:)
    integer :: test
    if (any(base /= arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check16

  subroutine check(base, arr, test)
    real :: base(:), arr(:)
    integer :: test
    if (any(base /= arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine check

  subroutine checkdp(base, arr, test)
    double precision :: base(:), arr(:)
    integer :: test
    if (any(base /= arr)) then
       print *, "Problem with test", test
       print *, "base: [", base, "]"
       print *, "arr: [", arr, "]"
       errorCount = errorCount + 1
    end if
  end subroutine checkdp

end program acetint40rc
