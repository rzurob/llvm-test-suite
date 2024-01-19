!******************************************************************************
!*  ===========================================================================
!*
!*  DATE                       : 2006-10-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : print real A.C. (value test)
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
!*  in a print statement.  All the values within the constructor should be
!*  converted to match the type specifier before the constructor is used to
!*  initialise the variable, whether widening or narrowing, so:
!*    print *, (/ real*8:: <r*4value>, <r*8value>, <r*16value> /)
!*  (e.g., print *, (/ real*8:: 1.1_4, 3.3_8, 7.7_16/))
!*  should print only real*8 values (and r*4 values cast as r*8 values)
!*  (so the output should be " 1.10000002384185791 3.29999999999999982 7.70000000000000018"
!*
!*  Overall, this test case is like acetint04, but printing instead of assigning
!*  and reals instead of integers.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint40rp

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

  character (300) :: base, out1, out2, out3

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

  write(base,'(12(g24.16:" "))') rarr4
  write(out1,'(12(g24.16:" "))') (/real(4):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  write(out2,'(12(g24.16:" "))') (/real(4):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  write(out3,'(12(g24.16:" "))') (/real(4):: (VAL4,i=1,1), (SMALL4,i=1,1), (HUGE4,i=1,1), (VAL8,i=1,1), (SMALL8,i=1,1), (HUGE8,i=1,1), (VAL16,i=1,1), &
                                   (SMALL16,i=1,1), (HUGE16,i=1,1), (VALDP,i=1,1), (SMALLDP,i=1,1), (HUGEDP,i=1,1)/)
  call check(4)

  write(base,'(12(g24.16:" "))') rarr8
  write(out1,'(12(g24.16:" "))') (/real(8):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  write(out2,'(12(g24.16:" "))') (/real(8):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  write(out3,'(12(g24.16:" "))') (/real(8):: (VAL4,i=1,1), (SMALL4,i=1,1), (HUGE4,i=1,1), (VAL8,i=1,1), (SMALL8,i=1,1), (HUGE8,i=1,1), (VAL16,i=1,1), &
                                   (SMALL16,i=1,1), (HUGE16,i=1,1), (VALDP,i=1,1), (SMALLDP,i=1,1), (HUGEDP,i=1,1)/)
  call check(8)

  write(base,'(12(g24.16:" "))') rarr16
  write(out1,'(12(g24.16:" "))') (/real(16):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  write(out2,'(12(g24.16:" "))') (/real(16):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  write(out3,'(12(g24.16:" "))') (/real(16):: (VAL4,i=1,1), (SMALL4,i=1,1), (HUGE4,i=1,1), (VAL8,i=1,1), (SMALL8,i=1,1), (HUGE8,i=1,1), (VAL16,i=1,1), &
                                   (SMALL16,i=1,1), (HUGE16,i=1,1), (VALDP,i=1,1), (SMALLDP,i=1,1), (HUGEDP,i=1,1)/)
  call check(16)

  write(base,'(12(g24.16:" "))') rarr
  write(out1,'(12(g24.16:" "))') (/real:: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  write(out2,'(12(g24.16:" "))') (/real:: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  write(out3,'(12(g24.16:" "))') (/real:: (VAL4,i=1,1), (SMALL4,i=1,1), (HUGE4,i=1,1), (VAL8,i=1,1), (SMALL8,i=1,1), (HUGE8,i=1,1), (VAL16,i=1,1), &
                                   (SMALL16,i=1,1), (HUGE16,i=1,1), (VALDP,i=1,1), (SMALLDP,i=1,1), (HUGEDP,i=1,1)/)
  call check(0) ! i.e., the default

  write(base,'(12(g24.16:" "))') dparr
  write(out1,'(12(g24.16:" "))') (/double precision:: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  write(out2,'(12(g24.16:" "))') (/double precision:: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  write(out3,'(12(g24.16:" "))') (/double precision:: (VAL4,i=1,1), (SMALL4,i=1,1), (HUGE4,i=1,1), (VAL8,i=1,1), (SMALL8,i=1,1), (HUGE8,i=1,1), (VAL16,i=1,1), &
                                   (SMALL16,i=1,1), (HUGE16,i=1,1), (VALDP,i=1,1), (SMALLDP,i=1,1), (HUGEDP,i=1,1)/)
  call check(2) ! i.e., double precision

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

end program acetint40rp
