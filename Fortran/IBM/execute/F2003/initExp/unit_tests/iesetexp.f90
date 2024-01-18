!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTIONS               : Testing SET_EXPONENT intrinsic function
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none
real*4, parameter ::  r4  = set_exponent(10.1_4, 2_4)
real*8, parameter ::  r8  = set_exponent(10.1_8, 2_8)
real*16, parameter :: r16 = set_exponent(10.1_16, 2_1)

real*4 rt4, re4
real*8 rt8, re8
real*16 rt16, re16

rt4  = set_exponent(10.1_4, 2_4)
rt8  = set_exponent(10.1_8, 2_8)
rt16 = set_exponent(10.1_16, 2_1)

if (r4 .ne. rt4) print *, r4, rt4
if (r8 .ne. rt8) error stop 2
if (r16 .ne. rt16) error stop 3

end
