!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTIONS               : Testing TAN intrinsic function
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none
real*4, parameter ::  r4  = tan(1.0_4)
real*8, parameter ::  r8  = tan(1.0_8)
real*16, parameter :: r16 = tan(1.0_16)

real*4 rt4, re4
real*8 rt8, re8
real*16 rt16, re16

logical, external :: precision_r4, precision_r8, precision_r6

rt4  = tan(1.0_4)
rt8  = tan(1.0_8)
rt16 = tan(1.0_16)

if (.not. precision_r4(r4, rt4)) error stop 1_4
if (.not. precision_r8(r8, rt8)) error stop 2_4
if (.not. precision_r6(r16, rt16)) error stop 3_4

end
