!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTIONS               : Testing ATAN2D intrinsic function
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none
real*4, parameter ::  r4  = atan2d(1.557_4, 1.0_4)
real*8, parameter ::  r8  = atan2d(1.557_8, 1.0_8)
real*16, parameter :: r16 = atan2d(1.557_16, 1.0_16)

real*4 rt4, re4
real*8 rt8, re8
real*16 rt16, re16

rt4  = atan2d(1.557_4, 1.0_4)
rt8  = atan2d(1.557_8, 1.0_8)
rt16 = atan2d(1.557_16, 1.0_16)

if (r4 .ne. rt4) error stop 1
if (r8 .ne. rt8) error stop 2
if (r16 .ne. rt16) print *, r16, rt16

end
