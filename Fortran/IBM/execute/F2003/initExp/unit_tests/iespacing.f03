!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTIONS               : Testing SPACING intrinsic function
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none
real*4, parameter ::  r4  = spacing(10.1_4)
real*8, parameter ::  r8  = spacing(10.1_8)
real*16, parameter :: r16 = spacing(10.1_16)

real*4 rt4, re4
real*8 rt8, re8
real*16 rt16, re16

rt4  = spacing(10.1_4)
rt8  = spacing(10.1_8)
rt16 = spacing(10.1_16)

if (r4 .ne. rt4) error stop 1
if (r8 .ne. rt8) error stop 2
if (r16 .ne. rt16) error stop 3

end
