!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Initialization Expression
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTIONS               : Testing NEAREST intrinsic function
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none
real*4, parameter ::  r4  = nearest(10.1_4, 2.0_4)
real*8, parameter ::  r8  = nearest(10.1_8, 2.0_8)
real*16, parameter :: r16 = nearest(10.1_16, 2.0_16)

real*4 rt4, re4
real*8 rt8, re8
real*16 rt16, re16

rt4  = nearest(10.1_4, 2.0_4)
rt8  = nearest(10.1_8, 2.0_8)
rt16 = nearest(10.1_16, 2.0_16)

if (r4 .ne. rt4) print *, r4, rt4
if (r8 .ne. rt8) error stop 2
if (r16 .ne. rt16) error stop 3

end
