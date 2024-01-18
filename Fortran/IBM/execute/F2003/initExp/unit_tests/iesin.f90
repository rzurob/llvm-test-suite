!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  INTRINSIC TESTED           : SIN/SINH
!*
!* ===================================================================
!20.567890120.567890120.567890120.567890120.567890120.567890120.567890

implicit none

logical,external :: precision_x8,precision_x6,precision_x3,precision_r4,precision_r8,precision_r6

real*4, parameter ::  r4  = sin(1.0_4)
real*8, parameter ::  r8  = sin(1.0_8)
real*16, parameter :: r16 = sin(1.0_16)

complex(kind=4),  parameter :: c4  = sin((1.0_4, 0.5_4))
complex(kind=8),  parameter :: c8  = sin((1.0_8, 0.5_8))
complex(kind=16), parameter :: c16 = sin((1.0_16, 0.5_16))

real*4 rt4, re4
real*8 rt8, re8
real*16 rt16, re16

complex(kind=4) ct4, ce4
complex(kind=8) ct8, ce8
complex(kind=16) ct16, ce16

real*4  ::  rd4  = sin(1.0_4)
real*8  ::  rd8  = sin(1.0_8)
real*16 ::  rd16 = sin(1.0_16)

complex(kind=4)  :: cd4  = sin((1.0_4, 0.5_4))
complex(kind=8)  :: cd8  = sin((1.0_8, 0.5_8))
complex(kind=16) :: cd16 = sin((1.0_16, 0.5_16))

rt4  = sin(1.0_4)
rt8  = sin(1.0_8)
rt16 = sin(1.0_16)
ct4  = sin((1.0_4, 0.5_4))
ct8  = sin((1.0_8, 0.5_8))
ct16 = sin((1.0_16, 0.5_16))

if (.not. precision_r4(r4,rt4)) error stop 1
if (.not. precision_r8(r8,rt8)) error stop 2
if (.not. precision_r6(r16,rt16)) error stop 3

if (.not. precision_x8(c4,ct4)) error stop 4
if (.not. precision_x6(c8,ct8)) error stop 5
if (.not. precision_x3(c16,ct16)) error stop 6

if (.not. precision_r4(rd4,rt4)) error stop 11
if (.not. precision_r8(rd8,rt8)) error stop 12
if (.not. precision_r6(rd16,rt16)) error stop 13

if (.not. precision_x8(cd4,ct4)) error stop 14
if (.not. precision_x6(cd8,ct8)) error stop 15
if (.not. precision_x3(cd16,ct16)) error stop 16

end
