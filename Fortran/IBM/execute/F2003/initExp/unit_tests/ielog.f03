!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  INTRINSIC TESTED           : LOG
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none
real*4, parameter ::  r4  = log(10.1_4)
real*8, parameter ::  r8  = log(10.1_8)
real*16, parameter :: r16 = log(10.1_16)

complex(kind=4),  parameter :: c4  = log((10.1_4, 3.5_4))
complex(kind=8),  parameter :: c8  = log((10.1_8, 3.5_8))
complex(kind=16), parameter :: c16 = log((10.1_16, 3.5_16))

real*4 rt4, re4
real*8 rt8, re8
real*16 rt16, re16

complex(kind=4) ct4, ce4
complex(kind=8) ct8, ce8
complex(kind=16) ct16, ce16

real*4  ::  rd4  = log(10.1_4)
real*8  ::  rd8  = log(10.1_8)
real*16 ::  rd16 = log(10.1_16)

complex(kind=4)  :: cd4  = log((10.1_4, 3.5_4))
complex(kind=8)  :: cd8  = log((10.1_8, 3.5_8))
complex(kind=16) :: cd16 = log((10.1_16, 3.5_16))

rt4  = log(10.1_4)
rt8  = log(10.1_8)
rt16 = log(10.1_16)
ct4  = log((10.1_4, 3.5_4))
ct8  = log((10.1_8, 3.5_8))
ct16 = log((10.1_16, 3.5_16))

if (r4 .ne. rt4) error stop 1
if (r8 .ne. rt8) error stop 2
if (r16 .ne. rt16) error stop 3

if (c4 .ne. ct4) error stop 4
if (c8 .ne. ct8) error stop 5
if (c16 .ne. ct16) error stop 6

if (rd4 .ne. rt4) error stop 11
if (rd8 .ne. rt8) error stop 12
if (rd16 .ne. rt16) error stop 13

if (cd4 .ne. ct4) error stop 14
if (cd8 .ne. ct8) error stop 15
if (cd16 .ne. ct16) error stop 16

end