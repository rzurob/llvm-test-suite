!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : COSH intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

logical precision_r4, precision_r8, precision_r16

real(4), parameter :: r4=1.0E-1
real(8), parameter :: r8=1.3527D-1
real(16), parameter :: r16=1.72349Q-1

real(4) :: a=cosh(r4)
real(8) :: b=cosh(r8)
real(16) :: c=cosh(r16)

if (.not. precision_r4(a, cosh(r4))) stop 1
if (.not. precision_r8(b, cosh(r8))) stop 2
if (.not. precision_r16(c, cosh(r16))) stop 3

end
