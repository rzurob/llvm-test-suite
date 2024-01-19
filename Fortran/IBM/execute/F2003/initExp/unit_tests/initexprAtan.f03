!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ATAN intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

logical precision_r4, precision_r8, precision_r16

real(4), parameter :: r4=3.0E-1
real(8), parameter :: r8=3.0D-1
real(16), parameter :: r16=1.72349Q-1

real(4) :: a=atan(r4)
real(8) :: b=atan(r8)
real(16) :: c=atan(r16)

if (.not. precision_r4(a, atan(r4))) error stop 1
if (.not. precision_r8(b, atan(r8))) error stop 2
if (.not. precision_r16(c, atan(r16))) error stop 3
end
