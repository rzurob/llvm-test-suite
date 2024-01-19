!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ACOS on initialization expression
!*
!* DESCRIPTION                : real type
!* ===================================================================

logical precision_r4, precision_r8, precision_r16

real(4), parameter :: r4=3.0E-1
real(8), parameter :: r8=3.0D-1
real(16), parameter :: r16=1.72349Q-1

real(4) :: a=acos(r4)
real(8) :: b=acos(r8)
real(16) :: c=acos(r16)

if (.not. precision_r4(a, acos(r4))) then
  write(*, '(2z10.8)') a, acos(r4)
  stop 1
endif
if (.not. precision_r8(b, acos(r8))) error stop 2
if (.not. precision_r16(c, acos(r16))) error stop 3
end
