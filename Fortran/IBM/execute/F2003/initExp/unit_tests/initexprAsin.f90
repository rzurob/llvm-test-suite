!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ASIN intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

logical precision_r4, precision_r8, precision_r16

real :: y=asin(0.97)
real(4) :: y4=asin(0.96E0)
real(8) :: y8=asin(0.91134D0)
real(16) :: y16=asin(0.8913Q0)

if (.not. precision_r4(y, asin(0.97))) stop 1
if (.not. precision_r4(y4, asin(0.96E0))) stop 2
if (.not. precision_r8(y8, asin(0.91134D0))) stop 3
if (.not. precision_r16(y16, asin(0.8913Q0))) stop 4

end
