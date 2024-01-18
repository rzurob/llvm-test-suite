!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : COS intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

logical precision_r4, precision_r8, precision_r16

real(4), parameter :: r4=3.0E0
real(8), parameter :: r8=3.0D0
real(16), parameter :: r16=3.0Q0+4.94065645841246544e-24

real(4)  :: a=cos(r4)
real(8)  :: b=cos(r8)
real(16) :: c=cos(r16)

if (.not. precision_r4(a, cos(r4))) then
  stop 1
endif
if (.not. precision_r8(b, cos(r8))) stop 2
if (.not. precision_r16(c, cos(r16))) stop 3
end
