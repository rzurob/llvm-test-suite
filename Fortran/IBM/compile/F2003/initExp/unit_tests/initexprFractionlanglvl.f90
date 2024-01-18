!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : FRACTION intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

implicit none

logical :: precision_r4, precision_r8, precision_r16
real :: f4=fraction(10.2)
real(8) :: f8=fraction(15005.20626_8)
real(16) :: f16=fraction(694799581.0_16)

if (.not. precision_r4( f4, fraction(10.2))) stop 1
if (.not. precision_r8(f8, fraction(15005.20626_8))) then
  write(*, '(2z20.16)') f8, fraction(15005.20626_8)
  stop 2
endif
if (.not. precision_r16(f16, fraction(694799581.0_16))) stop 3

end
