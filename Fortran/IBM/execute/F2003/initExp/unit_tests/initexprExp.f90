!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : EXP intrinsic
!*
!* DESCRIPTION                : real and complex types
!* ===================================================================

implicit none

logical :: precision_r4, precision_r8, precision_r16
logical :: precision_x8, precision_x16, precision_x32

real(4) :: r4a=exp(1.23), r4b
real(8) :: r8a=exp(-3.21D0), r8b
real(16) :: r16a=exp(-3.7221Q-1), r16b

complex(4) :: c4a=exp((1.23_4, 2.34_4))
complex(8) :: c8a=exp((-0.55D0, -7.1d-1))
complex(16) :: c16a=exp((11.0_16, 13.1_16))

r4b = exp(1.23)
r8b = exp(-3.21D0)
r16b = exp(-3.7221Q-1)

if (.not. precision_r4(r4a, r4b)) stop 1
if (.not. precision_r8(r8a, r8b)) stop 2
if (.not. precision_r16(r16a, r16b)) stop 3

if (.not. precision_x8(c4a, exp((1.23_4, 2.34_4)))) stop 4
if (.not. precision_x16(c8a, exp((-0.55D0, -7.1d-1)))) then
  write(*, '(4z20.16)') c8a, exp((-0.55D0, -7.1d-1))
  stop 5
endif
if (.not. precision_x32(c16a, exp((11.0_16, 13.1_16)))) stop 6
end
