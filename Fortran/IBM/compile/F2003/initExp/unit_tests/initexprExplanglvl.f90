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
!* DESCRIPTION                : langlvl message
!* ===================================================================

implicit none

real(4) :: r4a=exp(1.23)
real(8) :: r8a=exp(-3.21D0)
real(16) :: r16a=exp(-3.7221Q-1)

complex(4) :: c4a=exp((1.23_4, 2.34_4))
complex(8) :: c8a=exp((-0.55D0, -7.1d-1))
complex(16) :: c16a=exp((11.0_16, 13.1_16))
end
