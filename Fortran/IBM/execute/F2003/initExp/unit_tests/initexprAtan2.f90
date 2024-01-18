!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : ATAN2 intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================
logical precision_r4, precision_r8, precision_r16

real(4), parameter :: r4y=3.0E-1, r4x=4.24E0
real(8), parameter :: r8y=3.0D-1, r8x=8.13485D-2
real(16), parameter :: r16y=1.72349Q-1, r16x=17.894Q-2

real(4) :: a=atan2(r4y, r4x)
real(8) :: b=atan2(r8y, r8x)
real(16) :: c=atan2(r16y, r16x)
real(4) :: a2
real(8) :: b2
real(16) :: c2

a2 = atan2(r4y, r4x)
b2 = atan2(r8y, r8x)
c2 = atan2(r16y, r16x)

if (.not. precision_r4(a, a2)) stop 1
if (.not. precision_r8(b, b2)) stop 1
if (.not. precision_r16(c, c2)) stop 1
end
