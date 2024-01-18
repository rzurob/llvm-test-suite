!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : AIMAG intrinsic
!*
!* DESCRIPTION                : complex type
!* ===================================================================

logical precision_r4, precision_r8, precision_r16

complex(4), parameter :: c4=(3.0e0, -5.0e0)
complex(8), parameter :: c8=(3.00003d0, -5.0d0)
complex(16), parameter :: c16=(3.0q0, -5.14567q0)

real(4) :: a4=aimag(c4), d4
real(8) :: a8=aimag(c8), d8
real(16) :: a16=aimag(c16), d16

d4 = aimag(c4)
d8 = aimag(c8)
d16 = aimag(c16)

if (.not. precision_r4(a4, d4)) stop 1
if (.not. precision_r8(a8, d8)) stop 2
if (.not. precision_r16(a16, d16)) stop 3

end
