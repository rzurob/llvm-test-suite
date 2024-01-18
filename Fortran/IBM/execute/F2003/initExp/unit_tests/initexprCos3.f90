!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : COS intrinsic
!*
!* DESCRIPTION                : complex type
!* ===================================================================

logical precision_x8, precision_x16, precision_x32

complex(4), parameter :: c4=(1.5e0, 174.0e-2)
complex(8), parameter :: c8=(3.123D0, -1.384D0)
complex(16), parameter :: c16=(3.0Q0, 4.1Q0)

complex(4) :: c4a = cos(c4)
complex(8) :: c8a = cos(c8)
complex(16) :: c16a = cos(c16)

if (.not. precision_x8(c4a, cos(c4))) stop 1
if (.not. precision_x16(c8a, cos(c8))) stop 2
if (.not. precision_x32(c16a, cos(c16))) stop 3

end
