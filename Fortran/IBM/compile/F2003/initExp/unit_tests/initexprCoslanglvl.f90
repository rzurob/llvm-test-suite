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
!* DESCRIPTION                : langlvl message
!* ===================================================================

real(4) :: r4 = cos(1.5E0)
real(8) :: r8 = cos(1.5D0)
real(16) :: r16 = cos(1.5_16)

complex(4) :: c4a = cos((1.5e0, 174.0e-2))
complex(8) :: c8a = cos((3.123D0, -1.384D0))
complex(16) :: c16a = cos((3.0Q0, 4.1Q0))

end
