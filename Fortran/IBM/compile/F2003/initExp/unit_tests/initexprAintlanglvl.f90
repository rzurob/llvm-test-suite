!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : AINT intrinsic
!*
!* DESCRIPTION                : complex type
!* ===================================================================

real(4) :: a4=aint(1.1_4)
real(8) :: a8=aint(1.1_8)
real(16) :: a16=aint(1.4_16)

end
