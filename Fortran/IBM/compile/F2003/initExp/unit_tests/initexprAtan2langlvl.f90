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

real(4) :: a=atan2(1.0E-1, 2.0E-1)
real(8) :: b=atan2(2.1D-1, 3.1D-1)
real(16) :: c=atan2(3.2Q-1, 4.2Q-1)

end
