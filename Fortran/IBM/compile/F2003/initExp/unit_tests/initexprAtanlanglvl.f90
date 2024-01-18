!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : ATAN intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

real :: r=atan(-1.7)
real(4) :: a=atan(-1.78_4)
real(8) :: b=atan(-1.789_8)
real(16) :: c=atan(-1.7890_16)

end
