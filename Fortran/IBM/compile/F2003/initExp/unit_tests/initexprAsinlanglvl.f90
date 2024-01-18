!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : ASIN intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

real :: y=asin(0.97)
real(4) :: y4=asin(0.96E0)
real(8) :: y8=asin(0.91134D0)
real(16) :: y16=asin(0.8913Q0)

end
