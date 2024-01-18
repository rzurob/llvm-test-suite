!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : MINVAL intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

implicit none

integer(1) :: res1=minval((/1_1/))
integer(2) :: res2=minval((/1_2/))
integer(4) :: res4=minval((/1_4/))
integer(8) :: res8=minval((/1_8/))

character :: resc=minval((/'c'/))

real(4) :: Rres4=minval((/1.0_4/))
real(8) :: Rres8=minval((/1.0_8/))
real(16) :: Rres16=minval((/1.0_16/))
end
