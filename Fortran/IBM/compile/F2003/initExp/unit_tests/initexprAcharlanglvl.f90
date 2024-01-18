!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : ACHAR on initialization expression
!*
!* DESCRIPTION                : langlvl messages
!* ===================================================================

character(1), parameter :: c=achar(66)
character(1) :: c1=achar(43_1)
character*1  :: c2=achar(68_2)
character(1) :: c3=achar(70_4)
character :: c4=achar(91_8)

end
