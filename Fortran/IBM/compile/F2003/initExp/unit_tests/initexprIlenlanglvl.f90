!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : ILEN intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

integer(1) :: i1=ilen(2_1)
integer(2) :: i2=ilen(14_2)
integer :: i=ilen(14_4)
integer(8) :: i8=ilen(1141_8)

end
