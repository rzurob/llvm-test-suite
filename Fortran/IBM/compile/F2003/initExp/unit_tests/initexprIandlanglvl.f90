!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : IAND
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

integer(1) :: i1=iand(1_1,3_1)
integer(2) :: i2=iand(0_2,33_2)
integer(4) :: i4=iand(11_4,2_4)
integer(8) :: i8=iand(15_8,31_8)

end
