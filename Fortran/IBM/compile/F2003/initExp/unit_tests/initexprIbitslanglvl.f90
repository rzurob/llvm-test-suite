!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : IBITS intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================


integer(1) :: i1=ibits(12_1,1_1,2_1)
integer(2) :: i2=ibits(12_2,1_2,2_2)
integer(4) :: i4=ibits(12_4,1_4,2_4)
integer(8) :: i8=ibits(12_8,1_8,2_8)

end
