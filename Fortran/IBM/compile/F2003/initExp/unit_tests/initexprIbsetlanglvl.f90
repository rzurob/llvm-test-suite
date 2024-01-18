!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : IBSET intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

integer(1) :: i1=ibset(12_1, 1_1)
integer(2) :: i2=ibset(12_2, 1_2)
integer :: i=ibset(12,1)
integer(8) :: i8=ibset(12_8, 1_8)

end
