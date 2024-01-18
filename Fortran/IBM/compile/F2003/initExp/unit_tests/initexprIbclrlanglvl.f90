!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : IBCLR intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

integer(1) :: i1=ibclr(14_1,3)
integer(2) :: i2=ibclr(14_2,3)
integer(4) :: i4=ibclr(14_4,3)
integer(8) :: i9=ibclr(14_8,3)

end
