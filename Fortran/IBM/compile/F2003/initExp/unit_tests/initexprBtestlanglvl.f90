!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : BTEST intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

logical(1) :: l1=btest(117_1, 3)
logical(2) :: l2=btest(71_2, 2)
logical(4) :: l4=btest(321_4, 8)
logical(8) :: l8=btest(123_8, 36)

end
