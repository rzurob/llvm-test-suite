!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : INDEX intrinsic
!*
!* DESCRIPTION                : langlvl
!* ===================================================================


character(16), parameter :: cc='abcddcbaaabbccdd'
integer :: i=index(cc, 'dc')
integer(2) :: j=index(cc, 'aa')
integer(8) :: h=index(cc, 'd', back=.true.)

end
