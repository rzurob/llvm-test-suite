!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : INT intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

integer :: i=int(1.23)
integer :: j=int(42219263.9_8)

integer :: k4=int(668503418_4)
integer(8) :: k8=int(668503418_8, kind=8)

end
