!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : MAXLOC intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

implicit none

integer, parameter :: a(4,4)=reshape((/4,2,9,-7,9,1,4,5,8,-1,-1,7, &
                                     & -8,5,9,-3/),(/4,4/))
integer, dimension(2) :: res1=maxloc(a)

character, parameter :: c(4)=(/'c','x','z','a'/)
integer :: res2(1)=maxloc(c)

end
