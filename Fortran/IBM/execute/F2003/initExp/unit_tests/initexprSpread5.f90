!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : SPREAD intrinsic
!*
!* DESCRIPTION                : character type
!* ===================================================================

implicit none

character(5) :: resA(4,4)=spread((/'IBM  ','SUN  ','HP   ','INTEL'/), dim=1, ncopies=4)

if (.not. all(resA .eq. &
  & spread((/'IBM  ','SUN  ','HP   ','INTEL'/), dim=1, ncopies=4))) stop 1

end
