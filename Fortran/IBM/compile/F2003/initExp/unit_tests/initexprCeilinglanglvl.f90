!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : CEILING intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================
program main
  integer :: ii=ceiling(9.51139_4)
  integer :: jj=ceiling(9.51139_8)
  integer :: kk=ceiling(a=9.51139_16)
end program
