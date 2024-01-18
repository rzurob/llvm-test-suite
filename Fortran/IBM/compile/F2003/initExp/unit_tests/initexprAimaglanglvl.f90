!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : AIMAG intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

real(4) :: a4=aimag((3.0e0, -5.0e0))
real(8) :: a8=aimag((3.001003d0, -5.0d0))
real(16) :: a16=aimag((3.0q0, -5.14567q0))
end
