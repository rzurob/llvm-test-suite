!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : LGE, LGT, LLE and LLT intrinsics
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

implicit none

character(5), parameter :: A='xyzqb', B='aeycu'

logical(2) :: i2lge=lge(A, B)
logical(4) :: i4lge=lge(A, B)
logical(8) :: i8lge=lge(A, B)

logical(2) :: i2lgt=lgt(A, B)
logical(4) :: i4lgt=lgt(A, B)
logical(8) :: i8lgt=lgt(A, B)

logical(2) :: i2llt=llt(A, B)
logical(4) :: i4llt=llt(A, B)
logical(8) :: i8llt=llt(A, B)

logical(2) :: i2lle=lle(A, B)
logical(4) :: i4lle=lle(A, B)
logical(8) :: i8lle=lle(A, B)


end

