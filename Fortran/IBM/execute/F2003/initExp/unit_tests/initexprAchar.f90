!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : ACHAR on initialization expression
!*
!* DESCRIPTION                : integer type
!* ===================================================================

character(1), parameter :: c=achar(66)
character(1) :: c1=achar(43_1)
character(1) :: c2=achar(68_2)
character(1) :: c3=achar(70_4)
character(1) :: c4=achar(91_8)

if (c1 /= achar(43_1)) stop 1
if (c2 /= achar(68_2)) stop 2
if (c3 /= achar(70_4)) stop 3
if (c4 /= achar(91_8)) stop 4
if (c  /= achar(66)) stop 5
end
