!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : EXPONENT intrinsic
!*
!* DESCRIPTION                : integer type
!* ===================================================================

implicit none

integer :: ia=exponent(10.2), ib=exponent(14762839.0D7), &
    & ic=exponent(7829384.0Q12)

if (ia .ne. exponent(10.2)) stop 1
if (ib .ne. exponent(14762839.0D7)) stop 2
if (ic .ne. exponent(7829384.0Q12)) stop 3

end

