!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : EXPONENT intrinsic
!*
!* DESCRIPTION                : integer type
!* ===================================================================

implicit none

integer :: ia=exponent(10.2), ib=exponent(14762839.0D7), &
    & ic=exponent(7829384.0Q12)

if (ia .ne. exponent(10.2)) error stop 1
if (ib .ne. exponent(14762839.0D7)) error stop 2
if (ic .ne. exponent(7829384.0Q12)) error stop 3

end

