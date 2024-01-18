!* ===================================================================
!*
!* DATE                       : March 31, 2006
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
