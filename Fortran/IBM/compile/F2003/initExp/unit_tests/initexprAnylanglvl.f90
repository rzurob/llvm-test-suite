!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : ANY intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================
logical :: l=any((/ z'ab', z'cd' /) .ne. (/ z'12', z'34' /))
logical(1) :: l1=any((/1/) .eq. (/2/))
logical(2) :: l2=any((/1_2/) .eq. (/2_2/))
logical(4) :: l4=any((/1_4/) .eq. (/2_4/))
logical(8) :: l8=any((/1_8/) .eq. (/2_8/))
end
