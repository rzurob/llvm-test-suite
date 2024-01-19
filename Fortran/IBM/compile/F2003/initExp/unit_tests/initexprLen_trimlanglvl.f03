!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : LEN_TRIM intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

implicit none

integer(1) :: i1=len_trim(' a B ')
integer(2) :: i2=len_trim('            a B ')
integer(4) :: i4=len_trim(' a      B ')
integer(8) :: i8=len_trim(' a B         ')

end

