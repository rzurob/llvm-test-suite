!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ISHFTC intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

implicit none

integer :: i1=ishftc(3_1,1)
integer :: i2=ishftc(31_2,1)
integer :: i4=ishftc(311_4,2,size=2)
integer :: i8=ishftc(3112_8,11)

end
