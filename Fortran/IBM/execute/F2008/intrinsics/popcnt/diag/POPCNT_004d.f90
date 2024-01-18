! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : November 30, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test that if F2008 langlvl is specified,
!*                               the compiler will not issue error message
!*                               when the  POPCNT() is constant expression
!*
!*  TARGET(S)                  :
!* ===================================================================

program POPCNT_004d

implicit none

integer, parameter :: i = POPCNT(10)
integer, dimension(POPCNT(10)) :: arr
integer :: res1
integer :: res2

if(size(arr) /= 2) ERROR STOP 1
if(i /= 2) ERROR STOP 2

end program POPCNT_004d

