!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : MAXLOC intrinsic
!*
!* DESCRIPTION                : integer type
!* ===================================================================

implicit none

integer, parameter :: a(4,4)=reshape((/4,2,9,-7,9,1,4,5,8,-1,-1,7, &
                                     & -8,5,9,-3/),(/4,4/))

integer, dimension(2) :: res1=maxloc(a)
integer :: res2(4)=maxloc(a, dim=1, mask=a .lt. 7)

integer, parameter :: b(-2:2)=(/1,2,3,4,5/)
integer :: res3(1)=maxloc(b)

if (.not. all(res1 .eq. maxloc(a))) error stop 1
if (.not. all(res2 .eq. maxloc(a, dim=1, mask=a .lt. 7))) error stop 2
if (.not. all(res3 .eq. maxloc(b))) error stop 3

end
