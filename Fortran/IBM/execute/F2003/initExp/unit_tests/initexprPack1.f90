!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : PACK intrinsic
!*
!* DESCRIPTION                : integer type
!* ===================================================================

implicit none
integer :: i

integer(1), parameter :: z0(200)=(/(i,i=1,200)/)
integer(1), parameter :: zarray(2,5,2)=reshape(z0, (/2,5,2/))
integer(1) :: z(100)=pack(zarray, mask=zarray>100, vector=(/(-1_1,i=1,100)/))

integer(2), parameter :: y0(100)=(/(-i,i=1,100)/)
integer(2), parameter :: yarr(5,4,5)=reshape(y0, (/5,4,5/))
integer(2) :: y(100)=pack(yarr, mask=.true.)

integer(4), parameter, dimension(3,3) :: i0=reshape((/0,9,0,0,0,0,0,0,7/),(/3,3/))
integer(4) :: i4(2)=pack(i0, mask=i0/=0)
integer(4) :: i4a(6)=pack(i0, mask=i0 .ne. 0, vector=(/-1,-2,-3,-4,-5,-6/))

integer(8), parameter, dimension(2,3) :: &
  & a0=reshape((/0_8,9_8,1776810419_8,8872949_8,980739590_8,7_8/),(/2,3/))
integer(8) :: a4(3)=pack(a0, mask=a0>50)
integer(8) :: a4a(7)=pack(a0, mask=a0 .ne. 0, &
  & vector=(/-1_8,-2_8,-3_8,-4_8,-5_8,-6_8,-7_8/))

if (.not. all(z .eq. pack(zarray, mask=zarray>100, vector=(/(-1_1,i=1,100)/)))) stop 1

if (.not. all(y .eq. pack(yarr, mask=.true.))) stop 2

if (.not. all(i4 .eq. pack(i0, mask=i0/=0))) stop 3
if (.not. all(i4a .eq. pack(i0, mask=i0 .ne. 0, vector=(/-1,-2,-3,-4,-5,-6/)))) stop 4

if (.not. all(a4 .eq. pack(a0, mask=a0>50))) stop 5
if (.not. all(a4a .eq. pack(a0, mask=a0 .ne. 0, &
  & vector=(/-1_8,-2_8,-3_8,-4_8,-5_8,-6_8,-7_8/)))) stop 6

end
