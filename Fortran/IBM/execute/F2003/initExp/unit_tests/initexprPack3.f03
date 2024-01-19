!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : PACK intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

implicit none
integer :: i

real(4), parameter :: i0(3,2)=reshape((/1.2,5.5,0.0,5.0,6.1,1.4/),(/3,2/))
real(4) :: i4(3)=pack(i0, mask=i0 .gt. 1.5)
real(4) :: i4a(6)=pack(i0, mask=i0 .lt. 3.0, vector=(/-1.,-2.,-3.,-4.,-5.,-6./))

real(8), parameter, dimension(2,3) :: &
  & a0=reshape((/0.161_8,9.4_8,1.776419_8,88.72949_8,9807.39_8,7.4_8/),(/2,3/))
real(8) :: a4(2)=pack(a0, mask=a0>50.0)
real(8) :: a4a(7)=pack(a0, mask=a0 .lt. 1.0, &
  & vector=(/-1.7_8,-2.6_8,-3.5_8,-4.4_8,-5.3_8,-6.2_8,-7.1_8/))

real(16), parameter, dimension(2,2,2,2,2,1) :: &
  & c0=reshape((/(i,i=1,32)/),(/2,2,2,2,2,1/))
real(16) :: c1(32)=pack(c0, mask=.true.)

if (.not. all(i4 .eq. pack(i0, mask=i0 .gt. 1.5))) error stop 1
if (.not. all(i4a .eq. pack(i0, mask=i0 .lt. 3.0, &
  &  vector=(/-1.,-2.,-3.,-4.,-5.,-6./)))) error stop 2

if (.not. all(a4 .eq. pack(a0, mask=a0>50.0))) error stop 3
if (.not. all(a4a .eq. pack(a0, mask=a0 .lt. 1.0, &
  & vector=(/-1.7_8,-2.6_8,-3.5_8,-4.4_8,-5.3_8,-6.2_8,-7.1_8/)))) error stop 4

if (.not. all(c1 .eq. pack(c0, mask=.true.))) error stop 5
end
