!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : SUM intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

implicit none

integer :: i

real :: res1=sum( (/2,3,4/) )
real, parameter,dimension(5) :: a=(/-3,-7,-5,2,3/)
real :: res2=sum( a, mask=a.gt.-5)

real, parameter :: b(2,3)=reshape( (/-2,3,5,-4,7,3/), (/2,3/))
real :: res3(3)=sum(b, dim=1), res4(2)=sum(b, dim=2)
real :: res5(2)=sum(b, dim=2, mask=b .gt. 2)

real(8), parameter :: c(2,3,4,5)=reshape((/(i,i=1,120)/), &
  &  (/2,3,4,5/))
real(8) :: res18(2,3,5)=sum(c, dim=3)

real(16) :: res11=sum((/1.1_16,9.9_16,86.86_16,9.9_16,75.75_16,5.5_16/))

if (res2 .ne. sum( a, mask=a.gt.-5)) error stop 3
if (.not. all(res3 .eq. sum(b, dim=1))) error stop 4
if (.not. all(res4 .eq. sum(b, dim=2))) error stop 5
if (.not. all(res5 .eq. sum(b, dim=2, mask=b .gt. 2))) error stop 6

if (.not. all(res18 .eq. sum(c, dim=3))) error stop 7

if (res11 .ne. sum((/1.1_16,9.9_16,86.86_16,9.9_16,75.75_16,5.5_16/))) error stop 8

end
