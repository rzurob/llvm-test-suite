!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : SUM intrinsic
!*
!* DESCRIPTION                : integer type
!* ===================================================================

implicit none

integer :: i

integer(1) :: res11=sum((/-1_1,-9_1,-86_1,-9_1,-18_1,-5_1/))

integer(2), parameter :: res12(3,5)=reshape((/88,17,66,85,51,73,15,0, &
  &   0,50,91,28,56,47,12/),(/3,5/))
integer(2) :: res12a(3)=sum(res12, dim=2)

integer :: res1=sum( (/2,3,4/) )
integer, parameter,dimension(5) :: a=(/-3,-7,-5,2,3/)
integer :: res2=sum( a, mask=a.gt.-5)

integer, parameter :: b(2,3)=reshape( (/-2,3,5,-4,7,3/), (/2,3/))
integer :: res3(3)=sum(b, dim=1), res4(2)=sum(b, dim=2)
integer :: res5(2)=sum(b, dim=2, mask=b .gt. 2)

integer(8), parameter :: c(2,3,4,5)=reshape((/(i,i=1,120)/), &
  &  (/2,3,4,5/))
integer(8) :: res18(2,3,5)=sum(c, dim=3)

if (res11 .ne. sum((/-9_1,-86_1,-9_1-1_1,-18_1,-5_1/))) error stop 1
if (res11 .ne. -128) error stop 11

if (.not. all(res12a .eq. sum(res12, dim=2))) error stop 2

if (res2 .ne. sum( a, mask=a.gt.-5)) error stop 3
if (.not. all(res3 .eq. sum(b, dim=1))) error stop 4
if (.not. all(res4 .eq. sum(b, dim=2))) error stop 5
if (.not. all(res5 .eq. sum(b, dim=2, mask=b .gt. 2))) error stop 6

if (.not. all(res18 .eq. sum(c, dim=3))) error stop 7

end
