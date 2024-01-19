!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : SPREAD intrinsic
!*
!* DESCRIPTION                : integer type
!* ===================================================================

implicit none
integer i

integer(1) :: resA(3,3)=spread((/1,2,3/), dim=1, ncopies=3)

integer(2), parameter :: a(6)=(/-4, 7, 6, 1, 0, 3/)
integer(2), dimension(6,6) :: res1=spread(a, dim=1, ncopies=6), &
        res2=spread(a, dim=2, ncopies=6)

integer(4), dimension(4,4) :: res4=spread((/1895642558, 742962715, &
 & 129343661, 1853935317/), dim=2, ncopies=4)

integer(8), dimension(5,5), parameter :: c=reshape((/(i,i=1,25)/),(/5,5/))
integer(8), dimension(5,5,4) :: c1=spread(c, dim=3, ncopies=4)

if (.not. all(resA .eq. spread((/1,2,3/), dim=1, ncopies=3))) error stop 1

if (.not. all(res1 .eq. spread(a, dim=1, ncopies=6))) error stop 2
if (.not. all(res2 .eq. spread(a, dim=2, ncopies=6))) error stop 3

if (.not. all(res4 .eq. spread((/1895642558, 742962715, &
 & 129343661, 1853935317/), dim=2, ncopies=4))) error stop 4

if (.not. all(c1 .eq. spread(c, dim=3, ncopies=4))) error stop 5

end
