!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : SPREAD intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

real, parameter :: a(3)=(/-4.7, 6.1, 0.3/)
real, dimension(3,3) :: res1=spread(a, dim=1, ncopies=3), &
        res2=spread(a, dim=2, ncopies=3)

real(8), dimension(4,4) :: res4=spread((/1.895642558, 7.42962715, &
 & 1.29343661, 1.853935317/), dim=2, ncopies=4)

real(16), dimension(5,5), parameter :: c=reshape((/(i,i=1,25)/),(/5,5/))
real(16), dimension(5,5,4) :: c1=spread(c, dim=3, ncopies=4)

if (.not. all(res1 .eq. spread(a, dim=1, ncopies=3))) stop 1
if (.not. all(res2 .eq. spread(a, dim=2, ncopies=3))) stop 2

if (.not. all(res4 .eq. spread((/1.895642558, 7.42962715, &
 & 1.29343661, 1.853935317/), dim=2, ncopies=4))) stop 3

if (.not. all(c1 .eq. spread(c, dim=3, ncopies=4))) stop 4

end
