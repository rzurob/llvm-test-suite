!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : SPREAD intrinsic
!*
!* DESCRIPTION                : complex type
!* ===================================================================

implicit none

integer :: i

complex, parameter :: a(3)=(/(-4.7,2.01), (6.1,-1.7), (0.3,4.7)/)
complex, dimension(3,3) :: res1=spread(a, dim=1, ncopies=3), &
        res2=spread(a, dim=2, ncopies=3)

complex(8), dimension(4,4) :: res4=spread((/(-1.8956,4.2558), (7.4296,-2.715), &
 & (1.293,4.3661), (1.8539,-3.5317)/), dim=2, ncopies=4)

complex(16), dimension(5,5), parameter :: c=reshape((/(cmplx(i,-i),i=1,25)/),(/5,5/))
complex(16), dimension(5,5,4) :: c1=spread(c, dim=3, ncopies=4)

if (.not. all(res1 .eq. spread(a, dim=1, ncopies=3))) error stop 1
if (.not. all(res2 .eq. spread(a, dim=2, ncopies=3))) error stop 2

if (.not. all(res4 .eq. spread((/(-1.8956,4.2558), (7.4296,-2.715), &
 & (1.293,4.3661), (1.8539,-3.5317)/), dim=2, ncopies=4))) error stop 3

if (.not. all(c1 .eq. spread(c, dim=3, ncopies=4))) error stop 4

end
