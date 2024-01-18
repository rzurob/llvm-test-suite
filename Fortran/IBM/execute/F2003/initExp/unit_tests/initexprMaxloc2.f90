!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : MAXLOC intrinsic
!*
!* DESCRIPTION                : character type
!* ===================================================================

implicit none

character, parameter, dimension(3,3) :: c=reshape((/'a','z','c', &
                                      &  'e','d','f', &
                                      &  'i','g','h'/), (/3,3/))
integer :: res1d1(3)=maxloc(c, dim=1), res1d2(3)=maxloc(c, dim=2)
integer :: res2d1(3), res2d2(3)

res2d1 = maxloc(c, dim=1)
if (.not. all(res1d1 .eq. res2d1)) stop 1

res2d2 = maxloc(c, dim=2)
if (.not. all(res1d2 .eq. res2d2)) stop 2

end
