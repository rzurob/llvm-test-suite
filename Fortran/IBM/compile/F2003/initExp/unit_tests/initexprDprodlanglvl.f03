!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : DPROD intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

real, parameter :: x=9.51139770, y=61.3287299
real, parameter :: x8=8.51129771, y8=60.3287298
double precision :: z=dprod(x,y)
real(16) :: z16=dprod(x8,y8)

end