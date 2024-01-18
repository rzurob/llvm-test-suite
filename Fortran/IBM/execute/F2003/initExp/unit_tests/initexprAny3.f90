!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ANY intrinsic
!*
!* DESCRIPTION                : real type, DIM arg
!* ===================================================================

real(8), parameter, dimension(2,2,2,2) :: &
  & x=reshape((/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/), (/2,2,2,2/)), &
  & y=reshape((/1,2,4,4,5,6,7,8,9,10,11,12,13,14,15,16/), (/2,2,2,2/))

logical(8) :: dF=any(x .lt. y)
logical(8) :: dT=any(x .le. y)
logical(8) :: eA(2,2,2)=any(x .lt. y, dim=1)
logical(8) :: eB(2,2,2)=any(x .le. y, dim=2)
logical(8) :: eC(2,2,2)=any(x .le. y, dim=3)
logical(8) :: eD(2,2,2)=any(x .le. y, dim=4)

if (dF .neqv. any(x .lt. y)) stop 1
if (dT .neqv. any(x .le. y)) stop 2

if (.not. any(eA .eqv. any(x .lt. y, dim=1))) then
  print *, eF
  print *, any(x .lt. y, dim=2)
  stop 6
endif
if (.not. any(eB .eqv. any(x .le. y, dim=2))) stop 7

if (.not. any(eC .eqv. any(x .le. y, dim=3))) stop 8

if (.not. any(eD .eqv. any(x .le. y, dim=4))) stop 9

end

