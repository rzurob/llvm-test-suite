!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ALL intrinsic
!*
!* DESCRIPTION                : real type, DIM arg
!* ===================================================================

real(8), parameter, dimension(2,2,2,2) :: &
  & x=reshape((/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/), (/2,2,2,2/)), &
  & y=reshape((/1,2,4,4,5,6,7,8,9,10,11,12,13,14,15,16/), (/2,2,2,2/))

logical(8) :: dF=all(x .lt. y)
logical(8) :: dT=all(x .le. y)
logical(8) :: eA(2,2,2)=all(x .lt. y, dim=1)
logical(8) :: eB(2,2,2)=all(x .le. y, dim=2)
logical(8) :: eC(2,2,2)=all(x .le. y, dim=3)
logical(8) :: eD(2,2,2)=all(x .le. y, dim=4)

if (dF .neqv. all(x .lt. y)) error stop 1
if (dT .neqv. all(x .le. y)) error stop 2

if (.not. all(eA .eqv. all(x .lt. y, dim=1))) then
  print *, eF
  print *, all(x .lt. y, dim=2)
  stop 6
endif
if (.not. all(eB .eqv. all(x .le. y, dim=2))) error stop 7

if (.not. all(eC .eqv. all(x .le. y, dim=3))) error stop 8

if (.not. all(eD .eqv. all(x .le. y, dim=4))) error stop 9

end
