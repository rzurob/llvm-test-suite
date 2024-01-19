!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ANY intrinsic
!*
!* DESCRIPTION                : integer type
!* ===================================================================

integer, parameter, dimension(3) :: a=(/1,2,3/), b=(/2,2,3/)
integer, parameter, dimension(4,4) :: &
  & x=reshape((/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/), (/4,4/)), &
  & y=reshape((/1,2,4,4,5,6,7,8,9,10,11,12,13,14,15,16/), (/4,4/))

logical :: cT=any(a .le. b)
logical :: cF=any(b .le. a)
logical :: dF=any(x .lt. y)
logical :: dT=any(x .le. y)

logical :: eT=any(a(2:3) .eq. b(2:3))

logical :: fF(4)=any(x .lt. y, dim=2)
logical :: fT(4)=any(x .le. y, dim=1)

if (cT .neqv. any(a .le. b)) error stop 1
if (cF .neqv. any(b .le. a)) error stop 2

if (dF .neqv. any(x .lt. y)) error stop 3
if (dT .neqv. any(x .le. y)) error stop 4

if (eT .neqv. any(a(2:3) .eq. b(2:3))) error stop 5

if (.not. all(fF .eqv. any(x .lt. y, dim=2))) then
  print *, dF
  print *, any(x .lt. y, dim=2)
  stop 6
endif
if (.not. all(fT .eqv. any(x .le. y, dim=1))) error stop 7
end
