!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ALL intrinsic
!*
!* DESCRIPTION                : integer type, DIM arg
!* ===================================================================

integer, parameter, dimension(3) :: a=(/1,2,3/), b=(/2,2,3/)
integer, parameter, dimension(4,4) :: &
  & x=reshape((/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/), (/4,4/)), &
  & y=reshape((/1,2,4,4,5,6,7,8,9,10,11,12,13,14,15,16/), (/4,4/))

logical :: cT=all(a .le. b)
logical :: cF=all(b .le. a)
logical :: dF=all(x .lt. y)
logical :: dT=all(x .le. y)
logical :: eF(4)=all(x .lt. y, dim=2)
logical :: eT(4)=all(x .le. y, dim=1)

logical :: fT=all(a(2:3) .eq. b(2:3))

if (cT .neqv. all(a .le. b)) stop 1
if (cF .neqv. all(b .le. a)) stop 2

if (dF .neqv. all(x .lt. y)) stop 3
if (dT .neqv. all(x .le. y)) stop 4

if (fT .neqv. all(a(2:3) .eq. b(2:3))) stop 5

if (.not. all(eF .eqv. all(x .lt. y, dim=2))) then
  print *, eF
  print *, all(x .lt. y, dim=2)
  stop 6
endif
if (.not. all(eT .eqv. all(x .le. y, dim=1))) stop 7

end

