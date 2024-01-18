!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : MINVAL intrinsic
!*
!* DESCRIPTION                : integer types
!* ===================================================================

implicit none

integer :: i

integer(1), parameter, dimension(6) :: A1=(/-41,12,33,-61,25,11/)

integer(2), parameter, dimension(2,3,3,2) :: &
  & A2=reshape((/(i,i=1,36)/), (/2,3,3,2/))

integer(4), parameter, dimension(2,3) :: &
  & A4=reshape((/-41,12,33,-61,25,11/), (/2,3/))

integer(8), parameter, dimension(2,2,2,2,2,2) :: &
  & A8=reshape((/(i,i=32,-31,-1)/), (/2,2,2,2,2,2/))

integer(1) :: res1=minval(A1)
integer(2) :: res2=minval(A2)

integer(4) :: res4=minval(A4)
integer :: res4a(3)=minval(A4, dim=1)
integer :: res4b(2)=minval(A4, dim=2)
integer :: res4c(2)=minval(A4, dim=2, mask=A4 .lt. 30)

integer(8) :: res8=minval(A8), res8a(2,2,2,2,2)=minval(A8, dim=3, mask=A8<0)

if (res1 .ne. minval(A1)) stop 1

if (res2 .ne. minval(A2)) stop 2

if (res4 .ne. minval(A4)) stop 3
if (.not. all(res4a .eq. minval(A4, dim=1))) stop 4
if (.not. all(res4b .eq. minval(A4, dim=2))) stop 5
if (.not. all(res4c .eq. minval(A4, dim=2, mask=A4 .lt. 30))) stop 6

if (res8 .ne. minval(A8)) stop 7
if (.not. all(res8a .eq. minval(A8, dim=3, mask=A8<0))) then
  stop 8
endif

end
