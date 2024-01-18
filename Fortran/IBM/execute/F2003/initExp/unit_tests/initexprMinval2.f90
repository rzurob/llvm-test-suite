!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : MINVAL intrinsic
!*
!* DESCRIPTION                : real types
!* ===================================================================

implicit none

integer :: i

real(4), parameter, dimension(2,3) :: &
  & A4=reshape((/-41,12,33,-61,25,11/), (/2,3/))

real(8), parameter, dimension(2,2,2,2,2,2) :: &
  & A8=reshape((/(i,i=32,-31,-1)/), (/2,2,2,2,2,2/))

real(16), parameter, dimension(17) :: &
  & A16=(/20612068.Q84, 13701416.Q42, 9817824.Q10, 242798.q80, 8876313.q67, &
  &       1937925.q82, 9098313.q92, 10089890.q63, 13112395.q32, 20012776.q93, &
  &       14699567.q94, 2616147.q56, 21375206.q60, 16080190.q32, 6334616.q34, &
  &       14059258.q47, 333678.q94 /)

real(4) :: res4=minval(A4)
real :: res4a(3)=minval(A4, dim=1)
real :: res4b(2)=minval(A4, dim=2)
real :: res4c(2)=minval(A4, dim=2, mask=A4 .lt. 30)

real(8) :: res8=minval(A8), res8a(2,2,2,2,2,2)=minval(A8, mask=A8<0.0)

real(16) :: res16=minval(A16), &
         &  res16a=minval(A16, mask=A16>1.0Q40)

if (res4 .ne. minval(A4)) error stop 1
if (.not. all(res4a .eq. minval(A4, dim=1))) error stop 2
if (.not. all(res4b .eq. minval(A4, dim=2))) error stop 3
if (.not. all(res4c .eq. minval(A4, dim=2, mask=A4 .lt. 30))) error stop 4

if (res8 .ne. minval(A8)) error stop 5
if (.not. all(res8a .eq. minval(A8, mask=A8<0.0))) then
  stop 6
endif

if (res16 .ne. minval(A16)) error stop 7
if (res16a .ne. minval(A16, mask=A16>1.0Q40)) error stop 8

end
