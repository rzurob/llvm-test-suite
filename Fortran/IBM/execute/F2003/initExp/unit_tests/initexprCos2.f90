!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : COS intrinsic
!*
!* DESCRIPTION                : array argument
!* ===================================================================
implicit none
logical precision_r4, precision_r8, precision_r16
integer :: i

real(4), dimension(2), parameter :: r4=(/3.0E0, 4.0E0/)
real(8), dimension(2), parameter :: r8=(/3.0D0, 4.0D0/)
real(16), dimension(2), parameter :: r16=(/3.0Q0, 4.0Q0/)

real(4), dimension(2) :: a=cos(r4)
real(8), dimension(2) :: b=cos(r8)
real(16), dimension(2) :: c=cos(r16)

do i=1, 2
  if (.not. precision_r4(a(i), cos(r4(i)))) stop 1

  if (.not. precision_r8(b(i), cos(r8(i)))) stop 2

  if (.not. precision_r16(c(i), cos(r16(i)))) stop 3
enddo

end
