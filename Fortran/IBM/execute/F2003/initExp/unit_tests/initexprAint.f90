!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : AINT intrinsic
!*
!* DESCRIPTION                : complex type
!* ===================================================================

logical precision_r4, precision_r8, precision_r16

real(4), parameter :: r4=23.45e0
real(8), parameter :: r8=-0.6822345d7
real(16), parameter :: r16=23.1427645q23

real(4) :: a4=aint(r4), b4
real(8) :: a8=aint(r8), b8
real(16) :: a16=aint(r16), b16

b4 = aint(r4)
b8 = aint(r8)
b16 = aint(r16)

if (.not. precision_r4(a4, b4)) stop 1
if (.not. precision_r8(a8, b8)) stop 2
if (.not. precision_r16(a16, b16)) stop 3
end
