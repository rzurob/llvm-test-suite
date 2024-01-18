!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : DIM intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

integer(1), parameter :: i1a=18, i1b=72
integer(2), parameter :: i2a=61, i2b=95
integer(4), parameter :: i4a=202, i4b=85
integer(8), parameter :: i8a=31, i8b=10

integer(1) :: i1z=dim(i1a,i1b)
integer(2) :: i2z=dim(i2a,i2b)
integer(4) :: i4z=dim(i4a,i4b)
integer(8) :: i8z=dim(i8a,i8b)

real(4), parameter :: r4a=2.76, r4b=1.522
real(8), parameter :: r8a=2.076661410D-34, r8b=1.927647832D-37
real(16), parameter :: r16a=1922.0q-2, r16b=1992.0Q-2

real(4) :: r4z=dim(r4a,r4b)
real(8) :: r8z=dim(r8a,r8b)
real(16) :: r16z=dim(r16a,r16b)

end
