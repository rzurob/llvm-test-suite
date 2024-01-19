!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : CSHIFT intrinsic
!*
!* DESCRIPTION                : real, integer and character types
!* ===================================================================

real(4), parameter, dimension(6) :: v4=(/1,2,3,4,5,6/)
real(4), dimension(6) :: cv4=cshift(v4, shift=-2)
real(8), parameter, dimension(6) :: v8=(/1,2,3,4,5,6/)
real(8), dimension(6) :: cv8=cshift(v8, shift=-2)
real(16), parameter, dimension(6) :: v16=(/1,2,3,4,5,6/)
real(16), dimension(6) :: cv16=cshift(v16, shift=-2)

integer(1), parameter, dimension(6) :: i1=(/1,2,3,4,5,6/)
integer(1), dimension(6) :: iv1=cshift(i1, shift=-2)
integer(2), parameter, dimension(6) :: i2=(/1,2,3,4,5,6/)
integer(2), dimension(6) :: iv2=cshift(i2, shift=-2)
integer(4), parameter, dimension(6) :: i4=(/1,2,3,4,5,6/)
integer(4), dimension(6) :: iv4=cshift(i4, shift=-2)
integer(8), parameter, dimension(6) :: i8=(/1,2,3,4,5,6/)
integer(8), dimension(6) :: iv8=cshift(i8, shift=-2)

character(2), parameter, dimension(5) :: c2=(/'ab','bc','cd','de','ef'/)
character(2), dimension(5) :: cv2=cshift(c2, 4)

if (.not. all(cv4 .eq. cshift(v4, shift=-2))) error stop 1
if (.not. all(cv8 .eq. cshift(v8, shift=-2))) error stop 2
if (.not. all(cv16 .eq. cshift(v16, shift=-2))) error stop 3

if (.not. all(iv1 .eq. cshift(i1, shift=-2))) error stop 4
if (.not. all(iv2 .eq. cshift(i2, shift=-2))) error stop 5
if (.not. all(iv4 .eq. cshift(i4, shift=-2))) error stop 6
if (.not. all(iv8 .eq. cshift(i8, shift=-2))) error stop 7

if (.not. all(cv2 .eq. cshift(c2, shift=4))) error stop 8
end
