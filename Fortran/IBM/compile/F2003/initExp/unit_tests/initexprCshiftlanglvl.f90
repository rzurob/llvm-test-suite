!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : CSHIFT intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

real(4), dimension(6) :: cv4=cshift((/1,2,3,4,5,6/), 1)
real(8), parameter, dimension(6) :: v8=(/1,2,3,4,5,6/)
real(8), dimension(6) :: cv8=cshift((/1_8,2_8,3_8,4_8,5_8,6_8/), -1)
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

end
