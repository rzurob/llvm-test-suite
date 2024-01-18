!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : EOSHIFT intrinsic
!*
!* DESCRIPTION                : integer type
!* ===================================================================

implicit none

integer(1),dimension(4) :: i1=eoshift((/1_1,12_1,3_1,14_1/), &
  & shift=1, boundary=-1_1)

integer(2), dimension(2,2), parameter :: A2=reshape( &
  & (/12_2,23_2,34_2,45_2/),(/2,2/))
integer(2),dimension(2,2) :: i2=eoshift(A2, shift=(/1,-1/), boundary=100_2)

integer(4),dimension(4) :: i4=eoshift((/1,2,3,4/), shift=1, boundary=-1)
integer(4),dimension(4,4) :: i4a=eoshift(reshape((/1,2,3,4,5,6,7,8,9,&
  & 10,11,12,13,14,15,16/),(/4,4/)), shift=(/1,-1,0,2/), &
boundary=(/-1,-2,-3,-4/))
integer(4),dimension(2,2,2,2) :: i4b=eoshift(reshape((/1,2,3,4,5,6,7,8,9,&
  & 10,11,12,13,14,15,16/),(/2,2,2,2/)), &
  & shift=reshape((/1,-1,0,2,0,0,0,0/),(/2,2,2/)), dim=1)

if (.not. all(i4 .eq. eoshift((/1,2,3,4/), shift=1, boundary=-1))) stop 1
if (.not. all(i4a .eq. eoshift(reshape((/1,2,3,4,5,6,7,8,9, &
  & 10,11,12,13,14,15,16/),(/4,4/)), shift=(/1,-1,0,2/), &
  & boundary=(/-1,-2,-3,-4/)))) stop 2

if (.not. all(i4b .eq. eoshift(reshape((/1,2,3,4,5,6,7,8,9, &
  & 10,11,12,13,14,15,16/),(/2,2,2,2/)), &
  & shift=reshape((/1,-1,0,2,0,0,0,0/),(/2,2,2/)), dim=1))) stop 3

if (.not. all(i1 .eq. eoshift((/1_1,12_1,3_1,14_1/), &
  & shift=1, boundary=-1_1))) stop 4

if (.not. all(i2 .eq. eoshift(A2, shift=(/1,-1/), boundary=100_2))) stop 5

end
