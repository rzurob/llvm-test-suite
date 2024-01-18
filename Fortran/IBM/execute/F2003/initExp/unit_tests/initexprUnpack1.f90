!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : UNPACK intrinsic
!*
!* DESCRIPTION                : integer type
!* ===================================================================

implicit none

integer :: i, j

logical, parameter :: T=.true., F=.false.

integer, parameter :: v4(4)=(/5,6,7,8/)
logical, parameter :: msk4(3,3)=reshape((/F,T,F,T,F,F,T,F,T/), (/3,3/))
integer, parameter :: fld4(3,3)=reshape((/-1,-2,-3,-4,-5,-6,-7,-8,-9/),(/3,3/))

integer, dimension(3,3) :: res1=unpack(v4, msk4, fld4), &
  & res2=unpack(v4, msk4, field=0)

logical, parameter :: msk3(10,10)=reshape((/F, F, (T,i=1,96), F, F/), (/10,10/))
integer(8), parameter :: fld3(10,10)=reshape((/(-i,i=1,100)/), (/10,10/))
integer(8), parameter :: v3(100)=(/(i,i=1,100)/)
integer(8), dimension(10,10) :: res3=unpack(v3, mask=msk3, field=fld3), &
  & res3a

integer(1), parameter :: v1(84)=(/(i,i=1,84)/)
logical, parameter :: msk1(12,7)= &
  &  reshape((/F, F, (T,i=1,40), (F,j=1,40), T, T/), (/12,7/))
integer(1), parameter :: fld1(12,7)=reshape((/(125_1,i=1,84)/),(/12,7/))
integer(1) :: res11(12,7)=unpack(v1, mask=msk1, field=fld1), res11a(12,7)

integer(2), parameter :: v2(4)=(/1_2,3_2,5_2,7_2/)
logical, parameter :: msk2(2,2)=reshape((/T,F,T,F/), (/2,2/))
integer(2) :: res21(2,2)=unpack(v2, mask=msk2, field=0_2), res21a(2,2)

if (.not. all(res1 .eq. unpack(v4, msk4, fld4))) stop 1
if (.not. all(res2 .eq. unpack(v4, msk4, field=0))) stop 2

res3a = unpack(v3, mask=msk3, field=fld3)
if (.not. all(res3 .eq. res3a)) stop 3

res11a = unpack(v1, mask=msk1, field=fld1)
if (.not. all(res11 .eq. res11a)) stop 4

res21a = unpack(v2, mask=msk2, field=0_2)
if (.not. all(res21 .eq. res21a)) stop 5

end
