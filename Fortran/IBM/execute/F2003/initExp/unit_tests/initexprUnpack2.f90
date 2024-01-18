!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : UNPACK intrinsic
!*
!* DESCRIPTION                : logical type
!* ===================================================================

implicit none

integer :: i, j

logical, parameter :: T=.true., F=.false.

logical(4), parameter :: v4(4)=(/T,F,F,T/)
logical, parameter :: msk4(3,3)=reshape((/F,T,F,T,F,F,T,F,T/), (/3,3/))
logical(4), parameter :: fld4(3,3)=reshape((/T,T,T,F,F,F,T,T,T/),(/3,3/))

logical(4), dimension(3,3) :: res1=unpack(v4, msk4, fld4), &
  & res2=unpack(v4, msk4, field=.false._4)

logical, parameter :: msk3(10,10)=reshape((/F, F, (T,i=1,96), F, F/), (/10,10/))
logical(8), parameter :: fld3(10,10)=reshape((/(T,i=1,50),(F,i=1,50)/), (/10,10/))
logical(8), parameter :: v3(100)=(/(F,i=1,100)/)
logical(8), dimension(10,10) :: res3=unpack(v3, mask=msk3, field=fld3), &
  & res3a

logical(1), parameter :: v1(84)=(/(T,i=1,84)/)
logical, parameter :: msk1(12,7)= &
  &  reshape((/F, F, (T,i=1,40), (F,j=1,40), T, T/), (/12,7/))
logical(1), parameter :: fld1(12,7)=reshape((/(F,i=1,84)/),(/12,7/))
logical(1) :: res11(12,7)=unpack(v1, mask=msk1, field=fld1), res11a(12,7)

logical(2), parameter :: v2(4)=(/T,F,T,T/)
logical, parameter :: msk2(2,2)=reshape((/T,F,T,F/), (/2,2/))
logical(2) :: res21(2,2)=unpack(v2, mask=msk2, field=.true._2), res21a(2,2)

if (.not. all(res1 .eqv. unpack(v4, msk4, fld4))) stop 1
if (.not. all(res2 .eqv. unpack(v4, msk4, field=.false._4))) stop 2

res3a = unpack(v3, mask=msk3, field=fld3)
if (.not. all(res3 .eqv. res3a)) stop 3

res11a = unpack(v1, mask=msk1, field=fld1)
if (.not. all(res11 .eqv. res11a)) stop 4

res21a = unpack(v2, mask=msk2, field=.true._2)
if (.not. all(res21 .eqv. res21a)) stop 5

end
