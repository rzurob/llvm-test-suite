!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : UNPACK intrinsic
!*
!* DESCRIPTION                : complex type
!* ===================================================================

implicit none

integer :: i, j

logical, parameter :: T=.true., F=.false.

logical, parameter :: msk4(3,3)=reshape((/F,T,F,T,F,F,T,F,T/), (/3,3/))
complex(4), parameter :: v4(4)=(/ &
 & (1.096056, 14.858052), (8.538667, 11.030550), &
 & (18.868319, 17.025129), (14.899953, 6.526642)/)
complex(4), parameter :: fld4(3,3)=reshape((/ &
 & (4.641009, 10.480580), (13.397491, 5.739541), (8.936218, 5.580615), &
 & (15.453114, 0.848388), (14.009636, 12.144561), (7.780076, 1.848756), &
 & (20.927754, 4.096945), (6.782803, 20.192088), (18.010542, 4.846253) &
 & /),(/3,3/))
complex(4), dimension(3,3) :: res4=unpack(v4, msk4, fld4)

logical, parameter :: msk8(10,10)=reshape((/F, F, (T,i=1,96), F, F/), (/10,10/))
complex(8), parameter :: fld8(10,10)=reshape( &
  &  (/(cmplx(-1.0_8,i/5.1),i=1,50),(cmplx(-2.1_8,i*0.01),i=1,50)/), (/10,10/))
complex(8), parameter :: v8(100)=(/(cmplx(i),i=1,100)/)
complex(8), dimension(10,10) :: res8=unpack(v8, mask=msk8, field=fld8), &
  & res8a

logical, parameter :: msk16(3,7)=reshape( &
  & (/ (F,i=1,7), (T,i=8,14), (F,i=15,21) /), (/3,7/))
complex(16), parameter :: fld16(3,7)=reshape( &
  & (/((i,-1.1Q0),i=1,7), ((i,2.0Q2),i=8,14), ((i,-3.0Q1),i=15,21) /), (/3,7/))
complex(16), parameter :: v16(7)=(/ &
  &  (10.670563, 10.424179), (6.261701, 19.733476), (10.900613, 19.079081), &
  &  (18.003928 , 10.536997), (21.379005, 17.220545), (10.516653, 3.196384), &
  &  (20.820013, 2.730921)/)
complex(16), dimension(3,7) :: res16=unpack(v16, msk16, fld16), res16a

if (.not. all(res4 .eq. unpack(v4, msk4, fld4))) stop 1

res8a = unpack(v8, mask=msk8, field=fld8)
if (.not. all(res8 .eq. res8a)) stop 2

res16a = unpack(v16, msk16, fld16)
if (.not. all(res16 .eq. res16a)) stop 3

end
