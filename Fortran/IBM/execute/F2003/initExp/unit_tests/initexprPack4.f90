!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : PACK intrinsic
!*
!* DESCRIPTION                : complex type
!* ===================================================================

implicit none
integer :: i

complex(4), parameter :: i0(3,2)=reshape((/(1.2,9.1), (5.5,0.0), (5.0,6.1),  &
  & (1.4,2.9), (5.2,2.4), (7.0,3.3)/),(/3,2/))
complex(4) :: i4(5)=pack(i0, mask=i0 .ne. (5.5,0.0))
complex(4) :: i4a(6)=pack(i0, mask=i0 .ne. (3.0,3.3), &
  & vector=(/(-1.,0.),(-2.,0.),(-3.,0.),(-4.,0.),(-5.,0.),(-6.,0.)/))

complex(8), parameter, dimension(2,3) :: &
  & a0=reshape((/(0.161_8,9.4_8), (1.6419_8,8.72949_8),(9.80739_8,7.4_8), &
  &              (18.25_8,1.77_8), (3.206002_8,1.147_8), (1.33_8,3.1_8)/),(/2,3/))
complex(8) :: a4(6)=pack(a0, mask=.true.)

complex(16), parameter, dimension(2,2,2,2,2,1) :: &
  & c0=reshape((/(cmplx(i,-i),i=1,32)/),(/2,2,2,2,2,1/))
complex(16) :: c1(32)=pack(c0, mask=.true.)

if (.not. all(i4 .eq. pack(i0, mask=i0 .ne. (5.5,0.0)))) stop 1
if (.not. all(i4a .eq. pack(i0, mask=i0 .ne. (3.0,3.3), &
  &  vector=(/(-1.,0.),(-2.,0.),(-3.,0.),(-4.,0.),(-5.,0.),(-6.,0.)/)))) stop 2

if (.not. all(a4 .eq. pack(a0, mask=.true.))) stop 3

if (.not. all(c1 .eq. pack(c0, mask=.true.))) stop 5
end
