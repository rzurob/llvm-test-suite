!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : MATMUL intrinsic
!*
!* DESCRIPTION                : integer type
!* ===================================================================

integer(1), dimension(2,11), parameter :: &
  & A1=reshape((/(i*2,i=1,22)/),(/2,11/))
integer(1), dimension(11,1), parameter :: &
  & B1=reshape((/(i*i,i=1,11)/),(/11,1/))

integer(2), dimension(101,12), parameter :: &
  &  A2=reshape((/(i,i=1,1212)/), (/101,12/))
integer(2), dimension(12,211), parameter :: &
  &  B2=reshape((/(i,i=-2532,-1)/), (/12,211/))

integer(4), dimension(2,3) :: A4
integer(4), dimension(3,2) :: B4

integer(8), dimension(10,10), parameter :: &
  &  A8=reshape((/(i,i=101,200)/), (/10,10/))
integer(8), dimension(10,211), parameter :: &
  &  B8=reshape((/(i,i=1,2110)/), (/10,211/))

parameter (A4=reshape((/1,4,2,5,3,6/),(/2,3/)))
parameter (B4=reshape((/7,8,9,10,11,12/),(/3,2/)))

integer(1), dimension(2,1) :: res1a=matmul(A1, B1), res1b
integer(2), dimension(101,211) :: res2a=matmul(A2, B2), res2b
integer(4), dimension(2,2) :: res4a=matmul(A4, B4), res4b
integer(8), dimension(10,211) :: res8a=matmul(A8, B8), res8b

res1b = matmul(A1,B1)
if (.not. all(res1a .eq. res1b)) stop 1

res2b = matmul(A2,B2)
if (.not. all(res2a .eq. res2b)) stop 2

res4b = matmul(A4,B4)
if (.not. all(res4a .eq. res4b)) stop 3

res8b = matmul(A8,B8)
if (.not. all(res8a .eq. res8b)) stop 4


end
