!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : MATMUL intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

real(4), dimension(2,3) :: A4
real(4), dimension(3,2) :: B4

real(8), dimension(10,10), parameter :: &
  &  A8=reshape((/(i,i=101,200)/), (/10,10/))
real(8), dimension(10,211), parameter :: &
  &  B8=reshape((/(i,i=1,2110)/), (/10,211/))

real(16), dimension(101,12), parameter :: &
  &  A16=reshape((/(i,i=1,1212)/), (/101,12/))
real(16), dimension(12,211), parameter :: &
  &  B16=reshape((/(i,i=-2532,-1)/), (/12,211/))

parameter (A4=reshape((/1,4,2,5,3,6/),(/2,3/)))
parameter (B4=reshape((/7,8,9,10,11,12/),(/3,2/)))

real(4), dimension(2,2) :: res4a=matmul(A4, B4), res4b
real(8), dimension(10,211) :: res8a=matmul(A8, B8), res8b
real(16), dimension(101,211) :: res2a=matmul(A16, B16), res2b

res2b = matmul(A16,B16)
if (.not. all(res2a .eq. res2b)) stop 1

res4b = matmul(A4,B4)
if (.not. all(res4a .eq. res4b)) stop 2

res8b = matmul(A8,B8)
if (.not. all(res8a .eq. res8b)) stop 3

end
