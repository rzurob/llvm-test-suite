!* ===================================================================
!*
!* DATE                       : April 19, 2007
!*
!* PRIMARY FUNCTIONS TESTED   : real, cos and matmul intrinsics
!*
!* ===================================================================

implicit none

logical :: precision_r8
type base(baseKind, baseLen1, baseLen2)
   integer, kind :: baseKind
   integer, len  :: baseLen1, baseLen2
   real(baseKind) :: arr(baseLen1, baseLen2)
end type

real(kind = 8) :: arr(4,5), brr(5,4), res(4,4), res1(4,4)
integer i, j, k
type (base(8,5, 4)):: b
type (base(8,4,5)) :: c
do i = 1, 4
   do j = 1, 5
      arr(i,j) = sqrt(real(i * j))
      brr(j,i) = cos(real(i * j))
   end do
end do

res = matmul(arr, brr)
c%arr = arr
b%arr = brr
res1 = matmul(c%arr, b%arr)
if (.not. precision_r8(res, res1)) error stop 1
end
