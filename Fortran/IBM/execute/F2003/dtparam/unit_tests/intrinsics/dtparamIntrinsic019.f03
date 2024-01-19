!* ===================================================================
!*
!* DATE                       : April 19, 2007
!*
!* PRIMARY FUNCTIONS TESTED   : all, any and abs intrinsics
!*
!* ===================================================================
module m
type base(k, l, m)
   integer, kind :: k
   integer, len  :: l, m
   real(k), pointer :: arr1(:)
end type

type, extends(base) :: child(n)
   integer, len :: n
   real(k) :: arr2(n)
   type(base(k, l, m)), pointer :: ptb
end type
end module

use m
implicit none
type(base(8, 2, 5)), target :: b
type(child(k=8, l=2, m=5, n=10)) :: c
real(8), target :: arr(10)
integer i, j, k
logical res

k = 0
do i = 1, c%l
   do j =1, c%m
      k = k + 1
      arr(k) = abs(cos(real(k))) * 10.0
   end do
end do

allocate (c%arr1(b%l * c%m), source = arr)
allocate (b%arr1(c%l * b%m), source =c%arr1)
c%ptb => b

if ((all(c%ptb%arr1 .lt. 10.0)).eqv. .false.) error stop 1

end
