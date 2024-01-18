!!! Test derived type size
use, intrinsic :: iso_c_binding
integer result

type base
  integer :: i1
  real, allocatable :: r1
  character(3) :: c1
end type

type, extends (base) :: child
  character :: c2
  integer :: i2
end type

type(base), target :: b1(2)
type(child), target :: c1(2)
class(base), pointer :: bb1(:)

result = storage_size(b1)
if (result .ne. ((loc(b1(2)) - loc(b1(1))) * 8)) then
error stop 1_4
end if

result = storage_size(c1)
if (result .ne. ((loc(c1(2)) - loc(c1(1))) * 8)) then
error stop 2_4
end if

allocate(child :: bb1(2))
result = storage_size(bb1)
if (result .ne. ((loc(bb1(2)) - loc(bb1(1))) * 8)) then
error stop 3_4
end if
end
