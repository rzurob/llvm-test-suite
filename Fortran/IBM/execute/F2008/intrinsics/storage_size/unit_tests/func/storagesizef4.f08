!!! Test derived type size

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

type(base) :: b1
type(child) :: c1
class(base), pointer :: bb1

result = storage_size(b1)
if (result .ne. (sizeof(b1) * 8)) then
error stop 1_4
end if

result = storage_size(c1)
if (result .ne. (sizeof(c1) * 8)) then
error stop 2_4
end if

allocate(child :: bb1)
result = storage_size(bb1)
if (result .ne. (sizeof(bb1) * 8)) then
error stop 3_4
end if
end
