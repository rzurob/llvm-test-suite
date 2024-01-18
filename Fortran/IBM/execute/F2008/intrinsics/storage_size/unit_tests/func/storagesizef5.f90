!!! Test derived type with type parameter

integer result

type base (k, l)
  integer, kind :: k
  integer, len :: l
  integer(k) :: i1(l)
  real, allocatable :: r1
  character(l) :: c1
end type

type, extends (base) :: child(l2)
  integer, len :: l2
  character :: c2
  integer :: i2
end type

type(base(2, 4)) :: b1
type(child(2, 4, 2)) :: c1
class(base(2, :)), allocatable :: bb1(:)

result = storage_size(b1)
if (result .ne. (sizeof(b1) * 8)) then
error stop 1_4
end if

result = storage_size(c1)
if (result .ne. (sizeof(c1) * 8)) then
error stop 2_4
end if

allocate(child(2, 4, 2) :: bb1(4))
result = storage_size(bb1)
if (result .ne. (sizeof(bb1(1)) * 8)) then
error stop 3_4
end if

call sub1(b1, 2)
call sub2(c1, 4, 2)
call sub3(c1)
call sub4(bb1)
call sub5(bb1)

contains

subroutine sub1(arg1, n)
integer n
type(base(2, n)) :: arg1
result = storage_size(kind=4, a=arg1)
if (result .ne. (sizeof(arg1) * 8)) then
error stop 4_4
end if
end

subroutine sub2(arg2, n1, n2)
integer :: n1, n2
type(child(2, n1, n2)) :: arg2
result = storage_size(arg2)
if (result .ne. (sizeof(arg2) * 8)) then
error stop 5_4
end if
end

subroutine sub3(arg3)
type(child(2, *, *)) :: arg3
result = storage_size(arg3, kind=8)
if (result .ne. (sizeof(arg3) * 8)) then
error stop 6_4
end if
end

subroutine sub4(arg4)
class(*) :: arg4(*)
result = storage_size(arg4)
if (result .ne. (sizeof(arg4(1)) * 8)) then
error stop 7_4
end if
end

subroutine sub5(arg5)
class(base(2, :)), allocatable :: arg5(:)
result = storage_size(arg5)
if (result .ne. (sizeof(arg5(1)) * 8)) then
error stop 8_4
end if
end
