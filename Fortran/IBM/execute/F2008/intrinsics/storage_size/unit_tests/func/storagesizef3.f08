!!! Test different character types

integer result

character(4), allocatable :: c1
character(:), allocatable :: c2
character(:), allocatable :: c3(:)
character(4) :: c4(4)

result = storage_size(c1)
if (result .ne. (sizeof(c1) * 8)) then
error stop 1_4
end if

allocate(character(4) :: c2)
result = storage_size(c2)
if (result .ne. (sizeof(c2) * 8)) then
error stop 2_4
end if

allocate(character(4) :: c3(2))
result = storage_size(c3)
if (result .ne. (sizeof(c3(1)) * 8)) then
error stop 3_4
end if

call sub1(c2, 4)
call sub2(c2)
call sub3(c3)
call sub4(c4, 4)

contains
subroutine sub1(arg1, n)
integer n
character(n) :: arg1
result = storage_size(kind=8, a=arg1)
if (result .ne. (sizeof(arg1) * 8)) then
error stop 4_4
end if
end subroutine sub1

subroutine sub2(arg2)
character(:), allocatable :: arg2
result = storage_size(arg2)
if (result .ne. (sizeof(arg2) * 8)) then
error stop 5_4
end if
end subroutine sub2

subroutine sub3(arg3)
character(*) :: arg3(*)
result = storage_size(arg3, kind=4)
if (result .ne. (sizeof(arg3(1)) * 8)) then
error stop 6_4
end if
end subroutine sub3

subroutine sub4(arg4, n)
integer n
character(4) :: arg4(n)
result = storage_size(arg4)
if (result .ne. (sizeof(arg4(1)) * 8)) then
error stop 7_4
end if
end subroutine sub4
end
