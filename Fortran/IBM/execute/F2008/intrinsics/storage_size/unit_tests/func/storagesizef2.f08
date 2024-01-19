!!! The argument of STORAGE_SIZE can be constant or a subobject of constant.
!!! It can be also scalar or array
integer result
integer, parameter :: i = 4
real, parameter :: r = 1.0
integer, parameter ::  ii(3) = [1,2,3]
character(len=10), parameter :: digits = '.0123456789.'
integer j

j = 4

result = storage_size(i)
if (result .ne. (sizeof(i) * 8)) then
error stop 1_4
end if

result = storage_size(r)
if (result .ne. (sizeof(r) * 8)) then
error stop 2_4
end if

result = storage_size(ii)
if (result .ne. (sizeof(i1) * 8)) then
error stop 3_4
end if

result = storage_size(ii(1))
if (result .ne. (sizeof(ii(1)) * 8)) then
error stop 4_4
end if

result = storage_size(a=ii(1:2), kind=2)
if (result .ne. (sizeof(ii(1)) * 8)) then
error stop 5_4
end if

result = storage_size(a=ii, kind=2)
if (result .ne. (sizeof(ii(1)) * 8)) then
error stop 6_4
end if

result = storage_size(8)
if (result .ne. (sizeof(8) * 8)) then
error stop 7_4
end if

result = storage_size(2_8)
if (result .ne. (sizeof(2_8) * 8)) then
error stop 8_4
end if

result = storage_size(2.0)
if (result .ne. (sizeof(2.0) * 8)) then
error stop 9_4
end if

result = storage_size(a=2.0D-200, kind=8)
if (result .ne. (sizeof(2.0D-200) * 8)) then
error stop 10_4
end if

result = storage_size(2.0Q-300)
if (result .ne. (sizeof(2.0Q-300) * 8)) then
error stop 11_4
end if

result = storage_size("abc")
if (result .ne. (sizeof("abc") * 8)) then
error stop 12_4
end if

result = storage_size(digits(1:j))
if (result .ne. (sizeof(digits(1:j)) * 8)) then
error stop 13_4
end if

end
